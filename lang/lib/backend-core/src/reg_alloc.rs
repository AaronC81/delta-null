use std::collections::{HashMap, HashSet};

use delta_null_core_instructions::GPR;
use delta_null_lang_backend::{ir::{VariableId, Function}, analysis::{liveness::LivenessAnalysis, flow::ControlFlowGraph}};

use crate::{codegen::info::in_place_usage_variables, reg_pref::RegisterPreferences, PARAMETER_PASSING_REGISTERS};

/// Describes how an IR variable was allocated onto the core during a function's execution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Allocation {
    /// This variable can live in a general-purpose register for its whole lifetime.
    Register(GPR),

    /// There is no general-purpose register which can hold this value for its whole lifetime, and
    /// it will be stored on the stack instead. The given number is an increasing word index
    /// describing where it shall reside.
    Spill(usize),
}

/// Allocates IR variables to concrete locations during the function's execution - registers if
/// possible, falling back to the stack.
/// 
/// This is an implementation of linear scanning, as described by this journal article:
///   http://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf
/// Specifically following the pseudocode in §4.1.
pub fn allocate(func: &Function, cfg: &ControlFlowGraph, liveness: &LivenessAnalysis, reg_prefs: &RegisterPreferences) -> HashMap<VariableId, Allocation> {
    let intervals = liveness.live_intervals(cfg);
    let indexes = cfg.statement_ordering();

    let mut mapping = HashMap::new();
    let mut next_spill_index = 0;

    // For iteration convenience, convert map:
    //   var => (start, end)
    // into vec:
    //   (var, (start, end))
    // sorted by `start`
    let mut internals_by_increasing_start = intervals.iter().collect::<Vec<_>>();
    internals_by_increasing_start.sort_by_key(|(_, (start, _))| indexes[start]);

    // Set up our register tracker
    let mut regs = RegisterAllocator::new(reg_prefs);

    // Set up vector to track active allocations
    let mut active = vec![];

    // Allocate any registers for parameters first
    // These aren't really "allocated", more so "required" - the values are *already there* once the
    // function is called, so we have to make sure we don't trash them
    for (var, reg) in func.arguments.iter().zip(PARAMETER_PASSING_REGISTERS) {
        // If the parameter isn't used for the entire function, we can free up the register for
        // other variables later on
        let Some((start, end)) = intervals.get(var).copied() else {
            // The parameter has no interval - it was probably never used. We don't really have a
            // way of checking this at this stage... assume liveness analysis did its job right and
            // skip this parameter.
            continue;
        };
        mapping.insert(*var, Allocation::Register(reg));
        active.push((*var, start, end));
        active.sort_by_key(|(_, _, end)| indexes[end]);
        regs.take(reg);
        internals_by_increasing_start.extract_if(|(v, _)| v == &var).for_each(|_| ());
    }

    for (var, (start, end)) in internals_by_increasing_start {
        // Expire old intervals
        active.retain(|(active_var, _, active_end)| {
            if indexes[active_end] >= indexes[start] {
                return true;
            }
            if let Allocation::Register(r) = mapping[active_var] {
                regs.free(r);
            }
            false
        });

        // Optimisation: does the statement which introduces this variable support in-place usage?
        let mut force_allocation = None;
        let start_stmt = func.get_statement(*start);
        if let Some(candidates) = in_place_usage_variables(&start_stmt.instruction.kind) {
            // Are any of the parameters stored in registers, which are going to expire immediately
            // after this statement?
            let possible_regs = candidates.into_iter()
                .filter_map(|var| 
                    if let Allocation::Register(reg) = mapping[&var] {
                        Some((var, reg))
                    } else {
                        None
                    }
                )
                .filter(|(var, _)|
                    active.iter().any(|(active_var, _, active_end)|
                        active_var == var && active_end == start
                    )
                )
                .collect::<Vec<_>>();

            if let Some((reuse_var, reuse_reg)) = possible_regs.first() {
                // Yes! We can re-use this one
                force_allocation = Some(*reuse_reg);
                
                // Purge the active allocation for this variable.
                // This prevents it from being freed on the next interval - we don't want that
                // because it would release our re-used register back into the pool
                active.retain(|(var, _, _)| var != reuse_var);
            }
        }

        // Allocate this variable
        if let Some(reg) = force_allocation.or_else(|| regs.allocate(*var)) {
            mapping.insert(*var, Allocation::Register(reg));
            active.push((*var, *start, *end));
            active.sort_by_key(|(_, _, end)| indexes[end]);
        } else {
            // Spill at interval
            let (spill_var, _, spill_end) = *active.last().unwrap();
            if indexes[&spill_end] > indexes[end] {
                mapping.insert(*var, mapping[&spill_var].clone());
                mapping.insert(spill_var, Allocation::Spill(next_spill_index));
                next_spill_index += 1;
                active.retain(|(v, _, _)| v != &spill_var);

                active.push((*var, *start, *end));
                active.sort_by_key(|(_, _, end)| indexes[end]);
            } else {
                mapping.insert(*var, Allocation::Spill(next_spill_index));
                next_spill_index += 1;
            }
        }
    }

    mapping
}

/// Tracks registers and allocation preferences.
struct RegisterAllocator<'p> {
    preferences: &'p RegisterPreferences,

    all_preferred: HashSet<GPR>,
    all_rest: HashSet<GPR>,

    remaining_preferred: Vec<GPR>,
    remaining_rest: Vec<GPR>,
}

impl<'p> RegisterAllocator<'p> {
    /// Creates a new allocator based on the given preferences.
    pub fn new(preferences: &'p RegisterPreferences) -> Self {
        let remaining_preferred = preferences.all_registers()
            .into_iter().collect::<Vec<_>>();
        let remaining_rest = GPR::all().rev()
            .filter(|r| !remaining_preferred.contains(r)).collect::<Vec<_>>();

        Self {
            preferences,
            all_preferred: remaining_preferred.iter().copied().collect(),
            all_rest: remaining_rest.iter().copied().collect(),
            remaining_preferred,
            remaining_rest,
        }
    }

    /// Allocates a register for the given variable.
    pub fn allocate(&mut self, var: VariableId) -> Option<GPR> {
        if let Some(pref_reg) = self.preferences.get(var) && self.remaining_preferred.contains(&pref_reg) {
            self.remaining_preferred.retain(|r| *r != pref_reg);
            return Some(pref_reg)
        } else if let Some(reg) = self.remaining_rest.pop() {
            Some(reg)
        } else if let Some(reg) = self.remaining_preferred.pop() {
            // If we really need to, we can 'pinch' a preferred register for a non-preferred allocation
            Some(reg)
        } else {
            None
        }
    }

    /// "Steals" a register allocation for no particular variable.
    pub fn take(&mut self, gpr: GPR) {
        self.remaining_preferred.retain(|r| *r != gpr);
        self.remaining_rest.retain(|r| *r != gpr);
    }

    /// Return a register so that it may be allocated again. *Must* have been removed from the
    /// allocator with [allocate] or [take] first.
    pub fn free(&mut self, gpr: GPR) {
        if self.all_preferred.contains(&gpr) {
            self.remaining_preferred.push(gpr);
        } else if self.all_rest.contains(&gpr) {
            self.remaining_rest.push(gpr);
        } else {
            unreachable!("register {gpr:?} belongs to neither set");
        }
    }
}
