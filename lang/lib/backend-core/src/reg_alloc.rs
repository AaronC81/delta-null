use std::collections::HashMap;

use delta_null_core_instructions::GPR;
use delta_null_lang_backend::{ir::{VariableId, Function}, analysis::{liveness::LivenessAnalysis, flow::ControlFlowGraph}};

use crate::{reg_pref::RegisterPreferences, PARAMETER_PASSING_REGISTERS};

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

    // Reverse iterator
    // We pop from this list, so the registers we should pick *first* go at the *end*.
    // Put any registers which specific variables would like to prefer to use at the *start*, so
    // they get picked *last*.
    let mut free_registers = reg_prefs.all_registers();
    for reg in GPR::all().rev() {
        if !free_registers.contains(&reg) {
            free_registers.push(reg);
        }
    }

    // Set up vector to track active allocations
    let mut active = vec![];

    // Allocate any registers for parameters first
    // These aren't really "allocated", more so "required" - the values are *already there* once the
    // function is called, so we have to make sure we don't trash them
    for (var, reg) in func.arguments.iter().zip(PARAMETER_PASSING_REGISTERS) {
        // If the parameter isn't used for the entire function, we can free up the register for
        // other variables later on
        let (start, end) = intervals[var];
        mapping.insert(*var, Allocation::Register(reg));
        active.push((*var, start, end));
        active.sort_by_key(|(_, _, end)| indexes[end]);
        free_registers.retain(|r| *r != reg);
        internals_by_increasing_start.extract_if(|(v, _)| v == &var).for_each(|_| ());
    }

    for (var, (start, end)) in internals_by_increasing_start {
        // Expire old intervals
        active.retain(|(active_var, _, active_end)| {
            if indexes[active_end] >= indexes[start] {
                return true;
            }
            if let Allocation::Register(r) = mapping[active_var] {
                free_registers.push(r);
            }
            false
        });

        // Allocate this variable
        if !free_registers.is_empty() {
            // If there is an available preferred allocation, use it - else just pop anything
            let reg;
            if let Some(pref_reg) = reg_prefs.get(*var) && free_registers.contains(&pref_reg) {
                free_registers.retain(|r| *r != pref_reg);
                reg = pref_reg;
            } else {
                reg = free_registers.pop().expect("free registers pop failed");
            }

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
