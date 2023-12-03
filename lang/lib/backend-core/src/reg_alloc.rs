// TODO: implement linear scan

use std::collections::HashMap;

use delta_null_core_instructions::GPR;
use delta_null_lang_backend::{ir::{VariableId, Function, StatementId}, analysis::{liveness::LivenessAnalysis, flow::ControlFlowGraph}};

/// Describes how an IR variable was allocated onto the core during a function's execution.
#[derive(Debug, Clone)]
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
pub fn allocate(func: &Function, cfg: &ControlFlowGraph, liveness: &LivenessAnalysis) -> HashMap<VariableId, Allocation> {
    let intervals = liveness.live_intervals(&cfg);
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

    // Reverse iterator, so we start with R0
    let mut free_registers = GPR::all().rev().collect::<Vec<_>>();
    let mut active = vec![];
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
        if let Some(free_reg) = free_registers.pop() {
            mapping.insert(*var, Allocation::Register(free_reg));
            active.push((*var, *start, *end));
            active.sort_by_key(|(_, _, end)| indexes[end]);
        } else {
            // Spill at interval
            let (spill_var, _, spill_end) = active.last().unwrap().clone();
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

#[cfg(test)]
mod test {
    use delta_null_core_instructions::ToAssembly;
    use delta_null_lang_backend::{ir::{FunctionBuilder, ConstantValue, Instruction, InstructionKind, PrintIR, PrintOptions}, analysis::{liveness::liveness_analysis, flow::ControlFlowGraph}};

    use crate::reg_alloc::Allocation;

    use super::allocate;

    #[test]
    fn test_smoke_allocate() {
        // TODO
        let mut func = FunctionBuilder::new("foo");
        let (_, mut block) = func.new_basic_block();
        let mut last = block.add_constant(ConstantValue::U16(123));
        for _ in 0..10 {
            let one = block.add_constant(ConstantValue::U16(1));
            last = block.add_instruction(Instruction::new(InstructionKind::Add(last, one)));
        }
        block.add_terminator(Instruction::new(InstructionKind::Return(Some(last))));
        block.finalize();
        let func = func.finalize();

        let analysis = liveness_analysis(&func);
        let cfg = ControlFlowGraph::generate(&func);

        let allocation = allocate(&func, &cfg, &analysis);
        
        let additional_variable_info = Some(allocation.iter()
            .map(|(v, a)| (*v, match a {
                Allocation::Register(r) => r.to_assembly(),
                Allocation::Spill(id) => format!("spill {id}"),
            }))
            .collect());
        panic!("{}", func.print_ir(&PrintOptions { additional_variable_info }));
    }
}
