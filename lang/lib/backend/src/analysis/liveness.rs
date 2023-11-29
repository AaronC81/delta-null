use std::collections::{HashMap, HashSet};

use crate::ir::{StatementId, VariableId, Function};

#[derive(Debug, Clone)]
pub struct LivenessAnalysis {
    live_in: HashMap<StatementId, HashSet<VariableId>>,
    live_out: HashMap<StatementId, HashSet<VariableId>>,
}

impl LivenessAnalysis {
    /// Given a statement, returns the set of variables which are live upon entry into the
    /// statement.
    pub fn live_in(&self, stmt: StatementId) -> &HashSet<VariableId> {
        &self.live_in[&stmt]
    }

    /// Given a statement, returns the set of variables which are live upon exit from the
    /// statement.
    pub fn live_out(&self, stmt: StatementId) -> &HashSet<VariableId> {
        &self.live_out[&stmt]
    }

    /// Shortcut for `live_in` and `live_out`, in that order.
    pub fn live_in_out(&self, stmt: StatementId) -> (&HashSet<VariableId>, &HashSet<VariableId>) {
        (self.live_in(stmt), self.live_out(stmt))
    }
}

/// Performs liveness analysis, determining which variables are "live" going into and coming out of
/// each statement.
pub fn liveness_analysis(func: &Function) -> LivenessAnalysis {
    // Sources for implementation:
    //   https://www.seas.upenn.edu/~cis3410/current/_static/lectures/lec22.pdf  (mainly)
    //   https://www.cl.cam.ac.uk/teaching/1718/OptComp/slides/lecture03.pdf

    // Build state maps, `in` and `out`
    let mut in_map = func.statements()
        .map(|stmt| (stmt.id, HashSet::new()))
        .collect::<HashMap<_, _>>();
    let mut out_map = in_map.clone();

    // Iterate
    loop {
        let previous_in_map = in_map.clone();
        let previous_out_map = out_map.clone();

        for stmt in func.statements() {
            // out[n] := ∪(n’∈succ[n]) in[n’]
            out_map.insert(stmt.id,
                func.statement_successors(stmt.id)
                    .iter()
                    .map(|succ| in_map[succ].clone())
                    .flatten()
                    .collect()
            );

            // in[n] := use[n] ∪ (out[n] - def[n])
            in_map.insert(stmt.id,
                stmt.instruction.referenced_variables()
                    .union(
                        &out_map[&stmt.id]
                            .difference(&stmt.result.into_iter().collect())
                            .copied()
                            .collect()
                    )
                    .copied()
                    .collect()
            );
        }
        
        // If we've found a fixed point, we're done!
        if previous_in_map == in_map && previous_out_map == out_map {
            return LivenessAnalysis {
                live_in: in_map,
                live_out: out_map,
            }
        }
    }
}

#[cfg(test)]
mod test {
    use maplit::hashset;

    use crate::ir::{FunctionBuilder, Instruction, InstructionKind, ConstantValue};

    use super::liveness_analysis;

    #[test]
    fn test_liveness_simple() {
        let mut func = FunctionBuilder::new("foo");
        let (block_id, mut block) = func.new_basic_block();
        let a = block.add_constant(ConstantValue::U16(123));
        let b = block.add_constant(ConstantValue::U16(456));
        let a_plus_b = block.add_instruction(Instruction::new(InstructionKind::Add(a, b)));
        let c = block.add_constant(ConstantValue::U16(789));
        let a_plus_c = block.add_instruction(Instruction::new(InstructionKind::Add(a, c)));
        block.add_terminator(Instruction::new(InstructionKind::Return(Some(a_plus_c))));
        block.finalize();
        let func = func.finalize();

        let analysis = liveness_analysis(&func);

        assert_eq!(
            (&hashset! {}, &hashset! { a }),
            analysis.live_in_out(func.statement_assigning_to(a).id)
        );
        assert_eq!(
            (&hashset! { a }, &hashset! { a, b }),
            analysis.live_in_out(func.statement_assigning_to(b).id)
        );
        assert_eq!(
            (&hashset! { a, b }, &hashset! { a }),
            analysis.live_in_out(func.statement_assigning_to(a_plus_b).id)
        );
        assert_eq!(
            (&hashset! { a }, &hashset! { a, c }),
            analysis.live_in_out(func.statement_assigning_to(c).id)
        );
        assert_eq!(
            (&hashset! { a, c }, &hashset! { a_plus_c }),
            analysis.live_in_out(func.statement_assigning_to(a_plus_c).id)
        );
        assert_eq!(
            (&hashset! { a_plus_c }, &hashset! { }),
            analysis.live_in_out(func.blocks[&block_id].terminator().id)
        );
    }

    // TODO: branching tests
}
