use std::collections::{HashMap, HashSet};

use crate::ir::{StatementId, VariableId, Function};

use super::flow::ControlFlowGraph;

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

    /// Converts the results of this liveness into a list of live intervals for each variables,
    /// using the provided [ControlFlowGraph] to provide an ordering for statements.
    /// 
    /// Each value in the returned mapping is of the form `(start, end)` - both are inclusive.
    /// `start` is the statement where a variable is defined, and `end` is the last statement it is
    /// used in.
    /// 
    /// The results are suitable for linear scanning register allocation.
    pub fn live_intervals(&self, cfg: &ControlFlowGraph) -> HashMap<VariableId, (StatementId, StatementId)> {
        let statement_ordering = cfg.statement_ordering();

        // Calculate intervals by keeping track of highest `in` index, and lowest `out` index, for
        // each variable
        let mut ends = HashMap::new();
        for (stmt_id, vars) in &self.live_in {
            for var in vars {
                if let Some(existing) = ends.get(var) {
                    if statement_ordering[existing] < statement_ordering[stmt_id] {
                        ends.insert(*var, *stmt_id);    
                    }
                } else {
                    ends.insert(*var, *stmt_id);
                }
            }
        }

        let mut starts = HashMap::new();
        for (stmt_id, vars) in &self.live_out {
            for var in vars {
                if let Some(existing) = starts.get(var) {
                    if statement_ordering[existing] > statement_ordering[stmt_id] {
                        starts.insert(*var, *stmt_id);    
                    }
                } else {
                    starts.insert(*var, *stmt_id);
                }
            }
        }

        // Sanity check...
        // We expect the `starts` and `ends` sets to have exactly the same sets of keys, as 
        // every variable which starts existing should also stop existing at some point!
        let starts_keys = starts.keys().collect::<HashSet<_>>();
        let ends_keys = ends.keys().collect::<HashSet<_>>();
        if starts_keys != ends_keys {
            unreachable!("start and end sets do not match: {starts_keys:?}, {ends_keys:?}");
        }

        // Merge the two sets into the resulting map
        let mut map = HashMap::new();
        for var in starts.keys() {
            map.insert(*var, (starts[var], ends[var]));
        }
        map
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
                    .flat_map(|succ| in_map[succ].clone())
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
            // One late addition we'll make - parameters need to come from _somewhere_, else we end
            // up with weird inconsistencies between which variables exist.
            // To make our life easy, let's pretend that function arguments are live all the time.
            for arg in &func.arguments {
                for stmt in func.statements() {
                    in_map.get_mut(&stmt.id).unwrap().insert(*arg);
                    out_map.get_mut(&stmt.id).unwrap().insert(*arg);
                }
            }

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
        let func = FunctionBuilder::new("foo", &[]);
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
