use std::collections::{HashMap, HashSet};

use crate::ir::{Function, BasicBlockId};

#[derive(Debug, Clone)]
pub struct ControlFlowGraph {
    /// Maps a block to the other blocks that it could branch to.
    outgoing: HashMap<BasicBlockId, HashSet<BasicBlockId>>,

    /// Maps a block to the other blocks which it could be reached from.
    incoming: HashMap<BasicBlockId, HashSet<BasicBlockId>>,
}

/// Describes which basic blocks may branch to each other.
impl ControlFlowGraph {
    /// Generate a control-flow graph showing the flow between the basic blocks of a function.
    pub fn generate(func: &Function) -> Self {
        // Create blank CFG with map entries for all IDs
        let initial_map = func.blocks.keys()
            .map(|id| (*id, HashSet::new()))
            .collect::<HashMap<_, _>>();
        let mut cfg = ControlFlowGraph {
            incoming: initial_map.clone(),
            outgoing: initial_map,
        };

        // Populate maps
        for (id, block) in &func.blocks {
            let dests = block.terminator().instruction.branch_destinations();

            for dest in dests {
                // Insert both ways around
                cfg.incoming.get_mut(&dest).unwrap().insert(*id);
                cfg.outgoing.get_mut(id).unwrap().insert(dest);
            }
        }

        cfg
    }

    /// Given a block, returns the set of blocks which it could branch to.
    pub fn outgoing_branches(&self, block: BasicBlockId) -> &HashSet<BasicBlockId> {
        self.outgoing.get(&block).unwrap()
    }

    /// Given a block, returns the set of blocks which could branch from themselves to it.
    pub fn incoming_branches(&self, block: BasicBlockId) -> &HashSet<BasicBlockId> {
        self.incoming.get(&block).unwrap()
    }

    /// Returns the set of the blocks which don't branch to another block, instead terminating in
    /// some way external to the function.
    pub fn leaf_blocks(&self) -> HashSet<BasicBlockId> {
        self.outgoing.iter()
            .filter_map(|(id, blk)|
                if blk.is_empty() {
                    Some(*id)
                } else {
                    None
                }
            )
            .collect()
    }
}

#[cfg(test)]
mod test {
    use maplit::hashset;

    use crate::ir::{FunctionBuilder, Instruction, InstructionKind, ConstantValue};

    use super::ControlFlowGraph;

    #[test]
    fn test_simple_flow() {
        let mut func = FunctionBuilder::new("foo");

        // 0 -> 1 -> 2
        let (ids, mut blocks) = func.new_basic_blocks(3);
        blocks[0].add_terminator(Instruction::new(InstructionKind::Branch(ids[1])));
        blocks[1].add_terminator(Instruction::new(InstructionKind::Branch(ids[2])));
        blocks[2].add_terminator(Instruction::new(InstructionKind::Return(None)));
        func.finalize_blocks(blocks);
        let func = func.finalize();

        let cfg = ControlFlowGraph::generate(&func);

        assert_eq!(cfg.outgoing_branches(ids[0]), &hashset!{ ids[1] });
        assert_eq!(cfg.outgoing_branches(ids[1]), &hashset!{ ids[2] });
        assert_eq!(cfg.outgoing_branches(ids[2]), &hashset!{ });
        
        assert_eq!(cfg.incoming_branches(ids[0]), &hashset!{ });
        assert_eq!(cfg.incoming_branches(ids[1]), &hashset!{ ids[0] });
        assert_eq!(cfg.incoming_branches(ids[2]), &hashset!{ ids[1] });

        assert_eq!(cfg.leaf_blocks(), hashset!{ ids[2] });
    }

    #[test]
    fn test_looping_flow() {
        let mut func = FunctionBuilder::new("foo");

        // 0 -> 1 -> 2 -> 4 -> 6
        //      ^    v    v    ^
        //      '--- 3 <- 5 ---'
        let (ids, mut blocks) = func.new_basic_blocks(7);

        let condition = blocks[0].add_instruction(Instruction::new(InstructionKind::Constant(ConstantValue::Boolean(true))));
        blocks[0].add_terminator(Instruction::new(InstructionKind::Branch(ids[1])));
        blocks[1].add_terminator(Instruction::new(InstructionKind::Branch(ids[2])));
        blocks[2].add_terminator(Instruction::new(InstructionKind::ConditionalBranch { condition, true_block: ids[3], false_block: ids[4] }));
        blocks[3].add_terminator(Instruction::new(InstructionKind::Branch(ids[1])));
        blocks[4].add_terminator(Instruction::new(InstructionKind::ConditionalBranch { condition, true_block: ids[5], false_block: ids[6] }));
        blocks[5].add_terminator(Instruction::new(InstructionKind::ConditionalBranch { condition, true_block: ids[6], false_block: ids[3] }));
        blocks[6].add_terminator(Instruction::new(InstructionKind::Return(None)));
        func.finalize_blocks(blocks);
        let func = func.finalize();

        let cfg = ControlFlowGraph::generate(&func);

        assert_eq!(cfg.outgoing_branches(ids[0]), &hashset!{ ids[1] });
        assert_eq!(cfg.outgoing_branches(ids[1]), &hashset!{ ids[2] });
        assert_eq!(cfg.outgoing_branches(ids[2]), &hashset!{ ids[3], ids[4] });
        assert_eq!(cfg.outgoing_branches(ids[3]), &hashset!{ ids[1] });
        assert_eq!(cfg.outgoing_branches(ids[4]), &hashset!{ ids[5], ids[6] });
        assert_eq!(cfg.outgoing_branches(ids[5]), &hashset!{ ids[3], ids[6] });
        assert_eq!(cfg.outgoing_branches(ids[6]), &hashset!{ });
        
        assert_eq!(cfg.incoming_branches(ids[0]), &hashset!{ });
        assert_eq!(cfg.incoming_branches(ids[1]), &hashset!{ ids[0], ids[3] });
        assert_eq!(cfg.incoming_branches(ids[2]), &hashset!{ ids[1] });
        assert_eq!(cfg.incoming_branches(ids[3]), &hashset!{ ids[2], ids[5] });
        assert_eq!(cfg.incoming_branches(ids[4]), &hashset!{ ids[2] });
        assert_eq!(cfg.incoming_branches(ids[5]), &hashset!{ ids[4] });
        assert_eq!(cfg.incoming_branches(ids[6]), &hashset!{ ids[4], ids[5] });

        assert_eq!(cfg.leaf_blocks(), hashset!{ ids[6] });
    }
}
