use std::collections::{HashMap, HashSet};

use maplit::hashset;

use crate::ir::{Function, BasicBlockId};

#[derive(Debug, Clone)]
pub struct ControlFlowGraph {
    /// The entry point of the function represented by this CFG.
    entry_point: BasicBlockId,

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
            entry_point: *func.blocks.keys().next().unwrap(),
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

    /// Performs a breadth-first traversal of the basic blocks in the graph, and returns the blocks
    /// encountered for the first time at each depth.
    /// 
    /// No block ID is returned twice, and some blocks may not appear at all if there is no way to
    /// reach them. The first "layer" always contains just one block, the entry point.
    pub fn breadth_first(&self) -> Vec<HashSet<BasicBlockId>> {
        let mut result = vec![hashset! { self.entry_point }];
        let mut all_encountered = hashset! { self.entry_point };

        loop {
            let sources = result.last().unwrap();
            let mut next_layer = HashSet::new();

            for source in sources {
                next_layer.extend(
                    self.outgoing_branches(*source)
                        .difference(&all_encountered)
                )
            }

            if next_layer.is_empty() {
                // We reached the end!
                return result;
            }

            all_encountered.extend(next_layer.iter());
            result.push(next_layer);
        }
    }

    /// Performs a depth-first traversal of the basic blocks in the graph, and returns the blocks
    /// encountered in order.
    /// 
    /// There are multiple orders in which a graph can be depth-first traversed, and the order which
    /// this returns is not guaranteed.
    /// 
    /// No block ID is returned twice, and some blocks may not appear at all if there is no way to
    /// reach them.
    pub fn depth_first(&self) -> Vec<BasicBlockId> {
        let mut result = vec![];
        let mut pending = vec![self.entry_point];

        loop {
            // Take a block from the top of the pending stack
            let Some(next_to_traverse) = pending.pop() else {
                return result;
            };
            result.push(next_to_traverse);

            // Push pending searches for descendant blocks we haven't searched yet
            let mut children = self.outgoing_branches(next_to_traverse)
                .iter()
                .filter(|b| !result.contains(&b))
                .collect::<Vec<_>>();
            children.sort(); // Give deterministic ordering for tests
            pending.extend(children);
        }
    }

    /// Calculates a topological-style ordering for the basic blocks, using the information in this
    /// graph.
    /// 
    /// When a block appears in this ordering, all blocks which *forward*-branch to it have appeared
    /// before. The ordering in which "equivalent" branches appear is not defined.
    pub fn ordering(&self) -> Vec<BasicBlockId> {
        // I think a depth-first search naturally has the required properties?
        self.depth_first()
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

    #[test]
    fn test_block_traversal_ordering() {
        let mut func = FunctionBuilder::new("foo");

        // The graph will be         But the expected 
        // constructed like          ordering is...
        // this:
        //
        //        3                  = 0
        //        v
        //    .-- 2 <-.              = 1
        //    |   v   0              = 3
        //    |   4 --^              = 2
        //    |
        //    '-> 1                  = 4

        let (ids, mut blocks) = func.new_basic_blocks(5);
        blocks[3].add_terminator(Instruction::new(InstructionKind::Branch(ids[2])));
        let condition = blocks[2].add_instruction(Instruction::new(InstructionKind::Constant(ConstantValue::Boolean(true))));
        blocks[2].add_terminator(Instruction::new(InstructionKind::ConditionalBranch { condition, true_block: ids[1], false_block: ids[4] }));
        blocks[4].add_terminator(Instruction::new(InstructionKind::Branch(ids[0])));
        blocks[0].add_terminator(Instruction::new(InstructionKind::Branch(ids[2])));
        blocks[1].add_terminator(Instruction::new(InstructionKind::Return(None)));
        func.finalize_blocks(blocks);
        let func = func.finalize();

        let mut cfg = ControlFlowGraph::generate(&func);
        cfg.entry_point = ids[3];
    
        // Test traversal orders
        assert_eq!(
            vec![
                hashset! { ids[3] },
                hashset! { ids[2] },
                hashset! { ids[1], ids[4] },
                hashset! { ids[0] },
            ],
            cfg.breadth_first(),
        );
        assert_eq!(
            vec![ids[3], ids[2], ids[4], ids[0], ids[1]],
            cfg.depth_first(),
        );

        assert_eq!(
            vec![ids[3], ids[2], ids[4], ids[0], ids[1]],
            cfg.ordering(),
        )
    }
}
