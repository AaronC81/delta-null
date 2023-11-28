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
}

#[cfg(test)]
mod test {
    use std::collections::HashSet;

    use maplit::hashset;

    use crate::ir::{FunctionBuilder, Instruction, InstructionKind};

    use super::ControlFlowGraph;

    #[test]
    fn test_simple_flow() {
        let mut func = FunctionBuilder::new("foo");

        // 1 -> 2 -> 3
        let (id1, mut block1) = func.new_basic_block();
        let (id2, mut block2) = func.new_basic_block();
        let (id3, mut block3) = func.new_basic_block();
        block1.add_terminator(Instruction::new(InstructionKind::Branch(id2)));
        block2.add_terminator(Instruction::new(InstructionKind::Branch(id3)));
        block3.add_terminator(Instruction::new(InstructionKind::Return(None)));

        block1.finalize();
        block2.finalize();
        block3.finalize();

        let func = func.finalize();

        let cfg = ControlFlowGraph::generate(&func);

        assert_eq!(cfg.outgoing_branches(id1), &hashset!{ id2 });
        assert_eq!(cfg.outgoing_branches(id2), &hashset!{ id3 });
        assert_eq!(cfg.outgoing_branches(id3), &hashset!{ });
        
        assert_eq!(cfg.incoming_branches(id1), &hashset!{ });
        assert_eq!(cfg.incoming_branches(id2), &hashset!{ id1 });
        assert_eq!(cfg.incoming_branches(id3), &hashset!{ id2 });
    }
}
