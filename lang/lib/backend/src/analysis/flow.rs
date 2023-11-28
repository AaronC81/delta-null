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

// TODO: write tests, needs a decent "FunctionBuilder" system first to make not horrible
// Take inspiration from inkwell
