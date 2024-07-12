//! Finds variables which should be allocated into specific registers, if possible, to enable
//! better codegen.
//! 
//! Currently, this is used for allocating call parameters - if they're already in the correct
//! register, they don't need to be shuffled around.
//! 
//! It will likely not be possible to fulfill all preferences, and they aren't requirements for
//! codegen to work.

use std::collections::{HashMap, HashSet};

use delta_null_core_instructions::GPR;
use delta_null_lang_backend::ir::{BasicBlock, Function, InstructionKind, VariableId};

use crate::{CALL_TARGET_REGISTER, PARAMETER_PASSING_REGISTERS};

/// Tracks the preferred register allocations for variables.
#[derive(Debug, Clone)]
pub struct RegisterPreferences {
    inner: HashMap<VariableId, GPR>,
}

impl RegisterPreferences {
    /// Creates a blank preference mapping.
    pub fn new() -> Self {
        Self { inner: HashMap::new() }
    }

    /// Inserts a new preference mapping.
    pub fn insert(&mut self, var: VariableId, gpr: GPR) {
        self.inner.insert(var, gpr);
    }

    /// Gets the preference for a variable, if it has one.
    pub fn get(&self, var: VariableId) -> Option<GPR> {
        self.inner.get(&var).copied()
    }

    /// Gets all registers which are allocated to any preference. 
    pub fn all_registers(&self) -> Vec<GPR> {
        self.inner.values().copied()
            .collect::<HashSet<_>>() // Deduplicate
            .into_iter().collect()
    }
}

/// Finds register preferences within a function.
pub fn find_preferences(func: &Function) -> RegisterPreferences {
    let mut prefs = RegisterPreferences::new();

    // This search works per-basic-block (not sharing any info outside of each block).
    for (_, block) in &func.blocks {
        find_preferences_in_block(block, &mut prefs);
    }

    prefs
}

fn find_preferences_in_block(block: &BasicBlock, prefs: &mut RegisterPreferences) {
    // Walk backwards along the instructions in the block, so we can trace where they're used
    for stmt in block.statements.iter().rev() {
        // We only currently act on calls
        let InstructionKind::Call { target, arguments } = &stmt.instruction.kind else { continue };

        // If the call returns something, ideally that should continue to be allocated to `r0`,
        // since the EABI will put it there anyway
        if let Some(result) = stmt.result {
            prefs.insert(result, GPR::R0);
        }

        // The first four arguments should be allocated to their parameter-passing registers
        for (reg, arg) in PARAMETER_PASSING_REGISTERS.iter().zip(arguments) {
            prefs.insert(*arg, *reg);
        }

        // We use a particular register for targets by convention
        prefs.insert(*target, CALL_TARGET_REGISTER);
    }
}
