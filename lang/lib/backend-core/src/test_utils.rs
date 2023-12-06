use delta_null_core_assembler::{AssemblyItem, Builder};
use delta_null_core_emulator::{Core, memory::{Memory, SimpleMemory}, ExecutionError};
use delta_null_core_instructions::Instruction;
use delta_null_lang_backend::{ir::Function, analysis::{flow::ControlFlowGraph, liveness::liveness_analysis}};

use crate::{reg_alloc::allocate, codegen::FunctionGenerator};

/// Steps a core until it executes a `ret` instruction.
/// 
/// Assumes that the code being executed preserves `rp`, and that the code does not use the memory
/// address 0xFFFF.
pub fn step_until_return(core: &mut Core<impl Memory>) -> Result<(), ExecutionError> {
    // Modify memory and RP so that `ret` jumps to a `hlt`
    core.rp = 0xFFFF;
    core.memory.write(0xFFFF, 0xFFFF)?; // hlt

    core.step_until_halted()
}

/// Creates an interpreter for a list of encoded instructions, and executes them until they return.
/// 
/// Panics if the core encounters an execution error.
pub fn execute_function(instructions: &[u16]) -> Core<impl Memory> {
    let mut core = Core::new(SimpleMemory::with_content(instructions));
    step_until_return(&mut core).unwrap();
    core
}

/// Performs all necessary analysis on a function and then compiles it into DNA instructions.
pub fn compile_function(func: &Function) -> Vec<u16> {
    let analysis = liveness_analysis(&func);
    let cfg = ControlFlowGraph::generate(&func);

    let allocation = allocate(&func, &cfg, &analysis);

    let code = FunctionGenerator::new(&func, allocation);
    let asm = code.to_assembly();

    Builder::new().build(&asm, 0).unwrap()
}
