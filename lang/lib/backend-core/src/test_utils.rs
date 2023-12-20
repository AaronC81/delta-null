use delta_null_core_assembler::Builder;
use delta_null_core_emulator::{Core, memory::{Memory, SimpleMemory}, ExecutionError};
use delta_null_core_instructions::ToAssembly;
use delta_null_lang_backend::ir::{Function, Module};

use crate::compile_module;

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
    core.trace_execution = true;
    step_until_return(&mut core).unwrap();
    core
}

/// Performs all necessary analysis on a function and then compiles it into DNA instructions.
pub fn compile_function(func: &Function) -> Vec<u16> {
    let mut module = Module::new();
    module.functions.push(func.clone());
    module.entry = Some(func.name.clone());

    let asm = compile_module(&module).unwrap();
    Builder::new().build(&asm, 0).unwrap()
}
