//! Language backend targeting the Delta Null's processor soft-core.

use codegen::FunctionGenerator;
use delta_null_core_assembler::{Builder, BuildError, AssemblyItem};
use delta_null_lang_backend::{ir::Module, analysis::{liveness::liveness_analysis, flow::ControlFlowGraph}};
use reg_alloc::allocate;

mod reg_alloc;
mod codegen;

#[cfg(test)]
mod test_utils;

/// Compile a [Module] into Assembly instructions.
pub fn compile_module(module: &Module) -> Result<Vec<AssemblyItem>, Vec<BuildError>> {
    let Some(entry) = &module.entry else {
        todo!("modules without entry point are not yet supported");
    };

    if module.functions.len() != 1 {
        todo!("only one function currently supported");
    }
    let entry_func = &module.functions[0];
    if &entry_func.name != entry {
        panic!("single function is not the entry function");
    }

    let analysis = liveness_analysis(entry_func);
    let cfg = ControlFlowGraph::generate(entry_func);
    let allocation = allocate(entry_func, &cfg, &analysis);

    let generator = FunctionGenerator::new(entry_func, allocation);
    Ok(generator.to_assembly())
}
