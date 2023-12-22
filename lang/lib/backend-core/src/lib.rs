//! Language backend targeting the Delta Null's processor soft-core.

#![feature(drain_filter)]

use codegen::FunctionGenerator;
use delta_null_core_assembler::{BuildError, AssemblyItem};
use delta_null_lang_backend::{ir::{Module, Function}, analysis::{liveness::liveness_analysis, flow::ControlFlowGraph}};
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

    // Find entry function and compile it first, as that's how our program's entry point currently
    // works
    let (entry_funcs, other_funcs): (Vec<_>, Vec<_>) = module.functions.iter().partition(|f| &f.name == entry);
    if entry_funcs.len() != 1 {
        panic!("expected 1 entry point function named {entry}, but found {}", entry.len())
    }

    // Compile and concatenate all
    let mut items = vec![];
    for func in entry_funcs.iter().chain(other_funcs.iter()) {
        items.extend(compile_function(func)?);
    }

    Ok(items)
}

fn compile_function(func: &Function) -> Result<Vec<AssemblyItem>, Vec<BuildError>> {
    let analysis = liveness_analysis(func);
    let cfg = ControlFlowGraph::generate(func);
    let allocation = allocate(func, &cfg, &analysis);

    let generator = FunctionGenerator::new(func, allocation);
    Ok(generator.to_assembly())
}
