//! Language backend targeting the Delta Null's processor soft-core.

#![feature(extract_if)]
#![feature(let_chains)]

use asm::assemble;
use codegen::FunctionGenerator;
use delta_null_core_assembler::{AssemblyItem, AssemblyItemKind, BuildError};
use delta_null_core_instructions::InstructionOpcode;
use delta_null_lang_backend::{analysis::{flow::ControlFlowGraph, liveness::liveness_analysis, misc::is_leaf_function}, ir::{Function, Module, ModuleItem}};
use peephole::peephole_optimise;
use reg_alloc::allocate;

mod reg_alloc;
mod codegen;
mod peephole;
mod asm;

#[cfg(test)]
mod test_utils;

/// Compile a [Module] into Assembly instructions.
pub fn compile_module(module: &Module) -> Result<Vec<AssemblyItem>, Vec<BuildError>> {
    let Some(entry) = &module.entry else {
        todo!("modules without entry point are not yet supported");
    };

    // Find entry function and compile it first, as that's how our program's entry point currently
    // works
    let (entry_funcs, other_items): (Vec<_>, Vec<_>) = module.items.iter()
        .partition(|i|
            if let ModuleItem::Function(f) = i && &f.name == entry { true } else { false }
        );
    if entry_funcs.len() != 1 {
        panic!("expected 1 entry point function named {entry}, but found {}", entry.len())
    }

    // Compile and concatenate all
    let mut items = vec![];
    for item in entry_funcs.iter().chain(other_items.iter()) {
        match item {
            ModuleItem::Function(func) => items.extend(compile_function(func)?),
            ModuleItem::Data(datum) => {
                if datum.ty.word_size() != datum.value.len() {
                    panic!("value of data item `{}` does not match size of type", datum.name);
                }

                let mut is_first_word = true;
                for word in &datum.value {
                    let mut item = AssemblyItem::new_word_constant(*word);
                    if is_first_word {
                        item.labels.push(datum.name.clone());
                        is_first_word = false;
                    }
                    items.push(item);
                }
            },
            ModuleItem::Assembly(asm) => items.extend(assemble(asm)),
        }
    }

    Ok(items)
}

fn compile_function(func: &Function) -> Result<Vec<AssemblyItem>, Vec<BuildError>> {
    let liveness = liveness_analysis(func);
    let cfg = ControlFlowGraph::generate(func);
    let allocation = allocate(func, &cfg, &liveness);
    let is_leaf = is_leaf_function(func);

    let generator = FunctionGenerator::new(func, allocation, &liveness, is_leaf);
    let mut assembly = generator.to_assembly();

    peephole_optimise(&mut assembly);

    Ok(assembly)
}
