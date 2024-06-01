#![feature(try_trait_v2)]
#![feature(never_type)]
#![feature(type_changing_struct_update)]
#![feature(assert_matches)]
#![feature(box_patterns)]
#![feature(let_chains)]
#![feature(extract_if)]

use std::{collections::{HashMap, VecDeque}, error::Error, fs};

use delta_null_lang_backend::ir::Module;
use parser::Parser;
use source::SourceInputType;
use tokenizer::tokenize;

use crate::translate::ModuleTranslator;

pub mod fallible;
pub mod tokenizer;
pub mod node;
pub mod parser;
pub mod translate;
pub mod source;
pub mod type_check;
pub mod util;

/// Tokenize and parse a single module.
pub fn parse_one_module(code: &str, input_type: SourceInputType) -> Result<node::Module, Vec<Box<dyn Error>>> {
    // Tokenize
    let (tokens, errors) = tokenize(code, input_type);
    if !errors.is_empty() {
        return Err(errors.into_iter().map(|e| Box::new(e) as _).collect::<Vec<_>>())
    }

    let mut parser = Parser::new(tokens.into_iter().peekable());
    parser.parse_module().box_errors().into_result()
}

/// Tokenize and parse a starting module, as well as all modules which it imports, recursively.
/// Returns modules with a topological ordering, ensuring that each module would be translated
/// before any other modules which depend on it.
pub fn parse_all_modules(code: &str, input_type: SourceInputType) -> Result<Vec<node::Module>, Vec<Box<dyn Error>>> {
    // Get us started off by parsing the root module
    let root_module = parse_one_module(code, input_type.clone())?;

    // Create a queue of dependency modules to import. We can top this up if our dependencies also
    // have dependencies.
    let mut module_paths_to_parse = VecDeque::from(root_module.used_files());
    
    // Keep track of `module path => [modules which it imports]`. We'll use this to build
    // a topological ordering later.
    // Using paths as *values* instead of `SourceInputType`s is OK because you can't import a
    // buffer, but we need to use `SourceInputType` as a *key* because the buffer can import modules.
    let mut module_imports = HashMap::new();
    module_imports.insert(input_type.clone(), root_module.used_files());

    // Ensure we keep track of modules we've already parsed, so if there's an infinite loop, we
    // won't get stuck forever. (This case will actually be handled when we perform a topological
    // sort later.)
    let mut parsed_paths = vec![];
    if let SourceInputType::File(p) = &input_type {
        parsed_paths.push(p.clone());
    }

    let mut modules = vec![root_module];

    // Empty the queue, therefore parsing all modules and their dependencies
    while let Some(module_path) = module_paths_to_parse.pop_front() {
        // If we've already parsed this, skip
        if parsed_paths.contains(&module_path) {
            continue;
        }

        // Load and parse
        let contents = fs::read_to_string(&module_path).map_err(|e| vec![Box::new(e) as _])?;
        let module = parse_one_module(&contents, SourceInputType::File(module_path.clone()))?;
        parsed_paths.push(module_path.clone());

        // Add any dependencies of this dependency to the queue...
        module_paths_to_parse.extend(module.used_files());

        // ...and the usage map
        module_imports.insert(module.input_type.clone(), module.used_files());

        
        modules.push(module);
    }

    // Topological sort, implementing Kahn's algorithm
    let mut ordered_modules = vec![];
    loop {
        // Find modules with no unprocessed dependents
        let next_module = modules.extract_if(|module| {
            match module_imports.get(&module.input_type) {
                None => true,
                Some(d) if d.is_empty() => true,

                _ => false,
            }
        }).next();

        let Some(next_module) = next_module else { break };
        let next_module_input = next_module.input_type.clone();

        // Add to ordering
        ordered_modules.push(next_module);

        // Prune from dependencies
        for modules in module_imports.values_mut() {
            modules.retain(|module| SourceInputType::File(module.clone()) != next_module_input)
        }
    }
    if !modules.is_empty() {
        // TODO
        panic!("cycle(s) detected in imported modules; the following modules are involved: {}",
            modules.iter().map(|m| format!("{}", m.input_type)).collect::<Vec<_>>().join(", "))
    }
    
    Ok(ordered_modules)
}

/// Type-checks and translates one already-parsed module.
pub fn translate_one_module(parsed_module: node::Module) -> Result<Module, Vec<Box<dyn Error>>> {
    // Type-check
    let typed_module = type_check::type_check_module(parsed_module.clone()).box_errors().into_result()?;

    // Translate
    let mut translator = ModuleTranslator::new(&typed_module, "main");
    translator.translate_items().box_errors().into_result()?;
    let mut module = translator.finalize().box_errors().into_result()?.unwrap();

    Ok(module)
}
