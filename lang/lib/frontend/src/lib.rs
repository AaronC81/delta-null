#![feature(try_trait_v2)]
#![feature(never_type)]

use std::error::Error;

use delta_null_lang_backend::ir::Module;
use parser::Parser;
use tokenizer::tokenize;

use crate::translate::ModuleTranslator;

pub mod fallible;
pub mod tokenizer;
pub mod node;
pub mod parser;
pub mod translate;
pub mod source;

pub fn code_to_module(code: &str, filename: &str) -> Result<Module, Vec<Box<dyn Error>>> {
    // Tokenize
    let (tokens, errors) = tokenize(code, filename);
    if !errors.is_empty() {
        return Err(errors.into_iter().map(|e| Box::new(e) as _).collect::<Vec<_>>())
    }

    // Parse
    let mut parser = Parser::new(tokens.into_iter().peekable());
    let parsed_module = parser.parse_module().box_errors().into_result()?;

    // Translate
    let mut translator = ModuleTranslator::new();
    for item in parsed_module {
        translator.translate_item(&item).box_errors().into_result()?;
    }
    let mut module = translator.finalize();
    module.entry = Some("main".to_owned()); // TODO

    Ok(module)
}
