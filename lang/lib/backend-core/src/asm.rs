use delta_null_core_assembler::AssemblyItem;

pub fn assemble(input: &str) -> Vec<AssemblyItem> {
    // TODO: better errors
    let mut tokenizer = delta_null_core_assembler::Tokenizer::new(input.chars().peekable());
    let tokens = tokenizer.tokenize().unwrap();
    
    let mut parser = delta_null_core_assembler::Parser::from_tokens(&tokens);
    let items = parser.parse().unwrap();

    items
}