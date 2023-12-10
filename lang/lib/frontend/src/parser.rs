use std::{iter::Peekable, fmt::Display, error::Error};

use crate::{node::TopLevelItem, tokenizer::{Token, TokenKind}};

struct Parser<I: Iterator<Item = Token>> {
    tokens: Peekable<I>,
}

impl<I: Iterator<Item = Token>> Parser<I> {
    pub fn parse_module(&mut self) -> (Vec<TopLevelItem>, Vec<ParseError>) {
        let mut items = vec![];
        let mut errors = vec![];
    
        while let Some(peeked) = self.tokens.peek() {
            match peeked.kind {
                TokenKind::KwFn => propagate_single(self.parse_function_definition(), &mut items, &mut errors),

                _ => errors.push(ParseError::new(&format!("unexpected token at top-level: {:?}", peeked)))
            }
        }
    
        (items, errors)
    }

    pub fn parse_function_definition(&mut self) -> (TopLevelItem, Vec<ParseError>) {
        todo!()
    }
}

fn propagate<I, E>(result: (Vec<I>, Vec<E>), items: &mut Vec<I>, errors: &mut Vec<E>) {
    items.extend(result.0);
    errors.extend(result.1);
}

fn propagate_single<I, E>(result: (I, Vec<E>), items: &mut Vec<I>, errors: &mut Vec<E>) {
    items.push(result.0);
    errors.extend(result.1);
}


#[derive(Debug, Clone)]
pub struct ParseError {
    description: String,
}

impl ParseError {
    pub fn new(description: &str) -> Self {
        ParseError { description: description.to_owned() }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "tokenizer error: {}", self.description)
    }
}
impl Error for ParseError {}
