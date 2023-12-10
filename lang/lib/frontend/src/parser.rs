use std::{iter::Peekable, fmt::Display, error::Error};

use crate::{node::TopLevelItem, tokenizer::{Token, TokenKind}, fallible::Fallible};

struct Parser<I: Iterator<Item = Token>> {
    tokens: Peekable<I>,
}

impl<I: Iterator<Item = Token>> Parser<I> {
    pub fn parse_module(&mut self) -> Fallible<Vec<TopLevelItem>, ParseError> {
        let mut result = Fallible::new(vec![]);
    
        while let Some(peeked) = self.tokens.peek() {
            match peeked.kind {
                TokenKind::KwFn => self.parse_function_definition()
                    .integrate(&mut result, |l, i| l.push(i)),

                _ => result.push_error(ParseError::new(&format!("unexpected token at top-level: {:?}", peeked)))
            }
        }
    
        result
    }

    pub fn parse_function_definition(&mut self) -> Fallible<TopLevelItem, ParseError> {
        todo!()
    }
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
