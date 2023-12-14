use std::{fmt::Display, error::Error};

use crate::source::SourceLocation;

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: SourceLocation,
}

impl Token {
    pub fn new(kind: TokenKind, loc: SourceLocation) -> Self {
        Token { kind, loc }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum TokenKind {
    Identifier(String),
    Integer(String),

    KwFn,
    KwVar,
    KwReturn,
    KwLoop,
    KwIf,

    LBrace,
    RBrace,
    LParen,
    RParen,
    Colon,
    Semicolon,
    Plus,
    Equals,
    DoubleEquals,
}

pub fn tokenize(input: &str, filename: &str) -> (Vec<Token>, Vec<TokenizeError>) {
    let mut tokens = vec![];
    let mut errors = vec![];
    let mut chars = add_locations(input.chars(), filename.to_owned()).peekable();

    while let Some((peeked, loc)) = chars.peek() {
        let loc = loc.clone();

        // Ignore whitespace
        if peeked.is_whitespace() {
            chars.next();
            continue;
        }

        match *peeked {
            // Symbols
            '{' => { chars.next(); tokens.push(Token::new(TokenKind::LBrace, loc)) },
            '}' => { chars.next(); tokens.push(Token::new(TokenKind::RBrace, loc)) },
            '(' => { chars.next(); tokens.push(Token::new(TokenKind::LParen, loc)) },
            ')' => { chars.next(); tokens.push(Token::new(TokenKind::RParen, loc)) },
            ':' => { chars.next(); tokens.push(Token::new(TokenKind::Colon, loc)) },
            ';' => { chars.next(); tokens.push(Token::new(TokenKind::Semicolon, loc)) },
            '+' => { chars.next(); tokens.push(Token::new(TokenKind::Plus, loc)) },
            '=' => {
                chars.next();
                if chars.next_if(|(c, _)| *c == '=').is_some() {
                    tokens.push(Token::new(TokenKind::DoubleEquals, loc))
                } else {
                    tokens.push(Token::new(TokenKind::Equals, loc))
                }
            },

            // Identifier
            c if c.is_alphabetic() || c == '_' => {
                let mut buffer = String::new();
                while let Some((next, _)) = chars.next_if(|(c, _)| c.is_alphanumeric() || *c == '_') {
                    buffer.push(next);
                }

                // Check if this identifier is actually a keyword
                let kind = match buffer.as_ref() {
                    "fn" => TokenKind::KwFn,
                    "var" => TokenKind::KwVar,
                    "return" => TokenKind::KwReturn,
                    "loop" => TokenKind::KwLoop,
                    "if" => TokenKind::KwIf,
                    _ => TokenKind::Identifier(buffer),
                };
                tokens.push(Token::new(kind, loc));
            },

            // Integer
            c if c.is_ascii_digit() || c == '-' => {
                let mut buffer = String::new();
                buffer.push(chars.next().unwrap().0); // brought out to catch -
                while let Some((next, _)) = chars.next_if(|(c, _)| c.is_ascii_digit()) {
                    buffer.push(next);
                }
                tokens.push(Token::new(TokenKind::Integer(buffer), loc))
            },

            // Don't know!
            _ => {
                let (c, _) = chars.next().unwrap();
                errors.push(TokenizeError::new(&format!("unexpected character: {c}"), loc));
            }
        }
    }
    
    (tokens, errors)
}

fn add_locations(chars: impl Iterator<Item = char>, file: String) -> impl Iterator<Item = (char, SourceLocation)> {
    let mut col = 0;
    let mut line = 1;

    chars.into_iter()
        .map(move |c| {
            if c == '\n' {
                let nl = SourceLocation::new(file.clone(), line, col + 1);
                line += 1;
                col = 0;
                ('\n', nl)
            } else {
                col += 1;
                (c, SourceLocation::new(file.clone(), line, col))
            }
        })
}

#[derive(Debug, Clone)]
pub struct TokenizeError {
    description: String,
    loc: SourceLocation,
}

impl TokenizeError {
    pub fn new(description: &str, loc: SourceLocation) -> Self {
        TokenizeError { description: description.to_owned(), loc }
    }
}

impl Display for TokenizeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "tokenizer error: {}: {}", self.loc.describe(), self.description)
    }
}
impl Error for TokenizeError {}

#[cfg(test)]
mod test {
    use crate::{tokenizer::TokenKind, source::SourceLocation};

    use super::{tokenize, add_locations};

    #[test]
    fn test_fn() {
        let (tokens, errors) = tokenize("fn foo() { }", "");
        assert!(errors.is_empty());
        assert_eq!(
            vec![
                TokenKind::KwFn,
                TokenKind::Identifier("foo".to_string()),
                TokenKind::LParen,
                TokenKind::RParen,
                TokenKind::LBrace,
                TokenKind::RBrace,
            ],
            tokens.into_iter().map(|t| t.kind).collect::<Vec<_>>()
        )
    }

    #[test]
    fn test_integer() {
        let (tokens, errors) = tokenize("123 + -456", "");
        assert!(errors.is_empty());
        assert_eq!(
            vec![
                TokenKind::Integer("123".to_string()),
                TokenKind::Plus,
                TokenKind::Integer("-456".to_string()),
            ],
            tokens.into_iter().map(|t| t.kind).collect::<Vec<_>>()
        )
    }

    #[test]
    fn test_add_locations() {
        assert_eq!(
            vec![
                ('a',  SourceLocation::new("<file>".to_owned(), 1, 1)),
                ('b',  SourceLocation::new("<file>".to_owned(), 1, 2)),
                ('\n', SourceLocation::new("<file>".to_owned(), 1, 3)),

                ('c', SourceLocation::new("<file>".to_owned(), 2, 1)),
                ('d', SourceLocation::new("<file>".to_owned(), 2, 2)),
                ('e', SourceLocation::new("<file>".to_owned(), 2, 3)),
            ],
            add_locations("ab\ncde".chars(), "<file>".to_owned()).collect::<Vec<_>>()
        )
    }
}
