use std::{fmt::Display, error::Error};

use crate::{source::SourceLocation, frontend_error};

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
    KwBreak,
    KwIf,
    KwElse,
    KwTrue,
    KwFalse,
    KwAs,

    Plus,
    Minus,
    Star,
    ForwardSlash,
    Equals,
    DoubleEquals,
    Ampersand,

    LBrace,
    RBrace,
    LParen,
    RParen,
    LBracket,
    RBracket,

    Colon,
    Semicolon,
    RArrow,
    Comma,

    InlineAssemblyFragment(String),
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
            '[' => { chars.next(); tokens.push(Token::new(TokenKind::LBracket, loc)) },
            ']' => { chars.next(); tokens.push(Token::new(TokenKind::RBracket, loc)) },
            ':' => { chars.next(); tokens.push(Token::new(TokenKind::Colon, loc)) },
            ';' => { chars.next(); tokens.push(Token::new(TokenKind::Semicolon, loc)) },
            '+' => { chars.next(); tokens.push(Token::new(TokenKind::Plus, loc)) },
            '*' => { chars.next(); tokens.push(Token::new(TokenKind::Star, loc)) },
            ',' => { chars.next(); tokens.push(Token::new(TokenKind::Comma, loc)) },
            '&' => { chars.next(); tokens.push(Token::new(TokenKind::Ampersand, loc)) },
            '=' => {
                chars.next();
                if chars.next_if(|(c, _)| *c == '=').is_some() {
                    tokens.push(Token::new(TokenKind::DoubleEquals, loc))
                } else {
                    tokens.push(Token::new(TokenKind::Equals, loc))
                }
            },

            // Comment, or forward slash
            '/' => {
                chars.next();
                if chars.next_if(|(c, _)| *c == '/').is_some() {
                    // Consume characters until newline, or end
                    for (c, _) in chars.by_ref() {
                        if c == '\n' { break }
                    }
                } else {
                    tokens.push(Token::new(TokenKind::ForwardSlash, loc))
                }
            },

            // Identifier
            c if c.is_alphabetic() || c == '_' => {
                let mut buffer = String::new();
                while let Some((next, _)) = chars.next_if(|(c, _)| c.is_alphanumeric() || *c == '_') {
                    buffer.push(next);
                }

                // Check if this identifier is actually something special
                let kind = match buffer.as_ref() {
                    // Inline assembly fragment!
                    "asm" => {
                        // Skip whitespace
                        while let Some(_) = chars.next_if(|(c, _)| c.is_whitespace()) {}

                        // Take opening {
                        let Some(('{', _)) = chars.next() else {
                            errors.push(TokenizeError::new("expected { after `asm`", loc));
                            continue;
                        };

                        // Take contents until closing }
                        let mut contents = String::new();
                        while let Some((next, _)) = chars.next() {
                            if next == '}' {
                                break;
                            }
                            contents.push(next);
                        }

                        TokenKind::InlineAssemblyFragment(contents)
                    },

                    // Normal keyword
                    "fn" => TokenKind::KwFn,
                    "var" => TokenKind::KwVar,
                    "return" => TokenKind::KwReturn,
                    "loop" => TokenKind::KwLoop,
                    "break" => TokenKind::KwBreak,
                    "if" => TokenKind::KwIf,
                    "else" => TokenKind::KwElse,
                    "true" => TokenKind::KwTrue,
                    "false" => TokenKind::KwFalse,
                    "as" => TokenKind::KwAs,

                    // Just an identifier
                    _ => TokenKind::Identifier(buffer),
                };
                tokens.push(Token::new(kind, loc));
            },

            // Integer, or `->`
            c if c.is_ascii_digit() || c == '-' => {
                // The `-` could represent:
                //   * a negative integer
                //   * a lone `-` 
                //   * the first character of `->`
                // We're still peeking at the `-`, so to see the next character we need to take it.
                let mut buffer = String::new();
                buffer.push(chars.next().unwrap().0);

                if &buffer == "-" && chars.peek().map(|(o, _)| *o) == Some('>') {
                    chars.next().unwrap();
                    tokens.push(Token::new(TokenKind::RArrow, loc));
                    continue;
                }
                if &buffer == "-" && !chars.peek().map(|(o, _)| o.is_ascii_digit()).unwrap_or(false) {
                    chars.next().unwrap();
                    tokens.push(Token::new(TokenKind::Minus, loc));
                    continue;
                }

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

frontend_error!(TokenizeError, "tokenizer");

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
