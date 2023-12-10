use std::{fmt::Display, error::Error};

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
}

impl Token {
    pub fn new(kind: TokenKind) -> Self {
        Token { kind }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum TokenKind {
    Identifier(String),
    Integer(String),

    KwFn,

    LBrace,
    RBrace,
    LParen,
    RParen,
    Colon,
    Semicolon,
    Plus,
}

pub fn tokenize(input: &str) -> (Vec<Token>, Vec<TokenizeError>) {
    let mut tokens = vec![];
    let mut errors = vec![];
    let mut chars = input.chars().peekable();

    while let Some(peeked) = chars.peek() {
        // Ignore whitespace
        if peeked.is_whitespace() {
            chars.next();
            continue;
        }

        match *peeked {
            // Symbols
            '{' => { chars.next(); tokens.push(Token::new(TokenKind::LBrace)) },
            '}' => { chars.next(); tokens.push(Token::new(TokenKind::RBrace)) },
            '(' => { chars.next(); tokens.push(Token::new(TokenKind::LParen)) },
            ')' => { chars.next(); tokens.push(Token::new(TokenKind::RParen)) },
            ':' => { chars.next(); tokens.push(Token::new(TokenKind::Colon)) },
            ';' => { chars.next(); tokens.push(Token::new(TokenKind::Semicolon)) },
            '+' => { chars.next(); tokens.push(Token::new(TokenKind::Plus)) },

            // Identifier
            c if c.is_alphabetic() || c == '_' => {
                let mut buffer = String::new();
                while let Some(next) = chars.next_if(|c| c.is_alphanumeric() || *c == '_') {
                    buffer.push(next);
                }

                // Check if this identifier is actually a keyword
                let kind = match buffer.as_ref() {
                    "fn" => TokenKind::KwFn,
                    _ => TokenKind::Identifier(buffer),
                };
                tokens.push(Token::new(kind));
            },

            // Integer
            c if c.is_ascii_digit() || c == '-' => {
                let mut buffer = String::new();
                buffer.push(chars.next().unwrap()); // brought out to catch -
                while let Some(next) = chars.next_if(|c| c.is_ascii_digit()) {
                    buffer.push(next);
                }
                tokens.push(Token::new(TokenKind::Integer(buffer)))
            },

            // Don't know!
            _ => {
                let c = chars.next().unwrap();
                errors.push(TokenizeError::new(&format!("unexpected character: {c}")));
            }
        }
    }
    
    (tokens, errors)
}

#[derive(Debug, Clone)]
pub struct TokenizeError {
    description: String,
}

impl TokenizeError {
    pub fn new(description: &str) -> Self {
        TokenizeError { description: description.to_owned() }
    }
}

impl Display for TokenizeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "tokenizer error: {}", self.description)
    }
}
impl Error for TokenizeError {}

#[cfg(test)]
mod test {
    use crate::tokenizer::TokenKind;

    use super::tokenize;

    #[test]
    fn test_fn() {
        let (tokens, errors) = tokenize("fn foo() { }");
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
        let (tokens, errors) = tokenize("123 + -456");
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
}
