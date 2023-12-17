use std::{iter::Peekable, str::Chars};

use crate::ParseError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
}

impl Token {
    pub fn new(kind: TokenKind) -> Self {
        Self { kind }
    }

    pub fn describe(&self) -> String {
        self.kind.describe()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    /// An instruction or operand.
    Atom(String),

    Label(String),
    Directive(String),
    Comma,
    Newline,
}

impl TokenKind {
    pub fn describe(&self) -> String {
        match self {
            TokenKind::Atom(a) => format!("atom '{a}'"),
            TokenKind::Label(l) => format!("label '{l}'"),
            TokenKind::Directive(d) => format!("directive '{d}'"),
            TokenKind::Comma => "comma".to_string(),
            TokenKind::Newline => "newline".to_string(),
        }
    }
}

/// Converts lines of assembly into [Token]s.
pub struct Tokenizer<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(chars: Peekable<Chars<'a>>) -> Self {
        Self { chars }
    }

    pub fn from_str(string: &'a str) -> Tokenizer<'a> {
        Self { chars: string.chars().peekable() }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, Vec<ParseError>> {
        let mut tokens = vec![];
        let mut errors = vec![];

        while self.chars.peek().is_some() {
            match self.tokenize_one() {
                Ok(Some(t)) => tokens.push(t),
                Ok(None) => break,
                Err(e) => errors.push(e),
            }
        }

        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(errors)
        }
    }

    pub fn tokenize_one(&mut self) -> Result<Option<Token>, ParseError> {
        match self.chars.peek() {
            // Comments
            Some(';') => {
                while let Some(c) = self.chars.peek() {
                    if *c == '\n' {
                        break;
                    }
                    self.chars.next();
                }
                self.tokenize_one()
            }

            // Spacing
            Some('\n') => {
                self.chars.next();
                Ok(Some(Token::new(TokenKind::Newline)))
            }
            Some(c) if c.is_whitespace() => {
                self.chars.next();
                self.tokenize_one()
            }

            // Directives
            Some('.') => {
                self.chars.next();
                Ok(Some(Token::new(TokenKind::Directive(self.read_atom()?))))
            }

            // Comma symbol
            Some(',') => {
                self.chars.next();
                Ok(Some(Token::new(TokenKind::Comma)))
            }

            // Label/atom
            Some(c) if Self::is_valid_atom_char(*c) => {
                let atom = self.read_atom()?;
                if let Some(':') = self.chars.peek() {
                    self.chars.next();
                    Ok(Some(Token::new(TokenKind::Label(atom))))
                } else {
                    Ok(Some(Token::new(TokenKind::Atom(atom))))
                }
            }
            
            Some(c) => {
                let c = *c;
                self.chars.next();
                Err(ParseError::new(format!("unexpected character {c}")))
            }
            None => Ok(None),
        }
    }

    fn read_atom(&mut self) -> Result<String, ParseError> {
        let mut buffer = String::new();
        while let Some(c) = self.chars.peek() {
            if Self::is_valid_atom_char(*c) {
                buffer.push(self.chars.next().unwrap())
            } else {
                break;
            }
        }
    
        if buffer.is_empty() {
            let actually_found = self.describe_next();
            return Err(ParseError::new(format!("expected atom, found: {actually_found}")))
        }
    
        Ok(buffer)
    }
    
    fn is_valid_atom_char(c: char) -> bool {
        c.is_alphanumeric() || c == '_' || c == '/'
    }

    fn describe_next(&mut self) -> String {
        match self.chars.peek() {
            Some(c) => c.to_string(),
            None => "end of input".to_string(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{Tokenizer, Token, TokenKind};

    #[test]
    fn test_tokenize() {
        assert_eq!(
            Ok(vec![
                Token::new(TokenKind::Newline),

                Token::new(TokenKind::Label("data".to_string())),
                Token::new(TokenKind::Directive("word".to_string())),
                Token::new(TokenKind::Atom("0x1234".to_string())),
                Token::new(TokenKind::Newline),

                Token::new(TokenKind::Label("start".to_string())),
                Token::new(TokenKind::Newline),

                Token::new(TokenKind::Directive("put".to_string())),
                Token::new(TokenKind::Atom("r1".to_string())),
                Token::new(TokenKind::Comma),
                Token::new(TokenKind::Atom("data".to_string())),
                Token::new(TokenKind::Newline),

                Token::new(TokenKind::Atom("read".to_string())),
                Token::new(TokenKind::Atom("r2".to_string())),
                Token::new(TokenKind::Comma),
                Token::new(TokenKind::Atom("r1".to_string())),
                Token::new(TokenKind::Newline),
            ]),
            Tokenizer::from_str("
                data: .word 0x1234
                start:
                    .put r1, data ; load address
                    read r2, r1
            ").tokenize()
        )
    }
}
