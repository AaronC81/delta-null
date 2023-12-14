use std::{iter::Peekable, fmt::Display, error::Error};

use crate::{node::{TopLevelItem, Statement, TopLevelItemKind, StatementKind, Expression, ExpressionKind, Type, TypeKind}, tokenizer::{Token, TokenKind}, fallible::{Fallible, MaybeFatal}};

pub struct Parser<I: Iterator<Item = Token>> {
    tokens: Peekable<I>,
}

impl<I: Iterator<Item = Token>> Parser<I> {
    pub fn new(tokens: Peekable<I>) -> Self {
        Self { tokens }
    }

    pub fn parse_module(&mut self) -> Fallible<Vec<TopLevelItem>, ParseError> {
        let mut result = Fallible::new(vec![]);
    
        while let Some(peeked) = self.tokens.peek() {
            match peeked.kind {
                TokenKind::KwFn => self.parse_function_definition()
                    .integrate_if_ok(&mut result, |l, i| l.push(i)),

                _ => result.push_error(ParseError::new(&format!("unexpected token at top-level: {:?}", peeked)))
            }
        }
    
        result
    }

    pub fn parse_function_definition(&mut self) -> Fallible<MaybeFatal<TopLevelItem>, ParseError> {
        self.expect(TokenKind::KwFn)?;

        // Parse function name
        let Some(Token { kind: TokenKind::Identifier(name) }) = self.tokens.next() else {
            return Fallible::new_fatal(vec![
                ParseError::new("expected identifier after `fn`"),
            ])
        };

        // No arguments currently supported!
        self.expect(TokenKind::LParen)?;
        self.expect(TokenKind::RParen)?;

        // Body
        let statements = self.parse_body();
        statements.map(|stmts| {
            TopLevelItem::new(TopLevelItemKind::FunctionDefinition {
                name,
                body: Statement::new(StatementKind::Block {
                    body: stmts,
                    trailing_return: false, // TODO
                })
            }).into()
        })
    }

    pub fn parse_statement(&mut self) -> Fallible<MaybeFatal<Statement>, ParseError> {
        match self.tokens.peek().map(|t| &t.kind) {
            Some(TokenKind::KwReturn) => {
                self.tokens.next();

                // Parse rest of `return`
                let value = self.parse_expression()?;
                self.expect(TokenKind::Semicolon)?;

                value.map(|e|
                    Statement::new(StatementKind::Return(Some(e))).into())
            },

            Some(TokenKind::KwVar) => {
                self.tokens.next();
                let mut errors = Fallible::new(());

                // Parse name and type
                let name =
                    match self.tokens.next().map(|t| t.kind) {
                        Some(TokenKind::Identifier(i)) => i,
                        Some(_) => {
                            errors.push_error(ParseError::new("expected identifier after `var`"));
                            "<?>".to_owned()
                        },
                        None => return Fallible::new_fatal(vec![
                            ParseError::new("unexpected end-of-file"),
                        ]),
                    };
                self.expect(TokenKind::Colon)?;
                let ty = self.parse_type()?.propagate(&mut errors);

                // Parse initial value
                // (Currently required)
                self.expect(TokenKind::Equals)?;
                let value = self.parse_expression()?.propagate(&mut errors);
                self.expect(TokenKind::Semicolon)?;

                // Construct node
                errors.map(|_|
                    Statement::new(StatementKind::VariableDeclaration {
                        name,
                        ty,
                        value: Some(value),
                    }).into())
            }

            Some(TokenKind::LBrace) => self.parse_body()
                .map(|body| Statement::new(StatementKind::Block {
                    body,
                    trailing_return: false
                }).into()),

            Some(TokenKind::KwLoop) => {
                self.tokens.next();
                self.parse_statement()?
                    .map(|s| Statement::new(StatementKind::Loop(Box::new(s))).into())
            }

            Some(TokenKind::KwIf) => {
                self.tokens.next();
                self.parse_expression()?
                    .combine(self.parse_statement()?)
                    .map(|(condition, body)| Statement::new(StatementKind::If {
                        condition,
                        body: Box::new(body),
                    }).into())
            }

            Some(_) => {
                // Let's assume this is an expression
                let expr = self.parse_expression()?;

                // We want to do some further processing, but we need to be sure that parsing 
                // actually succeeded first
                if expr.has_errors() {
                    return expr.map(|e| MaybeFatal::Ok(Statement::new(StatementKind::Expression(e))));
                }
                let expr = expr.unwrap();

                // If the expression is an identifier, and our next token is `=`, then we have an
                // assignment!
                if let ExpressionKind::Identifier(id) = &expr.kind {
                    if self.tokens.peek().map(|t| &t.kind) == Some(&TokenKind::Equals) {
                        self.tokens.next();
                        return self.parse_expression()?
                            .combine(self.expect(TokenKind::Semicolon)?)
                            .map(|(value, _)| Statement::new(StatementKind::Assignment {
                                name: id.to_owned(),
                                value,
                            }).into());
                    }
                }

                Fallible::new_ok(Statement::new(StatementKind::Expression(expr)))
            },

            None => Fallible::new_fatal(vec![
                ParseError::new("expected statement, got end of file")
            ]),
        }
    }

    pub fn parse_expression(&mut self) -> Fallible<MaybeFatal<Expression>, ParseError> {
        self.parse_add()
    }

    pub fn parse_add(&mut self) -> Fallible<MaybeFatal<Expression>, ParseError> {
        let mut expr = self.parse_equals()?;

        if self.tokens.peek().map(|t| &t.kind) == Some(&TokenKind::Plus) {
            self.tokens.next();

            self.parse_expression()?
                .integrate(&mut expr, |lhs, rhs|
                    *lhs = Expression::new(ExpressionKind::Add(Box::new(lhs.clone()), Box::new(rhs))));
        }

        expr.map(|e| e.into())
    }

    pub fn parse_equals(&mut self) -> Fallible<MaybeFatal<Expression>, ParseError> {
        let mut expr = self.parse_atom()?;

        if self.tokens.peek().map(|t| &t.kind) == Some(&TokenKind::DoubleEquals) {
            self.tokens.next();

            self.parse_expression()?
                .integrate(&mut expr, |lhs, rhs|
                    *lhs = Expression::new(ExpressionKind::Equals(Box::new(lhs.clone()), Box::new(rhs))));
        }

        expr.map(|e| e.into())
    }

    pub fn parse_atom(&mut self) -> Fallible<MaybeFatal<Expression>, ParseError> {
        match self.tokens.peek().map(|t| &t.kind) {
            Some(TokenKind::Integer(_)) => {
                let TokenKind::Integer(i) = self.tokens.next().unwrap().kind else { unreachable!() };
                Fallible::new_ok(Expression::new(ExpressionKind::Integer(i)))
            },

            Some(TokenKind::Identifier(_)) => {
                let TokenKind::Identifier(i) = self.tokens.next().unwrap().kind else { unreachable!() };
                Fallible::new_ok(Expression::new(ExpressionKind::Identifier(i)))
            },

            Some(_) => {
                let t = self.tokens.next().unwrap().kind;
                Fallible::new_fatal(vec![
                    ParseError::new(&format!("expected expression, got {:?}", t))
                ])
            },
            None => Fallible::new_fatal(vec![
                ParseError::new("expected expression, got end of file")
            ]),
        }
    }
    
    pub fn parse_type(&mut self) -> Fallible<MaybeFatal<Type>, ParseError> {
        match self.tokens.next().map(|t| t.kind) {
            Some(TokenKind::Identifier(i)) => Fallible::new_ok(Type::new(TypeKind::Name(i))),

            Some(k) => {
                Fallible::new_fatal(vec![
                    ParseError::new(&format!("expected expression, got {k:?}"))
                ])
            },
            None => Fallible::new_fatal(vec![
                ParseError::new("expected type, got end of file")
            ]),
        }
    }

    pub fn parse_body(&mut self) -> Fallible<Vec<Statement>, ParseError> {
        if self.expect(TokenKind::LBrace).has_errors() {
            return Fallible::new_with_errors(
                vec![],
                vec![
                    ParseError::new("missing { for start of block"),
                ]
            )
        }

        let mut statements = Fallible::new(vec![]);
        loop {
            match self.tokens.peek().map(|t| &t.kind) {
                Some(TokenKind::RBrace) => {
                    self.tokens.next();
                    break;
                },
                Some(_) =>
                    self.parse_statement()
                        .integrate_if_ok(&mut statements, |stmts, s| stmts.push(s)),
                None => {
                    statements.push_error(ParseError::new("expected } before end of file"));
                    break;
                },
            }
        };
        statements
    }

    #[must_use]
    fn expect(&mut self, kind: TokenKind) -> Fallible<MaybeFatal<()>, ParseError> {
        if let Some(token) = self.tokens.next() {
            if token.kind != kind {
                Fallible::new_fatal(vec![
                    ParseError::new(&format!("expected {:?}, got {:?}", kind, token.kind))
                ])
            } else {
                Fallible::new_ok(())
            }
        } else {
            Fallible::new_fatal(vec![
                ParseError::new(&format!("expected {kind:?}, got end of file"))
            ])
        }
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
