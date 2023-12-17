use std::{iter::Peekable, fmt::Display, error::Error};

use crate::{node::{TopLevelItem, Statement, TopLevelItemKind, StatementKind, Expression, ExpressionKind, Type, TypeKind}, tokenizer::{Token, TokenKind}, fallible::{Fallible, MaybeFatal}, source::SourceLocation, frontend_error};

/// Parses an iterator of [Token]s, interpreting them into a "module" - a collection of
/// [TopLevelItem]s (like functions and definitions).
/// 
/// The methods within this parser are written to "cascade", like many similarly-implemented
/// recursive-descent parsers. Each method will drill all the way down a node hierarchy, including
/// to implement precedence.
pub struct Parser<I: Iterator<Item = Token>> {
    tokens: Peekable<I>,
}

impl<I: Iterator<Item = Token>> Parser<I> {
    /// Create a parser with the given stream of tokens.
    pub fn new(tokens: Peekable<I>) -> Self {
        Self { tokens }
    }

    /// Consume tokens to parse a list of [TopLevelItem]s.
    pub fn parse_module(&mut self) -> Fallible<Vec<TopLevelItem>, ParseError> {
        let mut result = Fallible::new(vec![]);
    
        while let Some(peeked) = self.tokens.peek() {
            match peeked.kind {
                TokenKind::KwFn => self.parse_function_definition()
                    .integrate_if_ok(&mut result, |l, i| l.push(i)),

                _ => {
                    let token = self.tokens.next().unwrap();
                    result.push_error(ParseError::new(&format!("unexpected token at top-level: {:?}", token.kind), token.loc));
                }
            }
        }
    
        result
    }

    /// Parse a function definition.
    pub fn parse_function_definition(&mut self) -> Fallible<MaybeFatal<TopLevelItem>, ParseError> {
        let loc = self.here_loc();
        self.expect(TokenKind::KwFn)?;

        // Parse function name
        let name_token = self.tokens.next();
        let Some(TokenKind::Identifier(name)) = name_token.as_ref().map(|t| &t.kind) else {
            return Fallible::new_fatal(vec![
                ParseError::new("expected identifier after `fn`", name_token.unwrap().loc),
            ])
        };
        let name = name.to_owned();

        // No arguments currently supported!
        self.expect(TokenKind::LParen)?;
        self.expect(TokenKind::RParen)?;

        // Parse return type, if provided - else default to void
        let return_type;
        if self.tokens.peek().map(|t| &t.kind) == Some(&TokenKind::RArrow) {
            self.tokens.next();
            return_type = self.parse_type()?;
        } else {
            return_type = Fallible::new(Type::new(TypeKind::Void, loc.clone()))
        }

        // Body
        let statements = self.parse_body();
        statements
            .combine(return_type)
            .map(|(stmts, return_type)| {
                TopLevelItem::new(TopLevelItemKind::FunctionDefinition {
                    name,
                    body: Statement::new(StatementKind::Block {
                        body: stmts,
                        trailing_return: false, // TODO
                    }, loc.clone()),
                    return_type,
                }, loc).into()
            })
    }

    /// Parse a single statement, including any nested statements or expressions.
    pub fn parse_statement(&mut self) -> Fallible<MaybeFatal<Statement>, ParseError> {
        let loc = self.here_loc();

        match self.tokens.peek().map(|t| &t.kind) {
            Some(TokenKind::KwReturn) => {
                self.tokens.next();

                // Parse rest of `return`
                let value = self.parse_expression()?;
                self.expect(TokenKind::Semicolon)?;

                value.map(|e|
                    Statement::new(StatementKind::Return(Some(e)), loc).into())
            },

            Some(TokenKind::KwVar) => {
                self.tokens.next();
                let mut errors = Fallible::new(());

                // Parse name and type
                let name_token = self.tokens.next();
                let name =
                    match name_token.as_ref().map(|t| &t.kind) {
                        Some(TokenKind::Identifier(i)) => i.to_owned(),
                        Some(_) => {
                            errors.push_error(ParseError::new("expected identifier after `var`", name_token.unwrap().loc));
                            "<?>".to_owned()
                        },
                        None => return Fallible::new_fatal(vec![
                            ParseError::new("unexpected end-of-file", SourceLocation::stub()),
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
                    }, loc).into())
            }

            Some(TokenKind::LBrace) => self.parse_body()
                .map(|body| Statement::new(StatementKind::Block {
                    body,
                    trailing_return: false
                }, loc).into()),

            Some(TokenKind::KwLoop) => {
                self.tokens.next();
                self.parse_statement()?
                    .map(|s| Statement::new(StatementKind::Loop(Box::new(s)), loc).into())
            }

            Some(TokenKind::KwBreak) => {
                self.tokens.next();
                self.expect(TokenKind::Semicolon)?;
                Fallible::new_ok(Statement::new(StatementKind::Break, loc))
            }

            Some(TokenKind::KwIf) => {
                self.tokens.next();
                self.parse_expression()?
                    .combine(self.parse_statement()?)
                    .combine({
                        if self.tokens.peek().map(|t| &t.kind) == Some(&TokenKind::KwElse) {
                            self.tokens.next();
                            self.parse_statement()?.map(Some)
                        } else {
                            Fallible::new(None)
                        }
                    })
                    .map(|((condition, true_body), false_body)| Statement::new(StatementKind::If {
                        condition,
                        true_body: Box::new(true_body),
                        false_body: false_body.map(Box::new),
                    }, loc).into())
            }

            Some(_) => {
                // Let's assume this is an expression
                let expr = self.parse_expression()?;

                // We want to do some further processing, but we need to be sure that parsing 
                // actually succeeded first
                if expr.has_errors() {
                    return expr.map(|e| MaybeFatal::Ok(Statement::new(StatementKind::Expression(e), loc)));
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
                            }, loc).into());
                    }
                }

                Fallible::new_ok(Statement::new(StatementKind::Expression(expr), loc))
            },

            None => Fallible::new_fatal(vec![
                ParseError::new("expected statement, got end of file", SourceLocation::stub())
            ]),
        }
    }

    /// Parse an expression.
    pub fn parse_expression(&mut self) -> Fallible<MaybeFatal<Expression>, ParseError> {
        self.parse_add()
    }

    /// Parse a usage of the `+` binary operator, or any expression with lower precedence.
    pub fn parse_add(&mut self) -> Fallible<MaybeFatal<Expression>, ParseError> {
        let mut expr = self.parse_equals()?;

        if self.tokens.peek().map(|t| &t.kind) == Some(&TokenKind::Plus) {
            let loc = self.tokens.next().unwrap().loc;

            self.parse_expression()?
                .integrate(&mut expr, |lhs, rhs|
                    *lhs = Expression::new(ExpressionKind::Add(Box::new(lhs.clone()), Box::new(rhs)), loc));
        }

        expr.map(|e| e.into())
    }

    /// Parse a usage of the `==` binary operator, or any expression with lower precedence.
    pub fn parse_equals(&mut self) -> Fallible<MaybeFatal<Expression>, ParseError> {
        let mut expr = self.parse_atom()?;

        if self.tokens.peek().map(|t| &t.kind) == Some(&TokenKind::DoubleEquals) {
            let loc = self.tokens.next().unwrap().loc;

            self.parse_expression()?
                .integrate(&mut expr, |lhs, rhs|
                    *lhs = Expression::new(ExpressionKind::Equals(Box::new(lhs.clone()), Box::new(rhs)), loc));
        }

        expr.map(|e| e.into())
    }

    /// Parse an atom, the lowest-precedence form of expression - typically a single token like an
    /// integer literal.
    pub fn parse_atom(&mut self) -> Fallible<MaybeFatal<Expression>, ParseError> {
        match self.tokens.peek().map(|t| &t.kind) {
            Some(TokenKind::Integer(_)) => {
                let t = self.tokens.next().unwrap();
                let TokenKind::Integer(i) = t.kind else { unreachable!() };
                Fallible::new_ok(Expression::new(ExpressionKind::Integer(i), t.loc))
            },

            Some(TokenKind::Identifier(_)) => {
                let t = self.tokens.next().unwrap();
                let TokenKind::Identifier(i) = t.kind else { unreachable!() };
                Fallible::new_ok(Expression::new(ExpressionKind::Identifier(i), t.loc))
            },

            Some(TokenKind::KwTrue) =>
                Fallible::new_ok(Expression::new(ExpressionKind::Boolean(true), self.tokens.next().unwrap().loc)),
            Some(TokenKind::KwFalse) =>
                Fallible::new_ok(Expression::new(ExpressionKind::Boolean(false), self.tokens.next().unwrap().loc)),

            Some(_) => {
                let t = self.tokens.next().unwrap();
                Fallible::new_fatal(vec![
                    ParseError::new(&format!("expected expression, got {:?}", t.kind), t.loc)
                ])
            },
            None => Fallible::new_fatal(vec![
                ParseError::new("expected expression, got end of file", SourceLocation::stub())
            ]),
        }
    }
    
    /// Parse a type, for example in a variable definition.
    pub fn parse_type(&mut self) -> Fallible<MaybeFatal<Type>, ParseError> {
        let token = self.tokens.next();
        match token.as_ref().map(|t| &t.kind) {
            Some(TokenKind::Identifier(i)) => Fallible::new_ok(
                Type::new(TypeKind::Name(i.to_owned()), token.unwrap().loc)
            ),

            Some(k) => {
                Fallible::new_fatal(vec![
                    ParseError::new(&format!("expected type, got {k:?}"), token.unwrap().loc)
                ])
            },
            None => Fallible::new_fatal(vec![
                ParseError::new("expected type, got end of file", SourceLocation::stub())
            ]),
        }
    }

    /// Parse a curly-brace-delimited body of statements.
    pub fn parse_body(&mut self) -> Fallible<Vec<Statement>, ParseError> {
        let l_brace_expect = self.expect(TokenKind::LBrace);
        if l_brace_expect.has_errors() {
            return Fallible::new_with_errors(vec![], l_brace_expect.into_errors())
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
                    statements.push_error(ParseError::new("expected } before end of file", SourceLocation::stub()));
                    break;
                },
            }
        };
        statements
    }

    /// Assume that the next token has the given [TokenKind], else fail with a parse error.
    #[must_use]
    fn expect(&mut self, kind: TokenKind) -> Fallible<MaybeFatal<()>, ParseError> {
        if let Some(token) = self.tokens.next() {
            if token.kind != kind {
                Fallible::new_fatal(vec![
                    ParseError::new(&format!("expected {:?}, got {:?}", kind, token.kind), token.loc)
                ])
            } else {
                Fallible::new_ok(())
            }
        } else {
            Fallible::new_fatal(vec![
                ParseError::new(&format!("expected {kind:?}, got end of file"), SourceLocation::stub())
            ])
        }
    }

    /// Peeks at the current token, and fetches its location. If at the end of the file, returns a
    /// marker stub instead.
    #[must_use]
    pub fn here_loc(&mut self) -> SourceLocation {
        match &mut self.tokens.peek() {
            Some(t) => t.loc.clone(),
            None => SourceLocation::stub(),
        }
    }
}

frontend_error!(ParseError, "parse");
