use std::{iter::Peekable, fmt::Display, error::Error};

use crate::{node::{TopLevelItem, Statement, TopLevelItemKind, StatementKind, Expression, ExpressionKind, Type, TypeKind, ArithmeticBinOp, FunctionParameter, ComparisonBinOp}, tokenizer::{Token, TokenKind}, fallible::{Fallible, MaybeFatal}, source::SourceLocation, frontend_error};

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
    
        while self.tokens.peek().is_some() {
            self.parse_top_level_item()
                .integrate_if_ok(&mut result, |l, i| l.push(i))
        }
    
        result
    }

    /// Parse one [TopLevelItem]. (Assumes that there is a token left in the input.)
    pub fn parse_top_level_item(&mut self) -> Fallible<MaybeFatal<TopLevelItem>, ParseError> {
        match self.tokens.peek().expect("`parse_top_level_item` called with no tokens").kind {
            TokenKind::KwFn => self.parse_function_definition(),
            TokenKind::KwType => self.parse_type_alias(),

            _ => {
                let token = self.tokens.next().unwrap();
                return Fallible::new_fatal(vec![
                    ParseError::new(&format!("unexpected token at top-level: {:?}", token.kind), token.loc)
                ]);
            }
        }
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

        // Parse parameters
        let parameters = self.parse_parenthesised_list_of(Self::parse_function_parameter);

        // Parse return type, if provided - else default to void
        let return_type =
            if self.tokens.peek().map(|t| &t.kind) == Some(&TokenKind::RArrow) {
                self.tokens.next();
                self.parse_type()?
            } else {
                Fallible::new(Type::new(TypeKind::Void, loc.clone()))
            };

        // Body
        let statements = self.parse_body();
        statements
            .combine(return_type)
            .combine(parameters)
            .map(|((stmts, return_type), parameters)| {
                TopLevelItem::new(TopLevelItemKind::FunctionDefinition {
                    name,
                    body: Statement::new(StatementKind::Block {
                        body: stmts,
                        trailing_return: false, // TODO
                    }, loc.clone()),
                    parameters,
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

                // Parse initial value, if given
                let value =
                    if let Some(TokenKind::Equals) = self.tokens.peek().map(|t| &t.kind) {
                        self.tokens.next();
                        Some(self.parse_expression()?.propagate(&mut errors))
                    } else {
                        None
                    };

                self.expect(TokenKind::Semicolon)?;

                // Construct node
                errors.map(|_|
                    Statement::new(StatementKind::VariableDeclaration {
                        name,
                        ty,
                        value,
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

            Some(TokenKind::InlineAssemblyFragment(contents)) => {
                let contents = contents.clone();
                self.tokens.next();

                Fallible::new_ok(Statement::new(StatementKind::InlineAssembly(contents), loc))
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

                // If our next token is `=`, then we have an assignment!
                if self.tokens.peek().map(|t| &t.kind) == Some(&TokenKind::Equals) {
                    self.tokens.next();
                    return self.parse_expression()?
                        .combine(self.expect(TokenKind::Semicolon)?)
                        .map(|(value, _)| Statement::new(StatementKind::Assignment {
                            target: expr,
                            value,
                        }, loc).into());
                }

                self.expect(TokenKind::Semicolon)?;

                Fallible::new_ok(Statement::new(StatementKind::Expression(expr), loc))
            },

            None => Fallible::new_fatal(vec![
                ParseError::new("expected statement, got end of file", SourceLocation::stub())
            ]),
        }
    }

    /// Parse an expression.
    pub fn parse_expression(&mut self) -> Fallible<MaybeFatal<Expression>, ParseError> {
        self.parse_comparison()
    }

    /// Parse a usage of the `==` binary operator, or any expression with higher precedence.
    pub fn parse_comparison(&mut self) -> Fallible<MaybeFatal<Expression>, ParseError> {
        let mut expr = self.parse_bitwise_or()?;

        if let Some(TokenKind::DoubleEquals | TokenKind::LAngle | TokenKind::RAngle) = self.tokens.peek().map(|t| &t.kind) {
            let op = match self.tokens.peek().unwrap().kind {
                TokenKind::DoubleEquals => ComparisonBinOp::Equals,
                TokenKind::LAngle => ComparisonBinOp::LessThan,
                TokenKind::RAngle => ComparisonBinOp::GreaterThan,
                _ => unreachable!(),
            };
            let loc = self.tokens.next().unwrap().loc;

            self.parse_bitwise_or()?
                .integrate(&mut expr, |lhs, rhs|
                    *lhs = Expression::new(ExpressionKind::ComparisonBinOp(op, Box::new(lhs.clone()), Box::new(rhs)), loc));
        }

        expr.map(|e| e.into())
    }

    /// Parse a usage of the `|` bitwise OR operator, or any expression with higher precedence.
    pub fn parse_bitwise_or(&mut self) -> Fallible<MaybeFatal<Expression>, ParseError> {
        self.parse_arithmetic_binop(
            &[(TokenKind::Bar, ArithmeticBinOp::BitwiseOr)],
            Self::parse_bitwise_xor,
        )
    }

    /// Parse a usage of the `&` bitwise AND operator, or any expression with higher precedence.
    pub fn parse_bitwise_xor(&mut self) -> Fallible<MaybeFatal<Expression>, ParseError> {
        self.parse_arithmetic_binop(
            &[(TokenKind::Caret, ArithmeticBinOp::BitwiseXor)],
            Self::parse_bitwise_and,
        )
    }

    /// Parse a usage of the `&` bitwise AND operator, or any expression with higher precedence.
    pub fn parse_bitwise_and(&mut self) -> Fallible<MaybeFatal<Expression>, ParseError> {
        self.parse_arithmetic_binop(
            &[(TokenKind::Ampersand, ArithmeticBinOp::BitwiseAnd)],
            Self::parse_add_sub,
        )
    }

    /// Parse a usage of the `+` or `-` binary operators, or any expression with higher precedence.
    pub fn parse_add_sub(&mut self) -> Fallible<MaybeFatal<Expression>, ParseError> {
        self.parse_arithmetic_binop(
            &[
                (TokenKind::Plus, ArithmeticBinOp::Add),
                (TokenKind::Minus, ArithmeticBinOp::Subtract),
            ],
            Self::parse_mul,
        )
    }

    /// Parse a usage of the `*` binary operator, or any expression with higher precedence.
    pub fn parse_mul(&mut self) -> Fallible<MaybeFatal<Expression>, ParseError> {
        self.parse_arithmetic_binop(
            &[(TokenKind::Star, ArithmeticBinOp::Multiply)],
            Self::parse_cast,
        )
    }

    /// Parses a cast with an `as` infix.
    pub fn parse_cast(&mut self) -> Fallible<MaybeFatal<Expression>, ParseError> {
        let mut expr = self.parse_call()?;

        while let Some(&TokenKind::KwAs) = self.tokens.peek().map(|t| &t.kind) {
            let Token { kind: _, loc } = self.tokens.next().unwrap();

            self.parse_type()?
                .integrate(&mut expr, |e, ty|
                    *e = Expression::new(ExpressionKind::Cast(Box::new(e.clone()), ty), loc))
        }

        expr.map(|e| e.into())
    }

    /// Parses a call.
    pub fn parse_call(&mut self) -> Fallible<MaybeFatal<Expression>, ParseError> {
        let mut expr = self.parse_unary()?;

        if let Some(&TokenKind::LParen) = self.tokens.peek().map(|t| &t.kind) {
            let mut errors = Fallible::new_ok(());

            // Parse arguments, separated by commas
            let arguments = self.parse_parenthesised_list_of(Self::parse_expression).propagate(&mut errors);

            expr = expr
                .map(|target| {
                    let loc = target.loc.clone();
                    Expression::new(ExpressionKind::Call {
                        target: Box::new(target),
                        arguments,
                    }, loc)
                });
        }

        expr.map(|e| e.into())
    }

    /// Parse a unary operator. All unary operators currently have the same precedence.
    pub fn parse_unary(&mut self) -> Fallible<MaybeFatal<Expression>, ParseError> {
        match self.tokens.peek().map(|t| &t.kind) {
            Some(&TokenKind::Ampersand) => {
                self.tokens.next();
                self.parse_unary()?.map(|e| {
                    let loc = e.loc.clone();
                    Expression::new(ExpressionKind::PointerTake(Box::new(e)), loc).into()
                })
            }

            Some(&TokenKind::Star) => {
                self.tokens.next();
                self.parse_unary()?.map(|e| {
                    let loc = e.loc.clone();
                    Expression::new(ExpressionKind::PointerDereference(Box::new(e)), loc).into()
                })
            }

            Some(&TokenKind::Tilde) => {
                self.tokens.next();
                self.parse_unary()?.map(|e| {
                    let loc = e.loc.clone();
                    Expression::new(ExpressionKind::BitwiseNot(Box::new(e)), loc).into()
                })
            }

            _ => self.parse_index(),
        }
    }

    /// Parse an array indexing expression.
    pub fn parse_index(&mut self) -> Fallible<MaybeFatal<Expression>, ParseError> {
        let mut expr = self.parse_atom()?;

        while let Some(&TokenKind::LBracket) = self.tokens.peek().map(|t| &t.kind) {
            let Token { kind: _, loc } = self.tokens.next().unwrap();

            self.parse_expression()?
                .combine(self.expect(TokenKind::RBracket)?)
                .integrate(&mut expr, |e, (index, _)|
                    *e = Expression::new(ExpressionKind::Index {
                        target: Box::new(e.clone()),
                        index: Box::new(index)
                    }, loc))
        }

        expr.map(|e| e.into())
    }

    /// Parse an atom, the lowest-precedence form of expression - typically a single token like an
    /// integer literal.
    pub fn parse_atom(&mut self) -> Fallible<MaybeFatal<Expression>, ParseError> {
        match self.tokens.peek().map(|t| &t.kind) {
            Some(TokenKind::Integer(_, _)) => {
                let t = self.tokens.next().unwrap();
                let TokenKind::Integer(i, base) = t.kind else { unreachable!() };
                Fallible::new_ok(Expression::new(ExpressionKind::Integer(i, base), t.loc))
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

            Some(TokenKind::LParen) => {
                self.tokens.next();
                self.parse_expression()
                    .combine(self.expect(TokenKind::RParen))
                    .map(|(e, _)| e)
            },

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

            Some(TokenKind::Star) =>
                self.parse_type()
                    .map_inner(|ty|
                        Type::new(TypeKind::Pointer(Box::new(ty)), token.unwrap().loc)),

            Some(TokenKind::LBracket) => {
                let size_token = self.tokens.next();
                let Some(TokenKind::Integer(size, size_base)) = size_token.map(|t| t.kind) else {
                    return Fallible::new_fatal(vec![
                        ParseError::new("expected array size", token.unwrap().loc)
                    ])
                };
                let Ok(size) = usize::from_str_radix(&size, size_base) else {
                    return Fallible::new_fatal(vec![
                        ParseError::new("array size must be a positive integer", token.unwrap().loc)
                    ])
                };
                self.expect(TokenKind::RBracket)?;

                self.parse_type()
                    .map_inner(|ty|
                        Type::new(TypeKind::Array(Box::new(ty), size), token.unwrap().loc))
            }

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

    /// Given a parsing function, calls it repeatedly to parse a comma-separated,
    /// parenthesis-delimited list of items.
    fn parse_parenthesised_list_of<T>(
        &mut self,
        mut item_parse_func: impl FnMut(&mut Self) -> Fallible<MaybeFatal<T>, ParseError>
    ) -> Fallible<Vec<T>, ParseError> {
        let mut errors = Fallible::new_ok(());

        // Opening left-paren
        self.expect(TokenKind::LParen).propagate(&mut errors);

        // Parse arguments, separated by commas
        let mut items = vec![];
        loop {
            let Some(token) = self.tokens.peek() else {
                errors.push_error(ParseError::new(
                    "unexpected end-of-file while parsing argument list", SourceLocation::stub()
                ));
                break;
            };
            
            // If it's a right-paren, end the list
            if token.kind == TokenKind::RParen {
                self.tokens.next();
                break;
            }

            // Anything else, parse an expression as the argument value
            let arg = item_parse_func(self).propagate(&mut errors);
            if let MaybeFatal::Ok(item) = arg {
                items.push(item);
            }

            // This should be followed by either...
            match self.tokens.peek().map(|t| &t.kind) {
                // A comma...
                Some(&TokenKind::Comma) => { self.tokens.next(); },

                // Or a right-paren
                // (The next iteration will deal with this)
                Some(&TokenKind::RParen) => (),

                // The next iteration will give an error for the EOF case
                None => (),
                
                Some(_) => {
                    let token = self.tokens.next().unwrap();
                    errors.push_error(ParseError::new(
                        &format!("unexpected token {:?} in argument list", token.kind), token.loc
                    ))
                }
            }
        }

        errors.map(|_| items)
    }

    /// Parses a function parameter of the form `name: type`.
    fn parse_function_parameter(&mut self) -> Fallible<MaybeFatal<FunctionParameter>, ParseError> {
        let name_token = self.tokens.next();
        let loc = name_token.as_ref().map(|t| t.loc.clone()).unwrap_or_else(|| SourceLocation::stub());
        let Some(TokenKind::Identifier(name)) = name_token.map(|t| t.kind) else {
            return Fallible::new_fatal(vec![
                ParseError::new(
                    &format!("expected function parameter name"), loc
                )
            ])
        };

        self.expect(TokenKind::Colon)?;

        self.parse_type()?.map(|ty|
            FunctionParameter {
                name: name.clone(),
                ty,
            }.into()
        )
    }

    /// Utility function to parse an arithmetic binary operation, for example `+` or `&`.
    /// (Arithmetic is defined loosely as an operation which returns the same types as the two
    ///  input operands - i.e. `T <op> T -> T`.)
    /// 
    /// Takes:
    ///   - A `mapping` of infix [TokenKind]s to their corresponding [ArithmeticBinOp]s
    ///   - Another parsing function to `cascade` to, which is used to parse the expressions on
    ///     each side of the infix operator
    /// 
    /// Parses left-associatively.
    fn parse_arithmetic_binop(
        &mut self,
        mapping: &[(TokenKind, ArithmeticBinOp)],
        mut cascade: impl FnMut(&mut Self) -> Fallible<MaybeFatal<Expression>, ParseError>
    ) -> Fallible<MaybeFatal<Expression>, ParseError> {
        let mut expr = cascade(self)?;

        while let Some(ref token) = self.tokens.peek().map(|t| &t.kind) {
            let Some(op) = mapping.iter()
                .find(|(candidate, _)| &candidate == token)
                .map(|(_, op)| *op)
                else { break };

            let Token { kind: _, loc } = self.tokens.next().unwrap();

            cascade(self)?
                .integrate(&mut expr, |lhs, rhs|
                    *lhs = Expression::new(ExpressionKind::ArithmeticBinOp(op, Box::new(lhs.clone()), Box::new(rhs)), loc));
        }

        expr.map(|e| e.into())
    }

    /// Parse a type alias.
    pub fn parse_type_alias(&mut self) -> Fallible<MaybeFatal<TopLevelItem>, ParseError> {
        let loc = self.here_loc();
        self.expect(TokenKind::KwType)?;

        // Parse type name
        let name_token = self.tokens.next();
        let Some(TokenKind::Identifier(name)) = name_token.as_ref().map(|t| &t.kind) else {
            return Fallible::new_fatal(vec![
                ParseError::new("expected identifier after `type`", name_token.unwrap().loc),
            ])
        };
        let name = name.to_owned();

        // Parse type we're making an alias of
        self.expect(TokenKind::Equals)?;
        let ty = self.parse_type()?;

        // Closing semicolon
        self.expect(TokenKind::Semicolon)?;

        // Construct alias item
        ty.map(|ty|
            MaybeFatal::Ok(TopLevelItem {
                kind: TopLevelItemKind::TypeAlias {
                    name,
                    ty,
                },
                loc,
            }
        ))
    }

    /// Assume that the next token has the given [TokenKind], and returns it, else fail with a parse
    /// error.
    #[must_use]
    fn expect(&mut self, kind: TokenKind) -> Fallible<MaybeFatal<Token>, ParseError> {
        if let Some(token) = self.tokens.next() {
            if token.kind != kind {
                Fallible::new_fatal(vec![
                    ParseError::new(&format!("expected {:?}, got {:?}", kind, token.kind), token.loc)
                ])
            } else {
                Fallible::new_ok(token)
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

#[cfg(test)]
mod test {
    use std::assert_matches::assert_matches;

    use crate::{node::{ArithmeticBinOp, ComparisonBinOp, Expression, ExpressionKind, TopLevelItem, TopLevelItemKind, Type, TypeKind}, tokenizer::tokenize};

    use super::Parser;

    fn parse_expression(code: &str) -> Expression {
        let (tokens, errors) = tokenize(code, "<test>");
        if !errors.is_empty() {
            panic!("{:?}", errors)
        }
        Parser::new(tokens.into_iter().peekable()).parse_expression().unwrap().unwrap()
    }

    fn parse_top_level_item(code: &str) -> TopLevelItem {
        let (tokens, errors) = tokenize(code, "<test>");
        if !errors.is_empty() {
            panic!("{:?}", errors)
        }
        Parser::new(tokens.into_iter().peekable()).parse_top_level_item().unwrap().unwrap()
    }

    #[test]
    fn test_equality_precedence() {
        assert_matches!(
            parse_expression("2 + 2 == 4"),
            Expression {
                kind: ExpressionKind::ComparisonBinOp(ComparisonBinOp::Equals, box Expression { 
                    kind: ExpressionKind::ArithmeticBinOp(ArithmeticBinOp::Add, _, _),
                    ..
                }, _),
                ..
            }
        );

        assert_matches!(
            parse_expression("4 == 2 + 2"),
            Expression {
                kind: ExpressionKind::ComparisonBinOp(ComparisonBinOp::Equals, _, box Expression { 
                    kind: ExpressionKind::ArithmeticBinOp(ArithmeticBinOp::Add, _, _),
                    ..
                }),
                ..
            }
        );
    }

    #[test]
    fn test_arithmetic_precedence() {
        assert_matches!(
            parse_expression("2 + 1 * 2 + 5 == 4"), // ((2 + (1 * 2)) + 5) == 4
            Expression {
                kind: ExpressionKind::ComparisonBinOp(ComparisonBinOp::Equals, box Expression { 
                    kind: ExpressionKind::ArithmeticBinOp(ArithmeticBinOp::Add,
                        box Expression {
                            kind: ExpressionKind::ArithmeticBinOp(ArithmeticBinOp::Add,
                                _,
                                box Expression {
                                    kind: ExpressionKind::ArithmeticBinOp(ArithmeticBinOp::Multiply, _, _),
                                    ..
                                },
                            ),
                            ..
                        },
                        _),
                    ..
                }, _),
                ..
            }
        );
    }

    #[test]
    fn test_call() {
        assert_matches!(
            parse_expression("a() + b()"),
            Expression {
                kind: ExpressionKind::ArithmeticBinOp(
                    ArithmeticBinOp::Add,
                    box Expression { kind: ExpressionKind::Call { .. }, .. },
                    box Expression { kind: ExpressionKind::Call { .. }, .. },
                ),
                ..
            }
        );

        match parse_expression("a(1, c) + b(e, 2, 4,)") {
            Expression {
                kind: ExpressionKind::ArithmeticBinOp(
                    ArithmeticBinOp::Add,
                    box Expression { kind: ExpressionKind::Call {
                        arguments: a_args, ..
                    }, .. },
                    box Expression { kind: ExpressionKind::Call {
                        arguments: b_args, ..
                    }, .. },
                ),
                ..
            } => {
                assert_matches!(a_args[..], [
                    Expression { kind: ExpressionKind::Integer(_, _), .. },
                    Expression { kind: ExpressionKind::Identifier(_), .. },
                ]);
                assert_matches!(b_args[..], [
                    Expression { kind: ExpressionKind::Identifier(_), .. },
                    Expression { kind: ExpressionKind::Integer(_, _), .. },
                    Expression { kind: ExpressionKind::Integer(_, _), .. },
                ]);
            }

            _ => panic!("top match failed"),
        }
    }

    #[test]
    fn test_type_alias() {
        assert_matches!(
            parse_top_level_item("type Word = u16;"),
            TopLevelItem {
                kind: TopLevelItemKind::TypeAlias {
                    ty: Type {
                        kind: TypeKind::Name(_),
                        ..
                    },
                    ..
                },
                ..
            }
        );
    }
}
