use std::iter::Peekable;

use delta_null_core_instructions::{InstructionOpcode, GPR, AnyRegister};

use crate::{AssemblyOperand, ParseError, Token, TokenKind};

/// An item in parsed assembly, typically an instruction or a directive.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssemblyItem {
    pub labels: Vec<String>,
    pub kind: AssemblyItemKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssemblyItemKind {
    Instruction(InstructionOpcode, Vec<AssemblyOperand>),
    WordConstant(u16),
    WordPut(GPR, AssemblyOperand),
}

impl AssemblyItem {
    pub fn new(kind: AssemblyItemKind) -> Self {
        Self { kind, labels: vec![] }
    }

    pub fn new_instruction(opcode: InstructionOpcode, operands: &[AssemblyOperand]) -> Self {
        Self::new(AssemblyItemKind::Instruction(opcode, operands.to_vec()))
    }

    pub fn new_word_constant(value: u16) -> Self {
        Self::new(AssemblyItemKind::WordConstant(value))
    }

    pub fn new_word_put(target: GPR, value: AssemblyOperand) -> Self {
        Self::new(AssemblyItemKind::WordPut(target, value))
    }

    /// The size of the item, in words (not bytes).
    pub fn word_size(&self) -> u16 {
        match self.kind {
            AssemblyItemKind::WordPut(_, _) => 2,
            _ => 1,
        }
    }
}

/// Parses lines of assembly into [AssemblyItem]s.
pub struct Parser<'a, T: Iterator<Item = &'a Token>> {
    tokens: Peekable<T>,
}

impl<'a> Parser<'a, std::slice::Iter<'a, Token>> {
    pub fn from_tokens(tokens: &'a [Token]) -> Self {
        Parser { tokens: tokens.iter().peekable() }
    }
}

impl<'a, T: Iterator<Item = &'a Token>> Parser<'a, T> {
    pub fn new(tokens: Peekable<T>) -> Self {
        Self { tokens }
    }

    pub fn parse(&mut self) -> Result<Vec<AssemblyItem>, Vec<ParseError>> {
        let mut items = vec![];
        let mut errors = vec![];
    
        // Loop over tokens until we run out
        'top: while self.tokens.peek().is_some() {
            // Collect labels
            let mut labels = vec![];
            loop {
                match self.tokens.peek() {
                    Some(Token { kind: TokenKind::Label(l) }) => {
                        labels.push(l.clone());
                        self.tokens.next();
                    }
                    Some(Token { kind: TokenKind::Newline }) => {
                        self.tokens.next(); // Discard and ignore
                    }

                    Some(_) => break,
                    None => break 'top,
                }
            }

            // After labels, we expect to find either:
            //   - An atom, which we'll interpret as an instruction mnemonic
            //   - A directive
            let operation = self.tokens.next().unwrap();

            // Parse operands
            // (Both atoms and directives need these, so we might as well do it now!)
            let operands = match self.parse_operands() {
                Ok(o) => o,
                Err(e) => {
                    errors.extend(e);
                    continue 'top;
                }
            };

            // Create item based on operation
            let item = match &operation.kind {
                TokenKind::Atom(mnemonic) => {
                    // Construct instruction
                    let Some(opcode) = InstructionOpcode::from_mnemonic(&mnemonic) else {
                        errors.push(ParseError::new(format!("no instruction: {mnemonic}"))); continue;
                    };
                    
                    AssemblyItem {
                        labels,
                        kind: AssemblyItemKind::Instruction(opcode, operands)
                    }
                },
                
                TokenKind::Directive(directive) => {
                    match directive.as_ref() {
                        "word" => {
                            if operands.len() != 1 {
                                errors.push(ParseError::new(format!(".word takes 1 operand")));
                                continue 'top;
                            }
                            let AssemblyOperand::Immediate(imm) = operands[0] else {
                                errors.push(ParseError::new(".word operand must be immediate".to_string()));
                                continue 'top;
                            };
                            
                            AssemblyItem {
                                labels,
                                kind: AssemblyItemKind::WordConstant(imm),
                            }
                        },
                        
                        "put" => {
                            if operands.len() != 2 {
                                errors.push(ParseError::new(format!(".put takes 2 operands")));
                                continue 'top;
                            }
                            let AssemblyOperand::Register(AnyRegister::G(gpr)) = operands[0] else {
                                errors.push(ParseError::new(".put first operand must be a GPR".to_string()));
                                continue 'top;
                            };
                            let (AssemblyOperand::Immediate(_) | AssemblyOperand::Label { access: None, .. }) = operands[1] else {
                                errors.push(ParseError::new(".put second operand must be either: immediate, or label without access specifier".to_string()));
                                continue 'top;
                            };

                            AssemblyItem {
                                labels,
                                kind: AssemblyItemKind::WordPut(gpr, operands[1].clone()),
                            }
                        },

                        _ => {
                            errors.push(ParseError::new(format!("unknown directive: {directive}")));
                            continue 'top;
                        }
                    }
                },

                t => {
                    errors.push(ParseError::new(format!("unexpected {}", t.describe())));
                    continue 'top;
                },
            };

            items.push(item);
        }
    
        if errors.is_empty() {
            Ok(items)
        } else {
            Err(errors)
        }
    }

    fn parse_operands(&mut self) -> Result<Vec<AssemblyOperand>, Vec<ParseError>> {
        let mut operands = vec![];
        let mut errors = vec![];

        let mut is_first_operand = true;
        loop {
            // Newline indicates end of operands
            if let None | Some(Token { kind: TokenKind::Newline }) = self.tokens.peek() {
                break;
            }

            // Keep track of whether a comma is required
            if !is_first_operand {
                if let Some(Token { kind: TokenKind::Comma }) = self.tokens.peek() {
                    self.tokens.next();
                } else {
                    errors.push(ParseError::new("expected comma separating operands".to_string()));
                }
            } else {
                is_first_operand = false;
            }

            // Take and parse operand
            match self.parse_operand() {
                Ok(operand) => operands.push(operand),
                Err(e) => errors.push(e),
            }
        }

        if errors.is_empty() {
            Ok(operands)
        } else {
            Err(errors)
        }
    }

    fn parse_operand(&mut self) -> Result<AssemblyOperand, ParseError> {
        let operand_atom = match self.tokens.next() {
            Some(Token { kind: TokenKind::Atom(a) }) => a,
            Some(t) => return Err(ParseError::new(format!("expected atom, got {}", t.describe()))),
            None => return Err(ParseError::new("expected atom, got end-of-file".to_string())),
        };
        match AssemblyOperand::parse(&operand_atom) {
            Ok(o) => Ok(o),
            Err(e) => Err(e),
        }
    }
}

#[cfg(test)]
pub mod test {
    use delta_null_core_instructions::{InstructionOpcode, AnyRegister, GPR, SPR};

    use crate::{parser::Parser, AssemblyItem, AssemblyItemKind, AssemblyOperand, LabelAccess, Tokenizer, ParseError};

    pub fn parse_str(s: &str) -> Result<Vec<AssemblyItem>, Vec<ParseError>> {
        let mut tokenizer = Tokenizer::from_str(&s);
        let tokens = tokenizer.tokenize().unwrap();
        
        let mut parser = Parser::from_tokens(&tokens);
        parser.parse()
    }

    #[test]
    fn test_no_operands() {
        // One instruction
        assert_eq!(
            Ok(vec![
                AssemblyItem {
                    labels: vec![],
                    kind: AssemblyItemKind::Instruction(InstructionOpcode::Nop, vec![]),
                },
            ]),
            parse_str("nop")
        );

        // Multiple instructions
        assert_eq!(
            Ok(vec![
                AssemblyItem {
                    labels: vec![],
                    kind: AssemblyItemKind::Instruction(InstructionOpcode::Nop, vec![]),
                },
                AssemblyItem {
                    labels: vec![],
                    kind: AssemblyItemKind::Instruction(InstructionOpcode::Nop, vec![]),
                },
                AssemblyItem {
                    labels: vec![],
                    kind: AssemblyItemKind::Instruction(InstructionOpcode::Hlt, vec![]),
                },
            ]),
            parse_str("
                nop
                nop

                hlt
            ")
        );
    }

    #[test]
    fn test_operands() {
        assert_eq!(
            Ok(vec![
                AssemblyItem {
                    labels: vec![],
                    kind: AssemblyItemKind::Instruction(InstructionOpcode::Eqz, vec![
                        AssemblyOperand::Register(AnyRegister::G(GPR::R5)),
                    ]),
                },
            ]),
            parse_str("eqz r5")
        );

        assert_eq!(
            Ok(vec![
                AssemblyItem {
                    labels: vec![],
                    kind: AssemblyItemKind::Instruction(InstructionOpcode::Putl, vec![
                        AssemblyOperand::Register(AnyRegister::G(GPR::R0)),
                        AssemblyOperand::Immediate(0x12),
                    ]),
                },
            ]),
            parse_str("putl r0, 0x12")
        );
    }

    #[test]
    fn test_labels() {
        assert_eq!(
            Ok(vec![
                AssemblyItem {
                    labels: vec![],
                    kind: AssemblyItemKind::Instruction(InstructionOpcode::Putl, vec![
                        AssemblyOperand::Register(AnyRegister::G(GPR::R1)),
                        AssemblyOperand::Immediate(1),
                    ]),
                },
                AssemblyItem {
                    labels: vec![],
                    kind: AssemblyItemKind::Instruction(InstructionOpcode::Putl, vec![
                        AssemblyOperand::Register(AnyRegister::G(GPR::R2)),
                        AssemblyOperand::Label { name: "loop".to_string(), access: Some(LabelAccess::Low) },
                    ]),
                },
                AssemblyItem {
                    labels: vec![],
                    kind: AssemblyItemKind::Instruction(InstructionOpcode::Puth, vec![
                        AssemblyOperand::Register(AnyRegister::G(GPR::R2)),
                        AssemblyOperand::Label { name: "loop".to_string(), access: Some(LabelAccess::High) },
                    ]),
                },

                AssemblyItem {
                    labels: vec!["loop".to_string()],
                    kind: AssemblyItemKind::Instruction(InstructionOpcode::Add, vec![
                        AssemblyOperand::Register(AnyRegister::G(GPR::R0)),
                        AssemblyOperand::Register(AnyRegister::G(GPR::R1)),
                    ]),
                },
                AssemblyItem {
                    labels: vec![],
                    kind: AssemblyItemKind::Instruction(InstructionOpcode::Movsi, vec![
                        AssemblyOperand::Register(AnyRegister::S(SPR::IP)),
                        AssemblyOperand::Register(AnyRegister::G(GPR::R2)),
                    ]),
                },
            ]),
            parse_str("
                putl r1, 1
                putl r2, loop/lo
                puth r2, loop/hi

                loop:
                add r0, r1
                movsi ip, r2
            ")
        );
    }

    #[test]
    fn test_word_constant() {
        assert_eq!(
            Ok(vec![
                AssemblyItem {
                    labels: vec![],
                    kind: AssemblyItemKind::WordConstant(0xABCD),
                },
                AssemblyItem {
                    labels: vec![],
                    kind: AssemblyItemKind::Instruction(InstructionOpcode::Nop, vec![]),
                },
                AssemblyItem {
                    labels: vec!["a".to_string(), "b".to_string()],
                    kind: AssemblyItemKind::WordConstant(123),
                },
            ]),
            parse_str("
                .word 0xABCD
                nop
                a: b: .word 123
            ")
        );
    }

    #[test]
    fn test_comments() {
        assert_eq!(
            Ok(vec![
                AssemblyItem {
                    labels: vec![],
                    kind: AssemblyItemKind::Instruction(InstructionOpcode::Nop, vec![]),
                },
                AssemblyItem {
                    labels: vec![],
                    kind: AssemblyItemKind::Instruction(InstructionOpcode::Hlt, vec![]),
                },
            ]),
            parse_str("
                ; a comment!
                nop ; this does nothing, you know

                hlt ; and that's the end.
                ; there's nothing more here!
            ")
        );
    }

    #[test]
    fn test_put_directive() {
        assert_eq!(
            Ok(vec![
                AssemblyItem {
                    labels: vec![],
                    kind: AssemblyItemKind::WordPut(GPR::R5, AssemblyOperand::Immediate(0xABCD)),
                },
                AssemblyItem {
                    labels: vec![],
                    kind: AssemblyItemKind::Instruction(InstructionOpcode::Nop, vec![]),
                },
                AssemblyItem {
                    labels: vec![],
                    kind: AssemblyItemKind::WordPut(GPR::R1, AssemblyOperand::Immediate(0x1234)),
                },
            ]),
            parse_str("
                .put r5, 0xABCD
                nop 
                .put r1, 0x1234 ; great number
            ")
        );
    }
}
