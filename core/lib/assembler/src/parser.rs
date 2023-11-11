use std::{iter::Peekable, str::Chars};

use delta_null_core_instructions::InstructionOpcode;

use crate::{AssemblyOperand, ParseError};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssemblyItem {
    pub labels: Vec<String>,
    pub kind: AssemblyItemKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssemblyItemKind {
    Instruction(InstructionOpcode, Vec<AssemblyOperand>),
    WordConstant(u16),
}

struct Parser<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(chars: Peekable<Chars<'a>>) -> Self {
        Self { chars }
    }

    pub fn from_str(string: &'a str) -> Parser<'a> {
        Self { chars: string.chars().peekable() }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.chars.peek() {
            if c.is_whitespace() {
                self.chars.next();
            } else {
                break
            }
        }
    }

    fn skip_same_line_whitespace(&mut self) {
        while let Some(c) = self.chars.peek() {
            if c.is_whitespace() && *c != '\n' {
                self.chars.next();
            } else {
                break
            }
        }
    }

    fn parse_atom(&mut self) -> Result<String, ParseError> {
        let mut buffer = String::new();
        while let Some(c) = self.chars.peek() {
            if c.is_alphanumeric() || *c == '_' || *c == '/' {
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

    fn describe_next(&mut self) -> String {
        match self.chars.peek() {
            Some(c) => c.to_string(),
            None => "end of input".to_string(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<AssemblyItem>, Vec<ParseError>> {
        let mut items = vec![];
        let mut errors = vec![];
    
        // Loop over characters until we run out
        'top: while self.chars.peek().is_some() {
            // Parse labels and mnemonic
            self.skip_whitespace();
            let mut labels = vec![];
            let mnemonic;
            loop {
                if self.chars.peek().is_none() {
                    break 'top;
                }

                // Check if this is a directive
                let mut is_directive = false;
                if let Some('.') = self.chars.peek() {
                    self.chars.next();
                    is_directive = true;
                }

                let atom = match self.parse_atom() {
                    Ok(a) => a,
                    Err(e) => {
                        errors.push(e);

                        // Try to recover to a vaguely sensible state - skip until next whitespace
                        while let Some(c) = self.chars.peek() {
                            if c.is_whitespace() {
                                break
                            }
                            self.chars.next();
                        }
                        continue 'top;
                    }
                };

                // Is this a label?
                if let Some(':') = self.chars.peek() {
                    labels.push(atom);
                    self.chars.next();
                    self.skip_whitespace();
                    continue;
                }

                // Is this a directive?
                if is_directive {
                    // Check against known ones
                    match atom.as_ref() {
                        "word" => {
                            self.skip_same_line_whitespace();

                            // Take an operand
                            // TODO: deduplicate
                            let operand_atom = match self.parse_atom() {
                                Ok(a) => a,
                                Err(e) => { errors.push(e); continue; }
                            };
                            let operand = match AssemblyOperand::parse(&operand_atom) {
                                Ok(o) => o,
                                Err(e) => { errors.push(e); continue; }
                            };

                            // Operand for `word` must always be an immediate
                            let AssemblyOperand::Immediate(imm) = operand else {
                                errors.push(ParseError::new(".word operand must be immediate".to_string()));
                                continue 'top;
                            };
                            items.push(AssemblyItem {
                                labels,
                                kind: AssemblyItemKind::WordConstant(imm),
                            });

                            // This should be the end of the line
                            self.skip_same_line_whitespace();

                            if let Some(c) = self.chars.next() {
                                if c != '\n' {
                                    errors.push(ParseError::new(".word takes one operand".to_string()));
                                }
                            }

                            continue 'top;
                        },

                        _ => {
                            errors.push(ParseError::new(format!("unknown directive: {atom}")))
                        }
                    }
                }

                // If not, assume it's a mnemonic
                mnemonic = atom;
                break;
            }

            // Parse operands
            let mut operands = vec![];
            let mut is_first_operand = true;
            loop {
                self.skip_same_line_whitespace();
                match self.chars.peek() {
                    None | Some('\n') => break,
                    _ => (),
                }

                // Keep track of whether a comma is required
                if !is_first_operand {
                    if let Some(',') = self.chars.peek() {
                        self.chars.next();
                        self.skip_same_line_whitespace();
                    } else {
                        errors.push(ParseError::new("expected comma separating operands".to_string()));
                    }
                } else {
                    is_first_operand = false;
                }

                // Take and parse operand
                let operand_atom = match self.parse_atom() {
                    Ok(a) => a,
                    Err(e) => { errors.push(e); continue; }
                };
                let operand = match AssemblyOperand::parse(&operand_atom) {
                    Ok(o) => o,
                    Err(e) => { errors.push(e); continue; }
                };
                operands.push(operand);
            }

            // Construct instruction
            let Some(opcode) = InstructionOpcode::from_mnemonic(&mnemonic) else {
                errors.push(ParseError::new(format!("no instruction: {mnemonic}"))); continue;
            };
            
            items.push(AssemblyItem {
                labels,
                kind: AssemblyItemKind::Instruction(opcode, operands)
            })
        }
    
        if errors.is_empty() {
            Ok(items)
        } else {
            Err(errors)
        }
    }
}

#[cfg(test)]
mod test {
    use delta_null_core_instructions::{InstructionOpcode, AnyRegister, GPR, SPR};

    use crate::{parser::Parser, AssemblyItem, AssemblyItemKind, AssemblyOperand, LabelAccess};

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
            Parser::from_str("nop").parse()
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
            Parser::from_str("
                nop
                nop

                hlt
            ").parse()
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
            Parser::from_str("eqz r5").parse()
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
            Parser::from_str("putl r0, 0x12").parse()
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
                        AssemblyOperand::Label { name: "loop".to_string(), access: LabelAccess::Low },
                    ]),
                },
                AssemblyItem {
                    labels: vec![],
                    kind: AssemblyItemKind::Instruction(InstructionOpcode::Puth, vec![
                        AssemblyOperand::Register(AnyRegister::G(GPR::R2)),
                        AssemblyOperand::Label { name: "loop".to_string(), access: LabelAccess::High },
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
            Parser::from_str("
                putl r1, 1
                putl r2, loop/lo
                puth r2, loop/hi

                loop:
                add r0, r1
                movsi ip, r2
            ").parse()
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
            Parser::from_str("
                .word 0xABCD
                nop
                a: b: .word 123
            ").parse()
        );
    }
}
