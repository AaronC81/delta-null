use std::{collections::HashMap, fmt::Display};

use delta_null_core_instructions::{AnyOperand, Encodable, InstructionOpcode};

use crate::{AssemblyItem, AssemblyItemKind, AssemblyOperand, LabelAccess};

/// Converts [AssemblyItem]s into [Instruction]s by resolving labels and converting directives.
#[derive(Debug, Clone)]
pub struct Builder {
    /// Maps label names to the address where they are defined.
    label_addresses: HashMap<String, u16>,
}

impl Builder {
    pub fn build_once(items: &[AssemblyItem], start_address: u16) -> Result<Vec<u16>, Vec<BuildError>> {
        let mut builder = Builder::new();
        builder.build(items, start_address)
    }

    pub fn new() -> Self {
        Self { label_addresses: HashMap::new() }
    }

    pub fn build(&mut self, items: &[AssemblyItem], start_address: u16) -> Result<Vec<u16>, Vec<BuildError>> {
        // First pass: find label definitions and save them
        let mut current_address = start_address;
        for item in items {
            for label in &item.labels {
                self.label_addresses.insert(label.to_string(), current_address);
            }
            current_address += item.word_size();
        }

        // Second pass: create word stream from items
        current_address = start_address;
        let mut word_stream = vec![];
        let mut errors = vec![];
        'item: for item in items {
            match &item.kind {
                AssemblyItemKind::Instruction(opcode, operands) => {
                    // Convert operands
                    let mut raw_operands = vec![];
                    for op in operands {
                        raw_operands.push(match op {
                            AssemblyOperand::Register(r) => AnyOperand::R(*r),
                            AssemblyOperand::Immediate(i) => {
                                match (*i).try_into() {
                                    Ok(i) => AnyOperand::I(i),
                                    Err(_) => {
                                        errors.push(BuildError::ImmediateOutOfRange(*i));
                                        continue 'item;
                                    }
                                }
                            },
                            AssemblyOperand::Label { name, access } => {
                                let address =
                                    match self.label_addresses.get(name) {
                                        Some(a) => *a,
                                        None => {
                                            errors.push(BuildError::UndefinedLabel(name.clone()));
                                            continue 'item;
                                        }
                                    };

                                let imm = match access {
                                    LabelAccess::High => ((address & 0xFF00) >> 8) as u8,
                                    LabelAccess::Low => (address & 0x00FF) as u8,
                                    LabelAccess::Offset => {
                                        // IP is one ahead of the instruction address
                                        let current_ip = current_address + 1;
                                        let big_offset = address as i32 - current_ip as i32;
                                        let offset: Result<i8, _> = big_offset.try_into();
                                        if let Ok(offset) = offset {
                                            offset as u8
                                        } else {
                                            errors.push(BuildError::OffsetOutOfRange { from: current_ip, to: address });
                                            continue 'item;
                                        }
                                    }
                                };
                                AnyOperand::I(imm)
                            }
                        })
                    }

                    // Build instruction
                    match opcode.build(&raw_operands) {
                        Some(instr) => word_stream.push(instr.encode()),
                        None => {
                            errors.push(BuildError::InvalidOperands(*opcode))
                        }
                    }
                },
                AssemblyItemKind::WordConstant(word) => word_stream.push(*word),
            }
            current_address += item.word_size();
        }

        if errors.is_empty() {
            Ok(word_stream)
        } else {
            Err(errors)
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BuildError {
    InvalidOperands(InstructionOpcode),
    ImmediateOutOfRange(u16),
    OffsetOutOfRange { from: u16, to: u16 },
    UndefinedLabel(String),
}

impl Display for BuildError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BuildError::InvalidOperands(opcode) => write!(f, "invalid operands for {}", opcode.mnemonic()),
            BuildError::ImmediateOutOfRange(imm) => write!(f, "immediate {imm} is out-of-range"),
            BuildError::OffsetOutOfRange { from, to } => write!(f, "offset operand cannot reach from {from} to {to}"),
            BuildError::UndefinedLabel(label) => write!(f, "undefined label {label}"),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{Builder, Parser};

    #[test]
    fn test_builder_basic() {
        assert_eq!(
            Ok(vec![0x0000, 0xFFFF]),
            Builder::build_once(
                &Parser::from_str("
                    nop
                    hlt
                ").parse().unwrap(),
                0
            )
        )
    }

    #[test]
    fn test_builder_operands() {
        assert_eq!(
            Ok(vec![0x1012, 0x1134, 0x4910, 0xFFFF]),
            Builder::build_once(
                &Parser::from_str("
                    putl r0, 0x12
                    putl r1, 0x34
                    add r0, r1
                    hlt
                ").parse().unwrap(),
                0
            )
        )
    }

    #[test]
    fn test_builder_directives() {
        assert_eq!(
            Ok(vec![0x1234, 0xFFFF]),
            Builder::build_once(
                &Parser::from_str("
                    .word 0x1234
                    .word 0xFFFF
                ").parse().unwrap(),
                0
            )
        )
    }

    #[test]
    fn test_builder_labels() {
        assert_eq!(
            Ok(vec![0x0000, 0x1005, 0x1840, 0x2001, 0xFFFF, 0x0000]),
            Builder::build_once(
                &Parser::from_str("
                    nop
                    putl r0, data/lo
                    puth r0, data/hi
                    read r1, r0
                    hlt
                    data: .word 0
                ").parse().unwrap(),
                0x4000
            )
        )
    }

    #[test]
    fn test_builder_offset() {
        assert_eq!(
            Ok(vec![0x0000, 0x6003, 0x0000, 0x0000, 0x0000, 0xFFFF]),
            Builder::build_once(
                &Parser::from_str("
                    nop
                    jmpoff dest/offset 
                    nop
                    nop
                    nop
                    dest: hlt
                ").parse().unwrap(),
                0x4000
            )
        )
    }
}
