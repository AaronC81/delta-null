use std::{fmt::Display, error::Error};

use delta_null_core_instructions::{AnyRegister, GPR, SPR, DR};

/// Describes how a label is accessed by an [AssemblyOperand].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LabelAccess {
    /// The high byte of the label's address.
    High,

    /// The low byte of the label's address.
    Low,

    /// The offset from the usage of the label to its definition.
    Offset,
}

/// An operand as found in a parsed assembly instruction.
/// 
/// This does not correspond to an operand as found in a core instruction, as this additionally
/// encodes assembly syntax sugar, such as labels.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AssemblyOperand {
    Register(AnyRegister),
    Immediate(u16),
    Label {
        name: String,
        access: LabelAccess,
    }
}

impl AssemblyOperand {
    /// Parse a single operand.
    pub fn parse(op: &str) -> Result<Self, ParseError> {
        match op {
            // GPRs
            "r0" => Ok(Self::Register(AnyRegister::G(GPR::R0))),
            "r1" => Ok(Self::Register(AnyRegister::G(GPR::R1))),
            "r2" => Ok(Self::Register(AnyRegister::G(GPR::R2))),
            "r3" => Ok(Self::Register(AnyRegister::G(GPR::R3))),
            "r4" => Ok(Self::Register(AnyRegister::G(GPR::R4))),
            "r5" => Ok(Self::Register(AnyRegister::G(GPR::R5))),
            "r6" => Ok(Self::Register(AnyRegister::G(GPR::R6))),
            "r7" => Ok(Self::Register(AnyRegister::G(GPR::R7))),

            // SPRs
            "ip" => Ok(Self::Register(AnyRegister::S(SPR::IP))),
            "rp" => Ok(Self::Register(AnyRegister::S(SPR::RP))),
            "sp" => Ok(Self::Register(AnyRegister::S(SPR::SP))),
            "ef" => Ok(Self::Register(AnyRegister::S(SPR::EF))),

            // DRs
            "d0" => Ok(Self::Register(AnyRegister::D(DR::D0))),
            "d1" => Ok(Self::Register(AnyRegister::D(DR::D1))),
            "d2" => Ok(Self::Register(AnyRegister::D(DR::D2))),
            "d3" => Ok(Self::Register(AnyRegister::D(DR::D3))),

            // Immediate or identifier
            _ => {
                // Check if number (either decimal, or start of 0x/0b prefix)
                if op.chars().next().unwrap().is_digit(10) {
                    let numeral;
                    let radix;
                    if op.starts_with("0x") {
                        numeral = &op[2..];
                        radix = 16;
                    } else if op.starts_with("0b") {
                        numeral = &op[2..];
                        radix = 2;
                    } else {
                        numeral = op;
                        radix = 10;
                    }

                    return match u16::from_str_radix(numeral, radix) {
                        Ok(i) => Ok(Self::Immediate(i)),
                        Err(e) => Err(ParseError::new(format!("integer parse error: {e}"))),
                    }
                }

                // Label usages must follow the format:
                //   - Name: numbers, letters, or underscores
                //   - Forward slash
                //   - Access: one of [hi]gh, [lo]w, [[off]s]et
                let Some((name, access)) = op.split_once('/') else {
                    return Err(ParseError::new(format!("use of label has no access specifier")))
                };
                if let Some(invalid) = name.chars().find(|c| !(c.is_alphanumeric() || *c == '_')) {
                    return Err(ParseError::new(format!("label name contains invalid character: {invalid}")))
                }
                let access = match access {
                    "hi" | "high" => LabelAccess::High,
                    "lo" | "low" => LabelAccess::Low,
                    "off" | "offs" | "offset" => LabelAccess::Offset,
                    _ => return Err(ParseError::new(format!("invalid access specifier: {access}"))),
                };

                Ok(Self::Label { name: name.to_string(), access })
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParseError {
    description: String,
}

impl ParseError {
    pub fn new(description: String) -> Self {
        Self { description }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "parse error: {}", self.description)
    }
}
impl Error for ParseError {}

#[cfg(test)]
mod test {
    use delta_null_core_instructions::{AnyRegister, GPR, SPR, DR};

    use crate::{AssemblyOperand, LabelAccess};

    #[test]
    fn test_register_parse() {
        assert_eq!(
            Ok(AssemblyOperand::Register(AnyRegister::G(GPR::R3))),
            AssemblyOperand::parse("r3"),
        );
        assert_eq!(
            Ok(AssemblyOperand::Register(AnyRegister::S(SPR::IP))),
            AssemblyOperand::parse("ip"),
        );
        assert_eq!(
            Ok(AssemblyOperand::Register(AnyRegister::D(DR::D2))),
            AssemblyOperand::parse("d2"),
        );
    }

    #[test]
    fn test_immediate_parse() {
        assert_eq!(
            Ok(AssemblyOperand::Immediate(1000)),
            AssemblyOperand::parse("1000"),
        );
        assert_eq!(
            Ok(AssemblyOperand::Immediate(0x123)),
            AssemblyOperand::parse("0x123"),
        );
        assert_eq!(
            Ok(AssemblyOperand::Immediate(0b11101100)),
            AssemblyOperand::parse("0b11101100"),
        );
    }

    #[test]
    fn test_label_parse() {
        assert_eq!(
            Ok(AssemblyOperand::Label { name: "abc".to_string(), access: LabelAccess::High }),
            AssemblyOperand::parse("abc/hi"),
        );
        assert_eq!(
            Ok(AssemblyOperand::Label { name: "my_name".to_string(), access: LabelAccess::Low }),
            AssemblyOperand::parse("my_name/low"),
        );
        assert_eq!(
            Ok(AssemblyOperand::Label { name: "foo123".to_string(), access: LabelAccess::Offset }),
            AssemblyOperand::parse("foo123/offset"),
        );

        assert!(AssemblyOperand::parse("missing_access").is_err());
        assert!(AssemblyOperand::parse("invalid_access/woah").is_err());
        assert!(AssemblyOperand::parse("has spaces/offset").is_err());
    }
}