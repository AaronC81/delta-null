use crate::{ToAssembly, Encodable};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum GeneralPurposeRegister {
    R0, R1, R2, R3, R4, R5, R6, R7,
}
pub type GPR = GeneralPurposeRegister;

impl GeneralPurposeRegister {
    /// All GPRs, from R0 to R7 in order.
    pub fn all() -> impl DoubleEndedIterator<Item = Self> {
        [Self::R0, Self::R1, Self::R2, Self::R3, Self::R4, Self::R5, Self::R6, Self::R7].into_iter()
    }
}

impl Encodable for GeneralPurposeRegister {
    fn encode(self) -> u16 {
        match self {
            Self::R0 => 0,
            Self::R1 => 1,
            Self::R2 => 2,
            Self::R3 => 3,
            Self::R4 => 4,
            Self::R5 => 5,
            Self::R6 => 6,
            Self::R7 => 7,
        }
    }

    fn decode(bits: u16) -> Option<Self> {
        Some(match bits {
            0 => Self::R0,
            1 => Self::R1,
            2 => Self::R2,
            3 => Self::R3,
            4 => Self::R4,
            5 => Self::R5,
            6 => Self::R6,
            7 => Self::R7,
            _ => return None,
        })
    }
}

impl ToAssembly for GeneralPurposeRegister {
    fn to_assembly(&self) -> String {
        format!("r{}", self.encode())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DecimalRegister {
    D0, D1, D2, D3,
    Placeholder(usize),
}
pub type DR = DecimalRegister;

impl Encodable for DecimalRegister {
    fn encode(self) -> u16 {
        match self {
            Self::D0 => 0,
            Self::D1 => 1,
            Self::D2 => 2,
            Self::D3 => 3,

            Self::Placeholder(_) => panic!(),
        }
    }

    fn decode(bits: u16) -> Option<Self> {
        Some(match bits {
            0 => Self::D0,
            1 => Self::D1,
            2 => Self::D2,
            3 => Self::D3,
            _ => return None,
        })
    }
}

impl ToAssembly for DecimalRegister {
    fn to_assembly(&self) -> String {
        format!("d{}", self.encode())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SpecialPurposeRegister {
    IP, RP, SP, EF,
}
pub type SPR = SpecialPurposeRegister;

impl Encodable for SpecialPurposeRegister {
    fn encode(self) -> u16 {
        match self {
            Self::IP => 0,
            Self::RP => 1,
            Self::SP => 2,
            Self::EF => 3,
        }
    }

    fn decode(bits: u16) -> Option<Self> {
        Some(match bits {
            0 => Self::IP,
            1 => Self::RP,
            2 => Self::SP,
            3 => Self::EF,
            _ => return None,
        })
    }
}

impl ToAssembly for SpecialPurposeRegister {
    fn to_assembly(&self) -> String {
        match self {
            SpecialPurposeRegister::IP => "ip",
            SpecialPurposeRegister::RP => "rp",
            SpecialPurposeRegister::SP => "sp",
            SpecialPurposeRegister::EF => "ef",
        }.to_string()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AnyRegister {
    G(GeneralPurposeRegister),
    D(DecimalRegister),
    S(SpecialPurposeRegister),
}

#[cfg(test)]
mod test {
    use crate::{*, Encodable};

    #[test]
    fn test_encode_decode() {
        assert_eq!(3, GeneralPurposeRegister::R3.encode());
        assert_eq!(Some(GeneralPurposeRegister::R3), GeneralPurposeRegister::decode(3));

        assert_eq!(0, DecimalRegister::D0.encode());
        assert_eq!(Some(DecimalRegister::D0), DecimalRegister::decode(0));

        assert_eq!(1, SpecialPurposeRegister::RP.encode());
        assert_eq!(Some(SpecialPurposeRegister::RP), SpecialPurposeRegister::decode(1));

        assert_eq!(None, GeneralPurposeRegister::decode(10));
    }
}
