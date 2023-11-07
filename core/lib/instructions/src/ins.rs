//! A machine representation of the instructions described in design/instructions.md.
//! 
//! This implementation should always match that one.
//! Whenever a complete list of instructions is specified somewhere (e.g. in a match), it should
//! use the same categories as used in that document, to aid traceability.

use bitmatch::bitmatch;

use crate::{GPR, SPR, DR, AnyRegister, Encodable};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Instruction {
    // Core
    Nop,
    Hlt,
    Mov { src: GPR, dest: GPR },
    DMov { src: DR, dest: DR },

    // Immediate Loads
    Putl { reg: GPR, imm: u8 },
    Puth { reg: GPR, imm: u8 },

    // Memory
    Read { addr: GPR, val: GPR },
    Write { addr: GPR, val: GPR },
    DRead { addr: GPR, val: DR },
    DWrite { addr: GPR, val: DR },

    // Special-purpose Registers
    Movso { src: SPR, dest: GPR },
    Movsi { src: GPR, dest: SPR },
    Spadd { val: GPR },

    // TODO
}

impl Instruction {
    pub fn read_registers(self) -> Vec<AnyRegister> {
        todo!();
    }

    pub fn written_registers(self) -> Vec<AnyRegister> {
        todo!();
    }
}

impl Encodable for Instruction {
    fn encode(self) -> u16 {
        match self {
            // Core
            Instruction::Nop => 0b_0000_0000_0000_0000,
            Instruction::Hlt => 0b_1111_1111_1111_1111,
            Instruction::Mov { src, dest }
                => 0b_0010_0001_0000_0000 | src.encode() << 4 | dest.encode(),
            Instruction::DMov { src, dest }
                => 0b_0010_0001_0000_1000 | src.encode() << 4 | dest.encode(),

            // Immediate Loads
            Instruction::Putl { reg, imm }
                => 0b_0001_0000_0000_0000 | reg.encode() << 8 | imm as u16,
            Instruction::Puth { reg, imm }
                => 0b_0001_1000_0000_0000 | reg.encode() << 8 | imm as u16,

            // Memory
            Instruction::Read { addr, val }
                => 0b_0010_0000_0000_0000 | addr.encode() << 4 | val.encode(),
            Instruction::Write { addr, val }
                => 0b_0010_0000_1000_0000 | addr.encode() << 4 | val.encode(),
            Instruction::DRead { addr, val }
                => 0b_0010_0000_0000_1000 | addr.encode() << 4 | val.encode(),
            Instruction::DWrite { addr, val }
                => 0b_0010_0000_1000_1000 | addr.encode() << 4 | val.encode(),

            // Special-Purpose Registers
            Instruction::Movso { src, dest }
                => 0b_0010_0001_1000_0000 | src.encode() << 4 | dest.encode(),
            Instruction::Movsi { src, dest }
                => 0b_0010_0001_1100_0000 | dest.encode() << 4 | src.encode(),
            Instruction::Spadd { val }
                => 0b_0010_0001_1101_1000 | val.encode(),
        }
    }

    #[bitmatch]
    fn decode(bits: u16) -> Option<Self> {
        use Instruction::*;

        Some(
            #[bitmatch]
            match bits {
                // Core
                "0000_0000_0000_0000" => Nop,
                "1111_1111_1111_1111" => Hlt,
                "0010_0001_0sss_0ddd" => Mov { src: GPR::decode(s)?, dest: GPR::decode(d)? },
                "0010_0001_00ss_10dd" => DMov { src: DR::decode(s)?, dest: DR::decode(d)? },

                // Immediate Loads
                "0001_0rrr_bbbb_bbbb" => Putl { reg: GPR::decode(r)?, imm: b as u8 },
                "0001_1rrr_bbbb_bbbb" => Puth { reg: GPR::decode(r)?, imm: b as u8 },

                // Memory
                "0010_0000_0aaa_0rrr" => Read { addr: GPR::decode(a)?, val: GPR::decode(r)? },
                "0010_0000_1aaa_0rrr" => Write { addr: GPR::decode(a)?, val: GPR::decode(r)? },
                "0010_0000_0aaa_10dd" => DRead { addr: GPR::decode(a)?, val: DR::decode(d)? },
                "0010_0000_1aaa_10dd" => DWrite { addr: GPR::decode(a)?, val: DR::decode(d)? },

                // Special-Purpose Registers
                "0010_0001_10ss_0ddd" => Movso { src: SPR::decode(s)?, dest: GPR::decode(d)? },
                "0010_0001_11dd_0sss" => Movsi { src: GPR::decode(s)?, dest: SPR::decode(d)? },
                "0010_0001_1101_1ooo" => Spadd { val: GPR::decode(o)? },

                _ => return None,
            }
        )
    }
}

#[test]
fn test_brute_mirror_encode_decode() {
    for bits in 0..u16::MAX {
        if let Some(ins) = Instruction::decode(bits) {
            let reencoded = ins.encode();
            assert_eq!(bits, reencoded);
        }
    }
}
