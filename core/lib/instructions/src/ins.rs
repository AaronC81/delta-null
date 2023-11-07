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

    // Bit Manipulation
    Not { reg: GPR },
    And { reg: GPR, val: GPR },
    Or { reg: GPR, val: GPR },
    Xor { reg: GPR, val: GPR },
    Shl { reg: GPR, val: GPR },
    Shr { reg: GPR, val: GPR },

    // General-Purpose Arithmetic
    Neg { reg: GPR },
    Inc { reg: GPR },
    Dec { reg: GPR },
    Add { reg: GPR, val: GPR },
    Sub { reg: GPR, val: GPR },
    Mulu { reg: GPR, val: GPR },
    Muli { reg: GPR, val: GPR },

    // Comparison
    Inv,
    Eqz { reg: GPR },
    Eq { left: GPR, right: GPR },
    Gt { left: GPR, right: GPR },
    Gteq { left: GPR, right: GPR },

    // Branching
    Jmpoff { offset: u8 },
    Cjmpoff { offset: u8 },
    Jmp { src: GPR },
    Cjmp { src: GPR },
    Call { src: GPR },
    Ret,
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

            // Bit Manipulation
            Instruction::Not { reg }
                => 0b_0100_0000_0000_0000 | reg.encode(),
            Instruction::And { reg, val }
                => 0b_0100_0001_0000_0000 | val.encode() << 4 | reg.encode(),
            Instruction::Or { reg, val }
                => 0b_0100_0010_0000_0000 | val.encode() << 4 | reg.encode(),
            Instruction::Xor { reg, val }
                => 0b_0100_0011_0000_0000 | val.encode() << 4 | reg.encode(),
            Instruction::Shl { reg, val }
                => 0b_0100_0100_0000_0000 | val.encode() << 4 | reg.encode(),
            Instruction::Shr { reg, val }
                => 0b_0100_0101_0000_0000 | val.encode() << 4 | reg.encode(),

            // General-Purpose Arithmetic
            Instruction::Neg { reg }
                => 0b_0100_1000_0000_0000 | reg.encode(),
            Instruction::Inc { reg }
                => 0b_0100_1000_0001_0000 | reg.encode(),
            Instruction::Dec { reg }
                => 0b_0100_1000_0010_0000 | reg.encode(),
            Instruction::Add { reg, val }
                => 0b_0100_1001_0000_0000 | val.encode() << 4 | reg.encode(),
            Instruction::Sub { reg, val }
                => 0b_0100_1010_0000_0000 | val.encode() << 4 | reg.encode(),
            Instruction::Mulu { reg, val }
                => 0b_0100_1011_0000_0000 | val.encode() << 4 | reg.encode(),
            Instruction::Muli { reg, val }
                => 0b_0100_1011_1000_0000 | val.encode() << 4 | reg.encode(),

            // Comparison
            Instruction::Inv => 0b_0101_0000_0000_0000,
            Instruction::Eqz { reg }
                => 0b_0101_0000_0001_0000 | reg.encode(),
            Instruction::Eq { left, right }
                => 0b_0101_0001_0000_0000 | left.encode() << 4 | right.encode(),
            Instruction::Gt { left, right }
                => 0b_0101_0010_0000_0000 | left.encode() << 4 | right.encode(),
            Instruction::Gteq { left, right }
                => 0b_0101_0011_0000_0000 | left.encode() << 4 | right.encode(),

            // Branching
            Instruction::Jmpoff { offset }
                => 0b_0110_0000_0000_0000 | offset as u16,
            Instruction::Cjmpoff { offset }
                => 0b_0110_0001_0000_0000 | offset as u16,
            Instruction::Jmp { src }
                => 0b_0110_0010_0000_0000 | src.encode(),
            Instruction::Cjmp { src } 
                => 0b_0110_0011_0000_0000 | src.encode(),
            Instruction::Call { src }
                => 0b_0110_0010_0001_0000 | src.encode(),
            Instruction::Ret => 0b_0110_0010_0001_1000,
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

                // Bit Manipulation
                "0100_0000_0000_0rrr" => Not { reg: GPR::decode(r)? },
                "0100_0001_0xxx_0rrr" => And { reg: GPR::decode(r)?, val: GPR::decode(x)? },
                "0100_0010_0xxx_0rrr" => Or { reg: GPR::decode(r)?, val: GPR::decode(x)? },
                "0100_0011_0xxx_0rrr" => Xor { reg: GPR::decode(r)?, val: GPR::decode(x)? },
                "0100_0100_0xxx_0rrr" => Shl { reg: GPR::decode(r)?, val: GPR::decode(x)? },
                "0100_0101_0xxx_0rrr" => Shr { reg: GPR::decode(r)?, val: GPR::decode(x)? },

                // General-Purpose Arithmetic
                "0100_1000_0000_0rrr" => Neg { reg: GPR::decode(r)? },
                "0100_1000_0001_0rrr" => Inc { reg: GPR::decode(r)? },
                "0100_1000_0010_0rrr" => Dec { reg: GPR::decode(r)? },
                "0100_1001_0xxx_0rrr" => Add { reg: GPR::decode(r)?, val: GPR::decode(x)? },
                "0100_1010_0xxx_0rrr" => Sub { reg: GPR::decode(r)?, val: GPR::decode(x)? },
                "0100_1011_0xxx_0rrr" => Mulu { reg: GPR::decode(r)?, val: GPR::decode(x)? },
                "0100_1011_1xxx_0rrr" => Muli { reg: GPR::decode(r)?, val: GPR::decode(x)? },

                // Comparison
                "0101_0000_0000_0000" => Inv,
                "0101_0000_0001_0rrr" => Eqz { reg: GPR::decode(r)? },
                "0101_0001_0aaa_0bbb" => Eq { left: GPR::decode(a)?, right: GPR::decode(b)? },
                "0101_0010_0aaa_0bbb" => Gt { left: GPR::decode(a)?, right: GPR::decode(b)? },
                "0101_0011_0aaa_0bbb" => Gteq { left: GPR::decode(a)?, right: GPR::decode(b)? },

                // Branching
                "0110_0000_bbbb_bbbb" => Jmpoff { offset: b as u8 },
                "0110_0001_bbbb_bbbb" => Cjmpoff { offset: b as u8 },
                "0110_0010_0000_0rrr" => Jmp { src: GPR::decode(r)? },
                "0110_0011_0000_0rrr" => Cjmp { src: GPR::decode(r)? },
                "0110_0010_0001_0rrr" => Call { src: GPR::decode(r)? },
                "0110_0010_0001_1000" => Ret,

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
