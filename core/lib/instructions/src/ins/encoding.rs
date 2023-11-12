use bitmatch::bitmatch;

use crate::{Instruction, Encodable, GPR, DR, SPR};

impl Encodable for Instruction {
    fn encode(self) -> u16 {
        use Instruction::*;

        match self {
            // Core
            Nop => 0b_0000_0000_0000_0000,
            Hlt => 0b_1111_1111_1111_1111,
            Mov { dest, src }
                => 0b_0010_0001_0000_0000 | src.encode() << 4 | dest.encode(),
            DMov { dest, src }
                => 0b_0010_0001_0000_1000 | src.encode() << 4 | dest.encode(),

            // Immediate Loads
            Putl { reg, imm }
                => 0b_0001_0000_0000_0000 | reg.encode() << 8 | imm as u16,
            Puth { reg, imm }
                => 0b_0001_1000_0000_0000 | reg.encode() << 8 | imm as u16,

            // Memory
            Read { addr, val }
                => 0b_0010_0000_0000_0000 | addr.encode() << 4 | val.encode(),
            Write { addr, val }
                => 0b_0010_0000_1000_0000 | addr.encode() << 4 | val.encode(),
            DRead { addr, val }
                => 0b_0010_0000_0000_1000 | addr.encode() << 4 | val.encode(),
            DWrite { addr, val }
                => 0b_0010_0000_1000_1000 | addr.encode() << 4 | val.encode(),

            // Special-Purpose Registers
            Movso { dest, src }
                => 0b_0010_0001_1000_0000 | src.encode() << 4 | dest.encode(),
            Movsi { dest, src }
                => 0b_0010_0001_1100_0000 | dest.encode() << 4 | src.encode(),
            Spadd { val }
                => 0b_0010_0001_1101_1000 | val.encode(),
            Spinc
                => 0b_0010_0010_1101_0000,
            Spdec
                => 0b_0010_0010_1101_0001,

            // Bit Manipulation
            Not { reg }
                => 0b_0100_0000_0000_0000 | reg.encode(),
            And { reg, val }
                => 0b_0100_0001_0000_0000 | val.encode() << 4 | reg.encode(),
            Or { reg, val }
                => 0b_0100_0010_0000_0000 | val.encode() << 4 | reg.encode(),
            Xor { reg, val }
                => 0b_0100_0011_0000_0000 | val.encode() << 4 | reg.encode(),
            Shl { reg, val }
                => 0b_0100_0100_0000_0000 | val.encode() << 4 | reg.encode(),
            Shr { reg, val }
                => 0b_0100_0101_0000_0000 | val.encode() << 4 | reg.encode(),

            // General-Purpose Arithmetic
            Neg { reg }
                => 0b_0100_1000_0000_0000 | reg.encode(),
            Inc { reg }
                => 0b_0100_1000_0001_0000 | reg.encode(),
            Dec { reg }
                => 0b_0100_1000_0010_0000 | reg.encode(),
            Add { reg, val }
                => 0b_0100_1001_0000_0000 | val.encode() << 4 | reg.encode(),
            Sub { reg, val }
                => 0b_0100_1010_0000_0000 | val.encode() << 4 | reg.encode(),
            Mulu { reg, val }
                => 0b_0100_1011_0000_0000 | val.encode() << 4 | reg.encode(),
            Muli { reg, val }
                => 0b_0100_1011_1000_0000 | val.encode() << 4 | reg.encode(),

            // Comparison
            Inv => 0b_0101_0000_0000_0000,
            Eqz { reg }
                => 0b_0101_0000_0001_0000 | reg.encode(),
            Eq { left, right }
                => 0b_0101_0001_0000_0000 | left.encode() << 4 | right.encode(),
            Gt { left, right }
                => 0b_0101_0010_0000_0000 | left.encode() << 4 | right.encode(),
            Gteq { left, right }
                => 0b_0101_0011_0000_0000 | left.encode() << 4 | right.encode(),

            // Branching
            Jmpoff { offset }
                => 0b_0110_0000_0000_0000 | offset as u16,
            Cjmpoff { offset }
                => 0b_0110_0001_0000_0000 | offset as u16,
            Cjmp { src } 
                => 0b_0110_0011_0000_0000 | src.encode(),
            Call { src }
                => 0b_0110_0010_0001_0000 | src.encode(),
            Ret => 0b_0110_0010_0001_1000,
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
                "0110_0011_0000_0rrr" => Cjmp { src: GPR::decode(r)? },
                "0110_0010_0001_0rrr" => Call { src: GPR::decode(r)? },
                "0110_0010_0001_1000" => Ret,

                _ => return None,
            }
        )
    }
}

#[cfg(test)]
mod test {
    use crate::{Instruction, Encodable};

    #[test]
    fn test_brute_mirror_encode_decode() {
        for bits in 0..u16::MAX {
            if let Some(ins) = Instruction::decode(bits) {
                let reencoded = ins.encode();
                assert_eq!(bits, reencoded);
            }
        }
    }
}
