#![feature(prelude_import)]
#![feature(stmt_expr_attributes)]
#![feature(proc_macro_hygiene)]
#[prelude_import]
use std::prelude::rust_2021::*;
#[macro_use]
extern crate std;
mod ins {
    //! A machine representation of the instructions described in design/instructions.md.
    //!
    //! This implementation should always match that one.
    //! Whenever a complete list of instructions is specified somewhere (e.g. in a match), it should
    //! use the same categories as used in that document, to aid traceability.
    use crate::{GPR, SPR, DR, AnyRegister};
    use strum::{AsRefStr, EnumDiscriminants};
    use delta_null_core_macros::{InstructionOpcodeMnemonics, InstructionBuild};
    mod analysis {
        use crate::{AnyRegister, Instruction};
        impl Instruction {
            pub fn read_registers(self) -> Vec<AnyRegister> {
                ::core::panicking::panic("not yet implemented");
            }
            pub fn written_registers(self) -> Vec<AnyRegister> {
                ::core::panicking::panic("not yet implemented");
            }
        }
    }
    pub use analysis::*;
    mod encoding {
        use bitmatch::bitmatch;
        use crate::{Instruction, Encodable, GPR, DR, SPR};
        impl Encodable for Instruction {
            fn encode(self) -> u16 {
                use Instruction::*;
                match self {
                    Nop => 0b_0000_0000_0000_0000,
                    Hlt => 0b_1111_1111_1111_1111,
                    Mov { dest, src } => {
                        0b_0010_0001_0000_0000 | src.encode() << 4 | dest.encode()
                    }
                    DMov { dest, src } => {
                        0b_0010_0001_0000_1000 | src.encode() << 4 | dest.encode()
                    }
                    Putl { reg, imm } => {
                        0b_0001_0000_0000_0000 | reg.encode() << 8 | imm as u16
                    }
                    Puth { reg, imm } => {
                        0b_0001_1000_0000_0000 | reg.encode() << 8 | imm as u16
                    }
                    Read { addr, val } => {
                        0b_0010_0000_0000_0000 | addr.encode() << 4 | val.encode()
                    }
                    Write { addr, val } => {
                        0b_0010_0000_1000_0000 | addr.encode() << 4 | val.encode()
                    }
                    DRead { addr, val } => {
                        0b_0010_0000_0000_1000 | addr.encode() << 4 | val.encode()
                    }
                    DWrite { addr, val } => {
                        0b_0010_0000_1000_1000 | addr.encode() << 4 | val.encode()
                    }
                    Movso { dest, src } => {
                        0b_0010_0001_1000_0000 | src.encode() << 4 | dest.encode()
                    }
                    Movsi { dest, src } => {
                        0b_0010_0001_1100_0000 | dest.encode() << 4 | src.encode()
                    }
                    Spadd { val } => 0b_0010_0001_1101_1000 | val.encode(),
                    Not { reg } => 0b_0100_0000_0000_0000 | reg.encode(),
                    And { reg, val } => {
                        0b_0100_0001_0000_0000 | val.encode() << 4 | reg.encode()
                    }
                    Or { reg, val } => {
                        0b_0100_0010_0000_0000 | val.encode() << 4 | reg.encode()
                    }
                    Xor { reg, val } => {
                        0b_0100_0011_0000_0000 | val.encode() << 4 | reg.encode()
                    }
                    Shl { reg, val } => {
                        0b_0100_0100_0000_0000 | val.encode() << 4 | reg.encode()
                    }
                    Shr { reg, val } => {
                        0b_0100_0101_0000_0000 | val.encode() << 4 | reg.encode()
                    }
                    Neg { reg } => 0b_0100_1000_0000_0000 | reg.encode(),
                    Inc { reg } => 0b_0100_1000_0001_0000 | reg.encode(),
                    Dec { reg } => 0b_0100_1000_0010_0000 | reg.encode(),
                    Add { reg, val } => {
                        0b_0100_1001_0000_0000 | val.encode() << 4 | reg.encode()
                    }
                    Sub { reg, val } => {
                        0b_0100_1010_0000_0000 | val.encode() << 4 | reg.encode()
                    }
                    Mulu { reg, val } => {
                        0b_0100_1011_0000_0000 | val.encode() << 4 | reg.encode()
                    }
                    Muli { reg, val } => {
                        0b_0100_1011_1000_0000 | val.encode() << 4 | reg.encode()
                    }
                    Inv => 0b_0101_0000_0000_0000,
                    Eqz { reg } => 0b_0101_0000_0001_0000 | reg.encode(),
                    Eq { left, right } => {
                        0b_0101_0001_0000_0000 | left.encode() << 4 | right.encode()
                    }
                    Gt { left, right } => {
                        0b_0101_0010_0000_0000 | left.encode() << 4 | right.encode()
                    }
                    Gteq { left, right } => {
                        0b_0101_0011_0000_0000 | left.encode() << 4 | right.encode()
                    }
                    Jmpoff { offset } => 0b_0110_0000_0000_0000 | offset as u16,
                    Cjmpoff { offset } => 0b_0110_0001_0000_0000 | offset as u16,
                    Jmp { src } => 0b_0110_0010_0000_0000 | src.encode(),
                    Cjmp { src } => 0b_0110_0011_0000_0000 | src.encode(),
                    Call { src } => 0b_0110_0010_0001_0000 | src.encode(),
                    Ret => 0b_0110_0010_0001_1000,
                }
            }
            fn decode(bits: u16) -> Option<Self> {
                use Instruction::*;
                Some(
                    match bits {
                        bits if bits & 0b1111111111111111 == 0b0000000000000000 => Nop,
                        bits if bits & 0b1111111111111111 == 0b1111111111111111 => Hlt,
                        bits if bits & 0b1111111110001000 == 0b0010000100000000 => {
                            let d = (bits & 0b111) >> 0usize;
                            let s = (bits & 0b1110000) >> 4usize;
                            Mov {
                                src: GPR::decode(s)?,
                                dest: GPR::decode(d)?,
                            }
                        }
                        bits if bits & 0b1111111111001100 == 0b0010000100001000 => {
                            let d = (bits & 0b11) >> 0usize;
                            let s = (bits & 0b110000) >> 4usize;
                            DMov {
                                src: DR::decode(s)?,
                                dest: DR::decode(d)?,
                            }
                        }
                        bits if bits & 0b1111100000000000 == 0b0001000000000000 => {
                            let b = (bits & 0b11111111) >> 0usize;
                            let r = (bits & 0b11100000000) >> 8usize;
                            Putl {
                                reg: GPR::decode(r)?,
                                imm: b as u8,
                            }
                        }
                        bits if bits & 0b1111100000000000 == 0b0001100000000000 => {
                            let b = (bits & 0b11111111) >> 0usize;
                            let r = (bits & 0b11100000000) >> 8usize;
                            Puth {
                                reg: GPR::decode(r)?,
                                imm: b as u8,
                            }
                        }
                        bits if bits & 0b1111111110001000 == 0b0010000000000000 => {
                            let a = (bits & 0b1110000) >> 4usize;
                            let r = (bits & 0b111) >> 0usize;
                            Read {
                                addr: GPR::decode(a)?,
                                val: GPR::decode(r)?,
                            }
                        }
                        bits if bits & 0b1111111110001000 == 0b0010000010000000 => {
                            let a = (bits & 0b1110000) >> 4usize;
                            let r = (bits & 0b111) >> 0usize;
                            Write {
                                addr: GPR::decode(a)?,
                                val: GPR::decode(r)?,
                            }
                        }
                        bits if bits & 0b1111111110001100 == 0b0010000000001000 => {
                            let a = (bits & 0b1110000) >> 4usize;
                            let d = (bits & 0b11) >> 0usize;
                            DRead {
                                addr: GPR::decode(a)?,
                                val: DR::decode(d)?,
                            }
                        }
                        bits if bits & 0b1111111110001100 == 0b0010000010001000 => {
                            let a = (bits & 0b1110000) >> 4usize;
                            let d = (bits & 0b11) >> 0usize;
                            DWrite {
                                addr: GPR::decode(a)?,
                                val: DR::decode(d)?,
                            }
                        }
                        bits if bits & 0b1111111111001000 == 0b0010000110000000 => {
                            let d = (bits & 0b111) >> 0usize;
                            let s = (bits & 0b110000) >> 4usize;
                            Movso {
                                src: SPR::decode(s)?,
                                dest: GPR::decode(d)?,
                            }
                        }
                        bits if bits & 0b1111111111001000 == 0b0010000111000000 => {
                            let d = (bits & 0b110000) >> 4usize;
                            let s = (bits & 0b111) >> 0usize;
                            Movsi {
                                src: GPR::decode(s)?,
                                dest: SPR::decode(d)?,
                            }
                        }
                        bits if bits & 0b1111111111111000 == 0b0010000111011000 => {
                            let o = (bits & 0b111) >> 0usize;
                            Spadd { val: GPR::decode(o)? }
                        }
                        bits if bits & 0b1111111111111000 == 0b0100000000000000 => {
                            let r = (bits & 0b111) >> 0usize;
                            Not { reg: GPR::decode(r)? }
                        }
                        bits if bits & 0b1111111110001000 == 0b0100000100000000 => {
                            let r = (bits & 0b111) >> 0usize;
                            let x = (bits & 0b1110000) >> 4usize;
                            And {
                                reg: GPR::decode(r)?,
                                val: GPR::decode(x)?,
                            }
                        }
                        bits if bits & 0b1111111110001000 == 0b0100001000000000 => {
                            let r = (bits & 0b111) >> 0usize;
                            let x = (bits & 0b1110000) >> 4usize;
                            Or {
                                reg: GPR::decode(r)?,
                                val: GPR::decode(x)?,
                            }
                        }
                        bits if bits & 0b1111111110001000 == 0b0100001100000000 => {
                            let r = (bits & 0b111) >> 0usize;
                            let x = (bits & 0b1110000) >> 4usize;
                            Xor {
                                reg: GPR::decode(r)?,
                                val: GPR::decode(x)?,
                            }
                        }
                        bits if bits & 0b1111111110001000 == 0b0100010000000000 => {
                            let r = (bits & 0b111) >> 0usize;
                            let x = (bits & 0b1110000) >> 4usize;
                            Shl {
                                reg: GPR::decode(r)?,
                                val: GPR::decode(x)?,
                            }
                        }
                        bits if bits & 0b1111111110001000 == 0b0100010100000000 => {
                            let r = (bits & 0b111) >> 0usize;
                            let x = (bits & 0b1110000) >> 4usize;
                            Shr {
                                reg: GPR::decode(r)?,
                                val: GPR::decode(x)?,
                            }
                        }
                        bits if bits & 0b1111111111111000 == 0b0100100000000000 => {
                            let r = (bits & 0b111) >> 0usize;
                            Neg { reg: GPR::decode(r)? }
                        }
                        bits if bits & 0b1111111111111000 == 0b0100100000010000 => {
                            let r = (bits & 0b111) >> 0usize;
                            Inc { reg: GPR::decode(r)? }
                        }
                        bits if bits & 0b1111111111111000 == 0b0100100000100000 => {
                            let r = (bits & 0b111) >> 0usize;
                            Dec { reg: GPR::decode(r)? }
                        }
                        bits if bits & 0b1111111110001000 == 0b0100100100000000 => {
                            let r = (bits & 0b111) >> 0usize;
                            let x = (bits & 0b1110000) >> 4usize;
                            Add {
                                reg: GPR::decode(r)?,
                                val: GPR::decode(x)?,
                            }
                        }
                        bits if bits & 0b1111111110001000 == 0b0100101000000000 => {
                            let r = (bits & 0b111) >> 0usize;
                            let x = (bits & 0b1110000) >> 4usize;
                            Sub {
                                reg: GPR::decode(r)?,
                                val: GPR::decode(x)?,
                            }
                        }
                        bits if bits & 0b1111111110001000 == 0b0100101100000000 => {
                            let r = (bits & 0b111) >> 0usize;
                            let x = (bits & 0b1110000) >> 4usize;
                            Mulu {
                                reg: GPR::decode(r)?,
                                val: GPR::decode(x)?,
                            }
                        }
                        bits if bits & 0b1111111110001000 == 0b0100101110000000 => {
                            let r = (bits & 0b111) >> 0usize;
                            let x = (bits & 0b1110000) >> 4usize;
                            Muli {
                                reg: GPR::decode(r)?,
                                val: GPR::decode(x)?,
                            }
                        }
                        bits if bits & 0b1111111111111111 == 0b0101000000000000 => Inv,
                        bits if bits & 0b1111111111111000 == 0b0101000000010000 => {
                            let r = (bits & 0b111) >> 0usize;
                            Eqz { reg: GPR::decode(r)? }
                        }
                        bits if bits & 0b1111111110001000 == 0b0101000100000000 => {
                            let a = (bits & 0b1110000) >> 4usize;
                            let b = (bits & 0b111) >> 0usize;
                            Eq {
                                left: GPR::decode(a)?,
                                right: GPR::decode(b)?,
                            }
                        }
                        bits if bits & 0b1111111110001000 == 0b0101001000000000 => {
                            let a = (bits & 0b1110000) >> 4usize;
                            let b = (bits & 0b111) >> 0usize;
                            Gt {
                                left: GPR::decode(a)?,
                                right: GPR::decode(b)?,
                            }
                        }
                        bits if bits & 0b1111111110001000 == 0b0101001100000000 => {
                            let a = (bits & 0b1110000) >> 4usize;
                            let b = (bits & 0b111) >> 0usize;
                            Gteq {
                                left: GPR::decode(a)?,
                                right: GPR::decode(b)?,
                            }
                        }
                        bits if bits & 0b1111111100000000 == 0b0110000000000000 => {
                            let b = (bits & 0b11111111) >> 0usize;
                            Jmpoff { offset: b as u8 }
                        }
                        bits if bits & 0b1111111100000000 == 0b0110000100000000 => {
                            let b = (bits & 0b11111111) >> 0usize;
                            Cjmpoff { offset: b as u8 }
                        }
                        bits if bits & 0b1111111111111000 == 0b0110001000000000 => {
                            let r = (bits & 0b111) >> 0usize;
                            Jmp { src: GPR::decode(r)? }
                        }
                        bits if bits & 0b1111111111111000 == 0b0110001100000000 => {
                            let r = (bits & 0b111) >> 0usize;
                            Cjmp { src: GPR::decode(r)? }
                        }
                        bits if bits & 0b1111111111111000 == 0b0110001000010000 => {
                            let r = (bits & 0b111) >> 0usize;
                            Call { src: GPR::decode(r)? }
                        }
                        bits if bits & 0b1111111111111111 == 0b0110001000011000 => Ret,
                        _ => return None,
                        _ => {
                            ::core::panicking::unreachable_display(
                                &"#[bitmatch] fallback branch",
                            )
                        }
                    },
                )
            }
        }
    }
    pub use encoding::*;
    mod assembly {
        use crate::{ToAssembly, Instruction, InstructionOpcode};
        impl ToAssembly for Instruction {
            fn to_assembly(&self) -> String {
                let opcode: InstructionOpcode = self.into();
                let opcode = opcode.mnemonic();
                let operands = operands_for_assembly(self);
                if operands.is_empty() {
                    opcode.to_string()
                } else {
                    {
                        let res = ::alloc::fmt::format(
                            format_args!(
                                "{0} {1}",
                                opcode,
                                operands
                                    .iter()
                                    .map(|o| o.to_assembly())
                                    .collect::<Vec<_>>()
                                    .join(", "),
                            ),
                        );
                        res
                    }
                }
            }
        }
        fn operands_for_assembly(ins: &Instruction) -> Vec<&dyn ToAssembly> {
            use Instruction::*;
            match ins {
                Nop => ::alloc::vec::Vec::new(),
                Hlt => ::alloc::vec::Vec::new(),
                Mov { dest, src } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([dest, src]))
                }
                DMov { dest, src } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([dest, src]))
                }
                Putl { reg, imm } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([reg, imm]))
                }
                Puth { reg, imm } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([reg, imm]))
                }
                Read { addr, val } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([addr, val]))
                }
                Write { addr, val } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([addr, val]))
                }
                DRead { addr, val } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([addr, val]))
                }
                DWrite { addr, val } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([addr, val]))
                }
                Movso { dest, src } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([dest, src]))
                }
                Movsi { dest, src } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([dest, src]))
                }
                Spadd { val } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([val]))
                }
                Not { reg } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([reg]))
                }
                And { reg, val } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([reg, val]))
                }
                Or { reg, val } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([reg, val]))
                }
                Xor { reg, val } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([reg, val]))
                }
                Shl { reg, val } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([reg, val]))
                }
                Shr { reg, val } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([reg, val]))
                }
                Neg { reg } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([reg]))
                }
                Inc { reg } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([reg]))
                }
                Dec { reg } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([reg]))
                }
                Add { reg, val } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([reg, val]))
                }
                Sub { reg, val } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([reg, val]))
                }
                Mulu { reg, val } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([reg, val]))
                }
                Muli { reg, val } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([reg, val]))
                }
                Inv => ::alloc::vec::Vec::new(),
                Eqz { reg } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([reg]))
                }
                Eq { left, right } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([left, right]))
                }
                Gt { left, right } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([left, right]))
                }
                Gteq { left, right } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([left, right]))
                }
                Jmpoff { offset } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([offset]))
                }
                Cjmpoff { offset } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([offset]))
                }
                Jmp { src } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([src]))
                }
                Cjmp { src } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([src]))
                }
                Call { src } => {
                    <[_]>::into_vec(#[rustc_box] ::alloc::boxed::Box::new([src]))
                }
                Ret => ::alloc::vec::Vec::new(),
            }
        }
    }
    pub use assembly::*;
    #[strum_discriminants(name(InstructionOpcode))]
    #[strum_discriminants(derive(AsRefStr, InstructionOpcodeMnemonics))]
    pub enum Instruction {
        Nop,
        Hlt,
        Mov { dest: GPR, src: GPR },
        DMov { dest: DR, src: DR },
        Putl { reg: GPR, imm: u8 },
        Puth { reg: GPR, imm: u8 },
        Read { addr: GPR, val: GPR },
        Write { addr: GPR, val: GPR },
        DRead { addr: GPR, val: DR },
        DWrite { addr: GPR, val: DR },
        Movso { dest: GPR, src: SPR },
        Movsi { dest: SPR, src: GPR },
        Spadd { val: GPR },
        Not { reg: GPR },
        And { reg: GPR, val: GPR },
        Or { reg: GPR, val: GPR },
        Xor { reg: GPR, val: GPR },
        Shl { reg: GPR, val: GPR },
        Shr { reg: GPR, val: GPR },
        Neg { reg: GPR },
        Inc { reg: GPR },
        Dec { reg: GPR },
        Add { reg: GPR, val: GPR },
        Sub { reg: GPR, val: GPR },
        Mulu { reg: GPR, val: GPR },
        Muli { reg: GPR, val: GPR },
        Inv,
        Eqz { reg: GPR },
        Eq { left: GPR, right: GPR },
        Gt { left: GPR, right: GPR },
        Gteq { left: GPR, right: GPR },
        Jmpoff { offset: u8 },
        Cjmpoff { offset: u8 },
        Jmp { src: GPR },
        Cjmp { src: GPR },
        Call { src: GPR },
        Ret,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Instruction {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                Instruction::Nop => ::core::fmt::Formatter::write_str(f, "Nop"),
                Instruction::Hlt => ::core::fmt::Formatter::write_str(f, "Hlt"),
                Instruction::Mov { dest: __self_0, src: __self_1 } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "Mov",
                        "dest",
                        __self_0,
                        "src",
                        &__self_1,
                    )
                }
                Instruction::DMov { dest: __self_0, src: __self_1 } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "DMov",
                        "dest",
                        __self_0,
                        "src",
                        &__self_1,
                    )
                }
                Instruction::Putl { reg: __self_0, imm: __self_1 } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "Putl",
                        "reg",
                        __self_0,
                        "imm",
                        &__self_1,
                    )
                }
                Instruction::Puth { reg: __self_0, imm: __self_1 } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "Puth",
                        "reg",
                        __self_0,
                        "imm",
                        &__self_1,
                    )
                }
                Instruction::Read { addr: __self_0, val: __self_1 } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "Read",
                        "addr",
                        __self_0,
                        "val",
                        &__self_1,
                    )
                }
                Instruction::Write { addr: __self_0, val: __self_1 } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "Write",
                        "addr",
                        __self_0,
                        "val",
                        &__self_1,
                    )
                }
                Instruction::DRead { addr: __self_0, val: __self_1 } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "DRead",
                        "addr",
                        __self_0,
                        "val",
                        &__self_1,
                    )
                }
                Instruction::DWrite { addr: __self_0, val: __self_1 } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "DWrite",
                        "addr",
                        __self_0,
                        "val",
                        &__self_1,
                    )
                }
                Instruction::Movso { dest: __self_0, src: __self_1 } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "Movso",
                        "dest",
                        __self_0,
                        "src",
                        &__self_1,
                    )
                }
                Instruction::Movsi { dest: __self_0, src: __self_1 } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "Movsi",
                        "dest",
                        __self_0,
                        "src",
                        &__self_1,
                    )
                }
                Instruction::Spadd { val: __self_0 } => {
                    ::core::fmt::Formatter::debug_struct_field1_finish(
                        f,
                        "Spadd",
                        "val",
                        &__self_0,
                    )
                }
                Instruction::Not { reg: __self_0 } => {
                    ::core::fmt::Formatter::debug_struct_field1_finish(
                        f,
                        "Not",
                        "reg",
                        &__self_0,
                    )
                }
                Instruction::And { reg: __self_0, val: __self_1 } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "And",
                        "reg",
                        __self_0,
                        "val",
                        &__self_1,
                    )
                }
                Instruction::Or { reg: __self_0, val: __self_1 } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "Or",
                        "reg",
                        __self_0,
                        "val",
                        &__self_1,
                    )
                }
                Instruction::Xor { reg: __self_0, val: __self_1 } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "Xor",
                        "reg",
                        __self_0,
                        "val",
                        &__self_1,
                    )
                }
                Instruction::Shl { reg: __self_0, val: __self_1 } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "Shl",
                        "reg",
                        __self_0,
                        "val",
                        &__self_1,
                    )
                }
                Instruction::Shr { reg: __self_0, val: __self_1 } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "Shr",
                        "reg",
                        __self_0,
                        "val",
                        &__self_1,
                    )
                }
                Instruction::Neg { reg: __self_0 } => {
                    ::core::fmt::Formatter::debug_struct_field1_finish(
                        f,
                        "Neg",
                        "reg",
                        &__self_0,
                    )
                }
                Instruction::Inc { reg: __self_0 } => {
                    ::core::fmt::Formatter::debug_struct_field1_finish(
                        f,
                        "Inc",
                        "reg",
                        &__self_0,
                    )
                }
                Instruction::Dec { reg: __self_0 } => {
                    ::core::fmt::Formatter::debug_struct_field1_finish(
                        f,
                        "Dec",
                        "reg",
                        &__self_0,
                    )
                }
                Instruction::Add { reg: __self_0, val: __self_1 } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "Add",
                        "reg",
                        __self_0,
                        "val",
                        &__self_1,
                    )
                }
                Instruction::Sub { reg: __self_0, val: __self_1 } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "Sub",
                        "reg",
                        __self_0,
                        "val",
                        &__self_1,
                    )
                }
                Instruction::Mulu { reg: __self_0, val: __self_1 } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "Mulu",
                        "reg",
                        __self_0,
                        "val",
                        &__self_1,
                    )
                }
                Instruction::Muli { reg: __self_0, val: __self_1 } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "Muli",
                        "reg",
                        __self_0,
                        "val",
                        &__self_1,
                    )
                }
                Instruction::Inv => ::core::fmt::Formatter::write_str(f, "Inv"),
                Instruction::Eqz { reg: __self_0 } => {
                    ::core::fmt::Formatter::debug_struct_field1_finish(
                        f,
                        "Eqz",
                        "reg",
                        &__self_0,
                    )
                }
                Instruction::Eq { left: __self_0, right: __self_1 } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "Eq",
                        "left",
                        __self_0,
                        "right",
                        &__self_1,
                    )
                }
                Instruction::Gt { left: __self_0, right: __self_1 } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "Gt",
                        "left",
                        __self_0,
                        "right",
                        &__self_1,
                    )
                }
                Instruction::Gteq { left: __self_0, right: __self_1 } => {
                    ::core::fmt::Formatter::debug_struct_field2_finish(
                        f,
                        "Gteq",
                        "left",
                        __self_0,
                        "right",
                        &__self_1,
                    )
                }
                Instruction::Jmpoff { offset: __self_0 } => {
                    ::core::fmt::Formatter::debug_struct_field1_finish(
                        f,
                        "Jmpoff",
                        "offset",
                        &__self_0,
                    )
                }
                Instruction::Cjmpoff { offset: __self_0 } => {
                    ::core::fmt::Formatter::debug_struct_field1_finish(
                        f,
                        "Cjmpoff",
                        "offset",
                        &__self_0,
                    )
                }
                Instruction::Jmp { src: __self_0 } => {
                    ::core::fmt::Formatter::debug_struct_field1_finish(
                        f,
                        "Jmp",
                        "src",
                        &__self_0,
                    )
                }
                Instruction::Cjmp { src: __self_0 } => {
                    ::core::fmt::Formatter::debug_struct_field1_finish(
                        f,
                        "Cjmp",
                        "src",
                        &__self_0,
                    )
                }
                Instruction::Call { src: __self_0 } => {
                    ::core::fmt::Formatter::debug_struct_field1_finish(
                        f,
                        "Call",
                        "src",
                        &__self_0,
                    )
                }
                Instruction::Ret => ::core::fmt::Formatter::write_str(f, "Ret"),
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for Instruction {
        #[inline]
        fn clone(&self) -> Instruction {
            let _: ::core::clone::AssertParamIsClone<GPR>;
            let _: ::core::clone::AssertParamIsClone<DR>;
            let _: ::core::clone::AssertParamIsClone<u8>;
            let _: ::core::clone::AssertParamIsClone<SPR>;
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for Instruction {}
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for Instruction {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for Instruction {
        #[inline]
        fn eq(&self, other: &Instruction) -> bool {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            let __arg1_tag = ::core::intrinsics::discriminant_value(other);
            __self_tag == __arg1_tag
                && match (self, other) {
                    (
                        Instruction::Mov { dest: __self_0, src: __self_1 },
                        Instruction::Mov { dest: __arg1_0, src: __arg1_1 },
                    ) => *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1,
                    (
                        Instruction::DMov { dest: __self_0, src: __self_1 },
                        Instruction::DMov { dest: __arg1_0, src: __arg1_1 },
                    ) => *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1,
                    (
                        Instruction::Putl { reg: __self_0, imm: __self_1 },
                        Instruction::Putl { reg: __arg1_0, imm: __arg1_1 },
                    ) => *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1,
                    (
                        Instruction::Puth { reg: __self_0, imm: __self_1 },
                        Instruction::Puth { reg: __arg1_0, imm: __arg1_1 },
                    ) => *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1,
                    (
                        Instruction::Read { addr: __self_0, val: __self_1 },
                        Instruction::Read { addr: __arg1_0, val: __arg1_1 },
                    ) => *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1,
                    (
                        Instruction::Write { addr: __self_0, val: __self_1 },
                        Instruction::Write { addr: __arg1_0, val: __arg1_1 },
                    ) => *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1,
                    (
                        Instruction::DRead { addr: __self_0, val: __self_1 },
                        Instruction::DRead { addr: __arg1_0, val: __arg1_1 },
                    ) => *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1,
                    (
                        Instruction::DWrite { addr: __self_0, val: __self_1 },
                        Instruction::DWrite { addr: __arg1_0, val: __arg1_1 },
                    ) => *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1,
                    (
                        Instruction::Movso { dest: __self_0, src: __self_1 },
                        Instruction::Movso { dest: __arg1_0, src: __arg1_1 },
                    ) => *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1,
                    (
                        Instruction::Movsi { dest: __self_0, src: __self_1 },
                        Instruction::Movsi { dest: __arg1_0, src: __arg1_1 },
                    ) => *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1,
                    (
                        Instruction::Spadd { val: __self_0 },
                        Instruction::Spadd { val: __arg1_0 },
                    ) => *__self_0 == *__arg1_0,
                    (
                        Instruction::Not { reg: __self_0 },
                        Instruction::Not { reg: __arg1_0 },
                    ) => *__self_0 == *__arg1_0,
                    (
                        Instruction::And { reg: __self_0, val: __self_1 },
                        Instruction::And { reg: __arg1_0, val: __arg1_1 },
                    ) => *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1,
                    (
                        Instruction::Or { reg: __self_0, val: __self_1 },
                        Instruction::Or { reg: __arg1_0, val: __arg1_1 },
                    ) => *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1,
                    (
                        Instruction::Xor { reg: __self_0, val: __self_1 },
                        Instruction::Xor { reg: __arg1_0, val: __arg1_1 },
                    ) => *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1,
                    (
                        Instruction::Shl { reg: __self_0, val: __self_1 },
                        Instruction::Shl { reg: __arg1_0, val: __arg1_1 },
                    ) => *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1,
                    (
                        Instruction::Shr { reg: __self_0, val: __self_1 },
                        Instruction::Shr { reg: __arg1_0, val: __arg1_1 },
                    ) => *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1,
                    (
                        Instruction::Neg { reg: __self_0 },
                        Instruction::Neg { reg: __arg1_0 },
                    ) => *__self_0 == *__arg1_0,
                    (
                        Instruction::Inc { reg: __self_0 },
                        Instruction::Inc { reg: __arg1_0 },
                    ) => *__self_0 == *__arg1_0,
                    (
                        Instruction::Dec { reg: __self_0 },
                        Instruction::Dec { reg: __arg1_0 },
                    ) => *__self_0 == *__arg1_0,
                    (
                        Instruction::Add { reg: __self_0, val: __self_1 },
                        Instruction::Add { reg: __arg1_0, val: __arg1_1 },
                    ) => *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1,
                    (
                        Instruction::Sub { reg: __self_0, val: __self_1 },
                        Instruction::Sub { reg: __arg1_0, val: __arg1_1 },
                    ) => *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1,
                    (
                        Instruction::Mulu { reg: __self_0, val: __self_1 },
                        Instruction::Mulu { reg: __arg1_0, val: __arg1_1 },
                    ) => *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1,
                    (
                        Instruction::Muli { reg: __self_0, val: __self_1 },
                        Instruction::Muli { reg: __arg1_0, val: __arg1_1 },
                    ) => *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1,
                    (
                        Instruction::Eqz { reg: __self_0 },
                        Instruction::Eqz { reg: __arg1_0 },
                    ) => *__self_0 == *__arg1_0,
                    (
                        Instruction::Eq { left: __self_0, right: __self_1 },
                        Instruction::Eq { left: __arg1_0, right: __arg1_1 },
                    ) => *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1,
                    (
                        Instruction::Gt { left: __self_0, right: __self_1 },
                        Instruction::Gt { left: __arg1_0, right: __arg1_1 },
                    ) => *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1,
                    (
                        Instruction::Gteq { left: __self_0, right: __self_1 },
                        Instruction::Gteq { left: __arg1_0, right: __arg1_1 },
                    ) => *__self_0 == *__arg1_0 && *__self_1 == *__arg1_1,
                    (
                        Instruction::Jmpoff { offset: __self_0 },
                        Instruction::Jmpoff { offset: __arg1_0 },
                    ) => *__self_0 == *__arg1_0,
                    (
                        Instruction::Cjmpoff { offset: __self_0 },
                        Instruction::Cjmpoff { offset: __arg1_0 },
                    ) => *__self_0 == *__arg1_0,
                    (
                        Instruction::Jmp { src: __self_0 },
                        Instruction::Jmp { src: __arg1_0 },
                    ) => *__self_0 == *__arg1_0,
                    (
                        Instruction::Cjmp { src: __self_0 },
                        Instruction::Cjmp { src: __arg1_0 },
                    ) => *__self_0 == *__arg1_0,
                    (
                        Instruction::Call { src: __self_0 },
                        Instruction::Call { src: __arg1_0 },
                    ) => *__self_0 == *__arg1_0,
                    _ => true,
                }
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralEq for Instruction {}
    #[automatically_derived]
    impl ::core::cmp::Eq for Instruction {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {
            let _: ::core::cmp::AssertParamIsEq<GPR>;
            let _: ::core::cmp::AssertParamIsEq<DR>;
            let _: ::core::cmp::AssertParamIsEq<u8>;
            let _: ::core::cmp::AssertParamIsEq<SPR>;
        }
    }
    #[automatically_derived]
    impl ::core::hash::Hash for Instruction {
        fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            ::core::hash::Hash::hash(&__self_tag, state);
            match self {
                Instruction::Mov { dest: __self_0, src: __self_1 } => {
                    ::core::hash::Hash::hash(__self_0, state);
                    ::core::hash::Hash::hash(__self_1, state)
                }
                Instruction::DMov { dest: __self_0, src: __self_1 } => {
                    ::core::hash::Hash::hash(__self_0, state);
                    ::core::hash::Hash::hash(__self_1, state)
                }
                Instruction::Putl { reg: __self_0, imm: __self_1 } => {
                    ::core::hash::Hash::hash(__self_0, state);
                    ::core::hash::Hash::hash(__self_1, state)
                }
                Instruction::Puth { reg: __self_0, imm: __self_1 } => {
                    ::core::hash::Hash::hash(__self_0, state);
                    ::core::hash::Hash::hash(__self_1, state)
                }
                Instruction::Read { addr: __self_0, val: __self_1 } => {
                    ::core::hash::Hash::hash(__self_0, state);
                    ::core::hash::Hash::hash(__self_1, state)
                }
                Instruction::Write { addr: __self_0, val: __self_1 } => {
                    ::core::hash::Hash::hash(__self_0, state);
                    ::core::hash::Hash::hash(__self_1, state)
                }
                Instruction::DRead { addr: __self_0, val: __self_1 } => {
                    ::core::hash::Hash::hash(__self_0, state);
                    ::core::hash::Hash::hash(__self_1, state)
                }
                Instruction::DWrite { addr: __self_0, val: __self_1 } => {
                    ::core::hash::Hash::hash(__self_0, state);
                    ::core::hash::Hash::hash(__self_1, state)
                }
                Instruction::Movso { dest: __self_0, src: __self_1 } => {
                    ::core::hash::Hash::hash(__self_0, state);
                    ::core::hash::Hash::hash(__self_1, state)
                }
                Instruction::Movsi { dest: __self_0, src: __self_1 } => {
                    ::core::hash::Hash::hash(__self_0, state);
                    ::core::hash::Hash::hash(__self_1, state)
                }
                Instruction::Spadd { val: __self_0 } => {
                    ::core::hash::Hash::hash(__self_0, state)
                }
                Instruction::Not { reg: __self_0 } => {
                    ::core::hash::Hash::hash(__self_0, state)
                }
                Instruction::And { reg: __self_0, val: __self_1 } => {
                    ::core::hash::Hash::hash(__self_0, state);
                    ::core::hash::Hash::hash(__self_1, state)
                }
                Instruction::Or { reg: __self_0, val: __self_1 } => {
                    ::core::hash::Hash::hash(__self_0, state);
                    ::core::hash::Hash::hash(__self_1, state)
                }
                Instruction::Xor { reg: __self_0, val: __self_1 } => {
                    ::core::hash::Hash::hash(__self_0, state);
                    ::core::hash::Hash::hash(__self_1, state)
                }
                Instruction::Shl { reg: __self_0, val: __self_1 } => {
                    ::core::hash::Hash::hash(__self_0, state);
                    ::core::hash::Hash::hash(__self_1, state)
                }
                Instruction::Shr { reg: __self_0, val: __self_1 } => {
                    ::core::hash::Hash::hash(__self_0, state);
                    ::core::hash::Hash::hash(__self_1, state)
                }
                Instruction::Neg { reg: __self_0 } => {
                    ::core::hash::Hash::hash(__self_0, state)
                }
                Instruction::Inc { reg: __self_0 } => {
                    ::core::hash::Hash::hash(__self_0, state)
                }
                Instruction::Dec { reg: __self_0 } => {
                    ::core::hash::Hash::hash(__self_0, state)
                }
                Instruction::Add { reg: __self_0, val: __self_1 } => {
                    ::core::hash::Hash::hash(__self_0, state);
                    ::core::hash::Hash::hash(__self_1, state)
                }
                Instruction::Sub { reg: __self_0, val: __self_1 } => {
                    ::core::hash::Hash::hash(__self_0, state);
                    ::core::hash::Hash::hash(__self_1, state)
                }
                Instruction::Mulu { reg: __self_0, val: __self_1 } => {
                    ::core::hash::Hash::hash(__self_0, state);
                    ::core::hash::Hash::hash(__self_1, state)
                }
                Instruction::Muli { reg: __self_0, val: __self_1 } => {
                    ::core::hash::Hash::hash(__self_0, state);
                    ::core::hash::Hash::hash(__self_1, state)
                }
                Instruction::Eqz { reg: __self_0 } => {
                    ::core::hash::Hash::hash(__self_0, state)
                }
                Instruction::Eq { left: __self_0, right: __self_1 } => {
                    ::core::hash::Hash::hash(__self_0, state);
                    ::core::hash::Hash::hash(__self_1, state)
                }
                Instruction::Gt { left: __self_0, right: __self_1 } => {
                    ::core::hash::Hash::hash(__self_0, state);
                    ::core::hash::Hash::hash(__self_1, state)
                }
                Instruction::Gteq { left: __self_0, right: __self_1 } => {
                    ::core::hash::Hash::hash(__self_0, state);
                    ::core::hash::Hash::hash(__self_1, state)
                }
                Instruction::Jmpoff { offset: __self_0 } => {
                    ::core::hash::Hash::hash(__self_0, state)
                }
                Instruction::Cjmpoff { offset: __self_0 } => {
                    ::core::hash::Hash::hash(__self_0, state)
                }
                Instruction::Jmp { src: __self_0 } => {
                    ::core::hash::Hash::hash(__self_0, state)
                }
                Instruction::Cjmp { src: __self_0 } => {
                    ::core::hash::Hash::hash(__self_0, state)
                }
                Instruction::Call { src: __self_0 } => {
                    ::core::hash::Hash::hash(__self_0, state)
                }
                _ => {}
            }
        }
    }
    impl ::core::convert::AsRef<str> for Instruction {
        fn as_ref(&self) -> &str {
            match *self {
                Instruction::Nop => "Nop",
                Instruction::Hlt => "Hlt",
                Instruction::Mov { .. } => "Mov",
                Instruction::DMov { .. } => "DMov",
                Instruction::Putl { .. } => "Putl",
                Instruction::Puth { .. } => "Puth",
                Instruction::Read { .. } => "Read",
                Instruction::Write { .. } => "Write",
                Instruction::DRead { .. } => "DRead",
                Instruction::DWrite { .. } => "DWrite",
                Instruction::Movso { .. } => "Movso",
                Instruction::Movsi { .. } => "Movsi",
                Instruction::Spadd { .. } => "Spadd",
                Instruction::Not { .. } => "Not",
                Instruction::And { .. } => "And",
                Instruction::Or { .. } => "Or",
                Instruction::Xor { .. } => "Xor",
                Instruction::Shl { .. } => "Shl",
                Instruction::Shr { .. } => "Shr",
                Instruction::Neg { .. } => "Neg",
                Instruction::Inc { .. } => "Inc",
                Instruction::Dec { .. } => "Dec",
                Instruction::Add { .. } => "Add",
                Instruction::Sub { .. } => "Sub",
                Instruction::Mulu { .. } => "Mulu",
                Instruction::Muli { .. } => "Muli",
                Instruction::Inv => "Inv",
                Instruction::Eqz { .. } => "Eqz",
                Instruction::Eq { .. } => "Eq",
                Instruction::Gt { .. } => "Gt",
                Instruction::Gteq { .. } => "Gteq",
                Instruction::Jmpoff { .. } => "Jmpoff",
                Instruction::Cjmpoff { .. } => "Cjmpoff",
                Instruction::Jmp { .. } => "Jmp",
                Instruction::Cjmp { .. } => "Cjmp",
                Instruction::Call { .. } => "Call",
                Instruction::Ret => "Ret",
            }
        }
    }
    /// Auto-generated discriminant enum variants
    pub enum InstructionOpcode {
        Nop,
        Hlt,
        Mov,
        DMov,
        Putl,
        Puth,
        Read,
        Write,
        DRead,
        DWrite,
        Movso,
        Movsi,
        Spadd,
        Not,
        And,
        Or,
        Xor,
        Shl,
        Shr,
        Neg,
        Inc,
        Dec,
        Add,
        Sub,
        Mulu,
        Muli,
        Inv,
        Eqz,
        Eq,
        Gt,
        Gteq,
        Jmpoff,
        Cjmpoff,
        Jmp,
        Cjmp,
        Call,
        Ret,
    }
    #[automatically_derived]
    impl ::core::clone::Clone for InstructionOpcode {
        #[inline]
        fn clone(&self) -> InstructionOpcode {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for InstructionOpcode {}
    #[automatically_derived]
    impl ::core::fmt::Debug for InstructionOpcode {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(
                f,
                match self {
                    InstructionOpcode::Nop => "Nop",
                    InstructionOpcode::Hlt => "Hlt",
                    InstructionOpcode::Mov => "Mov",
                    InstructionOpcode::DMov => "DMov",
                    InstructionOpcode::Putl => "Putl",
                    InstructionOpcode::Puth => "Puth",
                    InstructionOpcode::Read => "Read",
                    InstructionOpcode::Write => "Write",
                    InstructionOpcode::DRead => "DRead",
                    InstructionOpcode::DWrite => "DWrite",
                    InstructionOpcode::Movso => "Movso",
                    InstructionOpcode::Movsi => "Movsi",
                    InstructionOpcode::Spadd => "Spadd",
                    InstructionOpcode::Not => "Not",
                    InstructionOpcode::And => "And",
                    InstructionOpcode::Or => "Or",
                    InstructionOpcode::Xor => "Xor",
                    InstructionOpcode::Shl => "Shl",
                    InstructionOpcode::Shr => "Shr",
                    InstructionOpcode::Neg => "Neg",
                    InstructionOpcode::Inc => "Inc",
                    InstructionOpcode::Dec => "Dec",
                    InstructionOpcode::Add => "Add",
                    InstructionOpcode::Sub => "Sub",
                    InstructionOpcode::Mulu => "Mulu",
                    InstructionOpcode::Muli => "Muli",
                    InstructionOpcode::Inv => "Inv",
                    InstructionOpcode::Eqz => "Eqz",
                    InstructionOpcode::Eq => "Eq",
                    InstructionOpcode::Gt => "Gt",
                    InstructionOpcode::Gteq => "Gteq",
                    InstructionOpcode::Jmpoff => "Jmpoff",
                    InstructionOpcode::Cjmpoff => "Cjmpoff",
                    InstructionOpcode::Jmp => "Jmp",
                    InstructionOpcode::Cjmp => "Cjmp",
                    InstructionOpcode::Call => "Call",
                    InstructionOpcode::Ret => "Ret",
                },
            )
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for InstructionOpcode {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for InstructionOpcode {
        #[inline]
        fn eq(&self, other: &InstructionOpcode) -> bool {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            let __arg1_tag = ::core::intrinsics::discriminant_value(other);
            __self_tag == __arg1_tag
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralEq for InstructionOpcode {}
    #[automatically_derived]
    impl ::core::cmp::Eq for InstructionOpcode {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {}
    }
    impl ::core::convert::AsRef<str> for InstructionOpcode {
        fn as_ref(&self) -> &str {
            match *self {
                InstructionOpcode::Nop => "Nop",
                InstructionOpcode::Hlt => "Hlt",
                InstructionOpcode::Mov => "Mov",
                InstructionOpcode::DMov => "DMov",
                InstructionOpcode::Putl => "Putl",
                InstructionOpcode::Puth => "Puth",
                InstructionOpcode::Read => "Read",
                InstructionOpcode::Write => "Write",
                InstructionOpcode::DRead => "DRead",
                InstructionOpcode::DWrite => "DWrite",
                InstructionOpcode::Movso => "Movso",
                InstructionOpcode::Movsi => "Movsi",
                InstructionOpcode::Spadd => "Spadd",
                InstructionOpcode::Not => "Not",
                InstructionOpcode::And => "And",
                InstructionOpcode::Or => "Or",
                InstructionOpcode::Xor => "Xor",
                InstructionOpcode::Shl => "Shl",
                InstructionOpcode::Shr => "Shr",
                InstructionOpcode::Neg => "Neg",
                InstructionOpcode::Inc => "Inc",
                InstructionOpcode::Dec => "Dec",
                InstructionOpcode::Add => "Add",
                InstructionOpcode::Sub => "Sub",
                InstructionOpcode::Mulu => "Mulu",
                InstructionOpcode::Muli => "Muli",
                InstructionOpcode::Inv => "Inv",
                InstructionOpcode::Eqz => "Eqz",
                InstructionOpcode::Eq => "Eq",
                InstructionOpcode::Gt => "Gt",
                InstructionOpcode::Gteq => "Gteq",
                InstructionOpcode::Jmpoff => "Jmpoff",
                InstructionOpcode::Cjmpoff => "Cjmpoff",
                InstructionOpcode::Jmp => "Jmp",
                InstructionOpcode::Cjmp => "Cjmp",
                InstructionOpcode::Call => "Call",
                InstructionOpcode::Ret => "Ret",
            }
        }
    }
    impl InstructionOpcode {
        pub fn from_mnemonic(
            mnemonic: &str,
        ) -> ::std::option::Option<InstructionOpcode> {
            match mnemonic {
                "nop" => ::std::option::Option::Some(Self::Nop),
                "hlt" => ::std::option::Option::Some(Self::Hlt),
                "mov" => ::std::option::Option::Some(Self::Mov),
                "d_mov" => ::std::option::Option::Some(Self::DMov),
                "putl" => ::std::option::Option::Some(Self::Putl),
                "puth" => ::std::option::Option::Some(Self::Puth),
                "read" => ::std::option::Option::Some(Self::Read),
                "write" => ::std::option::Option::Some(Self::Write),
                "d_read" => ::std::option::Option::Some(Self::DRead),
                "d_write" => ::std::option::Option::Some(Self::DWrite),
                "movso" => ::std::option::Option::Some(Self::Movso),
                "movsi" => ::std::option::Option::Some(Self::Movsi),
                "spadd" => ::std::option::Option::Some(Self::Spadd),
                "not" => ::std::option::Option::Some(Self::Not),
                "and" => ::std::option::Option::Some(Self::And),
                "or" => ::std::option::Option::Some(Self::Or),
                "xor" => ::std::option::Option::Some(Self::Xor),
                "shl" => ::std::option::Option::Some(Self::Shl),
                "shr" => ::std::option::Option::Some(Self::Shr),
                "neg" => ::std::option::Option::Some(Self::Neg),
                "inc" => ::std::option::Option::Some(Self::Inc),
                "dec" => ::std::option::Option::Some(Self::Dec),
                "add" => ::std::option::Option::Some(Self::Add),
                "sub" => ::std::option::Option::Some(Self::Sub),
                "mulu" => ::std::option::Option::Some(Self::Mulu),
                "muli" => ::std::option::Option::Some(Self::Muli),
                "inv" => ::std::option::Option::Some(Self::Inv),
                "eqz" => ::std::option::Option::Some(Self::Eqz),
                "eq" => ::std::option::Option::Some(Self::Eq),
                "gt" => ::std::option::Option::Some(Self::Gt),
                "gteq" => ::std::option::Option::Some(Self::Gteq),
                "jmpoff" => ::std::option::Option::Some(Self::Jmpoff),
                "cjmpoff" => ::std::option::Option::Some(Self::Cjmpoff),
                "jmp" => ::std::option::Option::Some(Self::Jmp),
                "cjmp" => ::std::option::Option::Some(Self::Cjmp),
                "call" => ::std::option::Option::Some(Self::Call),
                "ret" => ::std::option::Option::Some(Self::Ret),
                _ => ::std::option::Option::None,
            }
        }
        pub fn mnemonic(&self) -> &str {
            match self {
                Self::Nop => "nop",
                Self::Hlt => "hlt",
                Self::Mov => "mov",
                Self::DMov => "d_mov",
                Self::Putl => "putl",
                Self::Puth => "puth",
                Self::Read => "read",
                Self::Write => "write",
                Self::DRead => "d_read",
                Self::DWrite => "d_write",
                Self::Movso => "movso",
                Self::Movsi => "movsi",
                Self::Spadd => "spadd",
                Self::Not => "not",
                Self::And => "and",
                Self::Or => "or",
                Self::Xor => "xor",
                Self::Shl => "shl",
                Self::Shr => "shr",
                Self::Neg => "neg",
                Self::Inc => "inc",
                Self::Dec => "dec",
                Self::Add => "add",
                Self::Sub => "sub",
                Self::Mulu => "mulu",
                Self::Muli => "muli",
                Self::Inv => "inv",
                Self::Eqz => "eqz",
                Self::Eq => "eq",
                Self::Gt => "gt",
                Self::Gteq => "gteq",
                Self::Jmpoff => "jmpoff",
                Self::Cjmpoff => "cjmpoff",
                Self::Jmp => "jmp",
                Self::Cjmp => "cjmp",
                Self::Call => "call",
                Self::Ret => "ret",
            }
        }
    }
    impl ::core::convert::From<Instruction> for InstructionOpcode {
        fn from(val: Instruction) -> InstructionOpcode {
            match val {
                Instruction::Nop => InstructionOpcode::Nop,
                Instruction::Hlt => InstructionOpcode::Hlt,
                Instruction::Mov { .. } => InstructionOpcode::Mov,
                Instruction::DMov { .. } => InstructionOpcode::DMov,
                Instruction::Putl { .. } => InstructionOpcode::Putl,
                Instruction::Puth { .. } => InstructionOpcode::Puth,
                Instruction::Read { .. } => InstructionOpcode::Read,
                Instruction::Write { .. } => InstructionOpcode::Write,
                Instruction::DRead { .. } => InstructionOpcode::DRead,
                Instruction::DWrite { .. } => InstructionOpcode::DWrite,
                Instruction::Movso { .. } => InstructionOpcode::Movso,
                Instruction::Movsi { .. } => InstructionOpcode::Movsi,
                Instruction::Spadd { .. } => InstructionOpcode::Spadd,
                Instruction::Not { .. } => InstructionOpcode::Not,
                Instruction::And { .. } => InstructionOpcode::And,
                Instruction::Or { .. } => InstructionOpcode::Or,
                Instruction::Xor { .. } => InstructionOpcode::Xor,
                Instruction::Shl { .. } => InstructionOpcode::Shl,
                Instruction::Shr { .. } => InstructionOpcode::Shr,
                Instruction::Neg { .. } => InstructionOpcode::Neg,
                Instruction::Inc { .. } => InstructionOpcode::Inc,
                Instruction::Dec { .. } => InstructionOpcode::Dec,
                Instruction::Add { .. } => InstructionOpcode::Add,
                Instruction::Sub { .. } => InstructionOpcode::Sub,
                Instruction::Mulu { .. } => InstructionOpcode::Mulu,
                Instruction::Muli { .. } => InstructionOpcode::Muli,
                Instruction::Inv => InstructionOpcode::Inv,
                Instruction::Eqz { .. } => InstructionOpcode::Eqz,
                Instruction::Eq { .. } => InstructionOpcode::Eq,
                Instruction::Gt { .. } => InstructionOpcode::Gt,
                Instruction::Gteq { .. } => InstructionOpcode::Gteq,
                Instruction::Jmpoff { .. } => InstructionOpcode::Jmpoff,
                Instruction::Cjmpoff { .. } => InstructionOpcode::Cjmpoff,
                Instruction::Jmp { .. } => InstructionOpcode::Jmp,
                Instruction::Cjmp { .. } => InstructionOpcode::Cjmp,
                Instruction::Call { .. } => InstructionOpcode::Call,
                Instruction::Ret => InstructionOpcode::Ret,
            }
        }
    }
    impl<'_enum> ::core::convert::From<&'_enum Instruction> for InstructionOpcode {
        fn from(val: &'_enum Instruction) -> InstructionOpcode {
            match val {
                Instruction::Nop => InstructionOpcode::Nop,
                Instruction::Hlt => InstructionOpcode::Hlt,
                Instruction::Mov { .. } => InstructionOpcode::Mov,
                Instruction::DMov { .. } => InstructionOpcode::DMov,
                Instruction::Putl { .. } => InstructionOpcode::Putl,
                Instruction::Puth { .. } => InstructionOpcode::Puth,
                Instruction::Read { .. } => InstructionOpcode::Read,
                Instruction::Write { .. } => InstructionOpcode::Write,
                Instruction::DRead { .. } => InstructionOpcode::DRead,
                Instruction::DWrite { .. } => InstructionOpcode::DWrite,
                Instruction::Movso { .. } => InstructionOpcode::Movso,
                Instruction::Movsi { .. } => InstructionOpcode::Movsi,
                Instruction::Spadd { .. } => InstructionOpcode::Spadd,
                Instruction::Not { .. } => InstructionOpcode::Not,
                Instruction::And { .. } => InstructionOpcode::And,
                Instruction::Or { .. } => InstructionOpcode::Or,
                Instruction::Xor { .. } => InstructionOpcode::Xor,
                Instruction::Shl { .. } => InstructionOpcode::Shl,
                Instruction::Shr { .. } => InstructionOpcode::Shr,
                Instruction::Neg { .. } => InstructionOpcode::Neg,
                Instruction::Inc { .. } => InstructionOpcode::Inc,
                Instruction::Dec { .. } => InstructionOpcode::Dec,
                Instruction::Add { .. } => InstructionOpcode::Add,
                Instruction::Sub { .. } => InstructionOpcode::Sub,
                Instruction::Mulu { .. } => InstructionOpcode::Mulu,
                Instruction::Muli { .. } => InstructionOpcode::Muli,
                Instruction::Inv => InstructionOpcode::Inv,
                Instruction::Eqz { .. } => InstructionOpcode::Eqz,
                Instruction::Eq { .. } => InstructionOpcode::Eq,
                Instruction::Gt { .. } => InstructionOpcode::Gt,
                Instruction::Gteq { .. } => InstructionOpcode::Gteq,
                Instruction::Jmpoff { .. } => InstructionOpcode::Jmpoff,
                Instruction::Cjmpoff { .. } => InstructionOpcode::Cjmpoff,
                Instruction::Jmp { .. } => InstructionOpcode::Jmp,
                Instruction::Cjmp { .. } => InstructionOpcode::Cjmp,
                Instruction::Call { .. } => InstructionOpcode::Call,
                Instruction::Ret => InstructionOpcode::Ret,
            }
        }
    }
    impl InstructionOpcode {
        pub fn build(&self, ops: &[AnyOperand]) -> Option<Instruction> {
            match self {
                Self::Nop => {
                    if !ops.is_empty() {
                        return None;
                    }
                    Some(Instruction::Nop)
                }
                Self::Hlt => {
                    if !ops.is_empty() {
                        return None;
                    }
                    Some(Instruction::Hlt)
                }
                Self::Mov => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    let AnyOperand::R(AnyRegister::G(operand_1)) = ops[1usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::DMov => {
                    let AnyOperand::R(AnyRegister::D(operand_0)) = ops[0usize] else {
                        return None
                    };
                    let AnyOperand::R(AnyRegister::D(operand_1)) = ops[1usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Putl => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    let AnyOperand::I(operand_1) = ops[1usize] else { return None };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Puth => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    let AnyOperand::I(operand_1) = ops[1usize] else { return None };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Read => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    let AnyOperand::R(AnyRegister::G(operand_1)) = ops[1usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Write => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    let AnyOperand::R(AnyRegister::G(operand_1)) = ops[1usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::DRead => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    let AnyOperand::R(AnyRegister::D(operand_1)) = ops[1usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::DWrite => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    let AnyOperand::R(AnyRegister::D(operand_1)) = ops[1usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Movso => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    let AnyOperand::R(AnyRegister::S(operand_1)) = ops[1usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Movsi => {
                    let AnyOperand::R(AnyRegister::S(operand_0)) = ops[0usize] else {
                        return None
                    };
                    let AnyOperand::R(AnyRegister::G(operand_1)) = ops[1usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Spadd => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Not => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::And => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    let AnyOperand::R(AnyRegister::G(operand_1)) = ops[1usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Or => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    let AnyOperand::R(AnyRegister::G(operand_1)) = ops[1usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Xor => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    let AnyOperand::R(AnyRegister::G(operand_1)) = ops[1usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Shl => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    let AnyOperand::R(AnyRegister::G(operand_1)) = ops[1usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Shr => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    let AnyOperand::R(AnyRegister::G(operand_1)) = ops[1usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Neg => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Inc => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Dec => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Add => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    let AnyOperand::R(AnyRegister::G(operand_1)) = ops[1usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Sub => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    let AnyOperand::R(AnyRegister::G(operand_1)) = ops[1usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Mulu => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    let AnyOperand::R(AnyRegister::G(operand_1)) = ops[1usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Muli => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    let AnyOperand::R(AnyRegister::G(operand_1)) = ops[1usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Inv => {
                    if !ops.is_empty() {
                        return None;
                    }
                    Some(Instruction::Inv)
                }
                Self::Eqz => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Eq => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    let AnyOperand::R(AnyRegister::G(operand_1)) = ops[1usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Gt => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    let AnyOperand::R(AnyRegister::G(operand_1)) = ops[1usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Gteq => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    let AnyOperand::R(AnyRegister::G(operand_1)) = ops[1usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Jmpoff => {
                    let AnyOperand::I(operand_0) = ops[0usize] else { return None };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Cjmpoff => {
                    let AnyOperand::I(operand_0) = ops[0usize] else { return None };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Jmp => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Cjmp => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Call => {
                    let AnyOperand::R(AnyRegister::G(operand_0)) = ops[0usize] else {
                        return None
                    };
                    ::core::panicking::panic("not yet implemented")
                }
                Self::Ret => {
                    if !ops.is_empty() {
                        return None;
                    }
                    Some(Instruction::Ret)
                }
            }
        }
    }
    pub enum AnyOperand {
        R(AnyRegister),
        I(u8),
    }
}
pub use ins::*;
mod reg {
    use crate::{ToAssembly, Encodable};
    pub enum GeneralPurposeRegister {
        R0,
        R1,
        R2,
        R3,
        R4,
        R5,
        R6,
        R7,
        Placeholder(usize),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for GeneralPurposeRegister {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                GeneralPurposeRegister::R0 => ::core::fmt::Formatter::write_str(f, "R0"),
                GeneralPurposeRegister::R1 => ::core::fmt::Formatter::write_str(f, "R1"),
                GeneralPurposeRegister::R2 => ::core::fmt::Formatter::write_str(f, "R2"),
                GeneralPurposeRegister::R3 => ::core::fmt::Formatter::write_str(f, "R3"),
                GeneralPurposeRegister::R4 => ::core::fmt::Formatter::write_str(f, "R4"),
                GeneralPurposeRegister::R5 => ::core::fmt::Formatter::write_str(f, "R5"),
                GeneralPurposeRegister::R6 => ::core::fmt::Formatter::write_str(f, "R6"),
                GeneralPurposeRegister::R7 => ::core::fmt::Formatter::write_str(f, "R7"),
                GeneralPurposeRegister::Placeholder(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Placeholder",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for GeneralPurposeRegister {
        #[inline]
        fn clone(&self) -> GeneralPurposeRegister {
            let _: ::core::clone::AssertParamIsClone<usize>;
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for GeneralPurposeRegister {}
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for GeneralPurposeRegister {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for GeneralPurposeRegister {
        #[inline]
        fn eq(&self, other: &GeneralPurposeRegister) -> bool {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            let __arg1_tag = ::core::intrinsics::discriminant_value(other);
            __self_tag == __arg1_tag
                && match (self, other) {
                    (
                        GeneralPurposeRegister::Placeholder(__self_0),
                        GeneralPurposeRegister::Placeholder(__arg1_0),
                    ) => *__self_0 == *__arg1_0,
                    _ => true,
                }
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralEq for GeneralPurposeRegister {}
    #[automatically_derived]
    impl ::core::cmp::Eq for GeneralPurposeRegister {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {
            let _: ::core::cmp::AssertParamIsEq<usize>;
        }
    }
    #[automatically_derived]
    impl ::core::hash::Hash for GeneralPurposeRegister {
        fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            ::core::hash::Hash::hash(&__self_tag, state);
            match self {
                GeneralPurposeRegister::Placeholder(__self_0) => {
                    ::core::hash::Hash::hash(__self_0, state)
                }
                _ => {}
            }
        }
    }
    pub type GPR = GeneralPurposeRegister;
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
                Self::Placeholder(_) => ::core::panicking::panic("explicit panic"),
            }
        }
        fn decode(bits: u16) -> Option<Self> {
            Some(
                match bits {
                    0 => Self::R0,
                    1 => Self::R1,
                    2 => Self::R2,
                    3 => Self::R3,
                    4 => Self::R4,
                    5 => Self::R5,
                    6 => Self::R6,
                    7 => Self::R7,
                    _ => return None,
                },
            )
        }
    }
    impl ToAssembly for GeneralPurposeRegister {
        fn to_assembly(&self) -> String {
            {
                let res = ::alloc::fmt::format(format_args!("r{0}", self.encode()));
                res
            }
        }
    }
    pub enum DecimalRegister {
        D0,
        D1,
        D2,
        D3,
        Placeholder(usize),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for DecimalRegister {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                DecimalRegister::D0 => ::core::fmt::Formatter::write_str(f, "D0"),
                DecimalRegister::D1 => ::core::fmt::Formatter::write_str(f, "D1"),
                DecimalRegister::D2 => ::core::fmt::Formatter::write_str(f, "D2"),
                DecimalRegister::D3 => ::core::fmt::Formatter::write_str(f, "D3"),
                DecimalRegister::Placeholder(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Placeholder",
                        &__self_0,
                    )
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for DecimalRegister {
        #[inline]
        fn clone(&self) -> DecimalRegister {
            let _: ::core::clone::AssertParamIsClone<usize>;
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for DecimalRegister {}
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for DecimalRegister {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for DecimalRegister {
        #[inline]
        fn eq(&self, other: &DecimalRegister) -> bool {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            let __arg1_tag = ::core::intrinsics::discriminant_value(other);
            __self_tag == __arg1_tag
                && match (self, other) {
                    (
                        DecimalRegister::Placeholder(__self_0),
                        DecimalRegister::Placeholder(__arg1_0),
                    ) => *__self_0 == *__arg1_0,
                    _ => true,
                }
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralEq for DecimalRegister {}
    #[automatically_derived]
    impl ::core::cmp::Eq for DecimalRegister {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {
            let _: ::core::cmp::AssertParamIsEq<usize>;
        }
    }
    #[automatically_derived]
    impl ::core::hash::Hash for DecimalRegister {
        fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            ::core::hash::Hash::hash(&__self_tag, state);
            match self {
                DecimalRegister::Placeholder(__self_0) => {
                    ::core::hash::Hash::hash(__self_0, state)
                }
                _ => {}
            }
        }
    }
    pub type DR = DecimalRegister;
    impl Encodable for DecimalRegister {
        fn encode(self) -> u16 {
            match self {
                Self::D0 => 0,
                Self::D1 => 1,
                Self::D2 => 2,
                Self::D3 => 3,
                Self::Placeholder(_) => ::core::panicking::panic("explicit panic"),
            }
        }
        fn decode(bits: u16) -> Option<Self> {
            Some(
                match bits {
                    0 => Self::D0,
                    1 => Self::D1,
                    2 => Self::D2,
                    3 => Self::D3,
                    _ => return None,
                },
            )
        }
    }
    impl ToAssembly for DecimalRegister {
        fn to_assembly(&self) -> String {
            {
                let res = ::alloc::fmt::format(format_args!("d{0}", self.encode()));
                res
            }
        }
    }
    pub enum SpecialPurposeRegister {
        IP,
        RP,
        SP,
        EF,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for SpecialPurposeRegister {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(
                f,
                match self {
                    SpecialPurposeRegister::IP => "IP",
                    SpecialPurposeRegister::RP => "RP",
                    SpecialPurposeRegister::SP => "SP",
                    SpecialPurposeRegister::EF => "EF",
                },
            )
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for SpecialPurposeRegister {
        #[inline]
        fn clone(&self) -> SpecialPurposeRegister {
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for SpecialPurposeRegister {}
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for SpecialPurposeRegister {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for SpecialPurposeRegister {
        #[inline]
        fn eq(&self, other: &SpecialPurposeRegister) -> bool {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            let __arg1_tag = ::core::intrinsics::discriminant_value(other);
            __self_tag == __arg1_tag
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralEq for SpecialPurposeRegister {}
    #[automatically_derived]
    impl ::core::cmp::Eq for SpecialPurposeRegister {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {}
    }
    #[automatically_derived]
    impl ::core::hash::Hash for SpecialPurposeRegister {
        fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            ::core::hash::Hash::hash(&__self_tag, state)
        }
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
            Some(
                match bits {
                    0 => Self::IP,
                    1 => Self::RP,
                    2 => Self::SP,
                    3 => Self::EF,
                    _ => return None,
                },
            )
        }
    }
    impl ToAssembly for SpecialPurposeRegister {
        fn to_assembly(&self) -> String {
            match self {
                SpecialPurposeRegister::IP => "ip",
                SpecialPurposeRegister::RP => "rp",
                SpecialPurposeRegister::SP => "sp",
                SpecialPurposeRegister::EF => "ef",
            }
                .to_string()
        }
    }
    pub enum AnyRegister {
        G(GeneralPurposeRegister),
        D(DecimalRegister),
        S(SpecialPurposeRegister),
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for AnyRegister {
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                AnyRegister::G(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "G", &__self_0)
                }
                AnyRegister::D(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "D", &__self_0)
                }
                AnyRegister::S(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(f, "S", &__self_0)
                }
            }
        }
    }
    #[automatically_derived]
    impl ::core::clone::Clone for AnyRegister {
        #[inline]
        fn clone(&self) -> AnyRegister {
            let _: ::core::clone::AssertParamIsClone<GeneralPurposeRegister>;
            let _: ::core::clone::AssertParamIsClone<DecimalRegister>;
            let _: ::core::clone::AssertParamIsClone<SpecialPurposeRegister>;
            *self
        }
    }
    #[automatically_derived]
    impl ::core::marker::Copy for AnyRegister {}
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for AnyRegister {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for AnyRegister {
        #[inline]
        fn eq(&self, other: &AnyRegister) -> bool {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            let __arg1_tag = ::core::intrinsics::discriminant_value(other);
            __self_tag == __arg1_tag
                && match (self, other) {
                    (AnyRegister::G(__self_0), AnyRegister::G(__arg1_0)) => {
                        *__self_0 == *__arg1_0
                    }
                    (AnyRegister::D(__self_0), AnyRegister::D(__arg1_0)) => {
                        *__self_0 == *__arg1_0
                    }
                    (AnyRegister::S(__self_0), AnyRegister::S(__arg1_0)) => {
                        *__self_0 == *__arg1_0
                    }
                    _ => unsafe { ::core::intrinsics::unreachable() }
                }
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralEq for AnyRegister {}
    #[automatically_derived]
    impl ::core::cmp::Eq for AnyRegister {
        #[inline]
        #[doc(hidden)]
        #[no_coverage]
        fn assert_receiver_is_total_eq(&self) -> () {
            let _: ::core::cmp::AssertParamIsEq<GeneralPurposeRegister>;
            let _: ::core::cmp::AssertParamIsEq<DecimalRegister>;
            let _: ::core::cmp::AssertParamIsEq<SpecialPurposeRegister>;
        }
    }
    #[automatically_derived]
    impl ::core::hash::Hash for AnyRegister {
        fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
            let __self_tag = ::core::intrinsics::discriminant_value(self);
            ::core::hash::Hash::hash(&__self_tag, state);
            match self {
                AnyRegister::G(__self_0) => ::core::hash::Hash::hash(__self_0, state),
                AnyRegister::D(__self_0) => ::core::hash::Hash::hash(__self_0, state),
                AnyRegister::S(__self_0) => ::core::hash::Hash::hash(__self_0, state),
            }
        }
    }
}
pub use reg::*;
mod asm {
    pub trait ToAssembly {
        fn to_assembly(&self) -> String;
    }
    impl ToAssembly for u8 {
        fn to_assembly(&self) -> String {
            self.to_string()
        }
    }
}
pub use asm::*;
pub trait Encodable: Sized {
    fn encode(self) -> u16;
    fn decode(bits: u16) -> Option<Self>;
}
