//! A machine representation of the instructions described in design/instructions.md.
//! 
//! This implementation should always match that one.
//! Whenever a complete list of instructions is specified somewhere (e.g. in a match), it should
//! use the same categories as used in that document, to aid traceability.

use crate::{GPR, SPR, DR, AnyRegister};
use strum::{AsRefStr, EnumDiscriminants, EnumIter};
use delta_null_core_macros::{InstructionOpcodeMnemonics, InstructionBuild};

mod analysis;
pub use analysis::*;

mod encoding;
pub use encoding::*;

mod assembly;
pub use assembly::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, AsRefStr, EnumDiscriminants, InstructionBuild)]
#[strum_discriminants(name(InstructionOpcode))]
#[strum_discriminants(derive(AsRefStr, InstructionOpcodeMnemonics))]
#[strum_discriminants(derive(EnumIter))]
pub enum Instruction {
    // Core
    Nop,
    Hlt,
    Mov { dest: GPR, src: GPR },
    DMov { dest: DR, src: DR },

    // Immediate Loads
    Putl { reg: GPR, imm: u8 },
    Puth { reg: GPR, imm: u8 },

    // Memory
    Read { val: GPR, addr: GPR },
    Write { addr: GPR, val: GPR },
    Spread { val: GPR, offset: u8 }, 
    Spwrite { offset: u8, val: GPR },
    DRead { val: DR, addr: GPR },
    DWrite { addr: GPR, val: DR },

    // Special-purpose Registers
    Movso { dest: GPR, src: SPR },
    Movsi { dest: SPR, src: GPR },
    Spadd { val: GPR },
    Spinc,
    Spdec,

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
    Mul { reg: GPR, val: GPR },

    // Comparison
    Inv,
    Eqz { reg: GPR },
    Eq { left: GPR, right: GPR },
    Gt { left: GPR, right: GPR },
    Gteq { left: GPR, right: GPR },

    // Branching
    Jmpoff { offset: u8 },
    Cjmpoff { offset: u8 },
    Cjmp { src: GPR },
    Call { src: GPR },
    Ret,
}

pub enum AnyOperand {
    R(AnyRegister),
    I(u8),
}

#[cfg(test)]
mod test {
    use crate::{Instruction, InstructionOpcode, AnyOperand, GPR, AnyRegister, DR};

    #[test]
    fn test_build() {
        assert_eq!(Some(Instruction::Nop), InstructionOpcode::Nop.build(&[]));
        assert_eq!(None, InstructionOpcode::Nop.build(&[AnyOperand::I(1)]));

        assert_eq!(
            Some(Instruction::Inc { reg: GPR::R0 }),
            InstructionOpcode::Inc.build(&[AnyOperand::R(AnyRegister::G(GPR::R0))]),
        );
        assert_eq!(
            None,
            InstructionOpcode::Inc.build(&[]),
        );
        assert_eq!(
            None,
            InstructionOpcode::Inc.build(&[AnyOperand::R(AnyRegister::D(DR::D1))]),
        );

        assert_eq!(
            Some(Instruction::Mov { dest: GPR::R0, src: GPR::R1 }),
            InstructionOpcode::Mov.build(&[AnyOperand::R(AnyRegister::G(GPR::R0)), AnyOperand::R(AnyRegister::G(GPR::R1))]),
        );
        assert_eq!(
            None,
            InstructionOpcode::Mov.build(&[]),
        );
        assert_eq!(
            None,
            InstructionOpcode::Mov.build(&[AnyOperand::R(AnyRegister::G(GPR::R0))]),
        );
    }    
}
