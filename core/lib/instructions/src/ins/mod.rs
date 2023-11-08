//! A machine representation of the instructions described in design/instructions.md.
//! 
//! This implementation should always match that one.
//! Whenever a complete list of instructions is specified somewhere (e.g. in a match), it should
//! use the same categories as used in that document, to aid traceability.

use crate::{GPR, SPR, DR};
use strum::{AsRefStr, EnumDiscriminants};
use delta_null_core_macros::InstructionOpcodeMnemonics;

mod analysis;
pub use analysis::*;

mod encoding;
pub use encoding::*;

mod assembly;
pub use assembly::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, AsRefStr, EnumDiscriminants)]
#[strum_discriminants(name(InstructionOpcode))]
#[strum_discriminants(derive(AsRefStr, InstructionOpcodeMnemonics))]
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
