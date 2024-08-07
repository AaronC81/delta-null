use std::{fmt::Display, error::Error};

use delta_null_core_instructions::{Instruction, Encodable, GPR, SPR, ToAssembly};

use crate::memory::{Memory, MemoryError};

pub struct Core<M: Memory> {
    pub gprs: [u16; 8],
    // TODO: decimal registers

    pub ip: u16,
    pub rp: u16,
    pub sp: u16,
    pub ef: u16,

    pub memory: M,

    /// If true, prints each instruction to stdout as it is executed.
    pub trace_execution: bool,
}

impl<M: Memory> Core<M> {
    pub fn new(memory: M) -> Self {
        Self {
            gprs: [0; 8],
            ip: 0,
            rp: 0,
            sp: 0xFFFF,
            ef: 0,
            memory,
            trace_execution: false,
        }
    }

    pub fn step_until_halted(&mut self) -> Result<(), ExecutionError> {
        while !self.is_halted() {
            self.step()?;
        }
        Ok(())
    }

    pub fn step(&mut self) -> Result<(), ExecutionError> {
        if self.is_halted() {
            return Ok(())
        }

        // Read instruction and advance IP
        let ip_before_increment = self.ip;
        let instr_bits = self.memory.read(self.ip)?;
        self.ip = self.ip.overflowing_add(1).0;

        // Decode instruction
        let Some(instr) = Instruction::decode(instr_bits) else {
            return Err(ExecutionError::DecodeError(instr_bits))
        };

        if self.trace_execution {
            println!("TRACE: Executing at ip={:04x}: {}", ip_before_increment, instr.to_assembly())
        }

        // Execute instruction
        use Instruction::*;
        match instr {
            // Core
            Nop => (),
            Hlt => self.ef |= 1, // Set halt flag
            Mov { dest, src }
                => self.write_gpr(dest, self.read_gpr(src)),
            DMov { .. } => todo!(),

            // Immediate Loads
            Putl { reg, imm }
                => self.write_gpr(reg, (self.read_gpr(reg) & 0xFF00) | imm as u16),
            Puth { reg, imm }
                => self.write_gpr(reg, (self.read_gpr(reg) & 0x00FF) | ((imm as u16) << 8)),

            // Memory
            Read { addr, val }
                => self.write_gpr(val, self.memory.read(self.read_gpr(addr))?),
            Write { addr, val }
                => self.memory.write(self.read_gpr(addr), self.read_gpr(val))?,
            Spread { val, offset }
                => self.write_gpr(val, self.memory.read(self.sp.overflowing_add(offset as u16).0)?),
            Spwrite { val, offset }
                => self.memory.write(self.sp.overflowing_add(offset as u16).0, self.read_gpr(val))?,
            DRead { .. } => todo!(),
            DWrite { .. } => todo!(),
            Push { val } => {
                self.write_spr(SPR::SP, self.read_spr(SPR::SP).overflowing_sub(1).0);
                self.memory.write(self.sp, self.read_gpr(val))?
            },
            Pop { val } => {
                self.write_gpr(val, self.memory.read(self.sp)?);
                self.write_spr(SPR::SP, self.read_spr(SPR::SP).overflowing_add(1).0);
            },

            // Special-Purpose Registers
            Movso { dest, src }
                => self.write_gpr(dest, self.read_spr(src)),
            Movsi { dest, src }
                => self.write_spr(dest, self.read_gpr(src)),
            Spadd { val }
                => self.write_spr(SPR::SP, self.read_spr(SPR::SP).overflowing_add(self.read_gpr(val)).0),
            Spinc
                => self.write_spr(SPR::SP, self.read_spr(SPR::SP).overflowing_add(1).0),
            Spdec
                => self.write_spr(SPR::SP, self.read_spr(SPR::SP).overflowing_sub(1).0),

            // Bit Manipulation
            Not { reg }
                => self.apply_gpr_unary(reg, |r| !r),
            And { reg, val }
                => self.apply_gpr_binary(reg, val, |r, v| r & v),
            Booland { reg, val }
                => self.apply_gpr_binary(reg, val, |r, v| if (r > 0) && (v > 0) { 1 } else { 0 }),
            Or { reg, val } 
                => self.apply_gpr_binary(reg, val, |r, v| r | v),
            Xor { reg, val } 
                => self.apply_gpr_binary(reg, val, |r, v| r ^ v),
            Shl { reg, val }
                => self.apply_gpr_binary(reg, val, |r, v| r << v),
            Shr { reg, val }
                => self.apply_gpr_binary(reg, val, |r, v| r >> v),
            Bitset { reg, idx } => {
                if self.is_cond_set() {
                    self.write_gpr(reg, self.read_gpr(reg) | (1 << self.read_gpr(idx)))
                } else {
                    self.write_gpr(reg, self.read_gpr(reg) & !(1 << self.read_gpr(idx)))
                }
            }

            // General-Purpose Arithmetic
            Neg { reg }
                => self.apply_gpr_unary(reg, |r| -(r as i16) as u16),
            Inc { reg }
                => self.apply_gpr_unary(reg, |r| r.overflowing_add(1).0),
            Dec { reg }
                => self.apply_gpr_unary(reg, |r| r.overflowing_sub(1).0),
            Add { reg, val }
                => self.apply_gpr_binary(reg, val, |r, v| r.overflowing_add(v).0),
            Sub { reg, val }
                => self.apply_gpr_binary(reg, val, |r, v: u16| r.overflowing_sub(v).0),
            Mul { reg, val }
                => self.apply_gpr_binary(reg, val, |r, v: u16| r.overflowing_mul(v).0),

            // Comparison
            Inv
                => self.set_cond(!self.is_cond_set()),
            Eqz { reg }
                => self.set_cond(self.read_gpr(reg) == 0),
            Eq { left, right }
                => self.set_cond(self.read_gpr(left) == self.read_gpr(right)),
            Gt { left, right }
                => self.set_cond(self.read_gpr(left) > self.read_gpr(right)),
            Gteq { left, right }
                => self.set_cond(self.read_gpr(left) >= self.read_gpr(right)),

            // Branching
            Jmpoff { offset }
                => self.write_spr(SPR::IP, self.read_spr(SPR::IP).overflowing_add(Self::sign_extend_u8_to_u16(offset)).0),
            Cjmpoff { offset }
                => if self.is_cond_set() {
                    self.write_spr(SPR::IP, self.read_spr(SPR::IP).overflowing_add(Self::sign_extend_u8_to_u16(offset)).0);
                }
            Cjmp { src }
                => if self.is_cond_set() {
                    self.write_spr(SPR::IP, self.read_gpr(src));
                }
            Call { src } => {
                self.write_spr(SPR::RP, self.read_spr(SPR::IP));
                self.write_spr(SPR::IP, self.read_gpr(src));
            }
            Ret
                => self.write_spr(SPR::IP, self.read_spr(SPR::RP)),
        }

        Ok(())
    }

    pub fn is_halted(&self) -> bool {
        self.ef & 1 > 0
    }

    pub fn is_cond_set(&self) -> bool {
        self.ef & 2 > 0
    }
    
    pub fn set_cond(&mut self, val: bool) {
        if val {
            self.ef |= 2;
        } else {
            self.ef &= !2;
        }
    }

    pub fn read_gpr(&self, reg: GPR) -> u16 {
        self.gprs[reg.encode() as usize]
    }

    pub fn write_gpr(&mut self, reg: GPR, value: u16) {
        self.gprs[reg.encode() as usize] = value;
    }

    pub fn apply_gpr_unary(&mut self, reg: GPR, func: impl FnOnce(u16) -> u16) {
        self.write_gpr(reg, (func)(self.read_gpr(reg)))
    }

    pub fn apply_gpr_binary(&mut self, reg: GPR, other: GPR, func: impl FnOnce(u16, u16) -> u16) {
        self.write_gpr(reg, (func)(self.read_gpr(reg), self.read_gpr(other)))
    }

    pub fn read_spr(&self, reg: SPR) -> u16 {
        match reg {
            SPR::IP => self.ip,
            SPR::RP => self.rp,
            SPR::SP => self.sp,
            SPR::EF => self.ef,
        }
    }

    pub fn write_spr(&mut self, reg: SPR, value: u16) {
        let r = match reg {
            SPR::IP => &mut self.ip,
            SPR::RP => &mut self.rp,
            SPR::SP => &mut self.sp,
            SPR::EF => &mut self.ef,
        };
        *r = value;
    }

    fn sign_extend_u8_to_u16(byte: u8) -> u16 {
        ((byte as i8) as i16) as u16
    }
}

#[derive(Debug, Clone)]
pub enum ExecutionError {
    MemoryError(MemoryError),
    DecodeError(u16),
}

impl From<MemoryError> for ExecutionError {
    fn from(value: MemoryError) -> Self { ExecutionError::MemoryError(value) }
}

impl Display for ExecutionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DecodeError(bits) => write!(f, "unable to decode {bits:x} as instruction"),
            Self::MemoryError(e) => write!(f, "memory error: {e}"),
        }
    }
}
impl Error for ExecutionError {}

// TODO: move later
#[cfg(test)]
mod test {
    use delta_null_core_instructions::{Instruction, GPR, Encodable};

    use crate::{memory::SimpleMemory, Core};

    #[test]
    fn test_basic() {
        let mut mem = SimpleMemory::new();
        mem.data[0] = Instruction::Putl { reg: GPR::R0, imm: 24 }.encode();
        mem.data[1] = Instruction::Hlt.encode();

        let mut core = Core::new(mem);
        core.step_until_halted().unwrap();

        assert_eq!(24, core.read_gpr(GPR::R0));
    }
}
