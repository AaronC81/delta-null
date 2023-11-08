use crate::{ToAssembly, Instruction, InstructionOpcode};

impl ToAssembly for Instruction {
    fn to_assembly(&self) -> String {
        let opcode: InstructionOpcode = self.into();
        let opcode = opcode.mnemonic();
        let operands = operands_for_assembly(self);

        if operands.is_empty() {
            opcode.to_string()
        } else {
            format!("{} {}",
                opcode,
                operands.iter()
                    .map(|o| o.to_assembly())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }
    }
}

fn operands_for_assembly(ins: &Instruction) -> Vec<&dyn ToAssembly> {
    use Instruction::*;

    match ins {
        // Core
        Nop => vec![],
        Hlt => vec![],
        Mov { src, dest } => vec![dest, src],
        DMov { src, dest } => vec![dest, src],

        // Immediate Loads
        Putl { reg, imm } => vec![reg, imm],
        Puth { reg, imm } => vec![reg, imm],

        // Memory
        Read { addr, val } => vec![addr, val],
        Write { addr, val }  => vec![addr, val],
        DRead { addr, val }  => vec![addr, val],
        DWrite { addr, val }  => vec![addr, val],

        // Special-Purpose Registers
        Movso { src, dest } => vec![dest, src],
        Movsi { src, dest } => vec![dest, src],
        Spadd { val } => vec![val],

        // Bit Manipulation
        Not { reg } => vec![reg],
        And { reg, val } => vec![reg, val],
        Or { reg, val } => vec![reg, val],
        Xor { reg, val } => vec![reg, val],
        Shl { reg, val } => vec![reg, val],
        Shr { reg, val } => vec![reg, val],

        // General-Purpose Arithmetic
        Neg { reg } => vec![reg],
        Inc { reg } => vec![reg],
        Dec { reg } => vec![reg],
        Add { reg, val } => vec![reg, val],
        Sub { reg, val } => vec![reg, val],
        Mulu { reg, val } => vec![reg, val],
        Muli { reg, val } => vec![reg, val],

        // Comparison
        Inv => vec![],
        Eqz { reg } => vec![reg],
        Eq { left, right } => vec![left, right],
        Gt { left, right } => vec![left, right],
        Gteq { left, right } => vec![left, right],

        // Branching
        Jmpoff { offset } => vec![offset],
        Cjmpoff { offset }  => vec![offset],
        Jmp { src } => vec![src],
        Cjmp { src } => vec![src],
        Call { src } => vec![src],
        Ret => vec![],
    }
}

#[cfg(test)]
mod test {
    use crate::{Instruction, ToAssembly, GPR, DR};

    #[test]
    fn test_assembly() {
        use Instruction::*;

        assert_eq!("nop", &Nop.to_assembly());
        assert_eq!("neg r1", &Neg { reg: GPR::R1 }.to_assembly());
        assert_eq!("mov r0, r7", &Mov { dest: GPR::R0, src: GPR::R7 }.to_assembly());
        assert_eq!("putl r0, 3", &Putl { reg: GPR::R0, imm: 3 }.to_assembly());
        assert_eq!("d_mov d0, d1", &DMov { dest: DR::D0, src: DR::D1 }.to_assembly());
    }
}
