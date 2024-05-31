use delta_null_core_instructions::{Instruction, Encodable};

use super::{Memory, MemoryError};

pub struct SimpleMemory {
    pub data: [u16; 0x10000],
}

impl Memory for SimpleMemory {
    fn read(&self, address: u16) -> Result<u16, MemoryError> {
        Ok(self.data[address as usize])
    }

    fn write(&mut self, address: u16, value: u16) -> Result<(), MemoryError> {
        self.data[address as usize] = value;
        Ok(())
    }
}

impl SimpleMemory {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self { data: [0; 0x10000] }
    }

    pub fn with_content(words: &[u16]) -> Self {
        let mut memory = Self::new();
        for (i, word) in words.iter().enumerate() {
            memory.write(i as u16, *word).unwrap();
        }
        memory
    }

    pub fn with_instructions(instructions: &[Instruction]) -> Self {
        Self::with_content(&instructions.iter().map(|i| i.encode()).collect::<Vec<_>>())
    }
}
