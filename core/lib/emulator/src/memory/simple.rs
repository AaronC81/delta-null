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
    pub fn new() -> Self {
        Self { data: [0; 0x10000] }
    }
}
