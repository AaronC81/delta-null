use super::{Memory, MemoryError};

pub struct SimpleMemory {
    pub data: [u16; 0x10000 / 2],
}

impl Memory for SimpleMemory {
    fn read(&self, address: u16) -> Result<u16, MemoryError> {
        Ok(self.data[Self::map_address_to_index(address)?])
    }

    fn write(&mut self, address: u16, value: u16) -> Result<(), MemoryError> {
        self.data[Self::map_address_to_index(address)?] = value;
        Ok(())
    }
}

impl SimpleMemory {
    pub fn new() -> Self {
        Self { data: [0; 0x10000 / 2] }
    }

    fn map_address_to_index(address: u16) -> Result<usize, MemoryError> {
        if address % 2 != 0 {
            return Err(MemoryError::UnalignedAccess)
        }

        Ok(address as usize / 2)
    }
}
