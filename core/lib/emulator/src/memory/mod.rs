mod simple;

use std::{fmt::Display, error::Error};

pub use simple::*;

pub trait Memory {
    fn read(&self, address: u16) -> Result<u16, MemoryError>;
    fn write(&mut self, address: u16, value: u16) -> Result<(), MemoryError>;
}

#[derive(Debug, Clone)]
pub enum MemoryError {
    UnalignedAccess,
    OutOfRange,
    Other(String),
}

impl Display for MemoryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MemoryError::UnalignedAccess => f.write_str("unaligned access"),
            MemoryError::OutOfRange => f.write_str("out of range"),
            MemoryError::Other(s) => f.write_str(&s),
        }
    }
}
impl Error for MemoryError {}
