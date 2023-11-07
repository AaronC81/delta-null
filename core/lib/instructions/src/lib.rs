#![feature(stmt_expr_attributes)]
#![feature(proc_macro_hygiene)]

mod ins;
pub use ins::*;

mod reg;
pub use reg::*;

mod asm;
pub use asm::*;

pub trait Encodable: Sized {
    fn encode(self) -> u16;
    fn decode(bits: u16) -> Option<Self>;
}
