#![feature(exit_status_error)]

// This library is functionally blank.
// It is only a harness for integration tests!

#[cfg(test)]
pub mod util;

#[cfg(test)]
pub mod flow;

#[cfg(test)]
pub mod types;

#[cfg(test)]
pub mod var;

#[cfg(test)]
pub mod op;

#[cfg(test)]
pub mod asm;
