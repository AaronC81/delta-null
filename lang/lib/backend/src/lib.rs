//! The shared parts of the language backend.
//! 
//! Other crates shall exist to implement specific backends.

#![feature(assert_matches)]

pub mod ir;
pub mod analysis;
