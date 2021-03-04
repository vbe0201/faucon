//! Assembler for the Falcon ISA.

pub mod error;
mod lexer;
mod parser;
pub mod span;

pub use error::*;
pub use span::*;
