//! Internal implementation details of `faucon-asm`.
//!
//! Do not use this crate directly!

#![deny(rust_2018_idioms)]

#[macro_use]
extern crate quote;

mod r#impl;
mod parser;

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

use r#impl::*;

/// A procedural derive macro for dynamically generating opcode lookup tables from the
/// variants of an enum.
///
/// This is only meant to be internally used by `faucon-asm`, so don't use it.
#[proc_macro_derive(Instruction, attributes(insn))]
pub fn instruction(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    impl_instruction(&ast).unwrap().into()
}
