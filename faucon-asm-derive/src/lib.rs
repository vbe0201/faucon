//! Internal implementation details of [`faucon-asm`].
//!
//! Do not use this crate directly!
//!
//! [`faucon-asm`]: ../faucon-asm/

#![deny(rustdoc::broken_intra_doc_links)]
#![forbid(unsafe_code)]

#[macro_use]
extern crate quote;

mod ast;
mod attrs;
mod r#impl;

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

use r#impl::*;

/// A procedural derive macro for dynamically generating opcode lookup tables from the
/// variants of an enum.
///
/// This is exclusively meant for internal use in [`faucon-asm`]!
///
/// [`faucon-asm`]: ../faucon-asm/
#[doc(hidden)]
#[proc_macro_derive(Instruction, attributes(insn))]
pub fn instruction(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    impl_instruction(&input)
        .unwrap_or_else(|e| e.to_compile_error())
        .into()
}
