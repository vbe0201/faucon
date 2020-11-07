//! Internal implementation details of `faucon-asm`.
//!
//! Do not use this crate directly!

extern crate proc_macro;

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
/// # Example
///
/// ```
/// use faucon_asm_derive::Instruction;
///
/// #[derive(Clone, Copy, Debug, PartialEq, Eq, Instruction)]
/// enum Instruction {
///     #[insn(opcode = 0x3D, subopcode = 0x04, operands(R2))]
///     CLEAR,
///
///     #[insn(opcode = 0xF9, subopcode = 0x00, operands(R2))]
///     PUSH,
///
///     #[insn(opcode = 0xFC, subopcode = 0x00, operands(R2))]
///     POP,
/// }
/// ```
///
/// # How it works
///
/// By adding the `Instruction` derive macro to the `#[derive()]` attribute of an arbitrary
/// enumeration, it will iterate over all the members at compile-time and parses all the
/// `#[insn]` attributes that individual variants are decorated with into `opcode`,
/// `subopcode` and `operands` values. These will be used to generate an [`InstructionMeta`]
/// object that will be stored at the position denoted by `subopcode` in a chosen array
/// which serves as an opcode lookup table.
///
/// These lookup tables are chosen for each instruction variant individually thorugh the
/// information stored in their opcode. Subopcode indexing ensures convenient and performant
/// instruction lookup in the disassembler. Thus, the macro will also implement a public
/// `lookup_meta` method on the enum that takes `sized`, `a` and `b` (the information stored
/// in the instruction opcode) plus the subopcode and attempts to find a corresponding opcode
/// in the opcode tables.
///
/// `faucon-asm` and dependant crates are laid out to make sure that extending the enum which
/// derives from the `Instruction` macro is the only required step to add support for new
/// instructions to the disassembler to spare time and to make it possible for people without
/// a background in Rust to contribute valuable details to the entire faucon project.
///
/// See the [`Instruction`] enum in `faucon-asm` for details on the instructions and their
/// variants that are already supported.
///
/// [`InstructionMeta`]: ../faucon-asm/isa/struct.InstructionMeta.html
/// [`Instruction`]: ../faucon-asm/isa/struct.Instruction.html
#[proc_macro_derive(Instruction, attributes(insn))]
pub fn instruction(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    impl_instruction(&ast).unwrap().into()
}
