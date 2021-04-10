use std::ffi::OsString;

use crate::assembler::error::ParseError;
use crate::assembler::parser;
use crate::assembler::span::ParseSpan;
use crate::isa::InstructionKind;
use crate::opcode::OperandSize;
use crate::operands::{MemoryAccess, Register};

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'a> {
    Directive(&'a str),
    Symbol((&'a str, bool)),
    Label(&'a str),
    Mnemonic((InstructionKind, OperandSize)),
    Register(Register),
    Flag(u8),
    Memory(MemoryAccess),
    String(&'a str),
    SignedInt(i32),
    UnsignedInt(u32),
    Bitfield((u32, u32)),
}

pub fn tokenize<'a>(
    input: &'a str,
    file_name: &'a OsString,
) -> Result<Vec<ParseSpan<Token<'a>>>, ParseError> {
    let result = parser::start(&file_name, parser::do_parse)(input);
    Ok(ParseError::check_tokenization(result)?)
}
