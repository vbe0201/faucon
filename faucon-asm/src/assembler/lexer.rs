use super::{error::AssemblerError, interner::FileId, parser, span::Spanned};
use crate::{
    isa::InstructionKind,
    opcode::OperandSize,
    operands::{MemoryAccess, Register},
};

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'s> {
    // -0x42, 0x33, ...
    SignedInteger(i32),
    // 0xFF, 0b1101, ...
    UnsignedInteger(u32),
    // "I'm a string"
    StrLiteral(&'s str),
    // 9:17
    BitField(u32, u32),

    // $c
    Flag(u8),
    // $r4, $REG15, $sp
    Register(Register),
    // D[$r5 + $r4], I[$r0]
    Memory(MemoryAccess),
    // #a, ##symbol
    Symbol(&'s str, bool),
    // .align
    Directive(&'s str),
    // label:
    Label(&'s str),
    // MOV.H
    Mnemonic(InstructionKind, OperandSize),
}

pub fn tokenize(input: &str, file: FileId) -> Result<Vec<Spanned<Token<'_>>>, AssemblerError> {
    let result = parser::start(file, parser::do_parse)(input);
    AssemblerError::check_tokenization(result)
}
