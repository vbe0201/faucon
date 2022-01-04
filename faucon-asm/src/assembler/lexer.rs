use num_traits::{cast, NumCast};

use super::{error::AssemblerError, interner::FileId, parser, span::Spanned};
use crate::{
    isa::InstructionKind,
    opcode::OperandSize,
    operands::{MemoryAccess, Register},
};

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'t> {
    // -0x42, 0x33, ...
    SignedInteger(i32),
    // 0xFF, 0b1101, ...
    UnsignedInteger(u32),
    // "I'm a string"
    StrLiteral(&'t str),
    // 9:17
    BitField(u32, u32),

    // $c
    Flag(u8),
    // $r4, $REG15, $sp
    Register(Register),
    // D[$r5 + $r4], I[$r0]
    Memory(MemoryAccess),
    // #a, ##symbol
    Symbol(&'t str, bool),
    // .align
    Directive(&'t str),
    // label:
    Label(&'t str),
    // MOV.H
    Mnemonic(InstructionKind, OperandSize),
}

impl<'t> Token<'t> {
    pub fn try_as_int<I: NumCast>(this: Spanned<Self>) -> Result<Spanned<I>, Spanned<Self>> {
        this.try_map(|t| match t {
            Token::UnsignedInteger(i) => cast::<_, I>(i).ok_or(t),
            Token::SignedInteger(i) => cast::<_, I>(i).ok_or(t),
            _ => Err(t),
        })
    }

    pub fn try_as_str(this: Spanned<Self>) -> Result<Spanned<&'t str>, Spanned<Self>> {
        this.try_map(|t| match t {
            Token::StrLiteral(s) => Ok(s),
            _ => Err(t),
        })
    }

    pub fn try_as_bitfield(this: Spanned<Self>) -> Result<Spanned<(u32, u32)>, Spanned<Self>> {
        this.try_map(|t| match t {
            Token::BitField(start, end) => Ok((start, end)),
            _ => Err(t),
        })
    }

    pub fn try_as_flag(this: Spanned<Self>) -> Result<Spanned<u8>, Spanned<Self>> {
        this.try_map(|t| match t {
            Token::Flag(f) => Ok(f),
            _ => Err(t),
        })
    }

    pub fn try_as_register(this: Spanned<Self>) -> Result<Spanned<Register>, Spanned<Self>> {
        this.try_map(|t| match t {
            Token::Register(reg) => Ok(reg),
            _ => Err(t),
        })
    }

    pub fn try_as_memory(this: Spanned<Self>) -> Result<Spanned<MemoryAccess>, Spanned<Self>> {
        this.try_map(|t| match t {
            Token::Memory(mem) => Ok(mem),
            _ => Err(t),
        })
    }

    pub fn try_as_symbol(this: Spanned<Self>) -> Result<Spanned<(&'t str, bool)>, Spanned<Self>> {
        this.try_map(|t| match t {
            Token::Symbol(ident, phys) => Ok((ident, phys)),
            _ => Err(t),
        })
    }

    pub fn try_as_directive(this: Spanned<Self>) -> Result<Spanned<&'t str>, Spanned<Self>> {
        this.try_map(|t| match t {
            Token::Directive(d) => Ok(d),
            _ => Err(t),
        })
    }

    pub fn try_as_label(this: Spanned<Self>) -> Result<Spanned<&'t str>, Spanned<Self>> {
        this.try_map(|t| match t {
            Token::Label(l) => Ok(l),
            _ => Err(t),
        })
    }

    pub fn try_as_mnemonic(
        this: Spanned<Self>,
    ) -> Result<Spanned<(InstructionKind, OperandSize)>, Spanned<Self>> {
        this.try_map(|t| match t {
            Token::Mnemonic(kind, size) => Ok((kind, size)),
            _ => Err(t),
        })
    }
}

pub fn tokenize(input: &str, file: FileId) -> Result<Vec<parser::Statement<'_>>, AssemblerError> {
    let result = parser::start(file, parser::do_parse)(input);
    AssemblerError::check_tokenization(result)
}
