use nom::branch::alt;
use nom::combinator::{eof, map};
use nom::multi::many_till;

use super::parser;
use super::span::{spanned, ParseSpan};
use crate::isa::InstructionKind;
use crate::opcode::OperandSize;
use crate::operands::{MemoryAccess, Register};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Token<'a> {
    Directive(&'a str),
    Expression(&'a str),
    Label(&'a str),
    Mnemonic((InstructionKind, OperandSize)),
    Register(Register),
    Flag(u8),
    Memory(MemoryAccess),
    String(&'a str),
    SignedInt(i32),
    UnsignedInt(u32),
}

impl<'a> Token<'a> {
    fn from_span(
        input: parser::LineSpan<'a>,
    ) -> nom::IResult<parser::LineSpan<'a>, ParseSpan<Self>> {
        spanned(alt((
            map(parser::directive, |d| Token::Directive(d)),
            map(parser::expression, |e| Token::Expression(e)),
            map(parser::register, |r| Token::Register(r)),
            map(parser::flag, |f| Token::Flag(f)),
            map(parser::memory_access, |m| Token::Memory(m)),
            map(parser::unsigned_integer, |i: u32| Token::UnsignedInt(i)),
            map(parser::signed_integer, |i: i32| Token::SignedInt(i)),
            map(parser::label_definition, |l| Token::Label(l)),
            map(parser::mnemonic, |m| Token::Mnemonic(m)),
            map(parser::string_literal, |s| Token::String(s)),
        )))(input)
    }
}

pub fn tokenize<'a>(
    input: &'a str,
) -> nom::IResult<parser::LineSpan<'a>, Vec<ParseSpan<Token<'a>>>> {
    parser::start(map(
        many_till(parser::ws1(Token::from_span), eof),
        |(t, _)| t,
    ))(input)
}
