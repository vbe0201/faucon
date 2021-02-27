use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::combinator::map;
use nom::multi::fold_many0;

use super::parser;
use crate::isa::InstructionKind;
use crate::opcode::OperandSize;
use crate::operands::{MemoryAccess, Register};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Token<'a> {
    Directive(&'a str),
    Expression(&'a str),
    Ident(&'a str),
    Label(&'a str),
    Semicolon,
    Mnemonic((InstructionKind, OperandSize)),
    Register(Register),
    Memory(MemoryAccess),
    SignedInt(i32),
    UnsignedInt(u32),
}

impl<'a> Token<'a> {
    pub fn parse(input: parser::LineSpan<'a>) -> parser::ParserResult<'a, Self> {
        alt((
            map(parser::whitespace(tag(";")), |_| Token::Semicolon),
            map(parser::whitespace(parser::directive), |d| {
                Token::Directive(d)
            }),
            map(parser::whitespace(parser::expression), |e| {
                Token::Expression(e)
            }),
            map(parser::whitespace(parser::register), |r| Token::Register(r)),
            map(parser::whitespace(parser::memory_access), |m| {
                Token::Memory(m)
            }),
            map(parser::whitespace(parser::signed_integer), |i: i32| {
                Token::SignedInt(i)
            }),
            map(parser::whitespace(parser::unsigned_integer), |i: u32| {
                Token::UnsignedInt(i)
            }),
            map(parser::whitespace(parser::label_definition), |l| {
                Token::Label(l)
            }),
            map(parser::whitespace(parser::mnemonic), |m| Token::Mnemonic(m)),
            map(parser::whitespace(parser::identifier), |i| Token::Ident(i)),
        ))(input)
    }
}

pub fn tokenize<'a>(input: &'a str) -> parser::ParserResult<'a, Vec<Token<'a>>> {
    parser::start(fold_many0(
        Token::parse,
        Vec::new(),
        |mut tokens: Vec<_>, t| {
            tokens.push(t);
            tokens
        },
    ))(input)
}
