use nom::branch::alt;
use nom::combinator::{eof, map};
use nom::multi::many_till;

use crate::assembler::error::ParseError;
use crate::assembler::parser;
use crate::assembler::span::{spanned, ParseSpan};
use crate::isa::InstructionKind;
use crate::opcode::OperandSize;
use crate::operands::{MemoryAccess, Register};

// Possible tokens that may occur in Falcon assembly.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Token<'a> {
    // A directive statement suggesting an action to be performed by the assembler.
    Directive(&'a str),
    // A symbol that evaluates either to a declared value or to the address of a
    // label.
    Symbol(&'a str),
    // A label declaration that can be referred to by expressions.
    Label(&'a str),
    // An assembly mnemonic with its corresponding instruction sizing.
    Mnemonic((InstructionKind, OperandSize)),
    // A special-purpose or general-purpose register referred to in code.
    Register(Register),
    // A named flag bit referred to in code.
    Flag(u8),
    // A memory access to an address in a specific SRAM space.
    Memory(MemoryAccess),
    // A string of any sort used in Falcon assembly.
    String(&'a str),
    // A signed integer literal either represented as binary, decimal, hexadecimal
    // or octal.
    SignedInt(i32),
    // An unsigned integer literal either represented as binary, decimal, hexadecimal
    // or octal.
    UnsignedInt(u32),
}

impl<'a> Token<'a> {
    // Parses the next token from the given line span, if applicable.
    pub fn from_span(
        input: parser::LineSpan<'a>,
    ) -> nom::IResult<parser::LineSpan<'a>, ParseSpan<Self>> {
        spanned(alt((
            map(parser::directive, |d| Token::Directive(d)),
            map(parser::symbol, |e| Token::Symbol(e)),
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

fn tokenize_impl<'a>(
    input: &'a str,
) -> nom::IResult<parser::LineSpan<'a>, Vec<ParseSpan<Token<'a>>>> {
    parser::start(map(
        many_till(parser::ws1(Token::from_span), eof),
        |(t, _)| t,
    ))(input)
}

// Tokenizes the given input until an EOF occurs.
pub fn tokenize(input: &str) -> Result<Vec<ParseSpan<Token>>, ParseError> {
    let result = tokenize_impl(input);
    Ok(ParseError::check_tokenization(result)?)
}
