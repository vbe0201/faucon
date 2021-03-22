use std::ffi::OsString;
use std::iter::Peekable;

use num_traits::{cast, NumCast};

use crate::assembler::lexer::Token;
use crate::assembler::span::ParseSpan;

// Assembler directives that may be used in Falcon assembly language.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Directive<'a> {
    // Specifies alignment of code to a given byte boundary.
    Align(u32),
    // Inserts a byte literal at the current position in code.
    Byte(u8),
    // Assigns a numeric value to a symbol with the supplied name.
    Equ(&'a str, u32),
    // Inserts a halfword literal at the current position in code in
    // Little-Endian byte order.
    Halfword(u16),
    // Includes another assembler source file relative to the file in
    // which the directive was used.
    Include(&'a str),
    // Inserts a word literal at the current position in code in
    // Little-Endian byte order.
    Word(u32),
    // Declares a code section with a name and an optional start address.
    Section(&'a str, Option<u32>),
    // Skips the given amount of bytes in code and optionally fills them
    // with the supplied value.
    Skip(u32, Option<u8>),
    // Inserts a string literal at the current position in code.
    Str(&'a str),
}

#[inline]
fn try_extract_integer<'a, I: NumCast>(
    span: ParseSpan<Token<'a>>,
) -> Result<I, ParseSpan<Token<'a>>> {
    match span.token() {
        Token::SignedInt(i) => cast::<i32, I>(*i).ok_or(span),
        Token::UnsignedInt(i) => cast::<u32, I>(*i).ok_or(span),
        _ => Err(span),
    }
}

#[inline]
fn try_extract_integer_optional<'a, I: NumCast>(span: &ParseSpan<Token<'a>>) -> Option<I> {
    match span.token() {
        Token::SignedInt(i) => cast::<i32, I>(*i),
        Token::UnsignedInt(i) => cast::<u32, I>(*i),
        _ => None,
    }
}

#[inline]
fn try_extract_expression_ident<'a>(
    span: ParseSpan<Token<'a>>,
) -> Result<&'a str, ParseSpan<Token<'a>>> {
    match span.token() {
        Token::Expression(e) => Ok(e),
        _ => Err(span),
    }
}

#[inline]
fn try_extract_string<'a>(span: ParseSpan<Token<'a>>) -> Result<&'a str, ParseSpan<Token<'a>>> {
    match span.token() {
        Token::String(s) => Ok(s),
        _ => Err(span),
    }
}

macro_rules! parse_next_token {
    (int: $iter:ident => $int:ty) => {
        $iter
            .next()
            .ok_or(None)
            .and_then(|s| try_extract_integer::<$int>(s).map_err(Some))
    };
    (optint: $iter:ident => $int:ty) => {
        if let Some(v) = $iter
            .peek()
            .and_then(|s| try_extract_integer_optional::<$int>(s))
        {
            $iter.next();
            Some(v)
        } else {
            None
        }
    };
    (expr: $iter:ident) => {
        $iter
            .next()
            .ok_or(None)
            .and_then(|s| try_extract_expression_ident(s).map_err(Some))
    };
    (str: $iter:ident) => {
        $iter
            .next()
            .ok_or(None)
            .and_then(|s| try_extract_string(s).map_err(Some))
    };
}

pub fn parse_directive<'a, I: Iterator<Item = ParseSpan<Token<'a>>>>(
    dir: &'a str,
    iter: &mut Peekable<I>,
) -> Result<Directive<'a>, Option<ParseSpan<Token<'a>>>> {
    match dir.to_ascii_lowercase().as_ref() {
        "align" => {
            let align = parse_next_token!(int: iter => u32)?;
            Ok(Directive::Align(align))
        }
        "byte" => {
            let byte = parse_next_token!(int: iter => u8)?;
            Ok(Directive::Byte(byte))
        }
        "equ" => {
            let symbol = parse_next_token!(expr: iter)?;
            let value = parse_next_token!(int: iter => u32)?;
            Ok(Directive::Equ(symbol, value))
        }
        "halfword" => {
            let halfword = parse_next_token!(int: iter => u16)?;
            Ok(Directive::Halfword(halfword))
        }
        "include" => {
            let include = parse_next_token!(str: iter)?;
            Ok(Directive::Include(include))
        }
        "word" => {
            let word = parse_next_token!(int: iter => u32)?;
            Ok(Directive::Word(word))
        }
        "section" => {
            let name = parse_next_token!(expr: iter)?;
            let addr = parse_next_token!(optint: iter => u32);
            Ok(Directive::Section(name, addr))
        }
        "skip" => {
            let skip = parse_next_token!(int: iter => u32)?;
            let filler = parse_next_token!(optint: iter => u8);
            Ok(Directive::Skip(skip, filler))
        }
        "str" => {
            let string = parse_next_token!(str: iter)?;
            Ok(Directive::Str(string))
        }
        _ => unimplemented!(),
    }
}

pub struct Context {
    pub context_name: OsString,
}

impl Context {
    pub fn new() -> Self {
        Context {
            context_name: OsString::from("<<main>>"),
        }
    }

    pub fn set_context_name(&mut self, name: OsString) {
        self.context_name = name;
    }
}
