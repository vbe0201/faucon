use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::error::ParseError;
use nom::multi::*;
use nom::sequence::*;
use nom::{IResult, Parser};
use num_traits::{Num, PrimInt, Signed, Unsigned};

use crate::isa::InstructionKind;
use crate::opcode::OperandSize;

pub fn whitespace<'a, O, E: ParseError<&'a str>, F: 'a>(
    parser: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
{
    delimited(multispace0, parser, multispace0)
}

pub fn eol_comment(input: &str) -> IResult<&str, ()> {
    value((), pair(tag("//"), is_not("\n\r")))(input)
}

pub fn pinline_comment(input: &str) -> IResult<&str, ()> {
    value((), tuple((tag("/*"), take_until("*/"), tag("*/"))))(input)
}

pub fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

macro_rules! parse_mnemonic {
    ($input:expr => $variant:expr) => {
        map(tag_no_case($input), |_| $variant)
    };
}

pub fn mnemonic(input: &str) -> IResult<&str, (InstructionKind, OperandSize)> {
    pair(
        alt((
            alt((
                parse_mnemonic!("cmpu" => InstructionKind::CMPU),
                parse_mnemonic!("cmps" => InstructionKind::CMPS),
                parse_mnemonic!("cmp" => InstructionKind::CMP),
                parse_mnemonic!("add" => InstructionKind::ADD),
                parse_mnemonic!("adc" => InstructionKind::ADC),
                parse_mnemonic!("sub" => InstructionKind::SUB),
                parse_mnemonic!("sbb" => InstructionKind::SBB),
                parse_mnemonic!("shl" => InstructionKind::SHL),
                parse_mnemonic!("shr" => InstructionKind::SHR),
                parse_mnemonic!("sar" => InstructionKind::SAR),
                parse_mnemonic!("shlc" => InstructionKind::SHLC),
                parse_mnemonic!("shrc" => InstructionKind::SHRC),
                parse_mnemonic!("not" => InstructionKind::NOT),
                parse_mnemonic!("neg" => InstructionKind::NEG),
                parse_mnemonic!("hswap" => InstructionKind::HSWAP),
                parse_mnemonic!("sethi" => InstructionKind::SETHI),
                parse_mnemonic!("clear" => InstructionKind::CLEAR),
                parse_mnemonic!("mulu" => InstructionKind::MULU),
                parse_mnemonic!("muls" => InstructionKind::MULS),
                parse_mnemonic!("sext" => InstructionKind::SEXT),
                parse_mnemonic!("and" => InstructionKind::AND),
            )),
            alt((
                parse_mnemonic!("or" => InstructionKind::OR),
                parse_mnemonic!("xor" => InstructionKind::XOR),
                parse_mnemonic!("xbit" => InstructionKind::XBIT),
                parse_mnemonic!("bset" => InstructionKind::BSET),
                parse_mnemonic!("bclr" => InstructionKind::BCLR),
                parse_mnemonic!("btgl" => InstructionKind::BTGL),
                parse_mnemonic!("div" => InstructionKind::DIV),
                parse_mnemonic!("mod" => InstructionKind::MOD),
                parse_mnemonic!("setp" => InstructionKind::SETP),
                parse_mnemonic!("mov" => InstructionKind::MOV),
                parse_mnemonic!("ld" => InstructionKind::LD),
                parse_mnemonic!("st" => InstructionKind::ST),
                parse_mnemonic!("push" => InstructionKind::PUSH),
                parse_mnemonic!("pop" => InstructionKind::POP),
                parse_mnemonic!("mpush" => InstructionKind::MPUSH),
                parse_mnemonic!("mpop" => InstructionKind::MPOP),
                parse_mnemonic!("mpopadd" => InstructionKind::MPOPADD),
                parse_mnemonic!("mpopret" => InstructionKind::MPOPRET),
                parse_mnemonic!("mpopaddret" => InstructionKind::MPOPADDRET),
                parse_mnemonic!("call" => InstructionKind::CALL),
                parse_mnemonic!("lcall" => InstructionKind::LCALL),
            )),
            alt((
                parse_mnemonic!("bra" => InstructionKind::BRA),
                parse_mnemonic!("lbra" => InstructionKind::LBRA),
                parse_mnemonic!("ret" => InstructionKind::RET),
                parse_mnemonic!("exit" => InstructionKind::EXIT),
                parse_mnemonic!("sleep" => InstructionKind::SLEEP),
                parse_mnemonic!("ptlb" => InstructionKind::PTLB),
                parse_mnemonic!("vtlb" => InstructionKind::VTLB),
                parse_mnemonic!("itlb" => InstructionKind::ITLB),
                parse_mnemonic!("iret" => InstructionKind::IRET),
                parse_mnemonic!("trap" => InstructionKind::TRAP),
                parse_mnemonic!("xcld" => InstructionKind::XCLD),
                parse_mnemonic!("xdld" => InstructionKind::XDLD),
                parse_mnemonic!("xdst" => InstructionKind::XDST),
                parse_mnemonic!("xcwait" => InstructionKind::XCWAIT),
                parse_mnemonic!("xdwait" => InstructionKind::XDWAIT),
                parse_mnemonic!("iord" => InstructionKind::IORD),
                parse_mnemonic!("iords" => InstructionKind::IORDS),
                parse_mnemonic!("iowr" => InstructionKind::IOWR),
                parse_mnemonic!("iowrs" => InstructionKind::IOWRS),
            )),
        )),
        map(
            opt(preceded(char('.'), one_of("bhwBHW"))),
            |out: Option<char>| match out {
                Some('b') | Some('B') => OperandSize::EightBit,
                Some('h') | Some('H') => OperandSize::SixteenBit,
                Some('w') | Some('W') => OperandSize::ThirtyTwoBit,
                None => OperandSize::Unsized,
                _ => unreachable!(),
            },
        ),
    )(input)
}

pub fn label(input: &str) -> IResult<&str, &str> {
    preceded(char('#'), identifier)(input)
}

pub fn label_definition(input: &str) -> IResult<&str, &str> {
    terminated(label, char(':'))(input)
}

pub fn directive(input: &str) -> IResult<&str, &str> {
    preceded(char('.'), identifier)(input)
}

#[inline]
fn parse_number<T>(literal: &str, radix: u32) -> Result<T, <T as Num>::FromStrRadixErr>
where
    T: PrimInt,
{
    T::from_str_radix(&str::replace(&literal, "_", ""), radix)
}

pub fn signed_decimal<T>(input: &str) -> IResult<&str, T>
where
    T: PrimInt + Signed,
{
    map_res(
        pair(
            map(opt(alt((tag("+"), tag("-")))), |sign: Option<&str>| {
                sign.map(|s| if s == "-" { true } else { false })
                    .unwrap_or(false)
            }),
            recognize(many1(terminated(one_of("0123456789"), many0(char('_'))))),
        ),
        |out: (bool, &str)| {
            parse_number(out.1, 10).and_then(|n: T| if out.0 { Ok(-n) } else { Ok(n) })
        },
    )(input)
}

pub fn unsigned_decimal<T>(input: &str) -> IResult<&str, T>
where
    T: PrimInt + Unsigned,
{
    map_res(
        preceded(
            opt(tag("+")),
            recognize(many1(terminated(one_of("0123456789"), many0(char('_'))))),
        ),
        |out: &str| parse_number(out, 10),
    )(input)
}

pub fn signed_hexadecimal<T>(input: &str) -> IResult<&str, T>
where
    T: PrimInt + Signed,
{
    map_res(
        pair(
            map(opt(alt((tag("+"), tag("-")))), |sign: Option<&str>| {
                sign.map(|s| if s == "-" { true } else { false })
                    .unwrap_or(false)
            }),
            preceded(
                complete(tag_no_case("0x")),
                recognize(many1(terminated(
                    one_of("0123456789abcdefABCDEF"),
                    many0(char('_')),
                ))),
            ),
        ),
        |out: (bool, &str)| {
            parse_number(out.1, 16).and_then(|n: T| if out.0 { Ok(-n) } else { Ok(n) })
        },
    )(input)
}

pub fn unsigned_hexadecimal<T>(input: &str) -> IResult<&str, T>
where
    T: PrimInt + Unsigned,
{
    map_res(
        preceded(
            opt(tag("+")),
            preceded(
                complete(tag_no_case("0x")),
                recognize(many1(terminated(
                    one_of("0123456789abcdefABCDEF"),
                    many0(char('_')),
                ))),
            ),
        ),
        |out: &str| parse_number(out, 16),
    )(input)
}

pub fn signed_octal<T>(input: &str) -> IResult<&str, T>
where
    T: PrimInt + Signed,
{
    map_res(
        pair(
            map(opt(alt((tag("+"), tag("-")))), |sign: Option<&str>| {
                sign.map(|s| if s == "-" { true } else { false })
                    .unwrap_or(false)
            }),
            preceded(
                complete(tag_no_case("0o")),
                recognize(many1(terminated(one_of("01234567"), many0(char('_'))))),
            ),
        ),
        |out: (bool, &str)| {
            parse_number(out.1, 8).and_then(|n: T| if out.0 { Ok(-n) } else { Ok(n) })
        },
    )(input)
}

pub fn unsigned_octal<T>(input: &str) -> IResult<&str, T>
where
    T: PrimInt + Unsigned,
{
    map_res(
        preceded(
            opt(tag("+")),
            preceded(
                complete(tag_no_case("0o")),
                recognize(many1(terminated(one_of("01234567"), many0(char('_'))))),
            ),
        ),
        |out: &str| parse_number(out, 8),
    )(input)
}

pub fn signed_binary<T>(input: &str) -> IResult<&str, T>
where
    T: PrimInt + Signed,
{
    map_res(
        pair(
            map(opt(alt((tag("+"), tag("-")))), |sign: Option<&str>| {
                sign.map(|s| if s == "-" { true } else { false })
                    .unwrap_or(false)
            }),
            preceded(
                complete(tag_no_case("0b")),
                recognize(many1(terminated(one_of("01"), many0(char('_'))))),
            ),
        ),
        |out: (bool, &str)| {
            parse_number(out.1, 2).and_then(|n: T| if out.0 { Ok(-n) } else { Ok(n) })
        },
    )(input)
}

pub fn unsigned_binary<T>(input: &str) -> IResult<&str, T>
where
    T: PrimInt + Unsigned,
{
    map_res(
        preceded(
            opt(tag("+")),
            preceded(
                complete(tag_no_case("0b")),
                recognize(many1(terminated(one_of("01"), many0(char('_'))))),
            ),
        ),
        |out: &str| parse_number(out, 2),
    )(input)
}
