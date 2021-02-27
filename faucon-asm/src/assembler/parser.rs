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
use crate::operands::{MemoryAccess, MemorySpace, Register, RegisterKind};

pub type LineSpan<'a> = nom_locate::LocatedSpan<&'a str>;
pub type ParserResult<'a, T> = IResult<LineSpan<'a>, T>;

macro_rules! parse_to_type {
    ($input:expr => $output:expr) => {
        map(tag_no_case($input), |_| $output)
    };
}

pub fn start<'a, T>(
    parser: impl Fn(LineSpan<'a>) -> ParserResult<'a, T>,
) -> impl Fn(&'a str) -> ParserResult<'a, T> {
    move |s: &'a str| parser(LineSpan::new(s))
}

fn eol_comment(input: LineSpan) -> ParserResult<()> {
    value((), tuple((tag("//"), none_of("/"), is_not("\n\r"))))(input)
}

fn pinline_comment(input: LineSpan) -> ParserResult<()> {
    value((), tuple((tag("/*"), take_until("*/"), tag("*/"))))(input)
}

pub fn whitespace<'a, T>(
    parser: impl FnMut(LineSpan<'a>) -> ParserResult<'a, T>,
) -> impl FnMut(LineSpan<'a>) -> ParserResult<'a, T> {
    delimited(
        many0(alt((value((), multispace1), eol_comment, pinline_comment))),
        parser,
        many0(alt((value((), multispace1), eol_comment, pinline_comment))),
    )
}

pub fn semicolon<'a, O, E: ParseError<LineSpan<'a>>, F: 'a>(
    parser: F,
) -> impl FnMut(LineSpan<'a>) -> IResult<LineSpan<'a>, O, E>
where
    F: Parser<LineSpan<'a>, O, E>,
{
    terminated(parser, preceded(multispace0, tag(";")))
}

pub fn identifier(input: LineSpan) -> ParserResult<&str> {
    let (ls, ident) = recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)?;
    Ok((ls, &ident))
}

pub fn mnemonic(input: LineSpan) -> ParserResult<(InstructionKind, OperandSize)> {
    pair(
        alt((
            alt((
                parse_to_type!("cmpu" => InstructionKind::CMPU),
                parse_to_type!("cmps" => InstructionKind::CMPS),
                parse_to_type!("cmp" => InstructionKind::CMP),
                parse_to_type!("add" => InstructionKind::ADD),
                parse_to_type!("adc" => InstructionKind::ADC),
                parse_to_type!("sub" => InstructionKind::SUB),
                parse_to_type!("sbb" => InstructionKind::SBB),
                parse_to_type!("shl" => InstructionKind::SHL),
                parse_to_type!("shr" => InstructionKind::SHR),
                parse_to_type!("sar" => InstructionKind::SAR),
                parse_to_type!("shlc" => InstructionKind::SHLC),
                parse_to_type!("shrc" => InstructionKind::SHRC),
                parse_to_type!("not" => InstructionKind::NOT),
                parse_to_type!("neg" => InstructionKind::NEG),
                parse_to_type!("hswap" => InstructionKind::HSWAP),
                parse_to_type!("sethi" => InstructionKind::SETHI),
                parse_to_type!("clear" => InstructionKind::CLEAR),
                parse_to_type!("mulu" => InstructionKind::MULU),
                parse_to_type!("muls" => InstructionKind::MULS),
                parse_to_type!("sext" => InstructionKind::SEXT),
                parse_to_type!("and" => InstructionKind::AND),
            )),
            alt((
                parse_to_type!("or" => InstructionKind::OR),
                parse_to_type!("xor" => InstructionKind::XOR),
                parse_to_type!("xbit" => InstructionKind::XBIT),
                parse_to_type!("bset" => InstructionKind::BSET),
                parse_to_type!("bclr" => InstructionKind::BCLR),
                parse_to_type!("btgl" => InstructionKind::BTGL),
                parse_to_type!("div" => InstructionKind::DIV),
                parse_to_type!("mod" => InstructionKind::MOD),
                parse_to_type!("setp" => InstructionKind::SETP),
                parse_to_type!("mov" => InstructionKind::MOV),
                parse_to_type!("ld" => InstructionKind::LD),
                parse_to_type!("st" => InstructionKind::ST),
                parse_to_type!("push" => InstructionKind::PUSH),
                parse_to_type!("pop" => InstructionKind::POP),
                parse_to_type!("mpush" => InstructionKind::MPUSH),
                parse_to_type!("mpop" => InstructionKind::MPOP),
                parse_to_type!("mpopadd" => InstructionKind::MPOPADD),
                parse_to_type!("mpopret" => InstructionKind::MPOPRET),
                parse_to_type!("mpopaddret" => InstructionKind::MPOPADDRET),
                parse_to_type!("call" => InstructionKind::CALL),
                parse_to_type!("lcall" => InstructionKind::LCALL),
            )),
            alt((
                parse_to_type!("bra" => InstructionKind::BRA),
                parse_to_type!("lbra" => InstructionKind::LBRA),
                parse_to_type!("ret" => InstructionKind::RET),
                parse_to_type!("exit" => InstructionKind::EXIT),
                parse_to_type!("sleep" => InstructionKind::SLEEP),
                parse_to_type!("ptlb" => InstructionKind::PTLB),
                parse_to_type!("vtlb" => InstructionKind::VTLB),
                parse_to_type!("itlb" => InstructionKind::ITLB),
                parse_to_type!("iret" => InstructionKind::IRET),
                parse_to_type!("trap" => InstructionKind::TRAP),
                parse_to_type!("xcld" => InstructionKind::XCLD),
                parse_to_type!("xdld" => InstructionKind::XDLD),
                parse_to_type!("xdst" => InstructionKind::XDST),
                parse_to_type!("xcwait" => InstructionKind::XCWAIT),
                parse_to_type!("xdwait" => InstructionKind::XDWAIT),
                parse_to_type!("iord" => InstructionKind::IORD),
                parse_to_type!("iords" => InstructionKind::IORDS),
                parse_to_type!("iowr" => InstructionKind::IOWR),
                parse_to_type!("iowrs" => InstructionKind::IOWRS),
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

fn general_purpose_register(input: LineSpan) -> ParserResult<Register> {
    map(
        preceded(
            alt((complete(tag_no_case("reg")), tag_no_case("r"))),
            digit1,
        ),
        |s: LineSpan| Register(RegisterKind::Gpr, s.fragment().parse::<usize>().unwrap()),
    )(input)
}

fn special_purpose_register(input: LineSpan) -> ParserResult<Register> {
    alt((
        parse_to_type!("iv0" => Register(RegisterKind::Spr, 0x0)),
        parse_to_type!("iv1" => Register(RegisterKind::Spr, 0x1)),
        parse_to_type!("iv2" => Register(RegisterKind::Spr, 0x2)),
        parse_to_type!("ev" => Register(RegisterKind::Spr, 0x3)),
        parse_to_type!("sp" => Register(RegisterKind::Spr, 0x4)),
        parse_to_type!("pc" => Register(RegisterKind::Spr, 0x5)),
        parse_to_type!("imb" => Register(RegisterKind::Spr, 0x6)),
        parse_to_type!("dmb" => Register(RegisterKind::Spr, 0x7)),
        parse_to_type!("csw" => Register(RegisterKind::Spr, 0x8)),
        parse_to_type!("ccr" => Register(RegisterKind::Spr, 0x9)),
        parse_to_type!("sec" => Register(RegisterKind::Spr, 0xA)),
        parse_to_type!("ctx" => Register(RegisterKind::Spr, 0xB)),
        parse_to_type!("exci" => Register(RegisterKind::Spr, 0xC)),
        parse_to_type!("sec1" => Register(RegisterKind::Spr, 0xD)),
        parse_to_type!("imb1" => Register(RegisterKind::Spr, 0xE)),
        parse_to_type!("dmb1" => Register(RegisterKind::Spr, 0xF)),
    ))(input)
}

pub fn register(input: LineSpan) -> ParserResult<Register> {
    preceded(
        char('$'),
        alt((general_purpose_register, special_purpose_register)),
    )(input)
}

pub fn memory_access(input: LineSpan) -> ParserResult<MemoryAccess> {
    // Parse the prefix that indicates the accessed memory space.
    let (input, space) = alt((
        parse_to_type!("i" => MemorySpace::IMem),
        parse_to_type!("d" => MemorySpace::DMem),
    ))(input)?;

    // Prepare a parser for the single-register form: `$rX`.
    let reg = map(register, move |base| MemoryAccess::Reg { space, base });
    // Prepare a parser for the double-register form: `$rX + $rY * scale`.
    let reg_reg = map(
        tuple((
            register,
            whitespace(tag("+")),
            register,
            opt(preceded(whitespace(tag("*")), unsigned_integer)),
        )),
        move |out: (Register, LineSpan, Register, Option<u8>)| MemoryAccess::RegReg {
            space,
            base: out.0,
            offset: out.2,
            scale: out.3.unwrap_or(1),
        },
    );
    // Prepare a parser for the register-immediate form: `$rX + imm`.
    let reg_imm = map(
        tuple((register, whitespace(tag("+")), unsigned_integer)),
        move |out: (Register, LineSpan, u32)| MemoryAccess::RegImm {
            space,
            base: out.0,
            offset: out.2,
        },
    );

    // Put it all together to parse the memory access.
    delimited(tag("["), whitespace(alt((reg_imm, reg_reg, reg))), tag("]"))(input)
}

pub fn expression(input: LineSpan) -> ParserResult<&str> {
    let (ls, expr) = preceded(char('#'), identifier)(input)?;
    Ok((ls, &expr))
}

pub fn label_definition(input: LineSpan) -> ParserResult<&str> {
    let (ls, expr) = terminated(expression, char(':'))(input)?;
    Ok((ls, &expr))
}

pub fn directive(input: LineSpan) -> ParserResult<&str> {
    let (ls, ident) = preceded(tag("."), identifier)(input)?;
    Ok((ls, &ident))
}

pub fn signed_integer<T>(input: LineSpan) -> ParserResult<T>
where
    T: PrimInt + Signed,
{
    alt((
        signed_binary,
        signed_hexadecimal,
        signed_octal,
        signed_decimal,
    ))(input)
}

pub fn unsigned_integer<T>(input: LineSpan) -> ParserResult<T>
where
    T: PrimInt + Unsigned,
{
    alt((
        unsigned_binary,
        unsigned_hexadecimal,
        unsigned_octal,
        unsigned_decimal,
    ))(input)
}

#[inline]
fn parse_number<T>(literal: &str, radix: u32) -> Result<T, <T as Num>::FromStrRadixErr>
where
    T: PrimInt,
{
    T::from_str_radix(&str::replace(&literal, "_", ""), radix)
}

fn signed_decimal<T>(input: LineSpan) -> ParserResult<T>
where
    T: PrimInt + Signed,
{
    map_res(
        pair(
            map(opt(alt((tag("+"), tag("-")))), |sign: Option<LineSpan>| {
                sign.map(|s| if *s.fragment() == "-" { true } else { false })
                    .unwrap_or(false)
            }),
            recognize(many1(terminated(one_of("0123456789"), many0(char('_'))))),
        ),
        |out: (bool, LineSpan)| {
            parse_number(out.1.fragment(), 10).and_then(|n: T| if out.0 { Ok(-n) } else { Ok(n) })
        },
    )(input)
}

fn unsigned_decimal<T>(input: LineSpan) -> ParserResult<T>
where
    T: PrimInt + Unsigned,
{
    map_res(
        preceded(
            opt(tag("+")),
            recognize(many1(terminated(one_of("0123456789"), many0(char('_'))))),
        ),
        |out: LineSpan| parse_number(out.fragment(), 10),
    )(input)
}

fn signed_hexadecimal<T>(input: LineSpan) -> ParserResult<T>
where
    T: PrimInt + Signed,
{
    map_res(
        pair(
            map(opt(alt((tag("+"), tag("-")))), |sign: Option<LineSpan>| {
                sign.map(|s| if *s.fragment() == "-" { true } else { false })
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
        |out: (bool, LineSpan)| {
            parse_number(out.1.fragment(), 16).and_then(|n: T| if out.0 { Ok(-n) } else { Ok(n) })
        },
    )(input)
}

fn unsigned_hexadecimal<T>(input: LineSpan) -> ParserResult<T>
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
        |out: LineSpan| parse_number(out.fragment(), 16),
    )(input)
}

fn signed_octal<T>(input: LineSpan) -> ParserResult<T>
where
    T: PrimInt + Signed,
{
    map_res(
        pair(
            map(opt(alt((tag("+"), tag("-")))), |sign: Option<LineSpan>| {
                sign.map(|s| if *s.fragment() == "-" { true } else { false })
                    .unwrap_or(false)
            }),
            preceded(
                complete(tag_no_case("0o")),
                recognize(many1(terminated(one_of("01234567"), many0(char('_'))))),
            ),
        ),
        |out: (bool, LineSpan)| {
            parse_number(out.1.fragment(), 8).and_then(|n: T| if out.0 { Ok(-n) } else { Ok(n) })
        },
    )(input)
}

fn unsigned_octal<T>(input: LineSpan) -> ParserResult<T>
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
        |out: LineSpan| parse_number(out.fragment(), 8),
    )(input)
}

fn signed_binary<T>(input: LineSpan) -> ParserResult<T>
where
    T: PrimInt + Signed,
{
    map_res(
        pair(
            map(opt(alt((tag("+"), tag("-")))), |sign: Option<LineSpan>| {
                sign.map(|s| if *s.fragment() == "-" { true } else { false })
                    .unwrap_or(false)
            }),
            preceded(
                complete(tag_no_case("0b")),
                recognize(many1(terminated(one_of("01"), many0(char('_'))))),
            ),
        ),
        |out: (bool, LineSpan)| {
            parse_number(out.1.fragment(), 2).and_then(|n: T| if out.0 { Ok(-n) } else { Ok(n) })
        },
    )(input)
}

fn unsigned_binary<T>(input: LineSpan) -> ParserResult<T>
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
        |out: LineSpan| parse_number(out.fragment(), 2),
    )(input)
}
