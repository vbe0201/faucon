use std::ffi::OsStr;

use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;
use num_traits::{Num, PrimInt, Signed, Unsigned};

use crate::isa::InstructionKind;
use crate::opcode::OperandSize;
use crate::operands::{MemoryAccess, MemorySpace, Register, RegisterKind};

pub type LineSpan<'a> = nom_locate::LocatedSpan<&'a str, (&'a OsStr, &'a str)>;

pub fn start<'a, T>(
    file_name: &'a OsStr,
    source: &'a str,
    mut parser: impl FnMut(LineSpan<'a>) -> IResult<LineSpan<'a>, T>,
) -> impl FnMut(&'a str) -> IResult<LineSpan<'a>, T> {
    move |s: &'a str| parser(LineSpan::new_extra(s, (file_name, source)))
}

fn eol_comment(input: LineSpan) -> IResult<LineSpan, ()> {
    value((), pair(tag("//"), is_not("\n\r")))(input)
}

fn pinline_comment(input: LineSpan) -> IResult<LineSpan, ()> {
    value((), tuple((tag("/*"), take_until("*/"), tag("*/"))))(input)
}

pub fn ws0<'a, T>(
    parser: impl FnMut(LineSpan<'a>) -> IResult<LineSpan<'a>, T>,
) -> impl FnMut(LineSpan<'a>) -> IResult<LineSpan<'a>, T> {
    delimited(
        many0(alt((value((), multispace1), eol_comment, pinline_comment))),
        parser,
        many0(alt((value((), multispace1), eol_comment, pinline_comment))),
    )
}

pub fn ws1<'a, T>(
    parser: impl FnMut(LineSpan<'a>) -> IResult<LineSpan<'a>, T>,
) -> impl FnMut(LineSpan<'a>) -> IResult<LineSpan<'a>, T> {
    delimited(
        many0(alt((value((), multispace1), eol_comment, pinline_comment))),
        parser,
        many1(alt((value((), multispace1), eol_comment, pinline_comment))),
    )
}

pub fn string_literal(input: LineSpan) -> IResult<LineSpan, &str> {
    let (ls, lit) = tuple((tag(r#"""#), take_until(r#"""#), tag(r#"""#)))(input)?;
    Ok((ls, &lit.1))
}

fn identifier(input: LineSpan) -> IResult<LineSpan, &str> {
    let (ls, ident) = recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)?;
    Ok((ls, &ident))
}

pub fn mnemonic(input: LineSpan) -> IResult<LineSpan, (InstructionKind, OperandSize)> {
    pair(
        alt((
            alt((
                value(InstructionKind::CMPU, tag_no_case("cmpu")),
                value(InstructionKind::CMPS, tag_no_case("cmps")),
                value(InstructionKind::CMP, tag_no_case("cmp")),
                value(InstructionKind::ADDSP, tag_no_case("addsp")),
                value(InstructionKind::CCR, tag_no_case("ccr")),
                value(InstructionKind::ADD, tag_no_case("add")),
                value(InstructionKind::ADC, tag_no_case("adc")),
                value(InstructionKind::SUB, tag_no_case("sub")),
                value(InstructionKind::SBB, tag_no_case("sbb")),
                value(InstructionKind::SHL, tag_no_case("shl")),
                value(InstructionKind::SHR, tag_no_case("shr")),
                value(InstructionKind::SAR, tag_no_case("sar")),
                value(InstructionKind::SHLC, tag_no_case("shlc")),
                value(InstructionKind::SHRC, tag_no_case("shrc")),
                value(InstructionKind::NOT, tag_no_case("not")),
                value(InstructionKind::NEG, tag_no_case("neg")),
                value(InstructionKind::HSWAP, tag_no_case("hswap")),
                value(InstructionKind::SETHI, tag_no_case("sethi")),
                value(InstructionKind::CLEAR, tag_no_case("clear")),
                value(InstructionKind::TEST, tag_no_case("test")),
                value(InstructionKind::MULU, tag_no_case("mulu")),
            )),
            alt((
                value(InstructionKind::MULS, tag_no_case("muls")),
                value(InstructionKind::SEXT, tag_no_case("sext")),
                value(InstructionKind::AND, tag_no_case("and")),
                value(InstructionKind::OR, tag_no_case("or")),
                value(InstructionKind::XOR, tag_no_case("xor")),
                value(InstructionKind::XBIT, tag_no_case("xbit")),
                value(InstructionKind::BSET, tag_no_case("bset")),
                value(InstructionKind::BCLR, tag_no_case("bclr")),
                value(InstructionKind::BTGL, tag_no_case("btgl")),
                value(InstructionKind::DIV, tag_no_case("div")),
                value(InstructionKind::MOD, tag_no_case("mod")),
                value(InstructionKind::SETP, tag_no_case("setp")),
                value(InstructionKind::MOV, tag_no_case("mov")),
                value(InstructionKind::LD, tag_no_case("ld")),
                value(InstructionKind::ST, tag_no_case("st")),
                value(InstructionKind::PUSH, tag_no_case("push")),
                value(InstructionKind::POP, tag_no_case("pop")),
                value(InstructionKind::MPUSH, tag_no_case("mpush")),
                value(InstructionKind::MPOP, tag_no_case("mpop")),
                value(InstructionKind::MPOPADD, tag_no_case("mpopadd")),
                value(InstructionKind::MPOPRET, tag_no_case("mpopret")),
            )),
            alt((
                value(InstructionKind::MPOPADDRET, tag_no_case("mpopaddret")),
                value(InstructionKind::CALL, tag_no_case("call")),
                value(InstructionKind::LCALL, tag_no_case("lcall")),
                value(InstructionKind::JMP, tag_no_case("jmp")),
                value(InstructionKind::BP, tag_no_case("bp")),
                value(InstructionKind::BC, tag_no_case("bc")),
                value(InstructionKind::BO, tag_no_case("bo")),
                value(InstructionKind::BS, tag_no_case("bs")),
                value(InstructionKind::BZ, tag_no_case("bz")),
                value(InstructionKind::BA, tag_no_case("ba")),
                value(InstructionKind::BNA, tag_no_case("bna")),
                value(InstructionKind::BRA, tag_no_case("bra")),
                value(InstructionKind::BNP, tag_no_case("bnp")),
                value(InstructionKind::BNC, tag_no_case("bnc")),
                value(InstructionKind::BNO, tag_no_case("bno")),
                value(InstructionKind::BNS, tag_no_case("bns")),
                value(InstructionKind::BNZ, tag_no_case("bnz")),
                value(InstructionKind::BGE, tag_no_case("bge")),
                value(InstructionKind::BG, tag_no_case("bg")),
                value(InstructionKind::BLE, tag_no_case("ble")),
                value(InstructionKind::BL, tag_no_case("bl")),
            )),
            alt((
                value(InstructionKind::LBRA, tag_no_case("lbra")),
                value(InstructionKind::RET, tag_no_case("ret")),
                value(InstructionKind::HALT, tag_no_case("halt")),
                value(InstructionKind::SLEEP, tag_no_case("sleep")),
                value(InstructionKind::PTLB, tag_no_case("ptlb")),
                value(InstructionKind::VTLB, tag_no_case("vtlb")),
                value(InstructionKind::ITLB, tag_no_case("itlb")),
                value(InstructionKind::IRET, tag_no_case("iret")),
                value(InstructionKind::TRAP, tag_no_case("trap")),
                value(InstructionKind::XCLD, tag_no_case("xcld")),
                value(InstructionKind::XDLD, tag_no_case("xdld")),
                value(InstructionKind::XDST, tag_no_case("xdst")),
                value(InstructionKind::XCWAIT, tag_no_case("xcwait")),
                value(InstructionKind::XDWAIT, tag_no_case("xdwait")),
                value(InstructionKind::XDFENCE, tag_no_case("xdfence")),
                value(InstructionKind::IORD, tag_no_case("iord")),
                value(InstructionKind::IORDS, tag_no_case("iords")),
                value(InstructionKind::IOWR, tag_no_case("iowr")),
                value(InstructionKind::IOWRS, tag_no_case("iowrs")),
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

fn general_purpose_register(input: LineSpan) -> IResult<LineSpan, Register> {
    map(
        preceded(
            alt((complete(tag_no_case("reg")), tag_no_case("r"))),
            digit1,
        ),
        |s: LineSpan| Register(RegisterKind::Gpr, s.fragment().parse::<usize>().unwrap()),
    )(input)
}

fn special_purpose_register(input: LineSpan) -> IResult<LineSpan, Register> {
    alt((
        value(Register(RegisterKind::Spr, 0x0), tag_no_case("iv0")),
        value(Register(RegisterKind::Spr, 0x1), tag_no_case("iv1")),
        value(Register(RegisterKind::Spr, 0x2), tag_no_case("iv2")),
        value(Register(RegisterKind::Spr, 0x3), tag_no_case("ev")),
        value(Register(RegisterKind::Spr, 0x4), tag_no_case("sp")),
        value(Register(RegisterKind::Spr, 0x5), tag_no_case("pc")),
        value(Register(RegisterKind::Spr, 0x6), tag_no_case("imb")),
        value(Register(RegisterKind::Spr, 0x7), tag_no_case("dmb")),
        value(Register(RegisterKind::Spr, 0x8), tag_no_case("csw")),
        value(Register(RegisterKind::Spr, 0x9), tag_no_case("ccr")),
        value(Register(RegisterKind::Spr, 0xA), tag_no_case("sec")),
        value(Register(RegisterKind::Spr, 0xB), tag_no_case("ctx")),
        value(Register(RegisterKind::Spr, 0xC), tag_no_case("exci")),
        value(Register(RegisterKind::Spr, 0xD), tag_no_case("sec1")),
        value(Register(RegisterKind::Spr, 0xE), tag_no_case("imb1")),
        value(Register(RegisterKind::Spr, 0xF), tag_no_case("dmb1")),
    ))(input)
}

pub fn register(input: LineSpan) -> IResult<LineSpan, Register> {
    preceded(
        char('$'),
        alt((general_purpose_register, special_purpose_register)),
    )(input)
}

pub fn flag(input: LineSpan) -> IResult<LineSpan, u8> {
    preceded(
        char('$'),
        alt((
            value(0x00, tag_no_case("p0")),
            value(0x01, tag_no_case("p1")),
            value(0x02, tag_no_case("p2")),
            value(0x03, tag_no_case("p3")),
            value(0x04, tag_no_case("p4")),
            value(0x05, tag_no_case("p5")),
            value(0x06, tag_no_case("p6")),
            value(0x07, tag_no_case("p7")),
            value(0x08, tag_no_case("c")),
            value(0x09, tag_no_case("o")),
            value(0x0A, tag_no_case("s")),
            value(0x0B, tag_no_case("z")),
            value(0x10, tag_no_case("ie0")),
            value(0x11, tag_no_case("ie1")),
            value(0x12, tag_no_case("ie2")),
            value(0x14, tag_no_case("is0")),
            value(0x15, tag_no_case("is1")),
            value(0x16, tag_no_case("is2")),
            value(0x18, tag_no_case("ea")),
        )),
    )(input)
}

pub fn memory_access(input: LineSpan) -> IResult<LineSpan, MemoryAccess> {
    // Parse the prefix that indicates the accessed memory space.
    let (input, space) = alt((
        value(MemorySpace::IMem, tag_no_case("i")),
        value(MemorySpace::DMem, tag_no_case("d")),
    ))(input)?;

    // Prepare a parser for the single-register form: `$rX`.
    let reg = map(register, move |base| MemoryAccess::Reg { space, base });
    // Prepare a parser for the double-register form: `$rX + $rY * scale`.
    let reg_reg = map(
        tuple((
            register,
            ws0(tag("+")),
            register,
            opt(preceded(ws0(tag("*")), unsigned_integer)),
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
        tuple((register, ws0(tag("+")), unsigned_integer)),
        move |out: (Register, LineSpan, u32)| MemoryAccess::RegImm {
            space,
            base: out.0,
            offset: out.2,
        },
    );

    // Put it all together to parse the memory access.
    delimited(tag("["), ws0(alt((reg_imm, reg_reg, reg))), tag("]"))(input)
}

pub fn symbol(input: LineSpan) -> IResult<LineSpan, (&str, bool)> {
    let (ls, (_, is_physical, ident)) = tuple((char('#'), opt(char('#')), identifier))(input)?;
    Ok((ls, (ident, is_physical.is_some())))
}

pub fn label_definition(input: LineSpan) -> IResult<LineSpan, &str> {
    let (ls, ident) = terminated(identifier, char(':'))(input)?;
    Ok((ls, &ident))
}

pub fn directive(input: LineSpan) -> IResult<LineSpan, &str> {
    let (ls, ident) = preceded(tag("."), identifier)(input)?;
    Ok((ls, &ident))
}

pub fn signed_integer<T>(input: LineSpan) -> IResult<LineSpan, T>
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

pub fn unsigned_integer<T>(input: LineSpan) -> IResult<LineSpan, T>
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

fn signed_decimal<T>(input: LineSpan) -> IResult<LineSpan, T>
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

fn unsigned_decimal<T>(input: LineSpan) -> IResult<LineSpan, T>
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

fn signed_hexadecimal<T>(input: LineSpan) -> IResult<LineSpan, T>
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

fn unsigned_hexadecimal<T>(input: LineSpan) -> IResult<LineSpan, T>
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

fn signed_octal<T>(input: LineSpan) -> IResult<LineSpan, T>
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

fn unsigned_octal<T>(input: LineSpan) -> IResult<LineSpan, T>
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

fn signed_binary<T>(input: LineSpan) -> IResult<LineSpan, T>
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

fn unsigned_binary<T>(input: LineSpan) -> IResult<LineSpan, T>
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
