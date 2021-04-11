use std::ffi::OsStr;

use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::{IResult, Slice};
use num_traits::{Num, PrimInt, Signed, Unsigned};

use crate::assembler::lexer::Token;
use crate::assembler::span::{spanned, ParseSpan};
use crate::isa::InstructionKind;
use crate::opcode::OperandSize;
use crate::operands::*;

pub type LineSpan<'a> = nom_locate::LocatedSpan<&'a str, Metadata<'a>>;

#[derive(Clone, Copy, Debug)]
pub struct Metadata<'a> {
    full_input: &'a str,
    pub file_name: &'a OsStr,
}

impl<'a> Metadata<'a> {
    pub fn new(full_input: &'a str, file_name: &'a OsStr) -> Self {
        Metadata {
            full_input,
            file_name,
        }
    }

    pub fn get_line_contents(&self, offset: usize) -> &'a str {
        let start = self
            .full_input
            .slice(..offset)
            .rfind('\n')
            .map_or(0, |x| x + 1);

        self.full_input.slice(offset..).find('\n').map_or_else(
            || self.full_input.slice(start..),
            |end| self.full_input.slice(start..(offset + end)),
        )
    }
}

#[inline]
fn parse_number<T>(literal: &str, radix: u32) -> Result<T, <T as Num>::FromStrRadixErr>
where
    T: PrimInt,
{
    // XXX: Do we really need to replace here?
    T::from_str_radix(&str::replace(&literal, "_", ""), radix)
}

pub fn start<'a, T>(
    file_name: &'a OsStr,
    mut parser: impl FnMut(LineSpan<'a>) -> IResult<LineSpan<'a>, T>,
) -> impl FnMut(&'a str) -> IResult<LineSpan<'a>, T> {
    move |s: &'a str| parser(LineSpan::new_extra(s, Metadata::new(s, file_name)))
}

// *separator_list*? ( *statement* *separator_list* )* *eof*
pub fn do_parse(input: LineSpan<'_>) -> IResult<LineSpan<'_>, Vec<ParseSpan<Token<'_>>>> {
    let (input, _) = opt(separator_list)(input)?;
    let (input, result) = fold_many0(
        pair(statement, separator_list),
        Vec::with_capacity(100),
        |mut acc, ((label, inst), _)| {
            if let Some(l) = label {
                acc.push(l);
            }
            if let Some(i) = inst {
                acc.push(i.0);
                acc.extend(i.1);
            }

            acc
        },
    )(input)?;
    let (input, _) = eof(input)?;

    Ok((input, result))
}

// *label_decl*? ( *instruction* *operand** )?
fn statement(
    input: LineSpan<'_>,
) -> IResult<
    LineSpan<'_>,
    (
        Option<ParseSpan<Token<'_>>>,
        Option<(ParseSpan<Token<'_>>, Vec<ParseSpan<Token<'_>>>)>,
    ),
> {
    pair(
        opt(ws0(label_decl)),
        opt(pair(
            instruction,
            many0(preceded(many1(whitespace), operand)),
        )),
    )(input)
}

// *label*
fn label_decl(input: LineSpan<'_>) -> IResult<LineSpan<'_>, ParseSpan<Token<'_>>> {
    spanned(label)(input)
}

// ( *mnemonic* | *directive* )
fn instruction(input: LineSpan<'_>) -> IResult<LineSpan<'_>, ParseSpan<Token<'_>>> {
    spanned(alt((mnemonic, directive)))(input)
}

// *register* | *flag* | *memory_access* | *string* | *signed_integer* | ...
fn operand(input: LineSpan<'_>) -> IResult<LineSpan<'_>, ParseSpan<Token<'_>>> {
    spanned(alt((
        directive,
        symbol,
        register,
        flag,
        memory_access,
        bitfield,
        unsigned_integer,
        signed_integer,
        string,
    )))(input)
}

// *whitespace** *parser* *whitespace**
fn ws0<'a, T>(
    parser: impl FnMut(LineSpan<'a>) -> IResult<LineSpan<'a>, T>,
) -> impl FnMut(LineSpan<'a>) -> IResult<LineSpan<'a>, T> {
    delimited(many0(whitespace), parser, many0(whitespace))
}

// *separator*+
fn separator_list(input: LineSpan<'_>) -> IResult<LineSpan<'_>, LineSpan<'_>> {
    recognize(many1(separator))(input)
}

// *whitespace** ( `;` | *line_terminator* ) *whitespace**
fn separator(input: LineSpan<'_>) -> IResult<LineSpan<'_>, LineSpan<'_>> {
    ws0(alt((line_terminator, recognize(char(';')))))(input)
}

// ` ` | 0x09 | 0x0b | 0x0c | 0x20 | *comment*
fn whitespace(input: LineSpan<'_>) -> IResult<LineSpan<'_>, LineSpan<'_>> {
    alt((recognize(one_of(" \t\x0b\x0c\r")), recognize(comment)))(input)
}

// *single_line_comment* | *multi_line_comment*
fn comment(input: LineSpan<'_>) -> IResult<LineSpan<'_>, ()> {
    value((), alt((single_line_comment, multi_line_comment)))(input)
}

// `//` *line_content*?
fn single_line_comment(input: LineSpan<'_>) -> IResult<LineSpan<'_>, LineSpan<'_>> {
    recognize(tuple((tag("//"), opt(line_content))))(input)
}

// `/*` ...? `*/`
fn multi_line_comment(input: LineSpan<'_>) -> IResult<LineSpan<'_>, LineSpan<'_>> {
    recognize(tuple((tag("/*"), take_until("*/"), tag("*/"))))(input)
}

// *anychar*+ *line_terminator*?
fn line_content(input: LineSpan<'_>) -> IResult<LineSpan<'_>, LineSpan<'_>> {
    recognize(many1(preceded(peek(not(line_terminator)), anychar)))(input)
}

// `\r`? `\n`
fn line_terminator(input: LineSpan<'_>) -> IResult<LineSpan<'_>, LineSpan<'_>> {
    line_ending(input)
}

// `.` *identifier*
fn directive(input: LineSpan<'_>) -> IResult<LineSpan<'_>, Token<'_>> {
    map(preceded(tag("."), identifier), Token::Directive)(input)
}

// `#` `#`? *identifier*
fn symbol(input: LineSpan<'_>) -> IResult<LineSpan<'_>, Token<'_>> {
    let (ls, (_, is_physical, ident)) = tuple((tag("#"), opt(tag("#")), identifier))(input)?;
    Ok((ls, Token::Symbol((ident, is_physical.is_some()))))
}

// *identifier* `:`
fn label(input: LineSpan<'_>) -> IResult<LineSpan<'_>, Token<'_>> {
    map(terminated(identifier, tag(":")), Token::Label)(input)
}

// ( *alpha1* | `_` ) ( *alphanumeric1* | `_` )*
fn identifier(input: LineSpan<'_>) -> IResult<LineSpan<'_>, &str> {
    let (ls, ident) = recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)?;
    Ok((ls, &ident))
}

// *_mnemonic*
fn mnemonic(input: LineSpan<'_>) -> IResult<LineSpan<'_>, Token<'_>> {
    map(_mnemonic, Token::Mnemonic)(input)
}

// *opcode* *operand_size*
fn _mnemonic(input: LineSpan<'_>) -> IResult<LineSpan<'_>, (InstructionKind, OperandSize)> {
    pair(opcode, operand_size)(input)
}

// `CMPU` | `CMPS` | `CMP` | ...
fn opcode(input: LineSpan<'_>) -> IResult<LineSpan<'_>, InstructionKind> {
    macro_rules! mnemonic_values {
        ($variant:ident) => {
            value(InstructionKind::$variant, tag_no_case(stringify!($variant)))
        };
        ($($variant:ident),+) => {
            // XXX: alt() blocks take at least two and at most twenty-one values.
            //      Keep that in mind when invoking this arm of the macro.
            alt((
                $(mnemonic_values!($variant)),*
            ))
        };
    }

    alt((
        mnemonic_values!(
            CMPU, CMPS, CMP, ADDSP, CCR, ADD, ADC, SUB, SBB, SHL, SHR, SAR, SHLC, SHRC, NOT, NEG,
            HSWAP, SETHI, CLEAR, TEST, MULU
        ),
        mnemonic_values!(
            MULS, SEXT, AND, OR, XOR, XBIT, BSET, BCLR, BTGL, DIV, MOD, SETP, EXTRS, EXTR, INS,
            MOV, LD, ST, PUSH, POP, MPUSH
        ),
        mnemonic_values!(
            MPOP, MPOPADD, MPOPRET, MPOPADDRET, CALL, LCALL, JMP, BCMPE, BCMPNE, BP, BC, BO, BS,
            BZ, BA, BNA, BRA, BNP, BNC, BNO, BNS
        ),
        mnemonic_values!(
            BNZ, BGE, BG, BLE, BL, LBRA, RET, HALT, SLEEP, IMBLK, IMTAG, IMINV, IRET, TRAP, IMLD,
            DMLD, DMST, IMWAIT, DMWAIT, DMFENCE, IORDS
        ),
        mnemonic_values!(IORD, IOWRS, IOWR),
    ))(input)
}

// ( `.` ( `b` | `h` | `w` | `B` | `H` | `W` ) )?
fn operand_size(input: LineSpan<'_>) -> IResult<LineSpan<'_>, OperandSize> {
    map(
        opt(preceded(char('.'), one_of("bhwBHW"))),
        |out| match out {
            Some('b') | Some('B') => OperandSize::EightBit,
            Some('h') | Some('H') => OperandSize::SixteenBit,
            Some('w') | Some('W') => OperandSize::ThirtyTwoBit,
            None => OperandSize::Unsized,
            _ => unreachable!(),
        },
    )(input)
}

// *_memory_access*
fn memory_access(input: LineSpan<'_>) -> IResult<LineSpan<'_>, Token<'_>> {
    map(_memory_access, Token::Memory)(input)
}

// *memory_space* `[` *whitespace** ( *reg_imm* | *reg_reg* | *reg* ) *whitespace** `]`
fn _memory_access(input: LineSpan<'_>) -> IResult<LineSpan<'_>, MemoryAccess> {
    // Parse the memory space to access.
    let (input, space) = ws0(memory_space)(input)?;

    // Prepare parsers for different forms of memory accesses.
    let reg = map(_register, move |base| MemoryAccess::Reg { space, base });
    let reg_reg = map(
        tuple((
            _register,
            ws0(tag("+")),
            _register,
            opt(preceded(ws0(tag("*")), _unsigned_integer)),
        )),
        move |(base, _, offset, scale)| MemoryAccess::RegReg {
            space,
            base,
            offset,
            scale: scale.unwrap_or(1),
        },
    );
    let reg_imm = map(
        tuple((_register, ws0(tag("+")), _unsigned_integer)),
        move |(base, _, offset)| MemoryAccess::RegImm {
            space,
            base,
            offset,
        },
    );

    // Put it all together.
    delimited(tag("["), ws0(alt((reg_imm, reg_reg, reg))), tag("]"))(input)
}

// `i` | `d` | `I` | `D`
fn memory_space(input: LineSpan<'_>) -> IResult<LineSpan<'_>, MemorySpace> {
    alt((
        value(MemorySpace::IMem, tag_no_case("i")),
        value(MemorySpace::DMem, tag_no_case("d")),
    ))(input)
}

// *_register*
fn register(input: LineSpan<'_>) -> IResult<LineSpan<'_>, Token<'_>> {
    map(_register, Token::Register)(input)
}

// `$` ( *general_purpose_register* | *special_purpose_register* )
fn _register(input: LineSpan<'_>) -> IResult<LineSpan<'_>, Register> {
    preceded(
        tag("$"),
        alt((general_purpose_register, special_purpose_register)),
    )(input)
}

// *general_purpose_register_tag* *digit1*
fn general_purpose_register(input: LineSpan<'_>) -> IResult<LineSpan<'_>, Register> {
    map_opt(preceded(general_purpose_register_tag, digit1), |s| {
        s.fragment()
            .parse::<usize>()
            .map(|i| Register(RegisterKind::Gpr, i))
            .ok()
    })(input)
}

// `reg` | `r`
fn general_purpose_register_tag(input: LineSpan<'_>) -> IResult<LineSpan<'_>, LineSpan<'_>> {
    alt((recognize(tag_no_case("reg")), recognize(tag_no_case("r"))))(input)
}

// `iv0` | `iv1` | `iv2` | `ev` | ...
fn special_purpose_register(input: LineSpan<'_>) -> IResult<LineSpan<'_>, Register> {
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

fn flag(input: LineSpan<'_>) -> IResult<LineSpan<'_>, Token<'_>> {
    map(_flag, Token::Flag)(input)
}

// `$` ( `p0` | `p1` | `p2` | `p3` | ... )
fn _flag(input: LineSpan<'_>) -> IResult<LineSpan<'_>, u8> {
    preceded(
        tag("$"),
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

// *_string*
fn string(input: LineSpan<'_>) -> IResult<LineSpan<'_>, Token<'_>> {
    map(_string, Token::String)(input)
}

// `"` *take_until* `"`
fn _string(input: LineSpan<'_>) -> IResult<LineSpan<'_>, &str> {
    let (span, (_, lit, _)) = tuple((tag("\""), take_until("\""), tag("\"")))(input)?;
    Ok((span, &lit))
}

// *_signed_integer*
fn signed_integer(input: LineSpan<'_>) -> IResult<LineSpan<'_>, Token<'_>> {
    map(_signed_integer, Token::SignedInt)(input)
}

// *_unsigned_integer*
fn unsigned_integer(input: LineSpan<'_>) -> IResult<LineSpan<'_>, Token<'_>> {
    map(_unsigned_integer, Token::UnsignedInt)(input)
}

// *_bitfield*
fn bitfield(input: LineSpan<'_>) -> IResult<LineSpan<'_>, Token<'_>> {
    map(_bitfield, Token::Bitfield)(input)
}

// *_unsigned_integer* `:` *_unsigned_integer*
fn _bitfield<T>(input: LineSpan<'_>) -> IResult<LineSpan<'_>, (T, T)>
where
    T: PrimInt + Unsigned,
{
    let (span, (start, _, end)) = tuple((_unsigned_integer, char(':'), _unsigned_integer))(input)?;
    Ok((span, (start, end)))
}

// *signed_hex* | *signed_binary* | *signed_octal* | *signed_decimal*
fn _signed_integer<T>(input: LineSpan<'_>) -> IResult<LineSpan<'_>, T>
where
    T: PrimInt + Signed,
{
    alt((signed_hex, signed_binary, signed_octal, signed_decimal))(input)
}

// *unsigned_hex* | *unsigned_binary* | *unsigned_octal* | *unsigned_hex*
fn _unsigned_integer<T>(input: LineSpan<'_>) -> IResult<LineSpan<'_>, T>
where
    T: PrimInt + Unsigned,
{
    alt((
        unsigned_hex,
        unsigned_binary,
        unsigned_octal,
        unsigned_decimal,
    ))(input)
}

// *sign* *decimal_digits*
fn signed_decimal<T>(input: LineSpan<'_>) -> IResult<LineSpan<'_>, T>
where
    T: PrimInt + Signed,
{
    map_res(pair(sign, decimal_digits), |(sign, span)| {
        parse_number(span.fragment(), 10).and_then(|n: T| if sign { Ok(-n) } else { Ok(n) })
    })(input)
}

// `+`? *decimal_digits*
fn unsigned_decimal<T>(input: LineSpan<'_>) -> IResult<LineSpan<'_>, T>
where
    T: PrimInt + Unsigned,
{
    map_res(pair(opt(tag("+")), decimal_digits), |(_, span)| {
        parse_number(span.fragment(), 10)
    })(input)
}

// ( `0` | `1` | `2` | `3` | `4` | `5` | `6` | `7` | `8` | `9` | `_` )+
fn decimal_digits(input: LineSpan<'_>) -> IResult<LineSpan<'_>, LineSpan<'_>> {
    recognize(many1(terminated(one_of("0123456789"), many0(char('_')))))(input)
}

// *sign* *hex_prefix* *hex_digits*
fn signed_hex<T>(input: LineSpan<'_>) -> IResult<LineSpan<'_>, T>
where
    T: PrimInt + Signed,
{
    map_res(
        pair(sign, preceded(hex_prefix, hex_digits)),
        |(sign, span)| {
            parse_number(span.fragment(), 16).and_then(|n: T| if sign { Ok(-n) } else { Ok(n) })
        },
    )(input)
}

// `+`? *hex_prefix* *hex_digits*
fn unsigned_hex<T>(input: LineSpan<'_>) -> IResult<LineSpan<'_>, T>
where
    T: PrimInt + Unsigned,
{
    map_res(
        pair(opt(tag("+")), preceded(hex_prefix, hex_digits)),
        |(_, span)| parse_number(span.fragment(), 16),
    )(input)
}

// `0x`
fn hex_prefix(input: LineSpan<'_>) -> IResult<LineSpan<'_>, LineSpan<'_>> {
    complete(tag_no_case("0x"))(input)
}

// ( `0` | `1` | `2` | `3` | `4` | `5` | `6` | `7` | `8` | `9` | `a` | ... )+
fn hex_digits(input: LineSpan<'_>) -> IResult<LineSpan<'_>, LineSpan<'_>> {
    recognize(many1(terminated(
        one_of("0123456789abcdefABCDEF"),
        many0(char('_')),
    )))(input)
}

// *sign* *octal_prefix* *octal_digits*
fn signed_octal<T>(input: LineSpan<'_>) -> IResult<LineSpan<'_>, T>
where
    T: PrimInt + Signed,
{
    map_res(
        pair(sign, preceded(octal_prefix, octal_digits)),
        |(sign, span)| {
            parse_number(span.fragment(), 8).and_then(|n: T| if sign { Ok(-n) } else { Ok(n) })
        },
    )(input)
}

// `+`? *octal_prefix* *octal_digits*
fn unsigned_octal<T>(input: LineSpan<'_>) -> IResult<LineSpan<'_>, T>
where
    T: PrimInt + Unsigned,
{
    map_res(
        pair(opt(tag("+")), preceded(octal_prefix, octal_digits)),
        |(_, span)| parse_number(span.fragment(), 8),
    )(input)
}

// `0o`
fn octal_prefix(input: LineSpan<'_>) -> IResult<LineSpan<'_>, LineSpan<'_>> {
    complete(tag_no_case("0o"))(input)
}

// ( `0` | `1` | `2` | `3` | `4` | `5` | `6` | `7` | `_` )+
fn octal_digits(input: LineSpan<'_>) -> IResult<LineSpan<'_>, LineSpan<'_>> {
    recognize(many1(terminated(one_of("01234567"), many0(char('_')))))(input)
}

// *sign* *binary_prefix* *binary_digits*
fn signed_binary<T>(input: LineSpan<'_>) -> IResult<LineSpan<'_>, T>
where
    T: PrimInt + Signed,
{
    map_res(
        pair(sign, preceded(binary_prefix, binary_digits)),
        |(sign, span)| {
            parse_number(span.fragment(), 2).and_then(|n: T| if sign { Ok(-n) } else { Ok(n) })
        },
    )(input)
}

// `+`? *binary_prefix* *binary_digits*
fn unsigned_binary<T>(input: LineSpan<'_>) -> IResult<LineSpan<'_>, T>
where
    T: PrimInt + Unsigned,
{
    map_res(
        pair(opt(tag("+")), preceded(binary_prefix, binary_digits)),
        |(_, span)| parse_number(span.fragment(), 2),
    )(input)
}

// `0b`
fn binary_prefix(input: LineSpan<'_>) -> IResult<LineSpan<'_>, LineSpan<'_>> {
    complete(tag_no_case("0b"))(input)
}

// ( `0` | `1` | `_` )+
fn binary_digits(input: LineSpan<'_>) -> IResult<LineSpan<'_>, LineSpan<'_>> {
    recognize(many1(terminated(one_of("01"), many0(char('_')))))(input)
}

// ( `+` | `-` )?
fn sign(input: LineSpan<'_>) -> IResult<LineSpan<'_>, bool> {
    map(
        opt(alt((value(false, tag("+")), value(true, tag("-"))))),
        |i| i.unwrap_or(false),
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_case {
        ($parser:expr, $input:expr, $expected:expr) => {
            let temp = std::ffi::OsString::new();
            assert_eq!(start(&temp, $parser)($input).unwrap().1, $expected);
        };
    }

    // For the sake of efficiency, we don't test every parser individually but rather
    // the most important ones which cover up all the smaller parsers as well. That
    // approach gives us the best picture of how well everything plays together.

    #[test]
    fn test_sign_parser() {
        test_case!(sign, "-", true);
        test_case!(sign, "---", true);
        test_case!(sign, "+", false);
        test_case!(sign, "+-", false);
        test_case!(sign, "", false);
    }

    #[test]
    fn test_binary_parsers() {
        test_case!(
            unsigned_binary::<u32>,
            "0b1111_1111_1111_1111_1111_1111_1111_1111",
            0b11111111111111111111111111111111
        );
        test_case!(unsigned_binary::<u8>, "+0b1_1_1_1", 0b1111);
        test_case!(unsigned_binary::<u32>, "0b11011_1101_11", 0b11011110111);

        test_case!(signed_binary::<i32>, "-0b11011_1101_11", -0b11011110111);
        test_case!(signed_binary::<i32>, "0b11011_1101_11", 0b11011110111);
        test_case!(signed_binary::<i8>, "-0b0100_0000", -0b1000000);
    }

    #[test]
    fn test_octal_parsers() {
        test_case!(unsigned_octal::<u32>, "0o12733324", 0o12733324);
        test_case!(unsigned_octal::<u8>, "0O111", 0o111);
        test_case!(unsigned_octal::<u32>, "+0o347234", 0o347234);

        test_case!(signed_octal::<i32>, "-0o34234", -0o34234);
        test_case!(signed_octal::<i8>, "+0O12", 0o12);
        test_case!(signed_octal::<i32>, "0O34234345", 0o34234345);
    }

    #[test]
    fn test_hex_parsers() {
        test_case!(unsigned_hex::<u32>, "0xFfFf_FFfF", 0xFFFF_FFFF);
        test_case!(unsigned_hex::<u8>, "+0x1", 0x1);
        test_case!(unsigned_hex::<u32>, "0x1_F1_F_FfF_F", 0x1F1F_FFFF);

        test_case!(signed_hex::<i32>, "-0x7FfF_FF_fF", -0x7FFF_FFFF);
        test_case!(signed_hex::<i32>, "+0x7FfF_FF_fF", 0x7FFF_FFFF);
        test_case!(signed_hex::<i8>, "0X28", 0x28);
    }

    #[test]
    fn test_decimal_parsers() {
        test_case!(unsigned_decimal::<u16>, "+1000", 1000);
        test_case!(unsigned_decimal::<u8>, "255", 255);
        test_case!(unsigned_decimal::<u32>, "4158930443", 4158930443);

        test_case!(signed_decimal::<i32>, "-487593589", -487593589);
        test_case!(signed_decimal::<i8>, "+43", 43);
        test_case!(signed_decimal::<i16>, "31873", 31873);
    }

    #[test]
    fn test_bitfield_parser() {
        test_case!(_bitfield::<u8>, "5:9", (5, 9));
        test_case!(_bitfield::<u8>, "12:20", (12, 20));
        test_case!(_bitfield::<u8>, "+8:+16", (8, 16));
        test_case!(_bitfield::<u8>, "34:+58", (34, 58));
    }

    #[test]
    fn test_string_parser() {
        test_case!(_string, "\"Test String\"", "Test String");
        test_case!(
            _string,
            "\"fDKajls\nsAF234d\nadFsj\n\"",
            "fDKajls\nsAF234d\nadFsj\n"
        );
        test_case!(_string, "\"\"", "");
    }

    #[test]
    fn test_flag_parser() {
        test_case!(_flag, "$ie0", 0x10);
        test_case!(_flag, "$o", 0x9);
        test_case!(_flag, "$p0", 0);
        test_case!(_flag, "$ea", 0x18);
    }

    #[test]
    fn test_register_parser() {
        test_case!(_register, "$reg5", Register(RegisterKind::Gpr, 5));
        test_case!(_register, "$ReG12", Register(RegisterKind::Gpr, 12));
        test_case!(_register, "$R8", Register(RegisterKind::Gpr, 8));

        test_case!(_register, "$sp", Register(RegisterKind::Spr, 4));
        test_case!(_register, "$pc", Register(RegisterKind::Spr, 5));
        test_case!(_register, "$iv2", Register(RegisterKind::Spr, 2));
    }

    #[test]
    fn test_memory_access_parser() {
        test_case!(
            _memory_access,
            "d[$R5 + $REG3 ]",
            MemoryAccess::RegReg {
                space: MemorySpace::DMem,
                base: Register(RegisterKind::Gpr, 5),
                offset: Register(RegisterKind::Gpr, 3),
                scale: 1
            }
        );
        test_case!(
            _memory_access,
            "D [$R0 + $REG12   * 5 ]",
            MemoryAccess::RegReg {
                space: MemorySpace::DMem,
                base: Register(RegisterKind::Gpr, 0),
                offset: Register(RegisterKind::Gpr, 12),
                scale: 5
            }
        );
        test_case!(
            _memory_access,
            "I  [ $REG15 ]",
            MemoryAccess::Reg {
                space: MemorySpace::IMem,
                base: Register(RegisterKind::Gpr, 15)
            }
        );
        test_case!(
            _memory_access,
            "D[   $R7 +    0x52 ]",
            MemoryAccess::RegImm {
                space: MemorySpace::DMem,
                base: Register(RegisterKind::Gpr, 7),
                offset: 0x52
            }
        );
    }

    #[test]
    fn test_mnemonic_parser() {
        test_case!(
            _mnemonic,
            "hswap.W",
            (InstructionKind::HSWAP, OperandSize::ThirtyTwoBit)
        );
        test_case!(
            _mnemonic,
            "mOV",
            (InstructionKind::MOV, OperandSize::Unsized)
        );
        test_case!(
            _mnemonic,
            "ADDSP",
            (InstructionKind::ADDSP, OperandSize::Unsized)
        );
        test_case!(
            _mnemonic,
            "aDD.b",
            (InstructionKind::ADD, OperandSize::EightBit)
        );
        test_case!(
            _mnemonic,
            "HALT",
            (InstructionKind::HALT, OperandSize::Unsized)
        );
    }

    #[test]
    fn test_identifier_parser() {
        test_case!(identifier, "_abc_def", "_abc_def");
        test_case!(identifier, "test123_abc", "test123_abc");
        test_case!(identifier, "this_is_an_ident", "this_is_an_ident");
        test_case!(identifier, "fja3", "fja3");
    }
}
