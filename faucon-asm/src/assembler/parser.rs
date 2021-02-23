use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;
use num_traits::{Num, PrimInt, Signed, Unsigned};

pub fn parse_eol_comment(input: &str) -> IResult<&str, ()> {
    value((), pair(tag("//"), is_not("\n\r")))(input)
}

pub fn parse_pinline_comment(input: &str) -> IResult<&str, ()> {
    value((), tuple((tag("/*"), take_until("*/"), tag("*/"))))(input)
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
