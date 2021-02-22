use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::multi::*;
use nom::sequence::*;
use nom::IResult;
use num_traits::{PrimInt, Unsigned};

fn unsigned_hexadecimal<T>(input: &str) -> IResult<&str, T>
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
        |out: &str| T::from_str_radix(&str::replace(&out, "_", ""), 16),
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_hex_u8() {
        assert_eq!(unsigned_hexadecimal::<u8>("0x12"), Ok(("", 0x12)));
        assert_eq!(unsigned_hexadecimal::<u8>("+0X42"), Ok(("", 0x42)));
        assert_eq!(unsigned_hexadecimal::<u8>("+0x53"), Ok(("", 0x53)));
        assert_eq!(unsigned_hexadecimal::<u8>("0XA5"), Ok(("", 0xA5)));
        assert_eq!(unsigned_hexadecimal::<u8>("0xFF"), Ok(("", 0xFF)));

        assert_eq!(unsigned_hexadecimal::<u8>("0x5 aaa"), Ok((" aaa", 0x5)));
        assert_eq!(unsigned_hexadecimal::<u8>("+0xC2 !@b"), Ok((" !@b", 0xC2)));

        assert_eq!(unsigned_hexadecimal::<u8>("0x0000"), Ok(("", 0x0)));
        assert_eq!(unsigned_hexadecimal::<u8>("0X0001"), Ok(("", 0x1)));
        assert_eq!(unsigned_hexadecimal::<u8>("+0x0010"), Ok(("", 0x10)));

        assert_eq!(unsigned_hexadecimal::<u8>("0x100").is_ok(), false);
    }

    #[test]
    fn parse_hex_u16() {
        assert_eq!(unsigned_hexadecimal::<u16>("0x0"), Ok(("", 0x0)));
        assert_eq!(unsigned_hexadecimal::<u16>("+0xDEAD"), Ok(("", 0xDEAD)));
        assert_eq!(unsigned_hexadecimal::<u16>("0XFFFF"), Ok(("", 0xFFFF)));

        assert_eq!(
            unsigned_hexadecimal::<u16>("0x5487 abc"),
            Ok((" abc", 0x5487))
        );
        assert_eq!(
            unsigned_hexadecimal::<u16>("0XF00D j%$"),
            Ok((" j%$", 0xF00D))
        );

        assert_eq!(unsigned_hexadecimal::<u16>("0x0000_FFFF"), Ok(("", 0xFFFF)));
        assert_eq!(unsigned_hexadecimal::<u16>("+0xA_B_C_D"), Ok(("", 0xABCD)));

        assert_eq!(unsigned_hexadecimal::<u16>("0x10000").is_ok(), false);
    }

    #[test]
    fn parse_hex_u32() {
        assert_eq!(unsigned_hexadecimal::<u32>("+0x0"), Ok(("", 0x0)));
        assert_eq!(
            unsigned_hexadecimal::<u32>("0xDEADBEEF"),
            Ok(("", 0xDEADBEEF))
        );
        assert_eq!(
            unsigned_hexadecimal::<u32>("+0XDEAD_BEEF"),
            Ok(("", 0xDEADBEEF))
        );

        assert_eq!(unsigned_hexadecimal::<u32>("0x1_0000_0000").is_ok(), false);
    }
}
