use nom::character::complete::hex_digit1;

named!(
    hex_u8<&str, u8>,
    preceded!(
        complete!(tag_no_case!("0x")),
        map_res!(hex_digit1, |num: &str| u8::from_str_radix(&num[..], 16))
    )
);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_hex_u8() {
        assert_eq!(hex_u8("0x12"), Ok(("", 0x12)));
        assert_eq!(hex_u8("0X42"), Ok(("", 0x42)));
        assert_eq!(hex_u8("0x53"), Ok(("", 0x53)));
        assert_eq!(hex_u8("0XA5"), Ok(("", 0xA5)));
        assert_eq!(hex_u8("0xFF"), Ok(("", 0xFF)));

        assert_eq!(hex_u8("0x5 aaa"), Ok((" aaa", 0x5)));
        assert_eq!(hex_u8("0xC2 !@b"), Ok((" !@b", 0xC2)));

        assert_eq!(hex_u8("0x0000"), Ok(("", 0x0)));
        assert_eq!(hex_u8("0X0001"), Ok(("", 0x1)));
        assert_eq!(hex_u8("0x0010"), Ok(("", 0x10)));

        assert_ne!(hex_u8("-0x0").is_ok(), true);
        assert_ne!(hex_u8("0x100").is_ok(), true);
    }
}
