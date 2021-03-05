// Assembler directives that may be used in Falcon assembly language.
#[derive(Debug, PartialEq, Eq)]
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
