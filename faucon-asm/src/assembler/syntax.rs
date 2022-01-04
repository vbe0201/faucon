use num_traits::NumCast;

use super::{error::AssemblerError, lexer::Token, parser, span::Spanned};
use crate::{isa::InstructionKind, opcode::OperandSize};

pub struct Statement<'ctx> {
    label: Option<Spanned<&'ctx str>>,
    expr: Option<Expression<'ctx>>,
}

impl<'ctx> Statement<'ctx> {
    fn from_parser(
        input: &'ctx str,
        stmt: parser::Statement<'ctx>,
    ) -> Result<Self, AssemblerError> {
        let label = stmt.label.map(|label| {
            Token::try_as_str(label)
                .unwrap_or_else(|e| panic!("Parser anomaly: {:?} at label position", e))
        });
        let expr = match stmt.expr {
            Some(expr) => {
                let parser::Expression { expr, data } = expr;
                Some(match Token::try_as_mnemonic(expr) {
                    Ok(mnemonic) => Expression::Instruction(Instruction {
                        mnemonic,
                        operands: data,
                    }),
                    Err(t) => Expression::Directive(parse_directive(input, t, data.into_iter())?),
                })
            }
            None => None,
        };

        Ok(Self { label, expr })
    }
}

pub enum Expression<'ctx> {
    Directive(Directive<'ctx>),
    Instruction(Instruction<'ctx>),
}

// Representation of the different Falcon security modes.
// TODO: Move this elsewhere?
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SecurityMode {
    // The Falcon No Secure mode. This is the only mode unsigned
    // microcode may execute in directly.
    None,
    // The Falcon Light Secure mode. Must be entered from Heavy
    // Secure context and grants debugging possibilities for
    // secure code.
    Light,
    // The Falcon Heavy Secure mode. Microcode in this mode is
    // granted the highest possible set of privileges while, at
    // the same time, all debugging features are disabled.
    Heavy,
}

// Assembler directives supported in the Falcon assembly language.
#[derive(Clone, Debug, PartialEq)]
pub enum Directive<'ctx> {
    // Specifies alignment of the following code to a given byte boundary.
    Align(u32),
    // Inserts a byte literal at the current position in code.
    Byte(u8),
    // Assigns a numeric constant to a symbol with the chosen name.
    Equ(&'ctx str, u32),
    // Inserts a halfword literal at the current position in code.
    //
    // Uses little endian byte ordering.
    HalfWord(u16),
    // Includes another assembler source file as an additional translation
    // unit into a source file.
    Include(&'ctx str),
    // Inserts a word literal at the current position in code.
    //
    // Uses little endian byte ordering.
    Word(u32),
    // Declares a code section with a name and an optional start address.
    Section(SecurityMode, &'ctx str, Option<u32>),
    // Skips bytes to set the program counter to the supplied value
    // relative to the current section.
    Size(u32),
    // Skips the given amount of bytes in code and optionally fills them
    // with a supplied value.
    Skip(u32, Option<u8>),
    // Inserts a string literal at the current position in code.
    Str(&'ctx str),
}

// Falcon assembly instruction which is lowered into machine code.
//
// This only stores the necessary information for instruction selection to
// enumerate possible forms and find the one that matches the operand list.
#[derive(Clone, Debug, PartialEq)]
pub struct Instruction<'ctx> {
    // The instruction mnemonic.
    pub mnemonic: Spanned<(InstructionKind, OperandSize)>,
    // Various kinds of operands validated during instruction selection.
    pub operands: Vec<Spanned<Token<'ctx>>>,
}

pub fn parse_integer<'ctx, I: Iterator<Item = Spanned<Token<'ctx>>>, U: NumCast>(
    input: &'ctx str,
    iter: &mut I,
) -> Result<Option<U>, AssemblerError> {
    match iter.next() {
        Some(t) => Token::try_as_int(t)
            .map(|u| Some(u.into_node()))
            .map_err(|e| AssemblerError::custom(input, e, "Expected integer operand")),
        None => Ok(None),
    }
}

pub fn parse_string<'ctx, I: Iterator<Item = Spanned<Token<'ctx>>>>(
    input: &'ctx str,
    iter: &mut I,
) -> Result<Option<&'ctx str>, AssemblerError> {
    match iter.next() {
        Some(t) => Token::try_as_str(t)
            .map(|s| Some(s.into_node()))
            .map_err(|e| AssemblerError::custom(input, e, "Expected string literal operand")),
        None => Ok(None),
    }
}

fn parse_directive<'ctx, I: Iterator<Item = Spanned<Token<'ctx>>>>(
    input: &'ctx str,
    dir: Spanned<Token<'ctx>>,
    mut iter: I,
) -> Result<Directive<'ctx>, AssemblerError> {
    match Token::try_as_directive(dir) {
        Ok(dir) => match *dir.node() {
            "align" => {
                let align = parse_integer(input, &mut iter)?.ok_or_else(|| {
                    AssemblerError::custom(
                        input,
                        dir,
                        "`align` requires integer operand denoting alignment",
                    )
                })?;

                Ok(Directive::Align(align))
            }
            "byte" => {
                let value = parse_integer(input, &mut iter)?.ok_or_else(|| {
                    AssemblerError::custom(
                        input,
                        dir,
                        "`byte` requires integer operand denoting a value",
                    )
                })?;

                Ok(Directive::Byte(value))
            }
            "equ" => {
                let symbol = parse_string(input, &mut iter)?.ok_or_else(|| {
                    AssemblerError::custom(
                        input,
                        dir.clone(),
                        "`equ` requires string literal denoting symbol",
                    )
                })?;
                let value = parse_integer(input, &mut iter)?.ok_or_else(|| {
                    AssemblerError::custom(
                        input,
                        dir,
                        "`equ` requires integer operand denoting constant value after symbol",
                    )
                })?;

                Ok(Directive::Equ(symbol, value))
            }
            "halfword" => {
                let value = parse_integer(input, &mut iter)?.ok_or_else(|| {
                    AssemblerError::custom(
                        input,
                        dir,
                        "`halfword` requires integer operand denoting a value",
                    )
                })?;

                Ok(Directive::HalfWord(value))
            }
            "include" => {
                let path = parse_string(input, &mut iter)?.ok_or_else(|| {
                    AssemblerError::custom(
                        input,
                        dir.clone(),
                        "`include` requires string literal denoting file path to include",
                    )
                })?;

                Ok(Directive::Include(path))
            }
            "word" => {
                let value = parse_integer(input, &mut iter)?.ok_or_else(|| {
                    AssemblerError::custom(
                        input,
                        dir,
                        "`word` requires integer operand denoting a value",
                    )
                })?;

                Ok(Directive::Word(value))
            }
            "nsection" => {
                let name = parse_string(input, &mut iter)?.ok_or_else(|| {
                    AssemblerError::custom(
                        input,
                        dir.clone(),
                        "`nsection` requires string literal denoting section name",
                    )
                })?;
                let addr = parse_integer(input, &mut iter)?;

                Ok(Directive::Section(SecurityMode::None, name, addr))
            }
            "lsection" => {
                let name = parse_string(input, &mut iter)?.ok_or_else(|| {
                    AssemblerError::custom(
                        input,
                        dir.clone(),
                        "`lsection` requires string literal denoting section name",
                    )
                })?;
                let addr = parse_integer(input, &mut iter)?;

                Ok(Directive::Section(SecurityMode::Light, name, addr))
            }
            "hsection" => {
                let name = parse_string(input, &mut iter)?.ok_or_else(|| {
                    AssemblerError::custom(
                        input,
                        dir.clone(),
                        "`hsection` requires string literal denoting section name",
                    )
                })?;
                let addr = parse_integer(input, &mut iter)?;

                Ok(Directive::Section(SecurityMode::Heavy, name, addr))
            }
            "size" => {
                let size = parse_integer(input, &mut iter)?.ok_or_else(|| {
                    AssemblerError::custom(
                        input,
                        dir,
                        "`size` requires integer operand denoting position to pad to",
                    )
                })?;

                Ok(Directive::Size(size))
            }
            "skip" => {
                let amount = parse_integer(input, &mut iter)?.ok_or_else(|| {
                    AssemblerError::custom(
                        input,
                        dir,
                        "`skip` requires integer operand denoting bytes to skip",
                    )
                })?;
                let fill = parse_integer(input, &mut iter)?;

                Ok(Directive::Skip(amount, fill))
            }
            "str" => {
                let lit = parse_string(input, &mut iter)?.ok_or_else(|| {
                    AssemblerError::custom(
                        input,
                        dir.clone(),
                        "`str` requires string literal to insert",
                    )
                })?;

                Ok(Directive::Str(lit))
            }

            _ => Err(AssemblerError::custom(input, dir, "Unknown directive")),
        },

        Err(e) => Err(AssemblerError::custom(
            input,
            e,
            "Expected assembler directive at this position",
        )),
    }
}
