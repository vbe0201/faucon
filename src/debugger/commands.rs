use std::borrow::Cow;
use std::str::FromStr;

use faucon_asm::RegisterKind;
use nom::{
    character::complete::{digit1, hex_digit1, space1},
    error::VerboseError,
};

/// Helper macro that will generate a `variants` method on an enum
/// and is used so that we don't have to write out the list ourselfs.
///
/// Currently it doesn't support struct enum variants.
macro_rules! enum_variants {
    ($(#[$($m:meta),+])+ pub enum $e:ident { $($(#[$vm:meta])+ $v:ident$(($($ty:path),+))?),+$(,)? }) => {
        $(#[$($m),+])+
        pub enum $e {
            $(
            $(#[$vm])+
            $v$(($($ty),+))?
            ),+
        }

        paste::paste! {
            impl $e {
                pub const fn variants() -> &'static [&'static str] {
                    &[$(stringify!([<$v:lower>])),+]
                }
            }
        }
    };
}

enum_variants! {

/// Commands that can be executed by the Falcon debugger.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Command {
    /// Prints usage details for the disassembler.
    Help,
    /// Exits the debugger and terminates the application.
    Exit,
    /// Repeats the previously used command.
    Repeat,
    /// Steps through a given amount of CPU instructions.
    Step(u32),
    /// Disassembles the next few instructions starting from the given
    /// address.
    Disassemble(u32, u32),
    /// Dumps all registers that belong to the given [`RegisterKind`]
    /// group.
    ///
    /// [`RegisterKind`]: /faucon-asm/operands/enum.RegisterKind.html
    RegDump(RegisterKind),
}

}

impl FromStr for Command {
    type Err = Cow<'static, str>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match command(s) {
            Ok((_, c)) => Ok(c),
            Err(nom::Err::Failure(err)) | Err(nom::Err::Error(err)) => {
                Err(format!("\n{}", nom::error::convert_error(s, err)).into())
            }
            Err(e) => Err(format!("{}", s).into()),
        }
    }
}

named!(
    command<&str, Command, VerboseError<&str>>,
    alt!(
        command_help
        | command_exit
        | command_repeat
        | command_step
        | command_disassemble
        | command_regdump
    )
);

named!(
    command_help<&str, Command, VerboseError<&str>>,
    do_parse!(
        alt!(complete!(tag_no_case!("help")) | complete!(tag_no_case!("h")))
            >> eof!()
            >> (Command::Help)
    )
);

named!(
    command_exit<&str, Command, VerboseError<&str>>,
    do_parse!(
        alt!(
            complete!(tag_no_case!("exit"))
            | complete!(tag_no_case!("quit"))
            | complete!(tag_no_case!("e"))
            | complete!(tag_no_case!("q"))
        )
            >> eof!()
            >> (Command::Exit)
    )
);

named!(
    command_repeat<&str, Command, VerboseError<&str>>,
    do_parse!(
        alt!(complete!(tag_no_case!("repeat")) | complete!(tag_no_case!("r")))
            >> eof!()
            >> (Command::Repeat)
    )
);

named!(
    command_step<&str, Command, VerboseError<&str>>,
    do_parse!(
        alt!(complete!(tag_no_case!("step")) | complete!(tag_no_case!("s")))
            >> count: opt!(preceded!(space1, integer))
            >> eof!()
            >> (Command::Step(count.unwrap_or(1)))
    )
);

named!(
    command_disassemble<&str, Command, VerboseError<&str>>,
    do_parse!(
        alt!(complete!(tag_no_case!("disasm")) | complete!(tag_no_case!("dis")))
            >> address: preceded!(space1, integer)
            >> count: opt!(preceded!(space1, integer))
            >> eof!()
            >> (Command::Disassemble(address, count.unwrap_or(10)))
    )
);

named!(
    command_regdump<&str, Command, VerboseError<&str>>,
    do_parse!(
        complete!(tag_no_case!("regdump"))
            >> kind: preceded!(
                space1,
                alt!(
                    flat_map!(complete!(tag_no_case!("gpr")), value!(RegisterKind::Gpr))
                    | flat_map!(complete!(tag_no_case!("spr")), value!(RegisterKind::Spr))
                )
            )
            >> eof!()
            >> (Command::RegDump(kind))
    )
);

named!(
    pub integer<&str, u32, VerboseError<&str>>,
    alt!(
        preceded!(
            complete!(tag!("0x")),
            map_res!(hex_digit1, |num: &str| u32::from_str_radix(&num[..], 16))
        )
        | flat_map!(digit1, parse_to!(u32))
    )
);
