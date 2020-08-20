use std::borrow::Cow;
use std::str::FromStr;

use nom::character::complete::{digit1, hex_digit1, space1};

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
}

impl FromStr for Command {
    type Err = Cow<'static, str>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match command(s) {
            Ok((_, c)) => Ok(c),
            e => Err(format!("{:?}", e).into()),
        }
    }
}

named!(
    command<&str, Command>,
    alt!(
        command_help
        | command_exit
        | command_repeat
        | command_step
        | command_disassemble
    )
);

named!(
    command_help<&str, Command>,
    do_parse!(
        alt!(complete!(tag_no_case!("help")) | complete!(tag_no_case!("h")))
            >> eof!()
            >> (Command::Help)
    )
);

named!(
    command_exit<&str, Command>,
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
    command_repeat<&str, Command>,
    do_parse!(
        alt!(complete!(tag_no_case!("repeat")) | complete!(tag_no_case!("r")))
            >> eof!()
            >> (Command::Repeat)
    )
);

named!(
    command_step<&str, Command>,
    do_parse!(
        alt!(complete!(tag_no_case!("step")) | complete!(tag_no_case!("s")))
            >> count: opt!(preceded!(space1, integer))
            >> eof!()
            >> (Command::Step(count.unwrap_or(1)))
    )
);

named!(
    command_disassemble<&str, Command>,
    do_parse!(
        alt!(complete!(tag_no_case!("disasm")) | complete!(tag_no_case!("dis")))
            >> address: preceded!(space1, integer)
            >> count: opt!(preceded!(space1, integer))
            >> eof!()
            >> (Command::Disassemble(address, count.unwrap_or(10)))
    )
);

named!(
    integer<&str, u32>,
    alt!(
        preceded!(
            complete!(tag!("0x")),
            map_res!(hex_digit1, |num: &str| u32::from_str_radix(&num[..], 16))
        )
        | flat_map!(digit1, parse_to!(u32))
    )
);
