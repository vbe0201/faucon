use std::borrow::Cow;
use std::str::FromStr;

use nom::character::complete::{digit1, space1};

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
        complete!(command_help)
        | complete!(command_exit)
        | complete!(command_repeat)
        | complete!(command_step)
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
        alt!(complete!(tag_no_case!(("step"))) | complete!(tag_no_case!("s")))
            >> count:
                opt!(preceded!(
                    space1,
                    map_res!(digit1, |s: &str| s.parse::<u32>())
                ))
            >> eof!()
            >> (Command::Step(count.unwrap_or(1)))
    )
);
