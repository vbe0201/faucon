//! Implementation of a CLI debugger for driving the emulator.

use std::io::{stdin, stdout, Write};

use faucon_asm::read_instruction;
use faucon_emu::cpu::Cpu;

use commands::Command;

mod commands;

/// The debugger used by the faucon emulator.
///
/// The debugger is a bridge between the user and the actual emulator.
/// By reading and parsing commands from user input in a command-line
/// interface, the debugger drives the behavior of the emulator and
/// allows for state examination to gain information about the inner
/// workings of a binary.
pub struct Debugger {
    /// The underlying Falcon processor.
    falcon: Cpu,
    /// The last command that was processed.
    last_command: Option<Command>,
}

impl Debugger {
    /// Constructs a new debugger that takes ownership of the [`Cpu`] used for
    /// emulation.
    ///
    /// [`Cpu`]: ../cpu/struct.Cpu.html
    pub fn new(falcon: Cpu) -> Self {
        Debugger {
            falcon,
            last_command: None,
        }
    }

    /// Runs the debugger.
    ///
    /// The debugger reads and processes input in an infinite loop,
    /// executing a given set of helpful commands for examining the
    /// emulated binary.
    pub fn run(&mut self) {
        loop {
            // Print the debugger cursor.
            print!("faucon> ");
            stdout().flush().unwrap();

            // Read input and continue if no command was supplied.
            let input = read_input();
            if input.is_empty() {
                continue;
            }

            // Parse and execute the command.
            let command = match (input.parse(), self.last_command) {
                (Ok(Command::Repeat), Some(command)) => Ok(command),
                (Ok(Command::Repeat), None) => Err("No last command available".into()),
                (Ok(command), _) => Ok(command),
                (Err(e), _) => Err(e),
            };

            match command {
                Ok(Command::Help) => self.show_help(),
                Ok(Command::Exit) => break,
                Ok(Command::Repeat) => unreachable!(),
                Ok(Command::Step(count)) => self.step(count),
                Ok(Command::Disassemble(address)) => self.disassemble(address as usize, 10),
                Err(ref e) => error!("Failed to parse command:", "{:?}", e),
            }

            // Store the command so the repeat command can find it.
            self.last_command = command.ok();
        }
    }

    /// Shows help details for the debugger.
    fn show_help(&self) {
        info!("faucon debugger", "\n---------------");
        ok!("(h)elp", "- Shows this message");
        ok!("(e)xit/(q)uit", "- Exits the debugger");
        ok!("(r)epeat", "- Repeats the last command");
        ok!(
            "(s)tep [count]",
            "- Steps through [count] instructions. [count] defaults to 1"
        );
        ok!(
            "(dis)asm [addr]",
            "- Disassembles the next few instructions starting from virtual address [addr]."
        );
    }

    fn step(&mut self, count: u32) {
        for _ in 0..count {
            // TODO: Print stepped instruction?
            self.falcon.step();
        }
    }

    fn disassemble(&mut self, address: usize, amount: u32) {
        let mut count = 0;
        while let Ok(insn) = read_instruction(&mut &self.falcon.memory.code[address..]) {
            if count >= amount {
                break;
            }

            println!("{}", insn);
            count += 1;
        }
    }
}

fn read_input() -> String {
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();

    input.trim().into()
}
