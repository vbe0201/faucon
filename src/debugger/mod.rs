//! Implementation of a CLI debugger for driving the emulator.

use std::io::{stdin, stdout, Write};

use faucon_asm::{get_spr_name, read_instruction, FalconError, RegisterKind};
use faucon_emu::cpu::Cpu;
use rustyline::{error::ReadlineError, Cmd, Config, Editor, KeyPress};

use commands::Command;

mod commands;
mod helper;

const VERSION: &str = env!("CARGO_PKG_VERSION");
const PROMPT: &str = "faucon> ";

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
    /// The `Editor` that is used for the `rustyline` integration.
    editor: Editor<helper::Helper>,
}

impl Debugger {
    /// Constructs a new debugger that takes ownership of the [`Cpu`] used for
    /// emulation.
    ///
    /// [`Cpu`]: ../cpu/struct.Cpu.html
    pub fn new(falcon: Cpu) -> Self {
        let config = Config::builder()
            .history_ignore_space(false)
            .completion_type(rustyline::CompletionType::List)
            .edit_mode(rustyline::EditMode::Emacs)
            .max_history_size(1000)
            .build();
        let mut editor = Editor::with_config(config);
        editor.set_helper(Some(helper::Helper::default()));

        editor.bind_sequence(KeyPress::Up, Cmd::LineUpOrPreviousHistory(1));
        editor.bind_sequence(KeyPress::Down, Cmd::LineDownOrNextHistory(1));
        editor.bind_sequence(KeyPress::Tab, Cmd::Complete);

        Debugger {
            falcon,
            last_command: None,
            editor,
        }
    }

    /// Runs the debugger.
    ///
    /// The debugger reads and processes input in an infinite loop,
    /// executing a given set of helpful commands for examining the
    /// emulated binary.
    pub fn run(&mut self) -> rustyline::Result<()> {
        // TODO: store and load history on disk
        println!("Faucon {}", VERSION);
        println!("Type 'help' for more information");

        loop {
            let line = self.editor.readline(PROMPT);
            match line {
                Ok(line) => self.execute_line(line),
                // Ctrl + c will abort the current line.
                Err(ReadlineError::Interrupted) => continue,
                // Ctrl + d will exit the repl.
                Err(ReadlineError::Eof) => return Ok(()),
                Err(err) => return Err(err),
            }
        }
    }

    fn execute_line(&mut self, input: String) {
        // Parse and execute the command.
        let command = match (input.parse(), self.last_command) {
            (Ok(Command::Repeat), Some(command)) => Ok(command),
            (Ok(Command::Repeat), None) => Err("No last command available".into()),
            (Ok(command), _) => Ok(command),
            (Err(e), _) => Err(e),
        };

        match command {
            Ok(Command::Help) => self.show_help(),
            Ok(Command::Exit) => std::process::exit(0),
            Ok(Command::Repeat) => unreachable!(),
            Ok(Command::Step(count)) => self.step(count),
            Ok(Command::Disassemble(address, amount)) => self.disassemble(address, amount),
            Ok(Command::RegDump(kind)) => self.regdump(kind),
            Err(ref e) => error!("Failed to parse command:", "{}", e),
        }

        // Store the command so the repeat command can find it.
        self.last_command = command.ok();
    }

    /// Shows help details for the debugger.
    fn show_help(&self) {
        info!("faucon debugger", "\n---------------");
        ok!("(h)elp", "- Shows this message");
        ok!("(e)xit/(q)uit", "- Exits the debugger");
        ok!("(r)epeat", "- Repeats the last command");
        ok!("(s)tep [count]", "- Steps through [count|1] instructions");
        ok!(
            "(dis)asm [addr] [10]",
            "- Disassembles the next [amount|10] instructions starting from virtual address [addr]"
        );
        ok!(
            "regdump [gpr|spr]",
            "- Dumps all values from a given kind of registers"
        );
    }

    fn step(&mut self, count: u32) {
        for _ in 0..count {
            // TODO: Print stepped instruction?
            self.falcon.step();
        }
    }

    fn disassemble(&mut self, vaddress: u32, amount: u32) {
        let address = self.falcon.memory.tlb.translate_addr(vaddress).unwrap() as usize;
        let mut offset = 0;
        let code = &mut &self.falcon.memory.code[address..];

        for _ in 0..amount {
            match unsafe { read_instruction(code, &mut offset) } {
                Ok(insn) => println!("{:08X}  {}", address as u32 + offset, insn),
                Err(FalconError::Eof) => break,
                Err(e) => {
                    match e {
                        FalconError::InvalidOpcode(_) => {
                            error!("Aborting due to error:", "An unknown instruction was hit")
                        }
                        FalconError::IoError(_) | FalconError::ParseError(_) => unreachable!(),
                        FalconError::Eof => {}
                    }
                    break;
                }
            };
        }
    }

    fn regdump(&self, kind: RegisterKind) {
        let registers = self.falcon.registers.debug_get(&kind);

        for (i, regs) in registers.chunks(4).enumerate() {
            for (mut j, reg) in regs.iter().enumerate() {
                // Fix register indexing to produce correct name.
                j += i << 2;

                // Pretty-print the register.
                let name = if kind == RegisterKind::Gpr {
                    format!("r{}", j)
                } else {
                    get_spr_name(j).to_string()
                };
                print!(
                    "{:>5}: {:<10}",
                    format!("${}", name),
                    format!("{:#08X}", reg)
                );
            }
            println!();
        }
    }
}

fn read_input() -> String {
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();

    input.trim().into()
}
