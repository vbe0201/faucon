#![allow(unused)]

#[macro_use]
extern crate clap;
#[macro_use]
extern crate nom;

#[macro_use]
mod macros;
mod code;
mod config;
mod debugger;

use std::env;
use std::fs::File;
use std::io::{self, BufRead, BufReader};
use std::path::Path;

use clap::App;
use color_eyre::eyre::{eyre, WrapErr};
use color_eyre::{Result, Section};

use config::Config;
use debugger::Debugger;
use faucon_asm::Disassembler;
use faucon_emu::cpu::Cpu;

const CONFIG_ENV: &str = "FAUCON_CONFIG";

fn read_config<P: AsRef<Path>>(config: Option<P>) -> Result<Config> {
    // Check for the config CLI argument.
    if let Some(path) = config {
        return Ok(Config::load(&path)?);
    }

    // Check for the FAUCON_CONFIG environment variable.
    if let Ok(path) = env::var(CONFIG_ENV) {
        return Ok(Config::load(&path)?);
    }

    Err(eyre!("no config provided")).with_suggestion(|| {
        format!(
            "provide a config via the -c flag or the {} environment variable",
            CONFIG_ENV
        )
    })
}

fn assemble<P: AsRef<Path>>(source: P, matches: &clap::ArgMatches<'_>) -> Result<()> {
    // TODO
    Ok(())
}

fn disassemble<P: AsRef<Path>>(bin: P, matches: &clap::ArgMatches<'_>) -> Result<()> {
    let file = File::open(bin)?;
    let mut reader = BufReader::new(file);

    let base = if let Some(num) = matches.value_of("base") {
        let num = if num.starts_with("0x") {
            usize::from_str_radix(&num[2..], 16)?
        } else {
            num.parse()?
        };
        Some(num as usize)
    } else {
        None
    };

    let mut disassembler = Disassembler::stdout();
    disassembler.disassemble_stream(&mut reader)?;
    Ok(())
}

fn emulate<P: AsRef<Path>>(bin: P, config: Config) -> Result<()> {
    // Prepare the CPU and load the supplied binary into IMEM.
    let mut cpu = Cpu::new();
    if let Err(()) = code::upload_to_imem(&mut cpu, 0, 0, &code::read_falcon_binary(bin)?) {
        return Err(eyre!("the binary file is too large"))
            .wrap_err("failed to upload code")
            .with_suggestion(|| {
                format!(
                    "load a binary that is smaller than {} bytes \
                    or increase the IMEM size in the config",
                    config.falcon.get_imem_size()
                )
            });
    }

    // Create the debugger and run the REPL until the user exits.
    let mut debugger = Debugger::new(cpu);
    debugger.run().wrap_err("error in debugger repl occurred")?;

    Ok(())
}

fn main() -> Result<()> {
    color_eyre::config::HookBuilder::default()
        .panic_section(
            "Consider reporting the bug on GitHub (https://github.com/vbe0201/faucon/issues)",
        )
        .install()?;

    let cli = load_yaml!("cli.yml");
    let matches = App::from_yaml(cli).get_matches();

    let config = read_config(matches.value_of("config")).wrap_err("failed to load config")?;
    match matches.subcommand() {
        ("asm", Some(matches)) => assemble(matches.value_of("INPUT").unwrap(), matches),
        ("dis", Some(matches)) => disassemble(matches.value_of("INPUT").unwrap(), matches),
        ("emu", _) => emulate(matches.value_of("INPUT").unwrap(), config),
        _ => Err(eyre!("please invoke faucon with a subcommand")
            .with_note(|| "See `faucon --help` for a list of supported commands.")),
    }
}
