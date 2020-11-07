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
use std::io::{self, BufRead, BufReader, Read, Write};
use std::path::Path;

use clap::{App, ArgMatches};
use color_eyre::{
    eyre::{eyre, WrapErr},
    Result, Section,
};

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

fn run_emulator<P: AsRef<Path>>(bin: P, config: Config) -> Result<()> {
    let mut cpu = Cpu::new(
        config.falcon.version,
        config.falcon.get_imem_size(),
        config.falcon.dmem_size,
        config.falcon.cycle_duration(),
    );

    // Upload the supplied binary into the Falcon code segment.
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

fn disassemble_file<P: AsRef<Path>>(bin: P, matches: &clap::ArgMatches<'_>) -> Result<()> {
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

fn pad_file<P>(bin: P, output: Option<P>) -> Result<()>
where
    P: AsRef<Path> + Copy,
{
    // If no output file was supplied, overwrite the binary provided by the user.
    let output = output.unwrap_or(bin);

    // Read the contents of the supplied binary.
    let mut file = File::open(bin)?;
    let mut contents = Vec::new();
    file.read_to_end(&mut contents)?;
    drop(file);

    // Pad it to a 0x100 byte alignment.
    code::pad_binary(&mut contents);

    // Write it back to the output file.
    let mut file = File::create(output)?;
    file.write_all(&contents)?;

    Ok(())
}

fn get_binary_file<'matches>(
    matches: &'matches clap::ArgMatches<'matches>,
) -> Result<&'matches str> {
    if let Some(bin) = matches.value_of("binary") {
        Ok(bin)
    } else {
        return Err(eyre!("no binary file to run provided"))
            .suggestion("provide a binary file using the -b argument");
    }
}

fn parse_subcommands(matches: ArgMatches, config: Config) -> Result<()> {
    if let Some(matches) = matches.subcommand_matches("emu") {
        run_emulator(get_binary_file(matches)?, config)
    } else if let Some(matches) = matches.subcommand_matches("dis") {
        disassemble_file(get_binary_file(matches)?, matches)
    } else if let Some(matches) = matches.subcommand_matches("pad") {
        pad_file(get_binary_file(matches)?, matches.value_of("output"))
    } else {
        Ok(())
    }
}

fn main() -> Result<()> {
    // Hook the panic handler to point users to the issue tracker for diagnosis.
    color_eyre::config::HookBuilder::default()
        .panic_section(concat!(
            "Consider reporting the bug on GitHub: ",
            env!("CARGO_PKG_REPOSITORY")
        ))
        .install()?;

    // Build the CLI.
    let cli = load_yaml!("cli.yml");
    let matches = App::from_yaml(cli).get_matches();

    // Read the configuration file.
    let config = read_config(matches.value_of("config")).wrap_err("failed to load config")?;

    // Parse and execute subcommands of the application.
    parse_subcommands(matches, config)
}
