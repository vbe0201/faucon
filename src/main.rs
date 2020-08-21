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
use std::path::Path;

use clap::App;

use config::Config;
use debugger::Debugger;
use faucon_emu::cpu::Cpu;

fn read_config<P: AsRef<Path>>(config: Option<P>) -> Option<Config> {
    // Check for the config CLI argument.
    if let Some(path) = config {
        return Some(Config::load(&path));
    }

    // Check for the FAUCON_CONFIG environment variable.
    if let Ok(path) = env::var("FAUCON_CONFIG") {
        return Some(Config::load(&path));
    }

    None
}

fn run_emulator<P: AsRef<Path>>(bin: P, config: Config) {
    // Prepare the CPU and load the supplied binary into IMEM.
    let mut cpu = Cpu::new();
    if let Err(()) = code::upload_to_imem(&mut cpu, 0, 0, &code::read_falcon_binary(bin)) {
        error!("Failed to upload code:", "The binary is too large!");
        return;
    }

    // Create the debugger and run the REPL until the user exits.
    let mut debugger = Debugger::new(cpu);
    debugger.run();
}

fn main() {
    // Build the CLI.
    let cli = load_yaml!("cli.yml");
    let matches = App::from_yaml(cli).get_matches();

    // Read the configuration file.
    let config = read_config(matches.value_of("config"))
        .expect("Please supply a value to the -c option or set the FAUCON_CONFIG env variable");

    if let Some(matches) = matches.subcommand_matches("emu") {
        if let Some(bin) = matches.value_of("binary") {
            run_emulator(bin, config);
        } else {
            panic!("Please provide a binary that should be loaded into the emulator")
        }
    } else {
        panic!("Please use a subcommand to invoke a tool. See `faucon help` for details");
    }
}
