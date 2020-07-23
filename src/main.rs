#![allow(dead_code)]

#[macro_use]
extern crate nom;

use std::env;

use debugger::Debugger;
use faucon_emu::cpu::Cpu;

#[macro_use]
mod macros;
mod code;
mod debugger;

fn main() {
    let binary = code::read_falcon_binary(env::args().nth(1).unwrap());

    let mut cpu = Cpu::new();
    code::upload_to_imem(&mut cpu, 0, 0, &binary);

    let mut debugger = Debugger::new(cpu);
    debugger.run();
}
