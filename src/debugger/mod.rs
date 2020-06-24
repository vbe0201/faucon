//! Implementation of a CLI debugger for driving the emulator.

use crate::cpu::Cpu;

mod commands;

pub struct Debugger {
    falcon: Cpu,
}
