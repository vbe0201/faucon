//! Tool suite for NVIDIA Falcon microprocessors.
//!
//! # About
//!
//! The **Fa**st **L**ogic **Con**troller is a series of general-purpose
//! embedded microprocessors that have been produced since around 2005.
//! With over three billion units shipped, Falcon is used in numerous
//! platforms, primarily NVIDIA GPUs starting from G98.
//!
//! Units consist of:
//!
//! - the core processor with separated SRAM for code and data
//! - the I/O space for communication with the host system
//! - FIFO interface logic (optional)
//! - memory interface logic (optional)
//! - cryptographic coprocessor with hardware code signing (optional)
//! - unit-specific extensions (optional)
//!
//! # Instruction Set Architecture
//!
//! The [`crate::isa`] module provides abstractions over Falcon machine
//! instructions and their operands.
//!
//! The [`StreamDisassembler`] iterator is used to translate machine
//! code into corresponding [`Instruction`][crate::isa::Instruction]
//! representations for processing.
//!
//! TODO: Write about assembling when supported.

mod disassemble;
pub use disassemble::*;

pub mod isa;

mod utils;
