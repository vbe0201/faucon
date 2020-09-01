//! Implementation of a pretty disassembler
//! that will print a formatted disassembled version of the instructions.

use crate::Instruction;
use std::io::{self, Write};

/// The disassembler is responsible for printing
/// out instructions using a simple but powerful
/// format.
pub struct Disassembler<W> {
    pc: usize,
    output: TrackWrite<W>,
}

impl<W> Disassembler<W> {
    /// Creates a new `Disassembler` that will write the disassembled code
    /// to the given output.
    pub fn new(output: W) -> Self {
        Self {
            pc: 0,
            output: TrackWrite::new(output),
        }
    }

    /// Sets the starting point of this `Disassembler` to the given base.
    ///
    /// The `Disassembler` will then start the disassembled instructions
    /// at the given base address.
    pub fn with_base(mut self, base: usize) -> Self {
        self.pc = base;
        self
    }
}

impl<W: Write> Disassembler<W> {
    /// Disassembles a list of instructions and writes them to the output of this `Disassembler`.
    ///
    /// This method will not reset the inner pc, so if you call it multiple times,
    /// the addresses will still increase.
    pub fn disassemble(&mut self, insns: impl Iterator<Item = Instruction>) -> io::Result<()> {
        let out = &mut self.output;
        for insn in insns {
            out.reset();
            write!(out, "{:08x} ", self.pc)?;

            insn.raw_bytes()
                .iter()
                .try_for_each(|byte| write!(out, "{:02x} ", byte))?;
            align_to(out, out.count, 32)?;
            writeln!(out, "{}", insn)?;

            self.pc += insn.len();
        }

        Ok(())
    }
}

fn align_to<W: Write>(out: &mut W, current: usize, width: usize) -> io::Result<()> {
    (current..width).try_for_each(|_| write!(out, " "))
}

struct TrackWrite<W> {
    count: usize,
    inner: W,
}

impl<W> TrackWrite<W> {
    fn new(inner: W) -> Self {
        Self { inner, count: 0 }
    }

    fn reset(&mut self) {
        self.count = 0;
    }
}

impl<W: Write> io::Write for TrackWrite<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let count = self.inner.write(buf)?;
        self.count += count;
        Ok(count)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }
}
