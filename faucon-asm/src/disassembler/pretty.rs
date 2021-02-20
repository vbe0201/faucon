//! Implementation of a pretty disassembler
//! that will print a formatted disassembled version of the instructions.

use crate::Instruction;
use std::io::{self, Read, Write};

/// The disassembler is responsible for printing
/// out instructions using a simple but powerful
/// format.
pub struct Disassembler<W> {
    base: usize,
    output: TrackWrite<W>,
}

impl<W> Disassembler<W> {
    /// Creates a new `Disassembler` that will write the disassembled code
    /// to the given output.
    pub fn new(output: W) -> Self {
        Self {
            base: 0,
            output: TrackWrite::new(output),
        }
    }

    /// Sets the starting point of this `Disassembler` to the given base.
    ///
    /// The `Disassembler` will then start the disassembled instructions
    /// at the given base address.
    pub fn with_base(mut self, base: usize) -> Self {
        self.base = base;
        self
    }
}

impl Disassembler<io::Stdout> {
    /// Creates a new `Disassembler` that will print out the disassembled
    /// code to stdout.
    pub fn stdout() -> Self {
        Self::new(io::stdout())
    }
}

impl<W: Write> Disassembler<W> {
    /// Disassembles a stream of bytes.
    ///
    /// This method will not return an error if an unknown or invalid instruction
    /// is encountered, but instead just print `[aborted] (invalid instruction)`
    /// to the output stream.
    ///
    /// It will also ignore any errors that occurred while reading from the `stream`.
    pub fn disassemble_stream<R: Read>(&mut self, stream: &mut R) -> io::Result<()> {
        use crate::Error;

        let mut offset = 0;
        let insns = std::iter::from_fn(|| match super::read_instruction(stream, &mut offset) {
            Ok(insn) => Some(insn),
            Err(Error::IoError) => None,
            Err(Error::UnknownInstruction(op)) => {
                println!("[aborted] (unknown instruction '{:x}')", op);
                None
            }
            Err(Error::Eof) => {
                println!("[aborted] (end of file)");
                None
            }
        });
        self.disassemble(insns)
    }

    /// Disassembles a list of instructions and writes them to the output of this `Disassembler`.
    ///
    /// This method will not reset the inner pc, so if you call it multiple times,
    /// the addresses will still increase.
    pub fn disassemble(&mut self, insns: impl IntoIterator<Item = Instruction>) -> io::Result<()> {
        let out = &mut self.output;
        for insn in insns {
            out.reset();
            write!(out, "{:08x} ", self.base + insn.memory_offset())?;

            insn.raw_bytes()
                .iter()
                .try_for_each(|byte| write!(out, "{:02x} ", byte))?;
            align_to(out, out.count, 32)?;
            writeln!(out, "{}", insn)?;
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
