//! Implementation of a pretty disassembler
//! that will print a formatted disassembled version of the instructions.

use crate::Instruction;
use std::io::{self, Write};

/// Writes a formatted version of the disassembled instructions to stdout.
pub fn pretty_print(insns: &[Instruction], base: Option<usize>) -> io::Result<()> {
    let stdout = io::stdout();
    let mut handle = stdout.lock();
    pretty_write(&mut handle, insns, base)
}

/// Writes a formatted version of the disassembled instructions to the given output.
///
/// The `base` addresse will be the first address in the disassembled dump.
pub fn pretty_write<W: Write>(
    out: &mut W,
    insns: &[Instruction],
    base: Option<usize>,
) -> io::Result<()> {
    let first = insns.first().map(|insn| (base.unwrap_or(0), insn));
    let mut rest = insns.iter().skip(1);

    let insns = std::iter::successors(first, |(addr, insn)| {
        Some((addr + insn.len(), rest.next()?))
    });

    let out = &mut TrackWrite::new(out);
    for (addr, insn) in insns {
        out.reset();
        write!(out, "{:08x}: ", addr)?;

        insn.raw_bytes()
            .iter()
            .try_for_each(|byte| write!(out, "{:02x} ", byte))?;
        align_to(out, out.count, 32)?;

        writeln!(out, "{}", insn)?;
    }

    Ok(())
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
