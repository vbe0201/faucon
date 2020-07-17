//! Representations of Falcon instruction operands.

use std::fmt;

use crate::arguments::{Argument, Immediate, Register};

/// Gets the dedicated name of a special-purpose register based on the given register
/// index.
#[rustfmt::skip]
pub fn get_spr_name(value: usize) -> Option<&'static str> {
    [
        /* 0x0 */ Some("iv0"),
        /* 0x1 */ Some("iv1"),
        /* 0x2 */ Some("iv2"),
        /* 0x3 */ Some("tv"),
        /* 0x4 */ Some("sp"),
        /* 0x5 */ Some("pc"),
        /* 0x6 */ Some("xcbase"),
        /* 0x7 */ Some("xdbase"),
        /* 0x8 */ Some("flags"),
        /* 0x9 */ Some("cx"),
        /* 0xA */ Some("cauth"),
        /* 0xB */ Some("xtargets"),
        /* 0xC */ Some("tstatus"),
        /* 0xD */ None,
        /* 0xE */ None,
        /* 0xF */ None,
    ][value]
}

/// Gets the dedicated name of a flag bit in the `$flags` register based on the given
/// bit index.
#[rustfmt::skip]
pub fn get_flag_name(value: usize) -> Option<&'static str> {
    // TODO: Figure out bits 0x1A-0x1F.
    // XXX: 0x1A-0x1C apparently have something to do with interrupts.
    // XXX: 0x1D-0x1F are copies of the above, stored by interrupt delivery and restored by iret.

    [
        /* 0x00 */ Some("p0"),
        /* 0x01 */ Some("p1"),
        /* 0x02 */ Some("p2"),
        /* 0x03 */ Some("p3"),
        /* 0x04 */ Some("p4"),
        /* 0x05 */ Some("p5"),
        /* 0x06 */ Some("p6"),
        /* 0x07 */ Some("p7"),
        /* 0x08 */ Some("c"),
        /* 0x09 */ Some("o"),
        /* 0x0A */ Some("s"),
        /* 0x0B */ Some("z"),
        /* 0x0C */ None,
        /* 0x0D */ None,
        /* 0x0E */ None,
        /* 0x0F */ None,
        /* 0x10 */ Some("ie0"),
        /* 0x11 */ Some("ie1"),
        /* 0x12 */ Some("ie2"),
        /* 0x13 */ None,
        /* 0x14 */ Some("is0"),
        /* 0x15 */ Some("is1"),
        /* 0x16 */ Some("is2"),
        /* 0x17 */ None,
        /* 0x18 */ Some("ta"),
        /* 0x19 */ None,
        /* 0x1A */ None,
        /* 0x1B */ None,
        /* 0x1C */ None,
        /* 0x1D */ None,
        /* 0x1E */ None,
        /* 0x1F */ None,
    ][value]
}

/// The Falcon memory spaces.
///
/// The Falcon utilizes separated memory spaces in SRAM that have special purposes
/// and act completely independent from each other. They have byte-oriented addressing
/// and unaligned access leads to data corruption.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MemorySpace {
    /// The Falcon code space that consists of memory pages tracked by a reverse
    /// page table.
    IMem,
    /// The Falcon data space that acts as a linear piece of memory storing data
    /// and the stack.
    DMem,
}

/// An operand in Falcon assembly that belongs to an [`Instruction`].
///
/// Operands usually denote CPU registers, immediates, and memory addressing for
/// the instruction to operate on. A [`Vec`] of instruction operands can be obtained
/// for every instruction individually through [`Instruction::operands`]. It is
/// at the user's responsibility to correctly interpret and process the operands.
///
/// [`Instruction`]: ../struct.Instruction.html
/// [`Vec`]: https://doc.rust-lang.org/std/vec/struct.Vec.html
/// [`Instruction::operands`]: ../struct.Instruction.html#method.operands
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operand {
    /// A general-purpose CPU register that wraps around the index that is assigned
    /// to the register.
    Gpr(usize),
    /// A special-purpose CPU register that wraps around the index that is assigned
    /// to the register.
    Spr(usize),
    /// A CPU flag that wraps around an 8-bit immediate denoting the index of a bit
    /// in the `$flags` register.
    Flag(u8),
    /// An 8-bit-sized immediate, represented through an [`u8`]. This covers signed
    /// and unsigned forms likewise.
    ///
    /// [`u8`]: https://doc.rust-lang.org/stable/std/primitive.u8.html
    I8(u8),
    /// A 16-bit-sized immediate, represented through an [`u16`]. This covers signed
    /// and unsigned forms likewise.
    ///
    /// [`u16`]: https://doc.rust-lang.org/stable/std/primitive.u16.html
    I16(u16),
    /// A 24-bit-sized immediate, represented through an [`u32`]. This covers signed
    /// and unsigned forms likewise.
    ///
    /// [`u32`]: https://doc.rust-lang.org/stable/std/primitive.u32.html
    I24(u32),
    /// A 32-bit-sized immediate, represented through an [`u32`]. This covers signed
    /// and unsigned forms likewise.
    ///
    /// [`u32`]: https://doc.rust-lang.org/stable/std/primitive.u32.html
    I32(u32),
}

impl Operand {
    /// Reads the value of an [`Argument`] from the instruction bytes and wraps it
    /// into a real [`Operand`].
    ///
    /// [`Argument`]: ../argument/enum.Argument.html
    /// [`Operand`]: enum.Operand.html
    pub(crate) fn read(arg: &Argument, insn: &[u8]) -> Self {
        match arg {
            // Already evaluated by the disassembler, unreachable at this point.
            Argument::SizeConverter(_) => unreachable!(),

            // Immediate forms.
            Argument::U8(imm) => Operand::I8(imm.read(insn)),
            Argument::I8(imm) => Operand::I8(imm.read(insn) as u8),
            Argument::U16(imm) => Operand::I16(imm.read(insn)),
            Argument::I16(imm) => Operand::I16(imm.read(insn) as u16),
            Argument::U24(imm) => Operand::I24(imm.read(insn)),
            Argument::I24(imm) => Operand::I24(imm.read(insn) as u32),
            Argument::U32(imm) => Operand::I32(imm.read(insn)),
            Argument::I32(imm) => Operand::I32(imm.read(insn) as u32),

            // Register forms.
            Argument::Gpr(reg) => Operand::Gpr(reg.read(insn) as usize),
            Argument::Spr(reg) => Operand::Spr(reg.read(insn) as usize),
            Argument::Flag(imm) => Operand::Flag(imm.read(insn)),

            // The Nop placeholder, which should never be interpreted.
            Argument::Nop => panic!("Attempt to parse an illegal Nop argument"),
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Gpr(reg) => write!(f, "$r{}", reg),
            Operand::Spr(reg) => write!(f, "${}", get_spr_name(*reg).unwrap_or("unk")),
            Operand::Flag(flag) => write!(f, "{}", get_flag_name(*flag as usize).unwrap_or("unk")),
            Operand::I8(val) => write!(f, "{:#02x}", val),
            Operand::I16(val) => write!(f, "{:#04x}", val),
            Operand::I24(val) => write!(f, "{:#06x}", val),
            Operand::I32(val) => write!(f, "{:#08x}", val),
        }
    }
}
