//! Representations of Falcon instruction operands.

use std::fmt;

use crate::arguments::{Argument, MemoryAccess as ArgMemoryAccess};

/// A Falcon CPU register.
///
/// It is described by a tuple which holds the kind of register and its index
/// which is required for addressing.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Register(pub RegisterKind, pub usize);

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0 == RegisterKind::Gpr {
            write!(f, "$r{}", self.1)
        } else {
            write!(f, "${}", get_spr_name(self.1).unwrap_or("unk"))
        }
    }
}

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

impl fmt::Display for MemorySpace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let fmt = match self {
            MemorySpace::IMem => "I",
            MemorySpace::DMem => "D",
        };

        write!(f, "{}", fmt)
    }
}

/// The types of CPU registers that are utilized by the Falcon processor.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RegisterKind {
    /// A general-purpose CPU register.
    Gpr,
    /// A special-purpose CPU register.
    Spr,
}

/// A direct memory access to an address in a specified space.
///
/// Some instructions directly operate on a given memory chunk, where the location can
/// be described in various variants:
///
/// - Access through a single register holding the address: `[$reg]`
/// - Access through two registers for address and offset with scale: `[$reg1 + $reg2 * scale]`
/// - Access through a register for address and an immediate for offset: `[$reg + imm]`
///
/// It is within the user's responsibility to correctly interpret and process the variants
/// of this enumeration.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MemoryAccess {
    /// A form where the memory address is derived from a single register: `[$reg]`
    Reg {
        /// The memory space which should be accessed.
        space: MemorySpace,
        /// A descriptor of the CPU register that holds the address.
        base: Register,
    },
    /// A form where the memory address is derived from two registers: `[$reg1 + $reg2 * scale]`
    RegReg {
        /// The memory space which should be accessed.
        space: MemorySpace,
        /// A descriptor of the CPU register that holds the base address.
        base: Register,
        /// An offset to the base address that is denoted by the register operand.
        offset: Register,
        /// A constant scale for the offset value.
        scale: u8,
    },
    /// A form where the memory address is derived from a register and an immediate: `[$reg + imm]`
    RegImm {
        /// The memory space which should be accessed.
        space: MemorySpace,
        /// A descriptor of the CPU register that holds the base address.
        base: Register,
        /// An offset of the base address that is denoted by the register operand.
        offset: u32,
    },
}

impl fmt::Display for MemoryAccess {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MemoryAccess::Reg { space, base } => write!(f, "{}[{}]", space, base),
            MemoryAccess::RegReg {
                space,
                base,
                offset,
                scale,
            } => {
                let offset_fmt = match scale {
                    0 => "".to_string(),
                    1 => format!(" + {}", offset),
                    _ => format!(" + {} * {}", offset, scale),
                };

                write!(f, "{}[{}{}]", space, base, offset_fmt)
            }
            MemoryAccess::RegImm {
                space,
                base,
                offset,
            } => {
                let offset_fmt = match offset {
                    0 => "".to_string(),
                    _ => format!(" + {:#02x}", offset),
                };

                write!(f, "{}[{}{}]", space, base, offset_fmt)
            }
        }
    }
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
    /// A CPU register that wraps around the kind of register and the index that is
    /// assigned to it.
    Register(Register),
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
    /// A direct access to a memory space at a given address.
    Memory(MemoryAccess),
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
            Argument::Register(reg) => {
                Operand::Register(Register(reg.kind, reg.read(insn) as usize))
            }
            Argument::Flag(imm) => Operand::Flag(imm.read(insn)),

            // Direct memory access.
            Argument::Memory(mem) => match mem {
                ArgMemoryAccess::Reg(space, reg) => {
                    let reg = reg.as_ref().unwrap();

                    Operand::Memory(MemoryAccess::Reg {
                        space: *space,
                        base: Register(reg.kind, reg.read(insn) as usize),
                    })
                }
                ArgMemoryAccess::RegReg(space, reg1, reg2, scale) => {
                    let reg1 = reg1.as_ref().unwrap();
                    let reg2 = reg2.as_ref().unwrap();

                    Operand::Memory(MemoryAccess::RegReg {
                        space: *space,
                        base: Register(reg1.kind, reg1.read(insn) as usize),
                        offset: Register(reg2.kind, reg2.read(insn) as usize),
                        scale: *scale,
                    })
                }
                ArgMemoryAccess::RegImm(space, reg, imm) => {
                    let reg = reg.as_ref().unwrap();
                    let imm = imm.as_ref().unwrap();

                    Operand::Memory(MemoryAccess::RegImm {
                        space: *space,
                        base: Register(reg.kind, reg.read(insn) as usize),
                        offset: imm.read(insn),
                    })
                }
            },

            // The Nop placeholder, which should never be interpreted.
            Argument::Nop => panic!("Attempt to parse an illegal Nop argument"),
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Register(reg) => write!(f, "{}", reg),
            Operand::Flag(flag) => write!(f, "{}", get_flag_name(*flag as usize).unwrap_or("unk")),
            Operand::I8(val) => write!(f, "{:#x}", val),
            Operand::I16(val) => write!(f, "{:#x}", val),
            Operand::I24(val) => write!(f, "{:#x}", val),
            Operand::I32(val) => write!(f, "{:#x}", val),
            Operand::Memory(mem) => write!(f, "{}", mem),
        }
    }
}
