//! Representations of Falcon instruction operands.

use std::fmt;

use crate::arguments::{Argument, MemoryAccess as ArgMemoryAccess};

/// A Falcon CPU register.
///
/// The Falcon utilizes separate register files for 16 general-purpose registers
/// and another 16 special-purpose registers. This structure stores information
/// on the type of register and the register index itself.
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

/// Types of CPU registers supported by the Falcon.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RegisterKind {
    /// A general-purpose CPU register.
    Gpr,
    /// A special-purpose CPU register.
    Spr,
}

/// Gets the qualified name of a special-purpose register based on the given
/// register index.
#[rustfmt::skip]
pub fn get_spr_name(value: usize) -> Option<&'static str> {
    assert!(value < 0x10);

    [
        /* 0x0 */ Some("iv0"),
        /* 0x1 */ Some("iv1"),
        /* 0x2 */ Some("iv2"),
        /* 0x3 */ Some("ev"),
        /* 0x4 */ Some("sp"),
        /* 0x5 */ Some("pc"),
        /* 0x6 */ Some("imb"),
        /* 0x7 */ Some("dmb"),
        /* 0x8 */ Some("csw"),
        /* 0x9 */ Some("ccr"),
        /* 0xA */ Some("sec"),
        /* 0xB */ Some("ctx"),
        /* 0xC */ Some("exci"),
        /* 0xD */ Some("sec1"),
        /* 0xE */ Some("imb1"),
        /* 0xF */ Some("dmb1"),
    ][value]
}

/// Gets the qualified name of a flag bit in the `$flags` register based on the
/// given bit index.
#[rustfmt::skip]
pub fn get_flag_name(value: usize) -> Option<&'static str> {
    assert!(value < 0x20);

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
        /* 0x18 */ Some("ea"),
        /* 0x19 */ None,
        /* 0x1A */ None,
        /* 0x1B */ None,
        /* 0x1C */ None,
        /* 0x1D */ None,
        /* 0x1E */ None,
        /* 0x1F */ None,
    ][value]
}

/// The used Falcon memory spaces in SRAM.
///
/// Falcon follows the Harvard computer model by utilizing separate memory spaces
/// for code and data. The *IMEM (instruction memory)* utilizes primitive paging
/// and contains the code that is executed. The *DMEM (data memory)* on the other
/// hand stores local variables and the stack. Unaligned access to it leads to
/// data corruption.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MemorySpace {
    /// The IMEM segment.
    IMem,
    /// The DMEM segment.
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

/// A direct memory access to an address in a [`MemorySpace`].
///
/// Some instructions directly access an address in a specific memory space. An
/// address can be described in the following formats:
///
/// - Access through a single register holding the address: `[$reg]`
/// - Access through two registers for base address and scaled offset: `[$reg1 + $reg2 * scale]`
/// - Access through a register for base address and an immediate for offset: `[$reg + imm]`
///
/// [`MemorySpace`]: enum.MemorySpace.html
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MemoryAccess {
    /// A form where the memory address is derived from a single register: `[$reg]`
    Reg {
        /// The memory space which should be accessed.
        space: MemorySpace,
        /// The register which holds the address to access.
        base: Register,
    },
    /// A form where the memory address is derived from two registers: `[$reg1 + $reg2 * scale]`
    RegReg {
        /// The memory space which should be accessed.
        space: MemorySpace,
        /// The register which holds the memory base address.
        base: Register,
        /// The register that holds the offset to the base address.
        offset: Register,
        /// A constant scale for the offset value. Determined by the instruction
        /// form.
        scale: u8,
    },
    /// A form where the memory address is derived from a register and an immediate: `[$reg + imm]`
    RegImm {
        /// The memory space which should be accessed.
        space: MemorySpace,
        /// The register which holds the memory base address.
        base: Register,
        /// An offset of the base address stored in an immediate.
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

/// An operand of a Falcon assembly [`Instruction`].
///
/// Operands usually denote CPU registers, immediates, and memory addressing for
/// the instruction to operate on. A `Vec` of instruction operands can be obtained
/// for every instruction individually through [`Instruction::operands`]. It is
/// at the user's responsibility to correctly interpret and process the operands.
///
/// [`Instruction`]: ../struct.Instruction.html
/// [`Instruction::operands`]: ../struct.Instruction.html#method.operands
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operand {
    /// A Falcon CPU register operand.
    Register(Register),
    /// An immediate operand that represents a specific bit of the `$csw`/`$flags`
    /// CPU register.
    Flag(u8),
    /// An 8-bit-sized signed immediate.
    I8(i8),
    /// An 8-bit-sized unsigned immediate.
    U8(u8),
    /// A 16-bit-sized signed immediate.
    I16(i16),
    /// A 16-bit-sized unsigned immediate.
    U16(u16),
    /// A 24-bit-sized signed immediate.
    I24(i32),
    /// A 24-bit-sized unsigned immediate.
    U24(u32),
    /// A 32-bit-sized signed immediate.
    I32(i32),
    /// A 32-bit-sized unsigned immediate.
    U32(u32),
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
            Argument::U8(imm) => Operand::U8(imm.read(insn)),
            Argument::I8(imm) => Operand::I8(imm.read(insn)),
            Argument::U16(imm) => Operand::U16(imm.read(insn)),
            Argument::I16(imm) => Operand::I16(imm.read(insn)),
            Argument::U24(imm) => Operand::U24(imm.read(insn)),
            Argument::I24(imm) => Operand::I24(imm.read(insn)),
            Argument::U32(imm) => Operand::U32(imm.read(insn)),
            Argument::I32(imm) => Operand::I32(imm.read(insn)),

            // Register forms.
            Argument::Register(reg) => {
                Operand::Register(Register(reg.kind, reg.read(insn) as usize))
            }
            Argument::Flag(imm) => Operand::Flag(imm.read(insn)),

            // Direct memory access.
            Argument::Memory(mem) => match mem {
                ArgMemoryAccess::Reg(space, reg) => Operand::Memory(MemoryAccess::Reg {
                    space: *space,
                    base: Register(reg.kind, reg.read(insn) as usize),
                }),
                ArgMemoryAccess::RegReg(space, reg1, reg2, scale) => {
                    Operand::Memory(MemoryAccess::RegReg {
                        space: *space,
                        base: Register(reg1.kind, reg1.read(insn) as usize),
                        offset: Register(reg2.kind, reg2.read(insn) as usize),
                        scale: *scale,
                    })
                }
                ArgMemoryAccess::RegImm(space, reg, imm) => Operand::Memory(MemoryAccess::RegImm {
                    space: *space,
                    base: Register(reg.kind, reg.read(insn) as usize),
                    offset: imm.read(insn),
                }),
            },
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Register(reg) => write!(f, "{}", reg),
            Operand::Flag(flag) => write!(f, "{}", get_flag_name(*flag as usize).unwrap_or("unk")),
            Operand::I8(val) => write!(f, "-{:#x}", val.abs()),
            Operand::U8(val) => write!(f, "{:#x}", val),
            Operand::I16(val) => write!(f, "-{:#x}", val.abs()),
            Operand::U16(val) => write!(f, "{:#x}", val),
            Operand::I24(val) => write!(f, "-{:#x}", val.abs()),
            Operand::U24(val) => write!(f, "{:#x}", val),
            Operand::I32(val) => write!(f, "-{:#x}", val.abs()),
            Operand::U32(val) => write!(f, "{:#x}", val),
            Operand::Memory(mem) => write!(f, "{}", mem),
        }
    }
}
