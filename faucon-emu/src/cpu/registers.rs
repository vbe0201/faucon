use std::ops::{Index, IndexMut};

use faucon_asm::{Register, RegisterKind};

/// A special-purpose register that holds the address for Interrupt Vector 0.
pub const IV0: Register = Register(RegisterKind::Spr, 0);

/// A special-purpose register that holds the address for Interrupt Vector 1.
pub const IV1: Register = Register(RegisterKind::Spr, 1);

/// A special-purpose register that holds the address for Interrupt Vector 2.
pub const IV2: Register = Register(RegisterKind::Spr, 2);

/// A special-purpose register that holds the address for the Trap Vector.
pub const TV: Register = Register(RegisterKind::Spr, 3);

/// A special-purpose register that holds the current stack pointer.
pub const SP: Register = Register(RegisterKind::Spr, 4);

/// A special-purpose register that holds the current program counter.
pub const PC: Register = Register(RegisterKind::Spr, 5);

/// A special-purpose register that holds the external base address for IMEM
/// transfers.
pub const XCBASE: Register = Register(RegisterKind::Spr, 6);

/// A special-purpose register that holds the external base address for DMEM
/// transfers.
pub const XDBASE: Register = Register(RegisterKind::Spr, 7);

/// A special-purpose register that holds various CPU flag bits.
pub const FLAGS: Register = Register(RegisterKind::Spr, 8);

/// A special-purpose register that holds the configuration bits for the SCP DMA
/// functionality.
pub const CX: Register = Register(RegisterKind::Spr, 9);

/// A special-purpose register that holds the configuration bits for the SCP
/// authentication process.
pub const CAUTH: Register = Register(RegisterKind::Spr, 10);

/// A special-purpose register that holds the configuration bits for the CTXDMA
/// ports.
pub const XTARGETS: Register = Register(RegisterKind::Spr, 11);

/// A special-purpose register that holds details on triggered traps.
pub const TSTATUS: Register = Register(RegisterKind::Spr, 12);

/// Representation of all Falcon CPU registers.
pub struct CpuRegisters {
    /// The general-purpose CPU registers of the Falcon.
    gpr: [u32; 0x10],
    /// The special-purpose CPU registers of the Falcon.
    spr: [u32; 0x10],
}

impl CpuRegisters {
    /// Creates a new instance of the registers and initializes all of
    /// them to zero.
    pub fn new() -> Self {
        CpuRegisters {
            gpr: [0; 0x10],
            spr: [0; 0x10],
        }
    }
}

impl Index<Register> for CpuRegisters {
    type Output = u32;

    fn index(&self, reg: Register) -> &Self::Output {
        match reg.0 {
            RegisterKind::Gpr => &self.gpr[reg.1],
            RegisterKind::Spr => &self.spr[reg.1],
        }
    }
}

impl IndexMut<Register> for CpuRegisters {
    fn index_mut(&mut self, reg: Register) -> &mut Self::Output {
        match reg.0 {
            RegisterKind::Gpr => &mut self.gpr[reg.1],
            RegisterKind::Spr => &mut self.spr[reg.1],
        }
    }
}
