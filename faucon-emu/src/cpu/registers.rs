use std::ops::{Index, IndexMut};

use faucon_asm::{Operand, Register, RegisterKind};

/// A special-purpose register that holds the address for Interrupt Vector 0.
pub const IV0: Register = Register(RegisterKind::Spr, 0);

/// A special-purpose register that holds the address for Interrupt Vector 1.
pub const IV1: Register = Register(RegisterKind::Spr, 1);

/// A special-purpose register that holds the address for Interrupt Vector 2.
pub const IV2: Register = Register(RegisterKind::Spr, 2);

/// A special-purpose register that holds the address for the Exception Vector.
pub const EV: Register = Register(RegisterKind::Spr, 3);

/// A special-purpose register that holds the current stack pointer.
pub const SP: Register = Register(RegisterKind::Spr, 4);

/// A special-purpose register that holds the current program counter.
pub const PC: Register = Register(RegisterKind::Spr, 5);

/// A special-purpose register that holds the external base address for IMEM
/// transfers.
pub const IMB: Register = Register(RegisterKind::Spr, 6);

/// A special-purpose register that holds the external base address for DMEM
/// transfers.
pub const DMB: Register = Register(RegisterKind::Spr, 7);

/// A special-purpose register that holds various CPU flag bits.
pub const CSW: Register = Register(RegisterKind::Spr, 8);

/// A special-purpose register that holds the configuration bits for the SCP DMA
/// override functionality.
pub const CCR: Register = Register(RegisterKind::Spr, 9);

/// A special-purpose register that holds the configuration bits for the SCP
/// authentication process.
pub const SEC: Register = Register(RegisterKind::Spr, 10);

/// A special-purpose register that holds the configuration bits for the CTXDMA
/// ports.
pub const CTX: Register = Register(RegisterKind::Spr, 11);

/// A special-purpose register that holds details on raised exceptions.
pub const EXCI: Register = Register(RegisterKind::Spr, 12);

/// Unknown. Marked as reserved.
pub const SEC1: Register = Register(RegisterKind::Spr, 13);

/// Unknown. Marked as reserved.
pub const IMB1: Register = Register(RegisterKind::Spr, 14);

/// Unknown. Marked as reserved.
pub const DMB1: Register = Register(RegisterKind::Spr, 15);

enum_from_primitive! {
    /// Flag bits for the `flags` special-purpose register.
    #[derive(Debug)]
    #[repr(u32)]
    pub enum CpuFlag {
        /// General-purpose predicate 0.
        P0 = 1 << 0,
        /// General-purpose predicate 1.
        P1 = 1 << 1,
        /// General-purpose predicate 2.
        P2 = 1 << 2,
        /// General-purpose predicate 3.
        P3 = 1 << 3,
        /// General-purpose predicate 4.
        P4 = 1 << 4,
        /// General-purpose predicate 5.
        P5 = 1 << 5,
        /// General-purpose predicate 6.
        P6 = 1 << 6,
        /// General-purpose predicate 7.
        P7 = 1 << 7,
        /// ALU carry flag.
        CARRY = 1 << 8,
        /// ALU signed overflow flag.
        OVERFLOW = 1 << 9,
        /// ALU sign/negative flag.
        NEGATIVE = 1 << 10,
        /// ALU zero flag.
        ZERO = 1 << 11,
        /// Interrupt 0 enable flag.
        IE0 = 1 << 16,
        /// Interrupt 1 enable flag.
        IE1 = 1 << 17,
        /// Interrupt 2 enable flag.
        IE2 = 1 << 18,
        /// Interrupt 0 saved enable flag.
        IS0 = 1 << 20,
        /// Interrupt 1 saved enable flag.
        IS1 = 1 << 21,
        /// Interrupt 2 saved enable flag.
        IS2 = 1 << 22,
        /// Trap handler active flag.
        TA = 1 << 24,
    }
}

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

    /// Sets a given CPU flag bit in the `$flags` register.
    pub fn set_flag(&mut self, flag: CpuFlag, set: bool) {
        if set {
            self[CSW] |= flag as u32;
        } else {
            self[CSW] &= !(flag as u32);
        }
    }

    /// Checks if a given CPU flag bit in the `$flags` register is set.
    pub fn get_flag(&self, flag: CpuFlag) -> bool {
        (self[CSW] & flag as u32) != 0
    }

    /// A debugging method that grants immutable access to the registers of the
    /// supplied [`RegisterKind`].
    ///
    /// NOTE: The intent behind this method is to query internal state for
    /// inspection during emulation.
    ///
    /// [`RegisterKind`]: /faucon-asm/operands/enum.RegisterKind.html
    pub fn debug_get(&self, kind: &RegisterKind) -> &[u32] {
        match kind {
            RegisterKind::Gpr => &self.gpr,
            RegisterKind::Spr => &self.spr,
            RegisterKind::Crypto => panic!("Crypto registers are only accessible through the SCP"),
        }
    }
}

impl Index<Register> for CpuRegisters {
    type Output = u32;

    fn index(&self, reg: Register) -> &Self::Output {
        match reg.0 {
            RegisterKind::Gpr => &self.gpr[reg.1],
            RegisterKind::Spr => &self.spr[reg.1],
            RegisterKind::Crypto => panic!("Crypto registers are only accessible through the SCP"),
        }
    }
}

impl IndexMut<Register> for CpuRegisters {
    fn index_mut(&mut self, reg: Register) -> &mut Self::Output {
        match reg.0 {
            RegisterKind::Gpr => &mut self.gpr[reg.1],
            RegisterKind::Spr => &mut self.spr[reg.1],
            RegisterKind::Crypto => panic!("Crypto registers are only accessible through the SCP"),
        }
    }
}

impl Index<Operand> for CpuRegisters {
    type Output = u32;

    fn index(&self, operand: Operand) -> &Self::Output {
        match operand {
            Operand::Register(reg) => &self[reg],
            _ => panic!("Invalid operand supplied"),
        }
    }
}

impl IndexMut<Operand> for CpuRegisters {
    fn index_mut(&mut self, operand: Operand) -> &mut Self::Output {
        match operand {
            Operand::Register(reg) => &mut self[reg],
            _ => panic!("Invalid operand supplied"),
        }
    }
}
