use faucon_asm::Register;

/// A helper macro to generate access APIs for named special-purpose registers.
macro_rules! impl_spr {
    ($registers:ident, $reg:ident, $val:tt) => {
        paste::item! {
            impl $registers {
                /// Gets the value of the special-purpose register.
                pub fn [<get_ $reg>](&self) -> u32 {
                    self.spr[$val]
                }

                /// Sets the value of the special-purpose register.
                pub fn [<set_ $reg>](&mut self, value: u32) {
                    self.spr[$val] = value;
                }
            }
        }
    };
}

/// Representation of the Falcon CPU registers.
///
/// This is a wrapper for the various general-purpose and special-purpose
/// registers to read/write to them and provide extended functionality
/// when appropriate.
pub struct CpuRegisters {
    /// The 16 general-purpose CPU registers of the Falcon.
    gpr: [u32; 0x10],
    /// The 16 special-purpose CPU registers of the Falcon.
    spr: [u32; 0x10],
}

impl CpuRegisters {
    /// Creates a new instance of the registers, all of them being initialized
    /// to zero by default.
    pub fn new() -> Self {
        CpuRegisters {
            gpr: [0; 0x10],
            spr: [0; 0x10],
        }
    }

    /// Gets the value of a given general-purpose register.
    pub fn get_gpr(&self, reg: Register) -> u32 {
        assert!(reg.value <= 15);

        self.gpr[reg.value]
    }

    /// Sets a given general-purpose register to a given value.
    pub fn set_gpr(&mut self, reg: Register, value: u32) {
        assert!(reg.value <= 15);

        self.gpr[reg.value] = value;
    }
}

// Interrupt vector 0 register.
impl_spr!(CpuRegisters, iv0, 0);

// Interrupt vector 1 register.
impl_spr!(CpuRegisters, iv1, 1);

// Interrupt vector 2 register.
impl_spr!(CpuRegisters, iv2, 2);

// Trap vector register.
impl_spr!(CpuRegisters, tv, 3);

// Stack pointer register.
impl_spr!(CpuRegisters, sp, 4);

// Program counter register.
impl_spr!(CpuRegisters, pc, 5);

// Code transfer external base register.
impl_spr!(CpuRegisters, xcbase, 6);

// Data transfer external base register.
impl_spr!(CpuRegisters, xdbase, 7);

// CPU flags register.
impl_spr!(CpuRegisters, flags, 8);

// TODO: Figure out bits 26 and 29.
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

impl CpuRegisters {
    /// Toggles a flag in the `flags` register based on the value of `set`.
    ///
    /// - `set = true` sets the given flag.
    /// - `set = false` clears the given flag.
    pub fn set_flag(&mut self, flag: CpuFlag, set: bool) {
        let mut value = self.get_flags();

        if set {
            value |= flag as u32;
        } else {
            value &= !(flag as u32);
        }

        self.set_flags(value);
    }

    /// Gets a flag from the `flags` register and indicates whether it is set.
    pub fn get_flag(&self, flag: CpuFlag) -> bool {
        (self.get_flags() & flag as u32) != 0
    }
}

// Crypt transfer mode register.
impl_spr!(CpuRegisters, cx, 9);

// Crypt auth code selection register.
impl_spr!(CpuRegisters, cauth, 10);

// Transfer port selection register.
impl_spr!(CpuRegisters, xtargets, 11);

// Trap status register.
impl_spr!(CpuRegisters, tstatus, 12);
