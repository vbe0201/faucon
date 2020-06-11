//! Falcon CPU abstractions.

// TODO: Figure out bits 26 and 29.
/// [`Cpu`] flag bits for the `flags` special-purpose register.
///
/// [`Cpu`]: struct.Cpu.html
#[derive(Debug)]
#[repr(i32)]
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

/// The execution states a Falcon processor can take.
#[derive(Debug)]
pub enum ExecutionState {
    /// The processor is actively executing instructions.
    Running,
    /// The processor is currently halted, unable to recover.
    Stopped,
    /// The processor is currently halted, but interrupts can restart execution.
    Sleeping,
}

/// Representation of the Falcon processor.
#[derive(Debug)]
pub struct Cpu {
    /// The 16 general-purpose registers.
    pub gprs: [i32; 0x10],
    /// The 16 special-purpose registers.
    pub sprs: [i32; 0x10],
    /// The 16 secretful registers.
    pub crs: [i32; 0x10],
    /// The current execution state.
    pub state: ExecutionState,
}

impl Cpu {
    /// Interrupt vector 0 register.
    pub const REG_IV0: usize = 0;
    /// Interrupt vector 1 register.
    pub const REG_IV1: usize = 1;
    /// Interrupt vector 2 register.
    pub const REG_IV2: usize = 2;
    /// Trap vector register.
    pub const REG_TV: usize = 3;
    /// Stack pointer register.
    pub const REG_SP: usize = 4;
    /// Program counter register.
    pub const REG_PC: usize = 5;
    /// Code xfer external base register.
    pub const REG_XCBASE: usize = 6;
    /// Data xfer external base register.
    pub const REG_XDBASE: usize = 7;
    /// CPU flags register.
    pub const REG_FLAGS: usize = 8;
    /// Crypt xfer mode register.
    pub const REG_CX: usize = 9;
    /// Crypt auth code selection register.
    pub const REG_CAUTH: usize = 10;
    /// Xfer port selection register.
    pub const REG_XTARGETS: usize = 11;
    /// Trap status register.
    pub const REG_TSTATUS: usize = 12;
}

impl Cpu {
    /// Toggles a flag in the `flags` register based on the value of `set`.
    ///
    /// - `set = true` sets the given flag.
    /// - `set = false` clears the given flag.
    pub fn set_flag(&mut self, flag: CpuFlag, set: bool) {
        if set {
            self.sprs[Self::REG_FLAGS] |= flag as i32;
        } else {
            self.sprs[Self::REG_FLAGS] &= !(flag as i32);
        }
    }

    /// Gets a flag from the `flags` register and indicates whether it is set.
    pub fn get_flag(&self, flag: CpuFlag) -> bool {
        (self.sprs[Self::REG_FLAGS] & flag as i32) != 0
    }
}
