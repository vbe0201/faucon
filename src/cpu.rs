//! Falcon CPU abstractions.

use crate::memory::Memory;

// TODO: Figure out bits 26 and 29.
/// [`Cpu`] flag bits for the `flags` special-purpose register.
///
/// [`Cpu`]: struct.Cpu.html
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
    pub gprs: [u32; 0x10],
    /// The 16 special-purpose registers.
    pub sprs: [u32; 0x10],
    /// The 16 secretful registers.
    pub crs: [u32; 0x10],
    /// The processor memory.
    pub memory: Memory,
    /// The current execution state.
    pub state: ExecutionState,
}

impl Cpu {
    // Special-purpose register indexes.
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

    // Trap reasons.
    /// The trap reason for software trap 0.
    pub const TRAP_SOFTWARE_0: u32 = 0x0;
    /// The trap reason for software trap 1.
    pub const TRAP_SOFTWARE_1: u32 = 0x1;
    /// The trap reason for software trap 2.
    pub const TRAP_SOFTWARE_2: u32 = 0x2;
    /// The trap reason for software trap 3.
    pub const TRAP_SOFTWARE_3: u32 = 0x3;
    /// The trap reason for when an invalid opcode occurs.
    pub const TRAP_INVALID_OPCODE: u32 = 0x8;
    /// The trap reason for when a page fault occurs due to no hits.
    pub const TRAP_PAGE_NO_HIT: u32 = 0xA;
    /// The trap reason for when a page fault occurs due to multiple hits.
    pub const TRAP_PAGE_MULTI_HIT: u32 = 0xB;
    /// The trap reason for when a breakpoint is hit.
    pub const TRAP_BREAKPOINT: u32 = 0xF;
}

impl Cpu {
    /// Toggles a flag in the `flags` register based on the value of `set`.
    ///
    /// - `set = true` sets the given flag.
    /// - `set = false` clears the given flag.
    pub fn set_flag(&mut self, flag: CpuFlag, set: bool) {
        if set {
            self.sprs[Self::REG_FLAGS] |= flag as u32;
        } else {
            self.sprs[Self::REG_FLAGS] &= !(flag as u32);
        }
    }

    /// Gets a flag from the `flags` register and indicates whether it is set.
    pub fn get_flag(&self, flag: CpuFlag) -> bool {
        (self.sprs[Self::REG_FLAGS] & flag as u32) != 0
    }

    /// Pushes a value onto the execution stack and decrements `sp` by 4.
    ///
    /// NOTE: The execution stack is part of the [`DataSpace`] memory.
    ///
    /// [`DataSpace`]: ../memory/struct.DataSpace.html
    pub fn push(&mut self, value: u32) {
        self.sprs[Self::REG_SP] -= 4;
        self.memory.data.write_word(self.sprs[Self::REG_SP], value);
    }

    /// Pops a value off the execution stack, increments `sp` by 4, and returns it.
    ///
    /// NOTE: The execution stack is part of the [`DataSpace`] memory.
    ///
    /// [`DataSpace`]: ../memory/struct.DataSpace.html
    pub fn pop(&mut self) -> u32 {
        let value = self.memory.data.read_word(self.sprs[Self::REG_SP]);
        self.sprs[Self::REG_SP] += 4;

        value
    }

    /// Delivers a trap to the processor.
    ///
    /// Traps behave similar to interrupts, but are triggered from events
    /// inside the UC.
    pub fn trap(&mut self, reason: u32) {
        // Indicate that a trap is currently active.
        self.set_flag(CpuFlag::TA, true);

        // Load the faulting `pc` along with the trap reason into `tstatus`.
        self.sprs[Self::REG_TSTATUS] = self.sprs[Self::REG_PC] | reason << 20;

        // Configure the `flags` register appropriately.
        // TODO: Missing flags?
        self.set_flag(CpuFlag::IS0, self.get_flag(CpuFlag::IE0));
        self.set_flag(CpuFlag::IS1, self.get_flag(CpuFlag::IE1));
        self.set_flag(CpuFlag::IE0, false);
        self.set_flag(CpuFlag::IE1, false);

        // Push the faulting `pc` onto the stack and execute the responsible `tv`.
        self.push(self.sprs[Self::REG_PC]);
        self.sprs[Self::REG_PC] = self.sprs[Self::REG_TV];
    }
}
