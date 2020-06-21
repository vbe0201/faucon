//! Falcon CPU abstractions.

use std::sync::Mutex;

pub use self::registers::*;
use crate::memory::Memory;

mod registers;

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
pub struct Cpu {
    registers: CpuRegisters,
    /// The processor memory.
    pub memory: Mutex<Memory>,
    /// The current execution state.
    pub state: ExecutionState,
}

impl Cpu {
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
    /// Pushes a value onto the execution stack and decrements `sp` by 4.
    ///
    /// NOTE: The execution stack is part of the [`DataSpace`] memory.
    ///
    /// [`DataSpace`]: ../memory/struct.DataSpace.html
    pub fn push(&mut self, value: u32) {
        let mut memory = self.memory.lock().unwrap();
        self.registers.set_sp(self.registers.get_sp() - 4);
        memory.data.write_word(self.registers.get_sp(), value);
    }

    /// Pops a value off the execution stack, increments `sp` by 4, and returns it.
    ///
    /// NOTE: The execution stack is part of the [`DataSpace`] memory.
    ///
    /// [`DataSpace`]: ../memory/struct.DataSpace.html
    pub fn pop(&mut self) -> u32 {
        let mut memory = self.memory.lock().unwrap();
        let value = memory.data.read_word(self.registers.get_sp());
        self.registers.set_sp(self.registers.get_sp() + 4);

        value
    }

    /// Delivers a trap to the processor.
    ///
    /// Traps behave similar to interrupts, but are triggered from events
    /// inside the UC.
    pub fn trap(&mut self, reason: u32) {
        // Indicate that a trap is currently active.
        self.registers.set_flag(CpuFlag::TA, true);

        // Load the faulting `pc` along with the trap reason into `tstatus`.
        self.registers
            .set_tstatus(self.registers.get_pc() | reason << 20);

        // Configure the `flags` register appropriately.
        // TODO: Missing flags?
        self.registers
            .set_flag(CpuFlag::IS0, self.registers.get_flag(CpuFlag::IE0));
        self.registers
            .set_flag(CpuFlag::IS1, self.registers.get_flag(CpuFlag::IE1));
        self.registers.set_flag(CpuFlag::IE0, false);
        self.registers.set_flag(CpuFlag::IE1, false);

        // Push the faulting `pc` onto the stack and execute the responsible `tv`.
        self.push(self.registers.get_pc());
        self.registers.set_pc(self.registers.get_tv());
    }
}
