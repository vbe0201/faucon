/// Representation of the Falcon processor.
#[derive(Debug)]
pub struct Cpu {
    /// The 16 general-purpose registers.
    pub gprs: [i32; 0x10],
    /// The 16 special-purpose registers.
    pub sprs: [i32; 0x10],
    /// The 16 secretful registers.
    pub crs: [i32; 0x10],
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
