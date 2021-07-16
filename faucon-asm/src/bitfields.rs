use std::cmp::{max, min};

use crate::bit_utils::BitField;
use crate::operands::*;

// Specifies the byte position of an encodable operand bitfield.
pub trait Position {
    // Gets the encoded position of a bitfield as a `(byte_start, byte_width)` tuple.
    //
    // In case the encoding revolves around multiple elements, the span will be large
    // enough to cover everything.
    fn position(&self) -> (usize, usize);
}

#[inline]
fn merge_positions(a: (usize, usize), b: (usize, usize)) -> (usize, usize) {
    let start = min(a.0, b.0);
    let end = max(a.0 + a.1, b.0 + b.1);

    (start, end - start)
}

impl<T> Position for BitField<T> {
    fn position(&self) -> (usize, usize) {
        (self.byte_start(), self.byte_width())
    }
}

// Defines helpers for machine code encoding and decoding of certain crate types.
pub trait MachineEncoding: Position {
    fn read(&self, pc: u32, buf: &[u8]) -> Operand;
}

// We do impls of `MachineEncoding` directly for `BitField` objects
// to produce 32-bit immediates without any unnecessary wrappers.

impl MachineEncoding for BitField<i32> {
    fn read(&self, _: u32, buf: &[u8]) -> Operand {
        Operand::Imm(BitField::<i32>::read(self, buf))
    }
}

impl MachineEncoding for BitField<u32> {
    fn read(&self, _: u32, buf: &[u8]) -> Operand {
        Operand::UImm(BitField::<u32>::read(self, buf))
    }
}

// General-purpose and special-purpose Falcon CPU and coprocessor registers.
pub struct RegisterEncoding<'b> {
    kind: RegisterKind,
    field: &'b BitField<u8>,
}

impl<'b> RegisterEncoding<'b> {
    pub fn read_raw(&self, buf: &[u8]) -> Register {
        Register(self.kind, self.field.read(buf) as usize)
    }
}

impl<'b> Position for RegisterEncoding<'b> {
    fn position(&self) -> (usize, usize) {
        self.field.position()
    }
}

impl<'b> MachineEncoding for RegisterEncoding<'b> {
    fn read(&self, _: u32, buf: &[u8]) -> Operand {
        Operand::Register(self.read_raw(buf))
    }
}

// Describes the encoding of flag bits from the `$csw` register.
pub struct FlagEncoding<'b>(&'b BitField<u8>);

impl<'b> FlagEncoding<'b> {
    pub fn read_raw(&self, buf: &[u8]) -> u8 {
        self.0.read(buf)
    }
}

impl<'b> Position for FlagEncoding<'b> {
    fn position(&self) -> (usize, usize) {
        self.0.position()
    }
}

impl<'b> MachineEncoding for FlagEncoding<'b> {
    fn read(&self, _: u32, buf: &[u8]) -> Operand {
        Operand::Flag(self.read_raw(buf))
    }
}

// Encodings of memory accesses to the Falcon's code or data segments.
pub enum MemoryEncoding<'b> {
    // A memory access to either DMEM or IMEM that is evaluated as `[$reg]`.
    Reg(MemorySpace, &'b RegisterEncoding<'b>),
    // A memory access to either DMEM or IMEM that is evaluated as `[$reg1 + $reg2 * scale]`.
    RegReg(
        MemorySpace,
        &'b RegisterEncoding<'b>,
        &'b RegisterEncoding<'b>,
        u8,
    ),
    // A memory access to either DMEM or IMEM that is evaluated as `[$reg + imm]`.
    RegImm(MemorySpace, &'b RegisterEncoding<'b>, &'b BitField<u32>),
}

impl<'b> MemoryEncoding<'b> {
    pub fn read_raw(&self, buf: &[u8]) -> MemoryAccess {
        match self {
            MemoryEncoding::Reg(space, reg) => MemoryAccess::Reg {
                space: *space,
                base: reg.read_raw(buf),
            },
            MemoryEncoding::RegReg(space, reg1, reg2, scale) => MemoryAccess::RegReg {
                space: *space,
                base: reg1.read_raw(buf),
                offset: reg2.read_raw(buf),
                scale: *scale,
            },
            MemoryEncoding::RegImm(space, reg, imm) => MemoryAccess::RegImm {
                space: *space,
                base: reg.read_raw(buf),
                offset: imm.read(buf),
            },
        }
    }
}

impl<'b> Position for MemoryEncoding<'b> {
    fn position(&self) -> (usize, usize) {
        match self {
            MemoryEncoding::Reg(_, reg) => reg.position(),
            MemoryEncoding::RegReg(_, reg1, reg2, _) => {
                merge_positions(reg1.position(), reg2.position())
            }
            MemoryEncoding::RegImm(_, reg, imm) => merge_positions(reg.position(), imm.position()),
        }
    }
}

impl<'b> MachineEncoding for MemoryEncoding<'b> {
    fn read(&self, _: u32, buf: &[u8]) -> Operand {
        Operand::Memory(self.read_raw(buf))
    }
}

// A signed immediate that represents a relative offset from the current program counter.
pub struct RelativeAddress<'b>(&'b BitField<i32>);

impl<'b> RelativeAddress<'b> {
    fn read_raw(&self, buf: &[u8]) -> i32 {
        self.0.read(buf)
    }
}

impl<'b> Position for RelativeAddress<'b> {
    fn position(&self) -> (usize, usize) {
        self.0.position()
    }
}

impl<'b> MachineEncoding for RelativeAddress<'b> {
    fn read(&self, pc: u32, buf: &[u8]) -> Operand {
        let absolute_address = pc.wrapping_add(self.read_raw(buf) as u32);
        Operand::UImm(absolute_address)
    }
}

// Describes the encoding of a bit range within an unsigned immediate.
pub struct BitRangeEncoding<'b> {
    // The lower start bound of the range.
    start: &'b BitField<u8>,
    // The number of bits to extract from `start` onwards, subtracted by one.
    nbits: &'b BitField<u8>,
}

impl<'b> BitRangeEncoding<'b> {
    pub fn read_raw(&self, buf: &[u8]) -> (u8, u8) {
        (self.start.read(buf), self.nbits.read(buf))
    }
}

impl<'b> Position for BitRangeEncoding<'b> {
    fn position(&self) -> (usize, usize) {
        merge_positions(self.start.position(), self.nbits.position())
    }
}

impl<'b> MachineEncoding for BitRangeEncoding<'b> {
    fn read(&self, _: u32, buf: &[u8]) -> Operand {
        let (start, nbits) = self.read_raw(buf);
        Operand::Bitfield(start as u32, nbits as u32)
    }
}

// Dispatches a Falcon operand encoding field.
//
// On sized instructions, the dispatch influences the operand sizing and thus
// the value the dispatch evaluates to, whereas operands of unsized instructions
// are bound to fixed fields.
#[derive(Clone)]
pub enum FieldDispatch<'field> {
    Sized(fn(u8) -> &'field dyn MachineEncoding),
    Fixed(&'field dyn MachineEncoding),
}

impl<'field> FieldDispatch<'field> {
    pub fn evaluate(&self, size: u8) -> &'field dyn MachineEncoding {
        match self {
            FieldDispatch::Sized(c) => c(size),
            FieldDispatch::Fixed(e) => *e,
        }
    }
}

// An unsigned 8-bit immediate.
//
// Used for bit positions, shifts, and 8-bit instructions.
pub const U8: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::U8);

// A signed 8-bit immediate.
//
// Used for comparisons and PC-relative offsets.
pub const I8: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::I8);

// A signed 8-bit immediate.
//
// Used for Falcon v5 MOV instructions.
pub const I8P1: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::I8P1);

// An unsigned 8-bit immediate shifted left by `1`.
//
// Used for memory addressing.
pub const U8S1: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::U8S1);

// An unsigned 8-bit immediate shifted left by `2`.
//
// Used for memory addressing.
pub const U8S2: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::U8S2);

// An unsigned 8-bit immediate shifted left by `16`.
//
// Used by the SETHI instruction.
pub const U8S16: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::U8S16);

// An unsigned 16-bit immediate.
//
// Used for 16-bit instructions.
pub const U16: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::U16);

// A signed 16-bit immediate.
//
// Used for signed comparisons.
pub const I16: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::I16);

// An unsigned 16-bit immediate.
//
// Used for Falcon v5 CALL instructions.
pub const U16P1: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::U16);

// A signed 16-bit immediate.
//
// Used for Falcon v5 MOV instructions.
pub const I16P1: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::I16P1);

// Dispatches a 16-bit immediate based on the operand size of the instruction.
pub const U16S: FieldDispatch<'_> = FieldDispatch::Sized(|size| match size {
    0b00 => &raw::U8,
    _ => &raw::U16,
});

// Dispatches a 16-bit immediate based on the operand size of the instruction.
pub const I16S: FieldDispatch<'_> = FieldDispatch::Sized(|size| match size {
    0b00 => &raw::I8,
    _ => &raw::I16,
});

// An unsigned 24-bit immediate.
//
// Used for absolute branch addresses.
pub const U24: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::U24);

// A signed 24-bit immediate.
//
// Used for MOV instructions.
pub const I24: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::I24);

// An unsigned 32-bit immediate.
//
// Used for Falcon v5 MOV instructions.
pub const U32: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::U32);

// An 8-bit PC-relative offset.
//
// Used for branches.
pub const PC8: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::PC8);

// A 16-bit PC-relative offset.
//
// Used for branches.
pub const PC16: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::PC16);

// An 8-bit PC-relative offset.
//
// Used for Falcon v5 conditional branch instructions.
pub const PC8P3: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::PC8P3);

// A 16-bit PC-relative offset.
//
// Used for Falcon v5 conditional branch instructions.
pub const PC16P3: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::PC16P3);

// An 8-bit PC-relative offset.
//
// Used for Falcon v5 conditional branch instructions.
pub const PC8P4: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::PC8P4);

// A 16-bit PC-relative offset.
//
// Used for Falcon v5 conditional branch instructions.
pub const PC16P4: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::PC16P4);

// A bit range spanning 8 bits.
//
// Used for bit manipulation instructions.
pub const BITR8: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::BITR8);

// A bit range spanning 16 bits.
//
// Used for bit manipulation instructions.
pub const BITR16: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::BITR16);

// Falcon general-purpose registers encoded in different locations.
pub const R0: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::R0);
pub const R1: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::R1);
pub const R2: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::R2);
pub const R3: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::R3);

// Falcon special-purpose registers encoded in different locations.
pub const SR1: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::SR1);
pub const SR2: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::SR2);

// Falcon special-purpose registers modified by various instructions.
pub const SP: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::SP);
pub const CSW: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::CSW);

// A selected flag bit in the Falcon `$csw` register.
pub const FLAG: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::FLAG);

// A selected predicate bit in the Falcon `$csw` register.
pub const PRED: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::PRED);

// A software trap identifier.
//
// Used by the `TRAP` instruction to trigger a software fault.
pub const TRAP: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::TRAP);

// A memory access to an address in Falcon DMem.
//
// Used by the `LD` and `ST` instructions.
pub const MEMR: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::MEMR);
pub const MEMRI: FieldDispatch<'_> = FieldDispatch::Sized(|size| match size {
    0b00 => &raw::MEMRI8,
    0b01 => &raw::MEMRI16,
    _ => &raw::MEMRI32,
});
pub const MEMSPI: FieldDispatch<'_> = FieldDispatch::Sized(|size| match size {
    0b00 => &raw::MEMSPI8,
    0b01 => &raw::MEMSPI16,
    _ => &raw::MEMSPI32,
});
pub const MEMSPR: FieldDispatch<'_> = FieldDispatch::Sized(|size| match size {
    0b00 => &raw::MEMSPR8,
    0b01 => &raw::MEMSPR16,
    _ => &raw::MEMSPR32,
});
pub const MEMRR: FieldDispatch<'_> = FieldDispatch::Sized(|size| match size {
    0b00 => &raw::MEMRR8,
    0b01 => &raw::MEMRR16,
    _ => &raw::MEMRR32,
});
pub const MEMRRALT: FieldDispatch<'_> = FieldDispatch::Sized(|size| match size {
    0b00 => &raw::MEMRRALT8,
    0b01 => &raw::MEMRRALT16,
    _ => &raw::MEMRRALT32,
});

// A memory access to an address in Falcon I/O space.
//
// Used by the `IORD(S)` and `IOWR(S)` opcodes.
pub const IOR: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::IOR);
pub const IORR: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::IORR);
pub const IORI: FieldDispatch<'_> = FieldDispatch::Fixed(&raw::IORI);

mod raw {
    use super::*;
    use crate::bit_utils::BitField;

    pub const U8: BitField<u32> = BitField::new(16..24, None);
    pub const I8: BitField<i32> = BitField::new(16..24, None);
    pub const I8P1: BitField<i32> = BitField::new(8..16, None);
    pub const I8P3: BitField<i32> = BitField::new(24..32, None);
    pub const I8P4: BitField<i32> = BitField::new(32..40, None);
    pub const U8S1: BitField<u32> = BitField::new(16..24, Some(1));
    pub const U8S2: BitField<u32> = BitField::new(16..24, Some(2));
    pub const U8S16: BitField<u32> = BitField::new(16..24, Some(16));
    pub const U16: BitField<u32> = BitField::new(16..32, None);
    pub const I16: BitField<i32> = BitField::new(16..32, None);
    pub const U16P1: BitField<u32> = BitField::new(8..24, None);
    pub const I16P1: BitField<i32> = BitField::new(8..24, None);
    pub const I16P3: BitField<i32> = BitField::new(24..40, None);
    pub const I16P4: BitField<i32> = BitField::new(32..48, None);
    pub const U24: BitField<u32> = BitField::new(8..32, None);
    pub const I24: BitField<i32> = BitField::new(8..32, None);
    pub const U32: BitField<u32> = BitField::new(8..40, None);
    pub const TRAP: BitField<u32> = BitField::new(8..10, None);

    pub const PC8: RelativeAddress<'_> = RelativeAddress(&I8);
    pub const PC16: RelativeAddress<'_> = RelativeAddress(&I16);
    pub const PC8P3: RelativeAddress<'_> = RelativeAddress(&I8P3);
    pub const PC8P4: RelativeAddress<'_> = RelativeAddress(&I8P4);
    pub const PC16P3: RelativeAddress<'_> = RelativeAddress(&I16P3);
    pub const PC16P4: RelativeAddress<'_> = RelativeAddress(&I16P4);

    pub const BITR8: BitRangeEncoding<'_> = BitRangeEncoding {
        start: &BitField::new(16..21, None),
        nbits: &BitField::new(21..24, None),
    };
    pub const BITR16: BitRangeEncoding<'_> = BitRangeEncoding {
        start: &BitField::new(16..21, None),
        nbits: &BitField::new(21..26, None),
    };

    pub const R0: RegisterEncoding<'_> = RegisterEncoding {
        kind: RegisterKind::Gpr,
        field: &BitField::new(0..4, None),
    };
    pub const R1: RegisterEncoding<'_> = RegisterEncoding {
        kind: RegisterKind::Gpr,
        field: &BitField::new(8..12, None),
    };
    pub const R2: RegisterEncoding<'_> = RegisterEncoding {
        kind: RegisterKind::Gpr,
        field: &BitField::new(12..16, None),
    };
    pub const R3: RegisterEncoding<'_> = RegisterEncoding {
        kind: RegisterKind::Gpr,
        field: &BitField::new(20..24, None),
    };
    pub const SR1: RegisterEncoding<'_> = RegisterEncoding {
        kind: RegisterKind::Spr,
        field: &BitField::new(12..16, None),
    };
    pub const SR2: RegisterEncoding<'_> = RegisterEncoding {
        kind: RegisterKind::Spr,
        field: &BitField::new(8..12, None),
    };
    pub const SP: RegisterEncoding<'_> = RegisterEncoding {
        kind: RegisterKind::Spr,
        field: &BitField::new_with_value(4),
    };
    pub const CSW: RegisterEncoding<'_> = RegisterEncoding {
        kind: RegisterKind::Spr,
        field: &BitField::new_with_value(8),
    };

    pub const FLAG: FlagEncoding<'_> = FlagEncoding(&BitField::new(16..21, None));
    pub const PRED: FlagEncoding<'_> = FlagEncoding(&BitField::new(8..11, None));

    pub const MEMR: MemoryEncoding<'_> = MemoryEncoding::Reg(MemorySpace::DMem, &R2);
    pub const MEMRI8: MemoryEncoding<'_> = MemoryEncoding::RegImm(MemorySpace::DMem, &R2, &U8);
    pub const MEMRI16: MemoryEncoding<'_> = MemoryEncoding::RegImm(MemorySpace::DMem, &R2, &U8S1);
    pub const MEMRI32: MemoryEncoding<'_> = MemoryEncoding::RegImm(MemorySpace::DMem, &R2, &U8S2);
    pub const MEMSPI8: MemoryEncoding<'_> = MemoryEncoding::RegImm(MemorySpace::DMem, &SP, &U8);
    pub const MEMSPI16: MemoryEncoding<'_> = MemoryEncoding::RegImm(MemorySpace::DMem, &SP, &U8S1);
    pub const MEMSPI32: MemoryEncoding<'_> = MemoryEncoding::RegImm(MemorySpace::DMem, &SP, &U8S2);
    pub const MEMSPR8: MemoryEncoding<'_> = MemoryEncoding::RegReg(MemorySpace::DMem, &SP, &R1, 1);
    pub const MEMSPR16: MemoryEncoding<'_> = MemoryEncoding::RegReg(MemorySpace::DMem, &SP, &R1, 2);
    pub const MEMSPR32: MemoryEncoding<'_> = MemoryEncoding::RegReg(MemorySpace::DMem, &SP, &R1, 4);
    pub const MEMRR8: MemoryEncoding<'_> = MemoryEncoding::RegReg(MemorySpace::DMem, &R2, &R1, 1);
    pub const MEMRR16: MemoryEncoding<'_> = MemoryEncoding::RegReg(MemorySpace::DMem, &R2, &R1, 2);
    pub const MEMRR32: MemoryEncoding<'_> = MemoryEncoding::RegReg(MemorySpace::DMem, &R2, &R1, 4);
    pub const MEMRRALT8: MemoryEncoding<'_> =
        MemoryEncoding::RegReg(MemorySpace::DMem, &R2, &R3, 1);
    pub const MEMRRALT16: MemoryEncoding<'_> =
        MemoryEncoding::RegReg(MemorySpace::DMem, &R2, &R3, 2);
    pub const MEMRRALT32: MemoryEncoding<'_> =
        MemoryEncoding::RegReg(MemorySpace::DMem, &R2, &R3, 4);
    pub const IOR: MemoryEncoding<'_> = MemoryEncoding::Reg(MemorySpace::IMem, &R2);
    pub const IORR: MemoryEncoding<'_> = MemoryEncoding::RegReg(MemorySpace::IMem, &R2, &R1, 4);
    pub const IORI: MemoryEncoding<'_> = MemoryEncoding::RegImm(MemorySpace::IMem, &R2, &U8S2);
}
