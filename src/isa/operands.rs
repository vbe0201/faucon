use crate::{
    isa::*,
    utils::{BitField, extend_sign},
};

#[derive(Debug)]
pub struct ImmediateSpec {
    field: BitField,
    signed: bool,
}

impl ImmediateSpec {
    #[inline]
    const fn unmask(&self, target: u64) -> u64 {
        self.field.encode(target, 0)
    }

    #[inline]
    fn required_read_size(&self) -> usize {
        self.field.byte_end() as usize
    }

    #[inline]
    fn decode(&self, target: u64) -> Immediate {
        let mut value = self.field.decode(target);
        if self.signed {
            value = extend_sign(value, self.field.bit_width()) as u64;
        }

        Immediate {
            value: value as u32,
            signed: self.signed,
        }
    }
}

#[derive(Debug)]
pub struct RegisterSpec {
    field: BitField,
    kind: RegisterKind,
}

impl RegisterSpec {
    #[inline]
    const fn unmask(&self, target: u64) -> u64 {
        self.field.encode(target, 0)
    }

    #[inline]
    fn required_read_size(&self) -> usize {
        self.field.byte_end() as usize
    }

    #[inline]
    fn decode(&self, target: u64) -> Register {
        Register {
            kind: self.kind,
            index: self.field.decode(target) as u8,
        }
    }
}

#[derive(Debug)]
pub struct FlagSpec {
    field: BitField,
}

impl FlagSpec {
    #[inline]
    const fn unmask(&self, target: u64) -> u64 {
        self.field.encode(target, 0)
    }

    #[inline]
    fn required_read_size(&self) -> usize {
        self.field.byte_end() as usize
    }

    #[inline]
    fn decode(&self, target: u64) -> Flag {
        Flag(self.field.decode(target) as u8)
    }
}

#[derive(Debug)]
pub struct MemSpec {
    space: MemorySpace,
    base: RegisterSpec,
    offset: MemOffsetSpec,
}

impl MemSpec {
    #[inline]
    const fn unmask(&self, mut target: u64) -> u64 {
        target = self.base.field.encode(target, 0);
        target = match &self.offset {
            MemOffsetSpec::None => target,
            MemOffsetSpec::Reg { index, .. } => index.unmask(target),
            MemOffsetSpec::Imm(disp) => disp.unmask(target),
        };

        target
    }

    #[inline]
    fn required_read_size(&self) -> usize {
        let base = self.base.field.byte_end() as usize;
        let offset = match &self.offset {
            MemOffsetSpec::None => 0,
            MemOffsetSpec::Reg { index, .. } => index.required_read_size(),
            MemOffsetSpec::Imm(disp) => disp.required_read_size(),
        };

        base.max(offset)
    }

    #[inline]
    fn decode(&self, target: u64) -> MemoryAccess {
        let base = self.base.decode(target);
        let offset = match &self.offset {
            MemOffsetSpec::None => MemoryOffset::None,
            MemOffsetSpec::Reg { index, scale } => MemoryOffset::Reg {
                index: index.decode(target),
                scale: *scale,
            },
            MemOffsetSpec::Imm(disp) => {
                let disp = disp.decode(target);
                assert!(!disp.signed);
                MemoryOffset::Imm(disp.value)
            }
        };

        MemoryAccess {
            space: self.space,
            base,
            offset,
        }
    }
}

#[derive(Debug)]
pub enum MemOffsetSpec {
    None,
    Reg { index: RegisterSpec, scale: u8 },
    Imm(ImmediateSpec),
}

#[derive(Debug)]
pub struct BitRangeSpec {
    start: BitField,
    num_bits: BitField,
}

impl BitRangeSpec {
    #[inline]
    const fn unmask(&self, mut target: u64) -> u64 {
        target = self.start.encode(target, 0);
        target = self.num_bits.encode(target, 0);
        target
    }

    #[inline]
    fn required_read_size(&self) -> usize {
        let start = self.start.byte_end() as usize;
        let num_bits = self.num_bits.byte_end() as usize;
        start.max(num_bits)
    }

    #[inline]
    fn decode(&self, target: u64) -> BitRange {
        let start = self.start.decode(target) as u8;
        let end = start + self.num_bits.decode(target) as u8;
        BitRange { start, end }
    }
}

#[derive(Debug)]
pub enum OperandSpec {
    Empty,
    Imm(ImmediateSpec),
    Reg(RegisterSpec),
    Flag(FlagSpec),
    Mem(MemSpec),
    Bits(BitRangeSpec),
    PcRel(ImmediateSpec),
}

impl OperandSpec {
    #[inline]
    pub const fn unmask(&self, target: u64) -> u64 {
        match self {
            Self::Empty => target,
            Self::Imm(spec) => spec.unmask(target),
            Self::Reg(spec) => spec.unmask(target),
            Self::Flag(spec) => spec.unmask(target),
            Self::Mem(spec) => spec.unmask(target),
            Self::Bits(spec) => spec.unmask(target),
            Self::PcRel(spec) => spec.unmask(target),
        }
    }

    #[inline]
    pub fn required_read_size(&self) -> usize {
        match self {
            Self::Empty => 0,
            Self::Imm(spec) => spec.required_read_size(),
            Self::Reg(spec) => spec.required_read_size(),
            Self::Flag(spec) => spec.required_read_size(),
            Self::Mem(spec) => spec.required_read_size(),
            Self::Bits(spec) => spec.required_read_size(),
            Self::PcRel(spec) => spec.required_read_size(),
        }
    }

    #[inline]
    pub fn decode(&self, target: u64) -> Operand {
        match self {
            Self::Empty => Operand::None,
            Self::Imm(spec) => Operand::Imm(spec.decode(target)),
            Self::Reg(spec) => Operand::Reg(spec.decode(target)),
            Self::Flag(spec) => Operand::Flag(spec.decode(target)),
            Self::Mem(spec) => Operand::Mem(spec.decode(target)),
            Self::Bits(spec) => Operand::Bits(spec.decode(target)),
            Self::PcRel(spec) => Operand::PcRel(spec.decode(target)),
        }
    }
}

pub const NONE: OperandSpec = OperandSpec::Empty;

pub const U8: OperandSpec = OperandSpec::imm(BitField::new(16, 24), false);
pub const I8: OperandSpec = OperandSpec::imm(BitField::new(16, 24), true);
pub const I8P1: OperandSpec = OperandSpec::imm(BitField::new(8, 16), true);
pub const I8P3: OperandSpec = OperandSpec::imm(BitField::new(24, 32), true);
pub const I8P4: OperandSpec = OperandSpec::imm(BitField::new(32, 40), true);
pub const U8S1: OperandSpec = OperandSpec::imm(BitField::new_with_shift(16, 24, 1), false);
pub const U8S2: OperandSpec = OperandSpec::imm(BitField::new_with_shift(16, 24, 2), false);
pub const U8S16: OperandSpec = OperandSpec::imm(BitField::new_with_shift(16, 24, 16), false);
pub const U16: OperandSpec = OperandSpec::imm(BitField::new(16, 32), false);
pub const I16: OperandSpec = OperandSpec::imm(BitField::new(16, 32), true);
pub const U16P1: OperandSpec = OperandSpec::imm(BitField::new(8, 24), false);
pub const I16P1: OperandSpec = OperandSpec::imm(BitField::new(8, 24), true);
pub const I16P3: OperandSpec = OperandSpec::imm(BitField::new(24, 40), true);
pub const I16P4: OperandSpec = OperandSpec::imm(BitField::new(32, 48), true);
pub const U24: OperandSpec = OperandSpec::imm(BitField::new(8, 32), false);
pub const I24: OperandSpec = OperandSpec::imm(BitField::new(8, 32), true);
pub const U32: OperandSpec = OperandSpec::imm(BitField::new(8, 40), false);
pub const TRAP: OperandSpec = OperandSpec::imm(BitField::new(8, 10), false);

pub const PC8: OperandSpec = OperandSpec::pc_rel(I8);
pub const PC16: OperandSpec = OperandSpec::pc_rel(I16);
pub const PC8P3: OperandSpec = OperandSpec::pc_rel(I8P3);
pub const PC8P4: OperandSpec = OperandSpec::pc_rel(I8P4);
pub const PC16P3: OperandSpec = OperandSpec::pc_rel(I16P3);
pub const PC16P4: OperandSpec = OperandSpec::pc_rel(I16P4);

pub const BITR8: OperandSpec = OperandSpec::bits(BitField::new(16, 21), BitField::new(21, 24));
pub const BITR16: OperandSpec = OperandSpec::bits(BitField::new(16, 21), BitField::new(21, 26));

pub const R0: OperandSpec = OperandSpec::reg(BitField::new(0, 4), RegisterKind::Gpr);
pub const R1: OperandSpec = OperandSpec::reg(BitField::new(8, 12), RegisterKind::Gpr);
pub const R2: OperandSpec = OperandSpec::reg(BitField::new(12, 16), RegisterKind::Gpr);
pub const R3: OperandSpec = OperandSpec::reg(BitField::new(20, 24), RegisterKind::Gpr);
pub const SR1: OperandSpec = OperandSpec::reg(BitField::new(12, 16), RegisterKind::Spr);
pub const SR2: OperandSpec = OperandSpec::reg(BitField::new(8, 12), RegisterKind::Spr);
pub const SP: OperandSpec = OperandSpec::reg(BitField::new_with_value(4), RegisterKind::Spr);
pub const CSW: OperandSpec = OperandSpec::reg(BitField::new_with_value(8), RegisterKind::Spr);

pub const FLAG: OperandSpec = OperandSpec::flag(BitField::new(16, 21));
pub const PRED: OperandSpec = OperandSpec::flag(BitField::new(8, 11));

pub const MEMR: OperandSpec = OperandSpec::mem(MemorySpace::Data, R2);
pub const MEMRI8: OperandSpec = OperandSpec::memimm(MemorySpace::Data, R2, U8);
pub const MEMRI16: OperandSpec = OperandSpec::memimm(MemorySpace::Data, R2, U8S1);
pub const MEMRI32: OperandSpec = OperandSpec::memimm(MemorySpace::Data, R2, U8S2);
pub const MEMSPI8: OperandSpec = OperandSpec::memimm(MemorySpace::Data, SP, U8);
pub const MEMSPI16: OperandSpec = OperandSpec::memimm(MemorySpace::Data, SP, U8S1);
pub const MEMSPI32: OperandSpec = OperandSpec::memimm(MemorySpace::Data, SP, U8S2);
pub const MEMSPR8: OperandSpec = OperandSpec::memreg(MemorySpace::Data, SP, R1, 1);
pub const MEMSPR16: OperandSpec = OperandSpec::memreg(MemorySpace::Data, SP, R1, 2);
pub const MEMSPR32: OperandSpec = OperandSpec::memreg(MemorySpace::Data, SP, R1, 4);
pub const MEMRR8: OperandSpec = OperandSpec::memreg(MemorySpace::Data, R2, R1, 1);
pub const MEMRR16: OperandSpec = OperandSpec::memreg(MemorySpace::Data, R2, R1, 2);
pub const MEMRR32: OperandSpec = OperandSpec::memreg(MemorySpace::Data, R2, R1, 4);
pub const MEMRRALT8: OperandSpec = OperandSpec::memreg(MemorySpace::Data, R2, R3, 1);
pub const MEMRRALT16: OperandSpec = OperandSpec::memreg(MemorySpace::Data, R2, R3, 2);
pub const MEMRRALT32: OperandSpec = OperandSpec::memreg(MemorySpace::Data, R2, R3, 4);
pub const IOR: OperandSpec = OperandSpec::mem(MemorySpace::Io, R2);
pub const IORR: OperandSpec = OperandSpec::memreg(MemorySpace::Io, R2, R1, 4);
pub const IORI: OperandSpec = OperandSpec::memimm(MemorySpace::Io, R2, U8S2);

impl OperandSpec {
    const fn imm(field: BitField, signed: bool) -> Self {
        Self::Imm(ImmediateSpec { field, signed })
    }

    const fn reg(field: BitField, kind: RegisterKind) -> Self {
        Self::Reg(RegisterSpec { field, kind })
    }

    const fn flag(field: BitField) -> Self {
        Self::Flag(FlagSpec { field })
    }

    const fn mem(space: MemorySpace, base: OperandSpec) -> Self {
        let OperandSpec::Reg(base) = base else {
            unreachable!();
        };

        Self::Mem(MemSpec {
            space,
            base,
            offset: MemOffsetSpec::None,
        })
    }

    const fn memreg(space: MemorySpace, base: OperandSpec, index: OperandSpec, scale: u8) -> Self {
        let OperandSpec::Reg(base) = base else {
            unreachable!();
        };
        let OperandSpec::Reg(index) = index else {
            unreachable!();
        };

        Self::Mem(MemSpec {
            space,
            base,
            offset: MemOffsetSpec::Reg { index, scale },
        })
    }

    const fn memimm(space: MemorySpace, base: OperandSpec, disp: OperandSpec) -> Self {
        let OperandSpec::Reg(base) = base else {
            unreachable!();
        };
        let OperandSpec::Imm(disp) = disp else {
            unreachable!();
        };

        Self::Mem(MemSpec {
            space,
            base,
            offset: MemOffsetSpec::Imm(disp),
        })
    }

    const fn bits(start: BitField, num_bits: BitField) -> Self {
        Self::Bits(BitRangeSpec { start, num_bits })
    }

    const fn pc_rel(imm: OperandSpec) -> Self {
        let OperandSpec::Imm(imm) = imm else {
            unreachable!();
        };

        Self::PcRel(imm)
    }
}
