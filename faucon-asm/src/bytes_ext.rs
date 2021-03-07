use num_traits::PrimInt;

// A trait that aids in performing saturating casts, e.g. from `u32` to `u8`
// on generic primitive integer types.
//
// This is used for truncating number types to their low 8 bits when lowering
// them into Falcon machine code.
pub trait SaturatingCast<T: PrimInt> {
    // Truncates this type into the target type `T`.
    fn saturating_cast(self) -> T;
}

macro_rules! impl_saturatingcast_for {
    ($int:ty, $target:ty) => {
        impl SaturatingCast<$target> for $int {
            fn saturating_cast(self) -> $target {
                self as $target
            }
        }
    };
}

impl_saturatingcast_for!(u8, u8);
impl_saturatingcast_for!(i8, u8);
impl_saturatingcast_for!(u16, u8);
impl_saturatingcast_for!(i16, u8);
impl_saturatingcast_for!(u32, u8);
impl_saturatingcast_for!(i32, u8);
