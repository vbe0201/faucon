//! Implementation of the Access Control List system that guards Falcon crypto registers.

use std::ops::{Deref, DerefMut};

/// Marks a crypto register as 'secure key'. Once cleared, this bit cannot be set again
/// and means that the register contains a "confidential" key, such as a hardware secret.
pub const SECURE_KEY: u8 = 1 << 0;
/// Marks a crypto register as 'secure readable'. Once cleared, this bit cannot be set
/// again and means that the contents of the register can be read from a secure context.
pub const SECURE_READABLE: u8 = 1 << 1;
/// Marks a crypto register as 'insecure key'. This bit can be toggled back and forth
/// and means that the register contains a "weak" key that can be read from a secure
/// context.
///
/// This bit is unconditionally forced to `0` if [`SECURE_KEY`] is set and forced to
/// `1` when [`INSECURE_READABLE`] is set.
///
/// [`SECURE_KEY`]: const.SECURE_KEY.html
/// [`INSECURE_READABLE`]: const.INSECURE_READABLE.html
pub const INSECURE_KEY: u8 = 1 << 2;
/// Marks a crypto register as 'insecure readable'. This bit can be toggled back and
/// forth and means that No Secure Mode code can read the contents of the register.
///
/// This bit is unconditionally forced to `0` whenever [`SECURE_READABLE`] is set.
///
/// [`SECURE_READABLE`]: const.SECURE_READABLE.html
pub const INSECURE_READABLE: u8 = 1 << 3;
/// Marks a crypto register as 'insecure overwritable'. This bit can be toggled back and
/// forth and means that No Secure Mode code can overwrite the register arbitrarily.
pub const INSECURE_OVERWRITABLE: u8 = 1 << 4;

/// A wrapper around a crypto register that enforces the Access Control List of the
/// Secure Co-Processor to guard the register from unauthorized or unsupported access.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct AclCell<T: Copy> {
    inner: T,
    value: u8,
}

impl<T: Copy> AclCell<T> {
    /// Creates a new ACL cell around the given type and initializes it to the default
    /// ACL value for registers on boot.
    pub fn new(inner: T) -> Self {
        AclCell {
            inner,
            value: SECURE_KEY
                | SECURE_READABLE
                | INSECURE_KEY
                | INSECURE_READABLE
                | INSECURE_OVERWRITABLE,
        }
    }

    /// Sets a new value for the encapsulated object.
    pub fn set_value(&mut self, value: T) {
        self.inner = value;
    }

    /// Unconditionally sets an arbitrary ACL value on the cell and overwrites the old one.
    pub fn set_unconditional_acl(&mut self, acl: u8) {
        self.value = acl & 0x1F;
    }
}

impl<T: Copy> Deref for AclCell<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T: Copy> DerefMut for AclCell<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}
