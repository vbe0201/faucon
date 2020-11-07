//! Implementation of the Falcon Security Co-Processor and related crypto functionality.

mod acl;
mod aes;
mod mac;

use rand::Rng;

use acl::AclCell;
pub use mac::*;

/// Representation of the Secure Co-Processor of the Falcon.
///
/// It provides support for hardware-accelerated cryptographic tasks on top of AES-ECB.
#[derive(Debug)]
pub struct Scp {
    registers: [AclCell<aes::Block>; 0x8],
    /// The secure mode the SCP is currently in.
    pub mode: SecureMode,
    key_register: Option<usize>,
}

/// Falcon secure modes which limit the access to memory and registers from Falcon
/// microcode.
#[derive(Debug)]
pub enum SecureMode {
    /// Representation of the No Secure mode, the least privileged execution level that
    /// is available to microcode that is not cryptographically signed.
    NoSecure,
    /// Representation of the Light Secure mode, an execution mode with fewer privileges
    /// than [`SecureMode::HeavySecure`], but more than [`SecureMode::NoSecure`].
    ///
    /// Mainly used for debugging purposes. Can be enabled from [`SecureMode::HeavySecure`]
    /// microcode.
    ///
    /// [`SecureMode::HeavySecure`]: enum.SecureMode.html#variant.HeavySecure
    /// [`SecureMode::NoSecure`]: enum.SecureMode.html#variant.NoSecure
    LightSecure,
    /// Representation of the Heavy Secure mode, which effectively locks the Falcon down
    /// into a black box and grants the highest possible amount of privileges.
    ///
    /// Only accessible to cryptographically signed microcode.
    HeavySecure,
}

// TODO: Properly handle ACL values and conditions.

impl Scp {
    /// Creates a new instance of the Secure Co-Processor as an extension to the core
    /// Falcon circuity.
    pub fn new() -> Self {
        Scp {
            registers: [AclCell::new(aes::Block::default()); 0x8],
            mode: SecureMode::NoSecure,
            key_register: None,
        }
    }

    /// Moves the block in the source register into the destination register.
    pub fn mov(&mut self, destination: usize, source: usize) {
        self.registers[destination].set_value(**&self.registers[source]);
    }

    /// Generates a block of random data and stores it in the destination register.
    pub fn rnd(&mut self, destination: usize) {
        self.registers[destination].set_value(rnd());
    }

    /// XORs every byte of the block in the destination register with every byte of the block
    /// in the source register.
    pub fn xor(&mut self, destination: usize, source: usize) {
        let source = &self.registers[source].clone();
        xor(&mut self.registers[destination], &source);
    }

    /// Adds an immediate to the block in the destination register.
    pub fn add(&mut self, destination: usize, source: u8) {
        self.registers[destination].set_value(add(&self.registers[destination], source as u128));
    }

    /// Applies a binary AND to every byte of the block in the destination register with
    /// every byte of the block in the source register.
    pub fn and(&mut self, destination: usize, source: usize) {
        let source = &self.registers[source].clone();
        xor(&mut self.registers[destination], &source);
    }

    /// Reverses the block in the source register and stores the result in the destination
    /// register.
    pub fn rev(&mut self, destination: usize, source: usize) {
        self.registers[destination].set_value(rev(&self.registers[source]));
    }

    /// Performs a Galois field multiplication of the block in the source register and
    /// stores the result in the destination register.
    pub fn gfmul(&mut self, destination: usize, source: usize) {
        self.registers[destination].set_value(gfmul(&self.registers[source]));
    }

    /// Configures a register which should be used to fetch the key for cryptographic
    /// operations.
    pub fn keyreg(&mut self, register: usize) {
        self.key_register = Some(register);
    }

    /// Goes through the AES Key Schedule to generate the last Round Key into the destination
    /// register using the Cipher Key in the source register.
    pub fn kexp(&mut self, destination: usize, source: usize) {
        self.registers[destination].set_value(kexp(&self.registers[source]));
    }

    /// Reverses the AES Key Schedule to generate the Cipher Key into the destination register
    /// using the last Round Key in the source register.
    pub fn krexp(&mut self, destination: usize, source: usize) {
        self.registers[destination].set_value(krexp(&self.registers[source]));
    }

    /// Encrypts the contents of a given source register with the key in the key register and
    /// stores the result to the destination register.
    pub fn enc(&mut self, destination: usize, source: usize) {
        let keyreg = self
            .key_register
            .expect("cannot encrypt a message without a key register");
        self.registers[destination]
            .set_value(enc(&self.registers[keyreg], &self.registers[source]));
    }

    /// Decrypts the contents of a given source register with the key in the key register and
    /// stores the result to the destination register.
    pub fn dec(&mut self, destination: usize, source: usize) {
        let keyreg = self
            .key_register
            .expect("cannot decrypt a message without a key register");
        self.registers[destination]
            .set_value(dec(&self.registers[keyreg], &self.registers[source]));
    }
}

/// Generates a block of random data using a strong RNG source.
pub fn rnd() -> aes::Block {
    rand::thread_rng().gen()
}

/// Overwrites the contents of the `a` block by XORing the contents of `b` into it.
pub fn xor(a: &mut aes::Block, b: &aes::Block) {
    for (x, y) in a.iter_mut().zip(b.iter().cycle()) {
        *x ^= y;
    }
}

/// Adds an immediate to the given block and returns a new block containing the result.
pub fn add(a: &aes::Block, b: u128) -> aes::Block {
    let result = u128::from_le_bytes(*a) + b;
    result.to_le_bytes()
}

/// Overwrites the contents of the `a` block by ANDing the contents of `b` into it.
pub fn and(a: &mut aes::Block, b: &aes::Block) {
    for (x, y) in a.iter_mut().zip(b.iter().cycle()) {
        *x &= y;
    }
}

/// Creates and returns a new block filled it with the byte-reversed input block.
pub fn rev(a: &aes::Block) -> aes::Block {
    let mut b = a.clone();
    b.reverse();

    b
}

/// Performs a multiplication of `a` interpreted as a `u128` by 2 in the [GF(2^8) finite field]
/// defined by the polynomial `x^8 + x^4 + x^3 + x + 1 = 0`.
///
/// [GF(2^8) finite field]: https://en.wikipedia.org/wiki/Finite_field_arithmetic
pub fn gfmul(a: &aes::Block) -> aes::Block {
    let block = u128::from_le_bytes(*a);
    let mut result = block << 1;
    if block & 0x80000000000000000000000000000000 != 0 {
        result ^= 0x1B; // TODO: Was it 0x1B or 0x87?
    }

    result.to_le_bytes()
}

/// Expands the given Cipher Key into the last Round Key, needed for decryption.
pub fn kexp(key: &aes::Key) -> aes::RoundKey {
    let mut round_key = key.clone();
    let mut rcon = 1;

    for _ in 0..10 {
        // Go through the Key Schedule to obtain the next Round Key.
        aes::key_schedule_round(&mut round_key, rcon);

        rcon = aes::gfmul2(rcon);
    }

    round_key
}

/// Walks up the AES Key Schedule to condense the given last Round Key back into
/// the Cipher Key.
pub fn krexp(round_key: &aes::RoundKey) -> aes::Key {
    let mut key = round_key.clone();
    let mut rcon = 0x36;

    for _ in 0..10 {
        // Go through the Key Schedule to obtain the previous Round Key.
        aes::invert_key_schedule_round(&mut key, rcon);

        rcon = aes::gfdiv2(rcon);
    }

    key
}

/// Encrypts a message with 128-bit AES-ECB using the given key.
pub fn enc(key: &aes::Key, message: &aes::Block) -> aes::Block {
    aes::encrypt(key, message)
}

/// Decrypts a message with 128-bit AES-ECB using the given key.
pub fn dec(round_key: &aes::RoundKey, message: &aes::Block) -> aes::Block {
    aes::decrypt(round_key, message)
}
