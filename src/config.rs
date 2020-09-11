//! Rust wrapper around the faucon configuration file.

use std::fs;
use std::path::Path;

use faucon_emu::memory::PAGE_SIZE;
use hex::FromHex;
use serde::de::Error;
use serde::{Deserialize, Deserializer};

/// Represents a cryptographic key for the AES-128-ECB functionality that the
/// SCP provides.
pub type Key = [u8; 0x10];

/// A wrapper around the faucon tool suite configuration file.
#[derive(Clone, Debug, Deserialize)]
pub struct Config {
    /// Direct control of the core CPU mechanisms.
    pub falcon: Falcon,
    /// Configuration of the PTIMER engine, if available.
    pub ptimer: Option<Ptimer>,
    /// Configuration of the Falcon Secure Co-Processor, if available.
    pub scp: Option<Scp>,
}

impl Config {
    /// Loads a configuration file from a given path and deserializes it
    /// into a [`Config`] instance.
    ///
    /// [`Config`]: struct.Config.html
    pub fn load<P: AsRef<Path>>(path: &P) -> color_eyre::Result<Self> {
        let config = fs::read_to_string(path)?;

        Ok(toml::from_str(&config)?)
    }
}

/// Generic Falcon MCU configuration options.
#[derive(Clone, Debug, Deserialize)]
pub struct Falcon {
    /// The version of the Falcon MCU to be emulated.
    #[serde(deserialize_with = "deserialize_version")]
    pub version: usize,
    /// The size of the Falcon code space.
    imem_size: u32,
    /// The size of the Falcon data space.
    pub dmem_size: u32,
    /// The clock frequency of the Falcon, measured in cycles/second.
    #[serde(deserialize_with = "deserialize_clock_freq")]
    clock_freq: f32,
    /// The Falcon DMA engine configuration.
    pub dma: Dma,
}

impl Falcon {
    /// Gets the correct size of the Falcon code segment.
    ///
    /// Falcon revisions prior to v3 use a linear code segment with byte-oriented
    /// addressing, whereas more recent Falcons use paging with 0x100 byte pages.
    /// Depending on the version, this function returns the total size of the
    /// segment in bytes.
    pub fn get_imem_size(&self) -> u32 {
        if self.version >= 3 {
            self.imem_size * PAGE_SIZE as u32
        } else {
            self.imem_size
        }
    }

    /// Gets the duration of a single clock cycle of the CPU.
    pub fn cycle_duration(&self) -> f32 {
        1.0 / self.clock_freq
    }
}

/// Falcon DMA Engine configuration.
#[derive(Clone, Debug, Deserialize)]
pub struct Dma {
    /// A vector containing available DMA ports to external memory.
    pub ports: Vec<Port>,
}

/// Representation of a Falcon DMA port to be used for doing DMA transfers between
/// internal and external memory.
#[derive(Clone, Debug, Deserialize)]
pub struct Port {
    /// The index of the port.
    #[serde(deserialize_with = "deserialize_port")]
    pub index: usize,
    /// The start address of the corresponding memory region.
    #[serde(rename = "start_addr")]
    pub start_address: u32,
    /// The size of the corresponding memory region.
    pub size: u32,
}

/// Falcon PTIMER engine configuration.
#[derive(Clone, Debug, Deserialize)]
pub struct Ptimer {
    /// The frequency of the integrated periodic timer which sets off interrupts
    /// in the specified interval.
    #[serde(rename = "periodic_freq")]
    pub periodic_frequency: f32,
    /// The frequency of the integrated watchdog timer which can be used to set off
    /// one-shot interrupt events.
    #[serde(rename = "watchdog_freq")]
    pub watchdog_frequency: f32,
}

/// Falcon Secure Co-Processor configuration.
#[derive(Clone, Debug, Deserialize)]
pub struct Scp {
    /// A list of available hardware secrets for cryptographic operations.
    pub secrets: Vec<Secret>,
    /// Customization options for the Heavy Secure Mode authenticating mechanism.
    pub auth: AuthenticationSettings,
}

/// Representation of a Falcon hardware secret.
#[derive(Clone, Debug, Deserialize)]
pub struct Secret {
    /// The index of the hardware secret.
    pub index: u8,
    /// The cryptographic 128-bit AES key provided by the secret.
    #[serde(deserialize_with = "deserialize_aes_key")]
    pub key: Key,
}

/// Settings for customizing the Falcon Heavy Secure Mode authentication mechanism.
#[derive(Clone, Debug, Deserialize)]
pub struct AuthenticationSettings {
    /// Whether the Heavy Secure Mode authentication should be bypassed entirely.
    ///
    /// This is mainly meant for code debugging purposes, but won't work on
    /// hardware without a proper authorization flow. Defaults to `false`.
    #[serde(default = "default_auth_bypass_setting")]
    pub bypass: bool,
    /// An alternative signing key to use over `aes_encrypt(secret(0x1), $c7)`.
    ///
    /// This can be supplied optionally for taking advantage of a similar
    /// behavior as the fake-signing vulnerability on hardware, without having
    /// to obtain plaintext secret 0x1.
    #[serde(default, deserialize_with = "deserialize_optional_aes_key")]
    pub signing_key: Option<Key>,
    /// The hardware secret to use for decrypting code during the Heavy Secure
    /// mode authentication process.
    ///
    /// Defaults to `0x6`, however if a user doesn't have the plaintext of this
    /// secret, an alternative one can be used for debugging purposes. However,
    /// the behavior can't be replicated on hardware without properly encrypting
    /// the code with real secret 0x6.
    #[serde(default = "default_encryption_secret_setting")]
    pub encryption_secret: u8,
}

fn deserialize_version<'de, D>(deserializer: D) -> Result<usize, D::Error>
where
    D: Deserializer<'de>,
{
    let value = toml::Value::deserialize(deserializer)?;

    match usize::deserialize(value) {
        Ok(5) => Ok(5),
        Ok(_) => Err(Error::custom(
            "for the time being, only fuc5 emulation is supported",
        )),
        Err(_) => Err(Error::custom(
            "invalid value for 'version' key in [falcon] config",
        )),
    }
}

fn deserialize_clock_freq<'de, D>(deserializer: D) -> Result<f32, D::Error>
where
    D: Deserializer<'de>,
{
    let value = toml::Value::deserialize(deserializer)?;

    match f32::deserialize(value) {
        Ok(clock_freq) => Ok(clock_freq * 10f32.powf(6.0)),
        Err(_) => Err(Error::custom(
            "invalid value for 'clock_freq' key in [falcon] config",
        )),
    }
}

fn deserialize_port<'de, D>(deserializer: D) -> Result<usize, D::Error>
where
    D: Deserializer<'de>,
{
    let value = toml::Value::deserialize(deserializer)?;

    match usize::deserialize(value) {
        Ok(port @ 0..=7) => Ok(port),
        Ok(_) => Err(Error::custom(
            "only DMA ports from 0 through 7 are supported",
        )),
        Err(_) => Err(Error::custom(
            "invalid value for 'index' field in port object",
        )),
    }
}

fn deserialize_aes_key<'de, D>(deserializer: D) -> Result<Key, D::Error>
where
    D: Deserializer<'de>,
{
    let value = toml::Value::deserialize(deserializer)?;

    match String::deserialize(value) {
        Ok(key) => {
            Ok(<Key>::from_hex(key).expect("cannot decode the string into an 128-bit AES key"))
        }
        Err(_) => Err(Error::custom(
            "invalid value for the field that is supposed to be an 128-bit AES key",
        )),
    }
}

fn deserialize_optional_aes_key<'de, D>(deserializer: D) -> Result<Option<Key>, D::Error>
where
    D: Deserializer<'de>,
{
    let value = Option::<String>::deserialize(deserializer)?;

    match value {
        Some(key) => Ok(Some(
            <Key>::from_hex(key).expect("cannot decode the string into an 128-bit AES key"),
        )),
        None => Err(Error::custom(
            "invalid value for the field that is supposed to be an 128-bit AES key",
        )),
    }
}

const fn default_auth_bypass_setting() -> bool {
    false
}

const fn default_encryption_secret_setting() -> u8 {
    0x6
}
