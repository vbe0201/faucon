//! Rust wrapper around the faucon configuration file.

use std::fs;
use std::path::Path;

use faucon_emu::memory::PAGE_SIZE;
use serde::{de, Deserialize, Deserializer};

/// A wrapper around the faucon tool suite configuration file.
#[derive(Clone, Debug, Deserialize)]
pub struct Config {
    /// Direct control of the core CPU mechanisms.
    pub falcon: Falcon,
    /// Configuration of the PTIMER engine, if available.
    pub ptimer: Option<Ptimer>,
}

impl Config {
    /// Loads a configuration file from a given path and deserializes it
    /// into a [`Config`] instance.
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

fn deserialize_version<'a, D>(deserializer: D) -> Result<usize, D::Error>
where
    D: Deserializer<'a>,
{
    let value = toml::Value::deserialize(deserializer)?;

    match usize::deserialize(value) {
        Ok(5) => Ok(5),
        Ok(_) => Err(de::Error::custom(
            "for the time being, only fuc5 emulation is supported",
        )),
        Err(_) => Err(de::Error::custom(
            "invalid value for 'version' key in [falcon] config",
        )),
    }
}

fn deserialize_clock_freq<'a, D>(deserializer: D) -> Result<f32, D::Error>
where
    D: Deserializer<'a>,
{
    let value = toml::Value::deserialize(deserializer)?;

    match f32::deserialize(value) {
        Ok(clock_freq) => Ok(clock_freq * 10f32.powf(6.0)),
        Err(_) => Err(de::Error::custom(
            "invalid value for 'clock_freq' key in [falcon] config",
        )),
    }
}

fn deserialize_port<'a, D>(deserializer: D) -> Result<usize, D::Error>
where
    D: Deserializer<'a>,
{
    let value = toml::Value::deserialize(deserializer)?;

    match usize::deserialize(value) {
        Ok(port @ 0..=7) => Ok(port),
        Ok(_) => Err(de::Error::custom(
            "only DMA ports from 0 through 7 are supported",
        )),
        Err(_) => Err(de::Error::custom(
            "invalid value for 'index' field in port object",
        )),
    }
}
