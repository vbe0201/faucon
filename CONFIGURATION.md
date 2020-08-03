# faucon Application Configuration

faucon, especially the emulator component, depends on certain user configuration to be
used for mature code debugging work. Configuration is stored in `.toml` files and can
be loaded from an arbitrary path supplied by the user.

There are two ways to specify the faucon configuration location:

* 1. An environment variable `FAUCON_CONFIG`, specifying the path to the file

* 2. As an argument to faucon via the `--config` flag

```sh
# Set the environment variable for the faucon configuration.
$ export FAUCON_CONFIG=~/.config/faucon.toml
$ faucon ...

# Pass the configuration file as a CLI argument.
$ faucon --config=faucon.toml ...
```

## Format

The following part of this document describes the individual configuration keys, along
with their purpose and how these values can be obtained.

### [falcon]

Configuration keys pertaining directly to the core logic of the Falcon MCUs.

##### version

The Falcon hardware revision that should be emulated. The only supported value at the
moment is `5`.

**Possible values:** `0`, `3`, `4`, `5`, `6`

```toml
version = 5
```

##### imem_size

The size of the IMem segment in SRAM, which is used to store code. If the Falcon
[`version`](#version) is smaller than `3`, the size should be specified as the
amount of bytes in the segment. For newer revisions, the size represents the amount of
0x100 byte pages in total.

```toml
imem_size = 0x80
```

#### dmem_size

The size of the DMem segment in SRAM, which is used to store data and the stack. The specified
value must be the total amount of bytes that the segment fits.

```toml
dmem_size = 0x4000
```

#### clock_freq

The clock frequency the CPU operates on. This information is necessary to derive the duration of
a clock cycle for more accurate emulation. The value should be specified in MHz as a floating-point
number.

```toml
clock_freq = 700.123456
```

### [ptimer]

Configuration options related to the timers that the Falcon uses and has access to.

**NOTE:** Not all Falcon engines (specifically PGRAPH CTXCTL) have access to PTIMER and thus the
entire timer logic may not be available. For such cases, this entire group of configuration entries
is optional and can be left away.

#### periodic_freq

Controls the frequency of the periodic timer, which is used to set off interrupts periodically. The
frequency should be specified in MHz as a floating-point number.

```toml
periodic_freq = 772.021912
```

#### watchdog_freq

Controls the frequency of the watchdog timer, which is used to set off a one-shot interrupt at a given
point in the future. The frequency should be specified in MHz as a floating-point number.

```toml
watchdog_freq = 772.081543
```
