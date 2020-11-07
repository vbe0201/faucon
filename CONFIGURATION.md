# faucon Application Configuration

faucon, especially the emulator component, depends on certain user configuration to be
used for mature code debugging work. Configuration is stored in `.toml` files and can
be loaded from an arbitrary path supplied by the user.

There are two ways to specify the faucon configuration location:

* An environment variable `FAUCON_CONFIG`, specifying the path to the file

* As an argument to faucon via the `--config` flag

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

The size of the DMem segment in SRAM, which is used to store data and the stack. The
specified value must be the total amount of bytes that the segment fits.

```toml
dmem_size = 0x4000
```

#### clock_freq

The clock frequency the CPU operates on. This information is necessary to derive the
duration of a clock cycle for more accurate emulation. The value should be specified
in MHz as a floating-point number.

```toml
clock_freq = 700.123456
```

### [falcon.dma]

Configuration options related to the DMA controller functionality of the Falcon.

#### ports

The external Falcon memory ports that should be emulated. A port is a buffer of memory
that is allocated on the host system, which can be utilised as the source/destination of
Falcon DMA transfers. These ports should be specified as objects, specifying the `index`
(0-7), the emulated `start_addr` and the `size`.
Up to 7 possible ports will be stored in an array by this configuration entry.

```toml
ports = [
    { index = 0, start_addr = 0xDEADBEEF, size = 0x10 },
    { index = 7, start_addr = 0xF00DBABE, size = 0x50 },
]
```

### [ptimer]

Configuration options related to the timers that the Falcon uses and has access to.

**NOTE:** Not all Falcon engines (specifically PGRAPH CTXCTL) have access to PTIMER and
thus the entire timer logic may not be available. For such cases, this entire group of
configuration entries is optional and can be left away.

#### periodic_freq

Controls the frequency of the periodic timer, which is used to set off interrupts
periodically. The frequency should be specified in MHz as a floating-point number.

```toml
periodic_freq = 772.021912
```

#### watchdog_freq

Controls the frequency of the watchdog timer, which is used to set off a one-shot
interrupt at a given point in the future. The frequency should be specified in MHz
as a floating-point number.

```toml
watchdog_freq = 772.081543
```

### [scp]

Configuration options related to the Security Co-Processor of the Falcon.

**NOTE:** Many Falcon engines don't have the SCP hardware for cryptographic operations
and thus the entire group of configuration entries is optional and can be left away.

#### secrets

The Falcon has access to 64 128-bit AES keys which are burned into silicon at factory.
These keys are the basis for most cryptographic operations done by the hardware. faucon
allows for loading in arbitrary secrets for debugging purposes. Missing keys will be
compensated by using sequences of zeroes instead.

Some of the secrets can be obtained in a chain of exploits on the Falcon processor itself
or microcode for it that is authenticated into Heavy Secure Mode. Others are unobtainble
other than through hardware glitching attacks.

```toml
secrets = [
    { index = 0x00, key = "00000000000000000000000000000000" },
    { index = 0x3F, key = "0123456789ABCDEF0123456789ABCDEF" },
]
```

### [scp.auth]

Configuration options related to the Heavy Secure Mode authenticating process that
cryptographically signed microcode can go through.

#### bypass

Controls whether the Heavy Secure Mode authentication should be bypassed entirely. This
allows for arbitrary, unsigned microcode to be granted HS privileges.

Defaults to `false`. Although this option bypasses signing restrictions, encrypted blobs
will still be loaded as garbage data, likely to crash the emulator. This option is mostly
meant for debugging custom code.

```toml
bypass = true
```

#### signing_key

The signing key that should be used to verify code before granting Heavy Secure Mode
privileges.

The algorithm that is used by real Falcon engines to derive this key is
`aes_encrypt(csecret(0x1), "00000000000000000000000000000000")`. Due to ACL protections
on the Falcon keys however, secret 0x1 is unobtainium except with a cumbersome hardware
glitching process.

In regards to the fake-signing vulnerability on the Falcon, which allows for
`aes_encrypt(csecret(0x1), $c7)` with an arbitrary seed in $c7, allowing users to abuse
the `csigenc` instruction to craft a signing key with secret 0x1 and the auth signature
of the currently running secure payload as the seed for $c7, this option fulfils a
similar purpose.

This can also be used to specify the real signing key (obtainable via a certain
undisclosed bootrom vulnerability) used by NVIDIA, if lacking the plaintext of
secret 0x1.

**NOTE:** As stated at the beginning, the official algorithm relies on secret 0x1. This
configuration key is entirely optional, as the algorithm doesn't rely on it. In such a 
case, the 0x1 secret is mandatory.

```toml
signing_key = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
```

#### encryption_secret

Bit 17 in the `$sec` register decides whether the Falcon should decrypt the supplied code
blob first before doing Heavy Secure Mode authentication. Real hardware utilizes secret
0x6 for this process.

The same issue as for the signing_key problem is given here, secret 0x6 is unobtainium
except with glitching. Thus, users can control which secret to use for the crypto. Keep
in mind that encrypted microcode by NVIDIA cannot be emulated in this case.

**NOTE:** The official algorithm relies on secret 0x6. This configuration key is entirely
optional, as the algorithm doesn't rely on it. In such a case, the 0x6 secret is
mandatory.

```toml
encryption_secret = 0x6
```
