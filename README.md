# faucon: NVIDIA Falcon CPU Emulator in Rust

faucon aims to be a spec-compliant emulator of the proprietary NVIDIA Falcon microprocessors,
with a strong focus on a public documentation effort of the processor internals based on
reverse-engineering results.

See [the wiki](https://github.com/vbe0201/faucon/wiki) for documentation on the architecture,
the source code for a reference implementation of the core CPU functionality.

## Components

- [`faucon-asm-derive`](./faucon-asm-derive): Internal implementation details of `faucon-asm`

- [`faucon-asm`](./faucon-asm): A crate for working with the Falcon ISA, effectively providing
a binary disassembler for the emulator

- [`faucon`](./src): The implementation of the CPU emulator

## Setup

Coming soon.

## Contributing

The project is in a very early state and heavily WIP and contributors are welcome.

Contributions to code and documentation are heavily appreciated, may it be a bug fix,
a new feature, or improvement of the code or wiki documentation.

Feel free to join the [Megaton Hammer Discord server](https://discord.gg/MZJbNZY) and
reach out to `Vale#5252` in #mirage.

## Credits

- Marcin Kościelnicki and contributors for the [envytools](https://github.com/envytools/envytools)
project and the [Falcon LLVM backend insight](https://0x04.net/%7Emwk/Falcon.html)

- Switchbrew contributors for the [TSEC wiki page](https://switchbrew.org/wiki/TSEC)

## License

faucon is distributed under the terms of either the Apache License (Version 2.0) or the
MIT license, at the user's choice.

See [LICENSE-APACHE](./LICENSE-APACHE) and [LICENSE-MIT](./LICENSE-MIT) for details.
