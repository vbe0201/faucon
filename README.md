# faucon: NVIDIA Falcon Microprocessor Suite

faucon aims to provide a comprehensive tooling suite for the Falcon microprocessors, including
an assembler,a  disassembler, a CPU emulator for debugging purposes, and documentation on the
architecture. For the time being, faucon explicitly targets the fuc5 generation of these MCUs.

See [the wiki](https://github.com/vbe0201/faucon/wiki) for architecture documentation.

## Components

- [`faucon-asm-derive`](./faucon-asm-derive): Internal implementation details of `faucon-asm`

- [`faucon-asm`](./faucon-asm): A crate for processing Falcon assembly, providing binary
disassembling capabilities

- [`faucon-emu`](./faucon-emu): Implementation of the CPU functionality for emulation

- [`faucon`](./src): Command-line interface for invoking and driving the provided tools

## Setup

Coming soon.

## Contributing

The project is in a very early state and heavily WIP and contributors are welcome.

Contributions to code and documentation are heavily appreciated, may it be a bug fix,
a new feature, or improvement of the code or wiki documentation.

Feel free to join the [Megaton Hammer Discord server](https://discord.gg/MZJbNZY) and
reach out to `Vale#5252` in #mirage.

## Credits

- [Marcelina Kościelnicka](https://github.com/mwkmwkmwk) and contributors for the
[envytools](https://github.com/envytools/envytools)
project and the [Falcon LLVM backend insight](https://0x04.net/%7Emwk/Falcon.html)

- Switchbrew contributors for the [TSEC wiki page](https://switchbrew.org/wiki/TSEC)

- [Thog](https://github.com/Thog) and [hthh](https://github.com/hthh) for the
[ghidra-falcon](https://github.com/Thog/ghidra_falcon) project

## License

faucon is distributed under the terms of either the Apache License (Version 2.0) or the
MIT license, at the user's choice.

See [LICENSE-APACHE](./LICENSE-APACHE) and [LICENSE-MIT](./LICENSE-MIT) for details.
Contributions to the faucon project must be made under the terms of both licenses.
