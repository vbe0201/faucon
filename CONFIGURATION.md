# faucon Application Configuration

faucon, especially the emulator component, depends on certain user configuration to be
used for mature code debugging work. Configuration is stored in `.toml` files and can
be loaded by the user at a given path.

Due to great variations in how individual Falcons are structured, users may have multiple
configuration files and specify the one to be loaded in the command-line options.

## Format

The following part of this document describes the individual configuration keys, along
with their purpose and how these values can be obtained.
