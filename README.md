# pdp8-emulator

PDP-8 emulator using R.T.Russell's BBC BASIC SDL.

Currently supports:
- PDP8/e CPU
- 32kW memory
- Console input and output
- Up to 4 RK05 disk units (images compatible with SimH)
- High-speed paper tape input (no output currently)

Features single-stepping, execution trace to screen or text file, capturing screen output to a text file, loading and saving images of the core memory (SimH compatible format).

The emulator is currently good enough to boot OS/8 from the emulated RK05, and has been tested running BASIC version 4A and Colossal Cave Adventure.

Planned features:
- Extended Arithmetic Element emulation
- Other storage media (DECtapes, RX floppies etc)
