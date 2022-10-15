# gears

In-progress Game Gear emulator written in Rust.

It has a mostly complete Z80 emulator, that passes [z80test](https://github.com/raxoft/z80test), [zexall](https://mdfs.net/Software/Z80/Exerciser/), and [FUSE](http://fuse-emulator.sourceforge.net/) tests.


## Current status

 - supports the game gear/master system rom banking
 - minimal interrupt support
 - Beginning of a VDP implementation, able to display a few things
 - Small UI relying on winit+pixels crates.

## TODO

 - Finish that VDP
 - PSG (sound) emulation
 - Input handling in UI
 - WASM target. Dependencies should allow that
 - Support a complete game gear game
 - It's fast enough but there is a lot of margin for improvement; for example it does allocations in the emulation path which are not needed.

## Learned lessons

Over the course of writing this emulator, I took a pause to reflect on some intricacies of the Z80, and gave a [talk on Z80's "last secrets" at FOSDEM 2022](https://fosdem.org/2022/schedule/event/z80/). It's not exhaustive, and there are many more such secrets that have been discovered in the past 10 years by the Z80 emulation community.
