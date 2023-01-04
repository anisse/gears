# gears

In-progress Game Gear emulator written in Rust.

It has a mostly complete Z80 emulator, that passes [z80test](https://github.com/raxoft/z80test), [zexall](https://mdfs.net/Software/Z80/Exerciser/), and [FUSE](http://fuse-emulator.sourceforge.net/) tests.


## Current status

 - supports the game gear/master system rom banking
 - minimal interrupt support
 - Beginning of a VDP implementation, able to display some games
 - Small UI relying on winit+pixels crates that supports input with keyboard and gamepad (both hardcoded bindings)
 - A test suite for the VDP with some ROM frames to prevent regressions

## TODO

 - Finish that VDP
   * still missing the horizontal interrupt (H counter, line completion)
   * fix bug where sonic 1 map screen is scrambled
 - PSG (sound) emulation
 - WASM target. Dependencies should allow that
 - Support a complete game gear game
 - It's fast enough but there is a lot of margin for improvement; for example it does allocations in the CPU emulation path which are not needed.

## Learned lessons

Over the course of writing this emulator, I took a pause to reflect on some intricacies of the Z80, and gave a [talk on Z80's "last secrets" at FOSDEM 2022](https://archive.fosdem.org/2022/schedule/event/z80/). It's not exhaustive, and there are many more such secrets that have been discovered in the past 10 years by the Z80 emulation community.
