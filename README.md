# gears
[![Rust](https://github.com/anisse/gears/actions/workflows/rust.yml/badge.svg)](https://github.com/anisse/gears/actions/workflows/rust.yml)

In-progress Game Gear emulator written in Rust.

It has a mostly complete Z80 emulator, that passes [z80test](https://github.com/raxoft/z80test) (1.0 only), [zexall](https://mdfs.net/Software/Z80/Exerciser/), and [FUSE](http://fuse-emulator.sourceforge.net/) tests.


## Current status

 - supports the game gear/master system SEGA rom banking
 - minimal CPU interrupt support
 - a VDP implementation, able to display some games
 - Small "UI" relying on winit+pixels+cpal+gilrs crates that supports input with keyboard and gamepad (both hardcoded bindings)
 - A test suite for the VDP with some ROM frames to prevent regressions
 - WASM target for in-browser emulation (WIP)

## TODO

 - Finish VDP details
   * still missing horizontal interrupt testing (H counter, line completion)
 - Polish the PSG (sound)
   * there is no filtering or downsampling strategy. A low-pass filter should do.
 - WASM is far from complete; lacks a complete UI, just like native.
 - Support more game gear games. Many work, but there might be bugs.
 - It's fast enough but there is a lot of margin for improvement; for example it does allocations in the CPU emulation path which are not needed.
 - Master system support at some point because some game gear cartridges actually shipped the SMS version. It will also be useful to enjoy the wider screen in some infamously hard games on Game Gear (Sonic 2 for example).

## Learned lessons

Over the course of writing this emulator, I took a pause to reflect on some intricacies of the Z80, and gave a [talk on Z80's "last secrets" at FOSDEM 2022](https://archive.fosdem.org/2022/schedule/event/z80/). It's not exhaustive, and there are many more such secrets that have been discovered in the past 10 years by the Z80 emulation community.

I also wrote the following updates:
 * [Talks and emulation: with some emulator progress](https://anisse.astier.eu/talks-emulation.html)
 * [December 2022: rendering bug(s), missing features](https://anisse.astier.eu/gears-update-2023-01.html)
 * [January 2023: fixing the map bug, VDP writes, tilesets and regions](https://anisse.astier.eu/gears-update-2023-02.html)
 * [February to April 2023: interrupts, VDP palette, and audio](https://anisse.astier.eu/gears-update-2023-03.html)
