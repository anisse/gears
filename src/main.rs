mod cpu;
mod disas;
mod io;
mod joystick;
mod mem;
mod system;
mod vdp;

use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

struct DebugIO {}
impl io::Device for DebugIO {
    fn out(&self, addr: u16, val: u8) -> Result<(), String> {
        println!("Unknown I/O write: @{:04X} {:02X} ", addr, val);
        panic!();
        Ok(())
    }
    fn input(&self, addr: u16) -> Result<u8, String> {
        println!("Unknown I/O read: @{:04X}, sending 0", addr);
        panic!();
        Ok(0)
    }
}

struct PSGorVDP<'a> {
    vdp: &'a vdp::VDP,
}
impl io::Device for PSGorVDP<'_> {
    fn out(&self, addr: u16, val: u8) -> Result<(), String> {
        println!("Ignored PSG write. @{:04X} {:02X}", addr, val);
        Ok(())
    }
    fn input(&self, addr: u16) -> Result<u8, String> {
        self.vdp.input(addr)
    }
}

pub fn main() {
    let args: Vec<String> = env::args().collect();
    let file = args.get(1).expect("needs an argument");
    let path = Path::new(file);

    let mut file = match File::open(&path) {
        Err(why) => panic!("Cannot open {}: {}", path.display(), why),
        Ok(f) => f,
    };

    let mut data: Vec<u8> = vec![];
    if let Err(why) = file.read_to_end(&mut data) {
        panic!("Could not read {}: {}", path.display(), why);
    }

    let mut cpu = cpu::init();
    cpu.mem = mem::Memory::init(mem::Mapper::SegaGG {
        rom: data,
        backup_ram: None,
    });
    cpu.io = io::IO::new();
    let dbg_io = DebugIO {};
    let vdp = vdp::VDP::default();
    let sys = system::System::default();
    let joy = joystick::Joystick::default();
    let psg = PSGorVDP { vdp: &vdp };
    cpu.io.register(0x7E, 0x7E, 0xFF00, &vdp);
    cpu.io.register(0xBE, 0xBF, 0xFF01, &vdp);
    cpu.io.register(0, 0, 0xFF00, &sys);
    cpu.io.register(0xDC, 0xDD, 0xFF00, &joy);
    cpu.io.register(0x7F, 0x7F, 0xFF00, &psg);
    cpu.io.register(0, 0, 0xFFFF, &dbg_io);
    loop {
        vdp.step();
        cpu::run(&mut cpu, 227, true).unwrap();
    }
}
