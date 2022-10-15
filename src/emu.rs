use std::rc::Rc;

use crate::cpu;
use crate::io;
use crate::joystick;
use crate::mem;
use crate::system;
use crate::vdp;

struct DebugIO {}
impl io::Device for DebugIO {
    fn out(&self, addr: u16, val: u8) -> Result<(), String> {
        println!("Unknown I/O write: @{:04X} {:02X} ", addr, val);
        panic!();
        //Ok(())
    }
    fn input(&self, addr: u16) -> Result<u8, String> {
        println!("Unknown I/O read: @{:04X}, sending 0", addr);
        panic!();
        //Ok(0)
    }
}

struct PSG {}
impl io::Device for PSG {
    fn out(&self, _addr: u16, _val: u8) -> Result<(), String> {
        //println!("Ignored PSG write. @{:04X} {:02X}", _addr, _val);
        Ok(())
    }
    fn input(&self, addr: u16) -> Result<u8, String> {
        panic!("Unsupported PSG read @{:04X}", addr);
    }
}
struct PSGorVDP {
    psg: Rc<PSG>,
    vdp: Rc<vdp::VDP>,
}
impl io::Device for PSGorVDP {
    fn out(&self, addr: u16, val: u8) -> Result<(), String> {
        self.psg.out(addr, val)
    }
    fn input(&self, addr: u16) -> Result<u8, String> {
        self.vdp.input(addr)
    }
}

pub struct Emulator {
    cpu: cpu::State,
    devs: Devices,
}

struct Devices {
    dbg_io: Rc<DebugIO>,
    sys: Rc<system::System>,
    joy: Rc<joystick::Joystick>,
    pov: Rc<PSGorVDP>,
}
impl Devices {
    pub fn new() -> Self {
        Self {
            dbg_io: Rc::new(DebugIO {}),
            sys: Rc::new(system::System::default()),
            joy: Rc::new(joystick::Joystick::default()),
            pov: Rc::new(PSGorVDP {
                psg: Rc::new(PSG {}),
                vdp: Rc::new(vdp::VDP::default()),
            }),
        }
    }
}
impl Emulator {
    pub fn init(rom: Vec<u8>) -> Self {
        let mut emu = Self {
            cpu: cpu::init(),
            devs: Devices::new(),
        };
        emu.cpu.mem = mem::Memory::init(mem::Mapper::SegaGG {
            rom,
            backup_ram: None,
        });
        emu.cpu.io = io::IO::new();
        emu.register();
        emu
    }
    fn register(&mut self) {
        let vdp: Rc<dyn io::Device> = Rc::clone(&self.devs.pov.vdp) as Rc<dyn io::Device>;
        self.cpu.io.register(0x7E, 0x7E, 0xFF00, Rc::clone(&vdp));
        self.cpu.io.register(0xBE, 0xBF, 0xFF01, vdp);
        self.cpu.io.register(
            0,
            0,
            0xFF00,
            Rc::clone(&self.devs.sys) as Rc<dyn io::Device>,
        );
        self.cpu.io.register(
            0xDC,
            0xDD,
            0xFF00,
            Rc::clone(&self.devs.joy) as Rc<dyn io::Device>,
        );
        self.cpu.io.register(
            0x7F,
            0x7F,
            0xFF00,
            Rc::clone(&self.devs.pov) as Rc<dyn io::Device>,
        );
        self.cpu.io.register(
            0x06,
            0x06,
            0xFF00,
            Rc::clone(&self.devs.pov.psg) as Rc<dyn io::Device>,
        );
        self.cpu.io.register(
            0,
            0,
            0xFFFF,
            Rc::clone(&self.devs.dbg_io) as Rc<dyn io::Device>,
        );
    }
    pub fn step(&mut self, pixels: &mut [u8]) -> bool {
        let (int, render) = self.devs.pov.vdp.step(pixels);
        if let vdp::VDPInt::InterruptGenerated = int {
            //println!("VDP sent an interrupt !");
            cpu::interrupt_mode_1(&mut self.cpu).unwrap();
        }
        cpu::run(&mut self.cpu, 227, false).unwrap();
        if let vdp::VDPDisplay::ScreenDone = render {
            return true;
        }
        false
    }
}
