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
    psg: PSG,
    vdp: vdp::VDP,
}
impl io::Device for PSGorVDP {
    fn out(&self, addr: u16, val: u8) -> Result<(), String> {
        self.psg.out(addr, val)
    }
    fn input(&self, addr: u16) -> Result<u8, String> {
        self.vdp.input(addr)
    }
}

pub struct Emulator<'a> {
    cpu: cpu::State<'a>,
}

pub struct Devices {
    dbg_io: DebugIO,
    sys: system::System,
    joy: joystick::Joystick,
    pov: PSGorVDP,
}
impl Devices {
    pub fn new() -> Self {
        Self {
            dbg_io: DebugIO {},
            sys: system::System::default(),
            joy: joystick::Joystick::default(),
            pov: PSGorVDP {
                psg: PSG {},
                vdp: vdp::VDP::default(),
            },
        }
    }
}
impl<'a> Emulator<'a> {
    pub fn init(rom: Vec<u8>, devs: &'a Devices) -> Self {
        let mut emu = Self { cpu: cpu::init() };
        emu.cpu.mem = mem::Memory::init(mem::Mapper::SegaGG {
            rom,
            backup_ram: None,
        });
        emu.cpu.io = io::IO::new();
        emu.register(devs);
        emu
    }
    fn register(&mut self, devices: &'a Devices) {
        self.cpu.io.register(0x7E, 0x7E, 0xFF00, &devices.pov.vdp);
        self.cpu.io.register(0xBE, 0xBF, 0xFF01, &devices.pov.vdp);
        self.cpu.io.register(0, 0, 0xFF00, &devices.sys);
        self.cpu.io.register(0xDC, 0xDD, 0xFF00, &devices.joy);
        self.cpu.io.register(0x7F, 0x7F, 0xFF00, &devices.pov);
        self.cpu.io.register(0x06, 0x06, 0xFF00, &devices.pov.psg);
        self.cpu.io.register(0, 0, 0xFFFF, &devices.dbg_io);
    }
    pub fn step(&mut self, devices: &Devices) {
        if devices.pov.vdp.step() {
            //println!("VDP sent an interrupt !");
            cpu::interrupt_mode_1(&mut self.cpu).unwrap();
        }
        cpu::run(&mut self.cpu, 227, false).unwrap();
    }
}
