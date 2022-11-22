use std::rc::Rc;

use crate::cpu;
use crate::io;
use crate::joystick;
use crate::mem;
use crate::system;
use crate::vdp;

pub enum Button {
    Start,
    One,
    Two,
    Up,
    Down,
    Left,
    Right,
}

impl From<Button> for joystick::Button {
    fn from(val: Button) -> Self {
        match val {
            Button::Start => panic!("Unsupported Start button to Joystick conversion"),
            Button::One => joystick::Button::One,
            Button::Two => joystick::Button::Two,
            Button::Up => joystick::Button::Up,
            Button::Down => joystick::Button::Down,
            Button::Left => joystick::Button::Left,
            Button::Right => joystick::Button::Right,
        }
    }
}

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

struct Psg {}
impl io::Device for Psg {
    fn out(&self, _addr: u16, _val: u8) -> Result<(), String> {
        //println!("Ignored PSG write. @{:04X} {:02X}", _addr, _val);
        Ok(())
    }
    fn input(&self, addr: u16) -> Result<u8, String> {
        panic!("Unsupported PSG read @{:04X}", addr);
    }
}
struct PsgOrVdp {
    psg: Rc<Psg>,
    vdp: Rc<vdp::Vdp>,
}
impl io::Device for PsgOrVdp {
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
    render_area: vdp::RenderArea,
}

struct Devices {
    dbg_io: Rc<DebugIO>,
    sys: Rc<system::System>,
    joy: Rc<joystick::Joystick>,
    pov: Rc<PsgOrVdp>,
}
impl Devices {
    pub fn new() -> Self {
        Self {
            dbg_io: Rc::new(DebugIO {}),
            sys: Rc::new(system::System::default()),
            joy: Rc::new(joystick::Joystick::default()),
            pov: Rc::new(PsgOrVdp {
                psg: Rc::new(Psg {}),
                vdp: Rc::new(vdp::Vdp::default()),
            }),
        }
    }
}
impl Emulator {
    pub fn init(rom: Vec<u8>, visible_only: bool) -> Self {
        let mut emu = Self {
            cpu: cpu::init(),
            devs: Devices::new(),
            render_area: if visible_only {
                vdp::RenderArea::VisibleOnly
            } else {
                vdp::RenderArea::EffectiveArea
            },
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
            5,
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
        let (int, render) = self.devs.pov.vdp.step(pixels, self.render_area);
        if let vdp::VdpInt::InterruptGenerated = int {
            //println!("VDP sent an interrupt !");
            cpu::interrupt_mode_1(&mut self.cpu).unwrap();
        }
        cpu::run(&mut self.cpu, 227, false).unwrap();
        if let vdp::VdpDisplay::ScreenDone = render {
            return true;
        }
        false
    }
    pub fn press(&mut self, button: Button) {
        match button {
            Button::Start => (*self.devs.sys).set_start_button(true),
            _ => (*self.devs.joy).set_button(button.into(), true),
        }
    }
    pub fn release(&mut self, button: Button) {
        match button {
            Button::Start => (*self.devs.sys).set_start_button(false),
            _ => (*self.devs.joy).set_button(button.into(), false),
        }
    }
}
