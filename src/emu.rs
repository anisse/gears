use std::rc::Rc;
use std::sync::Arc;
use std::sync::Mutex;

use crate::cpu;
use crate::io;
use crate::joystick;
use crate::mem;
use crate::system;
use crate::vdp;

#[derive(Debug, Clone, Copy)]
pub enum Button {
    Start,
    One,
    Two,
    Up,
    Down,
    Left,
    Right,
}

pub const LCD_HEIGHT: usize = vdp::VISIBLE_PIXEL_HEIGHT;
pub const LCD_WIDTH: usize = vdp::VISIBLE_PIXEL_WIDTH;

const CPU_CYCLES_PER_LINE: usize = 228;

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

pub type AudioCallback = Box<dyn FnMut(&mut [f32]) + Send + 'static>;
struct DebugIO {}
impl io::Device for DebugIO {
    fn out(&self, addr: u16, val: u8, _: u32) -> Result<(), String> {
        println!("Unknown I/O write: @{:04X} {:02X} ", addr, val);
        panic!();
        //Ok(())
    }
    fn input(&self, addr: u16, _: u32) -> Result<u8, String> {
        println!("Unknown I/O read: @{:04X}, sending 0", addr);
        panic!();
        //Ok(0)
    }
}

struct Psg {}
impl io::Device for Psg {
    fn out(&self, _addr: u16, _val: u8, _: u32) -> Result<(), String> {
        //println!("Ignored PSG write. @{:04X} {:02X}", _addr, _val);
        Ok(())
    }
    fn input(&self, addr: u16, _: u32) -> Result<u8, String> {
        panic!("Unsupported PSG read @{:04X}", addr);
    }
}
struct PsgOrVdp {
    psg: Rc<Psg>,
    vdp: Rc<vdp::Vdp>,
}
impl io::Device for PsgOrVdp {
    fn out(&self, addr: u16, val: u8, cycle: u32) -> Result<(), String> {
        self.psg.out(addr, val, cycle)
    }
    fn input(&self, addr: u16, cycle: u32) -> Result<u8, String> {
        self.vdp.input(addr, cycle)
    }
}

pub struct Emulator {
    cache: cpu::DisasCache,
    cpu: cpu::State,
    devs: Devices,
    render_area: vdp::RenderArea,
    over_cycles: usize,
    audio_phase: std::sync::Arc<std::sync::Mutex<usize>>,
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
            cache: cpu::DisasCache::init(),
            cpu: cpu::init(),
            devs: Devices::new(),
            render_area: if visible_only {
                vdp::RenderArea::VisibleOnly
            } else {
                vdp::RenderArea::EffectiveArea
            },
            over_cycles: 0,
            audio_phase: Arc::new(Mutex::new(0)),
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
        let cycles = cpu::run_cached(
            &self.cache,
            &mut self.cpu,
            CPU_CYCLES_PER_LINE - self.over_cycles,
            false,
        )
        .unwrap();
        self.over_cycles += cycles - CPU_CYCLES_PER_LINE;
        if let vdp::VdpDisplay::ScreenDone = render {
            return true;
        }
        false
    }
    pub fn vdp_dump_tileset(&self, pixels: &mut [u8]) {
        self.devs.pov.vdp.dump_tileset(pixels)
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
    pub fn audio_callback(&mut self) -> AudioCallback {
        let audio_ctx = Arc::clone(&self.audio_phase);
        Box::new(move |data: &mut [f32]| Emulator::audio_step(audio_ctx.clone(), data))
    }
    //TODO: add sample rate, format, channels
    fn audio_step(audio_phase: Arc<Mutex<usize>>, data: &mut [f32]) {
        const SAMPLE_RATE: usize = 44100;
        let channels = 2;
        let mut audio_phase = audio_phase.lock().unwrap();
        const fn gcd(mut a: usize, mut b: usize) -> usize {
            while a != b {
                if a > b {
                    a -= b;
                } else {
                    b -= a;
                }
            }
            a
        }
        const fn lcm(a: usize, b: usize) -> usize {
            a / gcd(a, b) * b
        }
        const FREQ: usize = 440;
        for (i, s) in data.iter_mut().enumerate() {
            let val = ((*audio_phase + i) * (FREQ) * 2) / (SAMPLE_RATE * channels);
            *s = (val % 2) as f32 / 3.0 - 1.0 / 6.0;
        }
        *audio_phase += data.len();
        const PHASEM: usize = lcm(FREQ, SAMPLE_RATE); // XXX: this does not take channels
                                                      // into account
        *audio_phase %= PHASEM;
        //println!("{data:?}");
    }
}
