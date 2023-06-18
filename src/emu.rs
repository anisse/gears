pub mod testcmd;

use std::rc::Rc;
use std::sync::Arc;
use std::sync::Mutex;

use crate::cpu;
use crate::devices;
use crate::io;
use crate::joystick;
use crate::mem;
use crate::psg;
use crate::vdp;

use testcmd::TestCommand;

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
pub use psg::AudioConf;

pub struct Emulator {
    cache: cpu::DisasCache,
    cpu: cpu::State,
    devs: Rc<devices::Devices>,
    render_area: vdp::RenderArea,
    over_cycles: isize,
    audio_conf: AudioConf,
    running: Arc<Mutex<bool>>, // TODO: atomic
}

impl Emulator {
    pub fn init(rom: Vec<u8>, visible_only: bool, audio_conf: AudioConf) -> Self {
        let mut emu = Self {
            cache: cpu::DisasCache::init(),
            cpu: cpu::init(),
            devs: Rc::new(devices::Devices::new()),
            render_area: if visible_only {
                vdp::RenderArea::VisibleOnly
            } else {
                vdp::RenderArea::EffectiveArea
            },
            over_cycles: 0,
            audio_conf,
            running: Default::default(),
        };
        emu.cpu.mem = mem::Memory::init(mem::Mapper::SegaGG {
            rom,
            backup_ram: None,
        });
        emu.cpu.io = io::RcDevice::new(emu.devs.clone());
        emu
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
            (CPU_CYCLES_PER_LINE as isize - self.over_cycles) as usize,
            false,
        )
        .unwrap();
        self.over_cycles += cycles as isize - CPU_CYCLES_PER_LINE as isize;
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
            Button::Start => self.devs.sys.set_start_button(true),
            _ => self.devs.joy.set_button(button.into(), true),
        }
    }
    pub fn release(&mut self, button: Button) {
        match button {
            Button::Start => self.devs.sys.set_start_button(false),
            _ => self.devs.joy.set_button(button.into(), false),
        }
    }
    pub fn audio_callback(&self) -> AudioCallback {
        let psg = Arc::clone(&self.devs.pov.psg);
        let audio_conf = self.audio_conf.clone();
        let running = self.running.clone();
        Box::new(move |dest: &mut [f32]| {
            if *running.lock().unwrap() {
                psg.synth_audio_f32(dest, audio_conf.clone())
                    .expect("synth audio failed");
            } else {
                // go back to zero, for silence
                let len = dest.len();
                if len == 0 {
                    return;
                }
                let start = *dest.last().unwrap();
                for (i, sample) in dest.iter_mut().enumerate() {
                    *sample = start * ((len - i - 1) as f32) / len as f32;
                }
            }
        })
    }
    pub fn run(&self, running: bool) {
        *self.running.lock().unwrap() = running;
    }
    pub fn run_commands(&mut self, pixels: &mut [u8], cmds: &[testcmd::TestCommand]) -> u32 {
        let mut frame = 0;
        for cmd in cmds.iter() {
            match cmd {
                TestCommand::WaitFrames(f) => {
                    for _ in 0..*f {
                        while !self.step(pixels) {}
                    }
                    frame += *f
                }
                TestCommand::PressButton(b) => self.press(*b),
                TestCommand::ReleaseButton(b) => self.release(*b),
            }
        }
        frame
    }
}
