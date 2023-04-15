use std::{
    collections::VecDeque,
    sync::{Arc, Mutex},
};

use crate::io;

const PSG_CMD: u16 = 0x7F;
const PSG_STEREO: u16 = 0x06;
const CPU_CLOCK_HZ: usize = 3579545;

const REG_MASK: u8 = 0b1000_0000;
const REG_DATA_MASK: u8 = 0b0000_1111;
const DATA_MASK: u8 = 0b0011_1111;
const DATA_SHIFT: usize = 4;

/*
const REG_TONE_MASK: u8 = 0b1001_0000;
const REG_TONE_FREQ: u8 = 0b1000_0000;
const REG_TONE_LEVEL: u8 = 0b1001_0000;
const REG_TONE_NUM_MASK: u8 = 0b0110_0000;
const REG_TONE_0: u8 = 0b0000_0000;
const REG_TONE_1: u8 = 0b0010_0000;
const REG_TONE_2: u8 = 0b0100_0000;

const REG_NOISE_MASK: u8 = 0b1110_0000;
const REG_NOISE: u8 = 0b1110_0000;
const REG_NOISE_TYPE_MASK: u8 = 0b0000_0100;
const REG_NOISE_NF_MASK: u8 = 0b0000_0011;
*/

const REG_LATCH_MASK: u8 = 0b0111_0000;

#[derive(Default)]
enum Latch {
    #[default]
    Tone0Freq = 0 << 4,
    Tone0Attn = 1 << 4,
    Tone1Freq = 2 << 4,
    Tone1Attn = 3 << 4,
    Tone2Freq = 4 << 4,
    Tone2Attn = 5 << 4,
    NoiseCtrl = 6 << 4,
    NoiseAttn = 7 << 4,
}
use Latch::*;
impl From<u8> for Latch {
    fn from(value: u8) -> Self {
        match (value & REG_LATCH_MASK) >> 4 {
            0 => Tone0Freq,
            1 => Tone0Attn,
            2 => Tone1Freq,
            3 => Tone1Attn,
            4 => Tone2Freq,
            5 => Tone2Attn,
            6 => NoiseCtrl,
            7 => NoiseAttn,
            _ => unreachable!(),
        }
    }
}

#[derive(Default)]
struct Tone {
    phase: usize,
    freq: usize,
    freq_div: u16,
    attenuation: u8,
    right: bool,
    left: bool,
}

impl Tone {
    fn gen(&mut self, data: &mut [f32], conf: AudioConf) {
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
        if self.freq == 0 || self.attenuation == 0x0F {
            return;
        }
        for (i, s) in data.iter_mut().enumerate() {
            let val = ((self.phase + i) * (self.freq) * 2)
                / (conf.sample_rate as usize * conf.channels as usize);
            *s = (val % 2) as f32 / 3.0 - 1.0 / 6.0;
            *s *= 1.0 / (0x0F - self.attenuation) as f32;
        }
        self.phase += data.len();
        let phasem: usize = lcm(self.freq, conf.sample_rate as usize); // XXX: this does not take channels
                                                                       // into account
        self.phase %= phasem;
        //println!("{data:?}");
    }
    fn update_level(&mut self, level: u8) {
        self.attenuation = level & REG_DATA_MASK;
    }
    fn update_freq(&mut self, level: u8) {
        self.freq_div &= (DATA_MASK as u16) << DATA_SHIFT;
        self.freq_div |= (level as u16) & REG_DATA_MASK as u16;
        self.freq();
    }
    fn update_freq_data(&mut self, level: u8) {
        self.freq_div &= REG_DATA_MASK as u16;
        self.freq_div |= (level as u16) << DATA_SHIFT;
        self.freq();
    }
    fn freq(&mut self) {
        if self.freq_div == 0 {
            self.freq = 0;
            return;
        }
        self.freq = CPU_CLOCK_HZ / (32 * self.freq_div as usize);
    }
}
#[test]
fn freq_440() {
    let mut t = Tone {
        freq_div: 0x0fe,
        ..Default::default()
    };
    t.freq();
    assert_eq!(t.freq, 440);
    t.freq = 0;
    t.freq_div = 0xFFFF;
    t.update_freq(0b1000_1110);
    t.update_freq_data(0b0000_1111);
    assert_eq!(t.freq, 440);
}

#[derive(Default)]
enum NoiseType {
    #[default]
    White,
    Periodic,
}

#[derive(Default)]
struct Noise {
    state: u16,
    attenuation: u8,
    right: bool,
    left: bool,
    typ: NoiseType,
}

impl Noise {
    fn update_level(&mut self, level: u8) {
        self.attenuation = level & REG_DATA_MASK;
    }
    fn ctrl(&mut self, ctrl: u8) {}
}

#[derive(Default)]
pub struct Synth {
    tone: [Tone; 3],
    noise: Noise,
    latch: Latch,
}

#[derive(Debug, Clone)]
pub struct AudioConf {
    channels: u16,
    sample_rate: u32,
    //TODO. sample type ?
}
impl AudioConf {
    pub fn new(channels: u16, sample_rate: u32) -> Result<Self, String> {
        match (channels, sample_rate) {
            (2, 44100) => Ok(Self {
                channels,
                sample_rate,
            }),
            _ => Err(format!(
                "Unsupported channels={channels}/sample_rate={sample_rate}"
            )),
        }
    }
    fn cycles_to_samples(&self, cycles: u32) -> usize {
        (cycles as usize * self.sample_rate as usize * self.channels as usize) / CPU_CLOCK_HZ
    }
    fn samples_to_cycles(&self, samples: usize) -> u32 {
        ((samples * CPU_CLOCK_HZ) / ((self.sample_rate * self.channels as u32) as usize)) as u32
    }
}

impl Synth {
    fn update_stereo(&mut self, channels: u8) {
        for (i, t) in self.tone.iter_mut().enumerate() {
            t.right = (channels & (1 << i)) != 0;
            t.left = (channels & (1 << (i + 4))) != 0;
        }
        self.noise.right = (channels & (1 << 3)) != 0;
        self.noise.left = (channels & (1 << (3 + 4))) != 0;
    }
    fn cmd(&mut self, cmd: u8) {
        if cmd & REG_MASK != 0 {
            self.update_reg(cmd);
        } else {
            self.update_data(cmd);
        }
    }
    fn update_reg(&mut self, cmd: u8) {
        self.latch = Latch::from(cmd);
        match self.latch {
            Tone0Freq => self.tone[0].update_freq(cmd),
            Tone0Attn => self.tone[0].update_level(cmd),
            Tone1Freq => self.tone[1].update_freq(cmd),
            Tone1Attn => self.tone[1].update_level(cmd),
            Tone2Freq => self.tone[2].update_freq(cmd),
            Tone2Attn => self.tone[2].update_level(cmd),
            NoiseCtrl => self.noise.ctrl(cmd),
            NoiseAttn => self.noise.update_level(cmd),
        }
    }
    fn update_data(&mut self, cmd: u8) {
        match self.latch {
            Tone0Freq => self.tone[0].update_freq_data(cmd),
            Tone0Attn => self.tone[0].update_level(cmd),
            Tone1Freq => self.tone[1].update_freq_data(cmd),
            Tone1Attn => self.tone[1].update_level(cmd),
            Tone2Freq => self.tone[2].update_freq_data(cmd),
            Tone2Attn => self.tone[2].update_level(cmd),
            NoiseCtrl => self.noise.ctrl(cmd),
            NoiseAttn => self.noise.update_level(cmd),
        }
    }
    fn audio_f32(&mut self, dest: &mut [f32], audio_conf: AudioConf) {
        //self.tone[0].freq = 440;
        self.tone[0].gen(dest, audio_conf);
    }
}

impl PsgState {
    fn synth_audio_f32(&mut self, dest: &mut [f32], audio_conf: AudioConf) {
        let mut current_sample = 0;
        while let Some(cmd) = self.cmds.pop_front() {
            match cmd {
                Cmd::Write(val) => self.synth.cmd(val),
                Cmd::WriteStereo(val) => self.synth.update_stereo(val),
                Cmd::Wait(cycles) => {
                    // synth
                    let samples = audio_conf.cycles_to_samples(cycles);
                    let end = if current_sample + samples > dest.len() {
                        self.cmds.push_back(Cmd::Wait(
                            audio_conf.samples_to_cycles(samples - (dest.len() - current_sample)),
                        ));
                        dest.len()
                    } else {
                        current_sample + samples
                    };
                    self.synth
                        .audio_f32(&mut dest[current_sample..end], audio_conf.clone());
                    current_sample = end;
                }
            }
            if current_sample == dest.len() {
                break;
            }
        }
        if current_sample < dest.len() {
            self.synth
                .audio_f32(&mut dest[current_sample..], audio_conf);
        }

        println!("{} cmds remaining", self.cmds.len());
        //self.cmds.clear();
    }
}

// Intentionnally structured a bit like the VGM file
#[derive(Clone)]
pub(crate) enum Cmd {
    Write(u8),
    WriteStereo(u8),
    Wait(u32), // in cycles, not samples or seconds
}

#[derive(Default)]
struct PsgState {
    cmds: VecDeque<Cmd>,
    synth: Synth,
    prev_cycle: u32,
}
#[derive(Default)]
pub struct Psg {
    state: Mutex<PsgState>,
}
impl Psg {
    pub fn synth_audio_f32(&self, dest: &mut [f32], audio_conf: AudioConf) -> Result<(), String> {
        let mut state = self
            .state
            .lock()
            .map_err(|e| format!("cannot lock self: {e}"))?;
        state.synth_audio_f32(dest, audio_conf);
        Ok(())
    }
}
impl io::Device for Arc<Psg> {
    fn out(&self, addr: u16, val: u8, cycle: u32) -> Result<(), String> {
        match addr & 0xFF {
            PSG_CMD | PSG_STEREO => {
                //let mut prev_cycle = self.prev_cycle.borrow_mut();
                let mut state = self
                    .state
                    .lock()
                    .map_err(|e| format!("cannot lock self: {e}"))?;
                let elapsed_cycles = if cycle >= state.prev_cycle {
                    cycle - state.prev_cycle
                } else {
                    // overflow, or a huge wait (unlikely and ignored)
                    u32::MAX - (state.prev_cycle - cycle)
                };
                state.prev_cycle = cycle;
                // Ignore any wait smaller than half a sample at 44100Hz
                if elapsed_cycles >= (CPU_CLOCK_HZ as u32 / 44100 / 2) {
                    state.cmds.push_back(Cmd::Wait(elapsed_cycles));
                }
                state.cmds.push_back(match addr & 0xFF {
                    PSG_CMD => Cmd::Write(val),
                    PSG_STEREO => Cmd::WriteStereo(val),
                    _ => unreachable!(),
                });
                println!(
                    "PSG Write @{addr:04X}: <{elapsed_cycles}> --> [{val}] (len: {})",
                    state.cmds.len()
                );
                Ok(())
            }
            _ => Err(format!(
                "unknown PSG output address @{addr:04X} ({val:02X} "
            )),
        }
    }
    fn input(&self, addr: u16, _: u32) -> Result<u8, String> {
        panic!("Unsupported PSG read @{:04X}", addr);
    }
}
