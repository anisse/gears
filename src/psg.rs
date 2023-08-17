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
const ATTENUATION_MAX: u8 = REG_DATA_MASK;

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

#[derive(Default, Clone, Copy, Debug)]
enum TonePolarity {
    #[default]
    Neg = 0,
    Pos = 1,
}
impl TonePolarity {
    fn flip(&mut self) {
        // XXX: maybe faster with math ?
        *self = match self {
            TonePolarity::Neg => TonePolarity::Pos,
            TonePolarity::Pos => TonePolarity::Neg,
        }
    }
}

struct Tone {
    reg: u16,
    counter: u16,
    polarity: TonePolarity,
    attenuation: u8,
    right: bool,
    left: bool,
}
impl Default for Tone {
    fn default() -> Self {
        Self {
            reg: 0,
            counter: 0,
            polarity: Default::default(),
            attenuation: ATTENUATION_MAX,
            right: true,
            left: true,
        }
    }
}
impl Tone {
    fn update_level(&mut self, level: u8) {
        self.attenuation = level & REG_DATA_MASK;
    }
    fn update_freq(&mut self, level: u8) {
        self.reg &= (DATA_MASK as u16) << DATA_SHIFT;
        self.reg |= (level as u16) & REG_DATA_MASK as u16;
    }
    fn update_freq_data(&mut self, level: u8) {
        self.reg &= REG_DATA_MASK as u16;
        self.reg |= (level as u16) << DATA_SHIFT;
    }
    fn freq(&mut self) -> Option<usize> {
        if self.reg == 0 {
            None
        } else {
            Some(CPU_CLOCK_HZ / (32 * self.reg as usize))
        }
    }
    fn attenuation_mul(&self) -> f32 {
        // from 0 to 28dB attenuation
        // >>> for i in range(0, 29, 2):
        // print(10.0 ** (-i / 10.0))
        [
            1.0,
            0.6309573444801932,
            0.3981071705534972,
            0.251188643150958,
            0.15848931924611134,
            0.1,
            0.06309573444801933,
            0.039810717055349734,
            0.025118864315095794,
            0.015848931924611134,
            0.01,
            0.00630957344480193,
            0.003981071705534973,
            0.0025118864315095794,
            0.001584893192461114,
            0.0,
        ][(self.attenuation & 0xF) as usize]
    }
    // Set next internal state, advancing the clock by a number of cycles
    fn update_state(&mut self, cycles: u32) {
        let psg_cycles = cycles / 16;
        if (self.counter as u32) > psg_cycles {
            self.counter -= psg_cycles as u16;
            return;
        }
        if self.reg == 0 {
            return;
        }
        let psg_cycles = psg_cycles - self.counter as u32;
        if (psg_cycles / self.reg as u32) % 2 == 0 {
            self.polarity.flip();
        }
        self.counter = (self.reg as u32 - (psg_cycles % self.reg as u32)) as u16;
    }
    fn next_f32_frame(&mut self, dest: &mut [f32], conf: AudioConf) {
        self.update_state(conf.samples_to_cycles_mono(1));
        let sample = self.polarity as i32 as f32 * 0.25 * self.attenuation_mul();
        /*
        println!(
            "Sample: {sample}, attenuation: {}, polarity: {:?}, reg: {}, counter: {}",
            self.attenuation, self.polarity, self.reg, self.counter
        );
        */
        if conf.channels == 2 && dest.len() == 2 {
            dest[0] += if self.left { sample } else { 0.0 };
            dest[1] += if self.right { sample } else { 0.0 };
            return;
        }
        for s in dest.iter_mut() {
            *s += sample;
        }
    }
}
#[test]
fn freq_440() {
    let mut t = Tone {
        reg: 0x0fe,
        ..Default::default()
    };
    t.freq();
    assert_eq!(t.freq(), Some(440));
    t.reg = 0xFFFF;
    t.update_freq(0b1000_1110);
    t.update_freq_data(0b0000_1111);
    assert_eq!(t.freq(), Some(440));
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
    const fn cycles_to_samples(&self, cycles: u32) -> usize {
        (cycles as usize * self.sample_rate as usize * self.channels as usize) / CPU_CLOCK_HZ
    }
    fn cycles_to_ms(&self, cycles: u32) -> f32 {
        (self.cycles_to_samples(cycles) as f32 * 1000.0) / self.sample_rate as f32
    }
    const fn samples_to_cycles(&self, samples: usize) -> u32 {
        ((samples * CPU_CLOCK_HZ) / ((self.sample_rate * self.channels as u32) as usize)) as u32
    }
    const fn samples_to_cycles_mono(&self, samples: usize) -> u32 {
        ((samples * CPU_CLOCK_HZ) / (self.sample_rate as usize)) as u32
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
        for frame in dest.chunks_mut(audio_conf.channels.into()) {
            // for now only tone 0
            frame.fill_with(Default::default);
            self.tone
                .iter_mut()
                .for_each(|t| t.next_f32_frame(frame, audio_conf.clone()));
        }
    }
}

impl PsgState {
    fn synth_audio_f32(&mut self, dest: &mut [f32], audio_conf: AudioConf) {
        let mut current_sample = 0;
        while let Some(cmd) = self.cmds.pop_front() {
            if !matches!(cmd, Cmd::Wait(_)) {
                self.empty_cycles = 0;
            }
            match cmd {
                Cmd::Write(val) => self.synth.cmd(val),
                Cmd::WriteStereo(val) => self.synth.update_stereo(val),
                Cmd::Wait(cycles) => {
                    // First check that we didn't have any empty_cycles to consume
                    if self.empty_cycles > 0 && cycles <= self.empty_cycles {
                        println!(
                            "consuming {cycles} empty cycles out of {}",
                            self.empty_cycles
                        );
                        self.empty_cycles -= cycles;
                        continue;
                    }
                    let cycles = cycles - self.empty_cycles + self.remaining_cycles;
                    // synth
                    let samples = audio_conf.cycles_to_samples(cycles);
                    let end = if current_sample + samples > dest.len() {
                        let remaining = samples - (dest.len() - current_sample);
                        self.cmds
                            .push_front(Cmd::Wait(audio_conf.samples_to_cycles(remaining)));
                        dest.len()
                    } else {
                        current_sample + samples
                    };
                    self.remaining_cycles = cycles - audio_conf.samples_to_cycles(samples);
                    self.empty_cycles = 0;
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
            /*println!(
                "filling {} samples or {}ms",
                dest.len() - current_sample,
                audio_conf.cycles_to_ms(audio_conf.samples_to_cycles(dest.len() - current_sample)),
            );*/
            self.empty_cycles += audio_conf.samples_to_cycles(dest.len() - current_sample);
            self.synth
                .audio_f32(&mut dest[current_sample..], audio_conf.clone());
        }

        println!(
            "{} empty cycles, {} cmds remaining: {} cycles or {}ms",
            self.empty_cycles,
            self.cmds.len(),
            self.wait_cycles(),
            audio_conf.cycles_to_ms(self.wait_cycles()),
        );
        //self.cmds.clear();
    }
    fn wait_cycles(&self) -> u32 {
        self.cmds
            .iter()
            .filter_map(|c| if let Cmd::Wait(x) = c { Some(x) } else { None })
            .sum::<u32>()
    }
}

// Intentionnally structured a bit like the VGM file
#[derive(Clone, Debug)]
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
    empty_cycles: u32,
    remaining_cycles: u32, // basically the opposite of empty_cycles, maybe we should merge the two in a
                           // single signed value
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
    pub fn debug_frame(&self) -> Result<(), String> {
        println!("EOF wait cycles in queue: {}", self.wait_cycles()?);
        Ok(())
    }
    fn wait_cycles(&self) -> Result<u32, String> {
        let state = self
            .state
            .lock()
            .map_err(|e| format!("cannot lock self: {e}"))?;

        Ok(state.wait_cycles())
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
                state.cmds.push_back(Cmd::Wait(elapsed_cycles));
                state.cmds.push_back(match addr & 0xFF {
                    PSG_CMD => Cmd::Write(val),
                    PSG_STEREO => Cmd::WriteStereo(val),
                    _ => unreachable!(),
                });
                /*
                println!(
                    "PSG Write @{addr:04X}: <{elapsed_cycles}> --> [{val}] (len: {})",
                    state.cmds.len()
                );
                */
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
