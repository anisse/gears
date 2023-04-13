use std::sync::{Arc, Mutex};

use crate::io;

const PSG_CMD: u16 = 0x7F;
const PSG_STEREO: u16 = 0x06;
const CPU_CLOCK_HZ: u32 = 3579545;

const REG_MASK: u8 = 0b1000_0000;

const REG_TONE_MASK: u8 = 0b1001_0000;
const REG_TONE_FREQ: u8 = 0b1000_0000;
const REG_TONE_LEVEL: u8 = 0b1001_0000;
const REG_TONE_NUM_MASK: u8 = 0b0110_0000;
const REG_TONE_0: u8 = 0b0000_0000;
const REG_TONE_1: u8 = 0b0010_0000;
const REG_TONE_2: u8 = 0b0100_0000;

const REG_NOISE_MASK: u8 = 0b1111_0000;
const REG_NOISE: u8 = 0b1110_0000;
const REG_NOISE_TYPE_MASK: u8 = 0b0000_0100;
const REG_NOISE_NF_MASK: u8 = 0b0000_0011;

const REG_LATCH_MASK: u8 = 0b0111_0000;

#[derive(Default)]
enum Latch {
    #[default]
    Tone0Freq = 0 << 4,
    Tone0Level = 1 << 4,
    Tone1Freq = 2 << 4,
    Tone1Level = 3 << 4,
    Tone2Freq = 4 << 4,
    Tone2Level = 5 << 4,
    NoiseCtrl = 6 << 4,
    NoiseLevel = 7 << 4,
}

#[derive(Default)]
struct Tone {
    freq: usize,
    phase: usize,
    level: u16,
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
        for (i, s) in data.iter_mut().enumerate() {
            let val = ((self.phase + i) * (self.freq) * 2)
                / (conf.sample_rate as usize * conf.channels as usize);
            *s = (val % 2) as f32 / 3.0 - 1.0 / 6.0;
        }
        self.phase += data.len();
        let phasem: usize = lcm(self.freq, conf.sample_rate as usize); // XXX: this does not take channels
                                                                       // into account
        self.phase %= phasem;
        //println!("{data:?}");
    }
}

#[derive(Default)]
struct Noise {
    state: u16,
    right: bool,
    left: bool,
    //TODO: type ?
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
    fn update_reg(&mut self, cmd: u16) {}
    fn audio_f32(&mut self, dest: &mut [f32], audio_conf: AudioConf) {
        self.tone[0].freq = 440;
        self.tone[0].gen(dest, audio_conf);
    }
}

impl PsgState {
    fn synth_audio_f32(&mut self, dest: &mut [f32], audio_conf: AudioConf) {
        let mut current_sample = 0;
        while let Some(cmd) = self.cmds.pop() {
            match cmd {
                Cmd::Write(val) => {}
                Cmd::WriteStereo(val) => self.synth.update_stereo(val),
                Cmd::Wait(cycles) => {
                    // synth
                    let samples: usize =
                        (cycles * audio_conf.sample_rate * audio_conf.channels as u32) as usize
                            / CPU_CLOCK_HZ as usize;
                    let end = if current_sample + samples > dest.len() {
                        self.cmds
                            .push(Cmd::Wait((samples - (dest.len() - current_sample)) as u32));
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
    cmds: Vec<Cmd>,
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
                if elapsed_cycles >= (CPU_CLOCK_HZ / 44100 / 2) {
                    state.cmds.push(Cmd::Wait(elapsed_cycles));
                }
                state.cmds.push(match addr & 0xFF {
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