use std::sync::{Arc, Mutex};

use crate::io;

const PSG_CMD: u16 = 0x7F;
const PSG_STEREO: u16 = 0x06;
const CPU_CLOCK_HZ: u32 = 3579545;

#[derive(Default)]
struct Tone {
    freq: usize,
    phase: usize,
    level: u16,
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
    //TODO: type ?
}

#[derive(Default)]
pub struct Synth {
    tone: [Tone; 3],
    noise: Noise,
    left_right: u8,
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

impl PsgState {
    fn synth_audio_f32(&mut self, dest: &mut [f32], audio_conf: AudioConf) {
        /*
        for cmd in self.cmds {
            match cmd {
                Cmd::Wait(_) => todo!(),
                Cmd::Write(_) => todo!(),
                Cmd::WriteStereo(_) => todo!(),
            }
        }
        */
        self.synth.tone[0].freq = 440;
        self.synth.tone[0].gen(dest, audio_conf);

        println!("Got {} cmds", self.cmds.len());
        self.cmds.clear();
    }
    fn render_samples_f32_dual_channel(&mut self, dest: &mut [f32]) {}
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
