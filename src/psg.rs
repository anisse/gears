use std::cell::RefCell;

use crate::channel::*;
use crate::io;

const PSG_CMD: u16 = 0x7F;
const PSG_STEREO: u16 = 0x06;
const CPU_CLOCK_HZ: u64 = 3579545;
const CPU_TO_PSG_CLOCK_DIVIDER: u64 = 16;

const REG_MASK: u8 = 0b1000_0000;
const REG_DATA_MASK: u8 = 0b0000_1111;
const DATA_MASK: u8 = 0b0011_1111;
const DATA_SHIFT: usize = 4;
const ATTENUATION_MAX: u8 = REG_DATA_MASK;
const NOISE_TYPE_MASK: u8 = 0b0000_0100;
const NOISE_CLOCK_MASK: u8 = 0b0000_0011;
const NOISE_REG_BASE_VALUE: u16 = 16;
const NOISE_STATE_INITIAL: u16 = 0x8000;
const NOISE_WHITE_TAPPED_BIT_0: u32 = 0;
const NOISE_WHITE_TAPPED_BIT_1: u32 = 3;
const NOISE_OUTPUT: u16 = 1;

const REG_LATCH_MASK: u8 = 0b0111_0000;

const AUDIO_F32_CHANNEL_MAX: f32 = 0.25; // 1.0 / 4

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

#[derive(Default, Clone, Copy, Debug, PartialEq)]
enum TonePolarity {
    #[default]
    Neg = 0,
    Pos = 1,
}

impl std::ops::Not for TonePolarity {
    type Output = Self;

    fn not(self) -> Self::Output {
        // XXX: maybe faster with math ?
        match self {
            TonePolarity::Neg => TonePolarity::Pos,
            TonePolarity::Pos => TonePolarity::Neg,
        }
    }
}
impl TonePolarity {
    fn flip(&mut self) {
        *self = !*self;
    }
}

impl From<u16> for TonePolarity {
    fn from(value: u16) -> Self {
        match value & 1 {
            0 => TonePolarity::Neg,
            1 => TonePolarity::Pos,
            _ => unreachable!(),
        }
    }
}

struct Tone {
    reg: u16,
    counter: u16,
    remaining_millicycles: u16,
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
            remaining_millicycles: 0,
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
        //println!("Updated attenuation to {}", self.attenuation);
    }
    fn update_freq(&mut self, level: u8) {
        self.reg &= (DATA_MASK as u16) << DATA_SHIFT;
        self.reg |= (level as u16) & REG_DATA_MASK as u16;
        //println!("Updated freq hi, now {}", self.reg);
    }
    fn update_freq_data(&mut self, level: u8) {
        self.reg &= REG_DATA_MASK as u16;
        self.reg |= (level as u16) << DATA_SHIFT;
        //println!("Updated freq lo, now {}", self.reg);
    }
    #[allow(dead_code)] // used in test
    fn freq(&mut self) -> Option<u64> {
        if self.reg == 0 {
            None
        } else {
            Some(CPU_CLOCK_HZ / (32 * self.reg as u64))
        }
    }
    fn attenuation_mul(&self) -> f32 {
        // from 0 to 28dB attenuation
        // >>> for i in range(0, 29, 2):
        // print(10.0 ** (-i / 10.0))
        #[allow(clippy::excessive_precision)] // in case we want to change to higher-precision type
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
    // Returns number of psg cycles after last polarity change
    fn update_state(&mut self, psg_cycles: u32) -> Option<u16> {
        if (self.counter as u32) > psg_cycles {
            self.counter -= psg_cycles as u16;
            return None;
        }
        if self.reg == 0 || self.reg == 1 {
            self.polarity = TonePolarity::Pos;
            return None;
        }
        let psg_cycles = psg_cycles - self.counter as u32;
        if (psg_cycles / self.reg as u32) % 2 == 0 {
            self.polarity.flip();
        }
        self.counter = (self.reg as u32 - (psg_cycles % self.reg as u32)) as u16;
        Some(self.reg - self.counter)
    }
    fn consume_frame(&mut self, conf: AudioConf) -> u32 {
        // Work on millicycles to absorb precision issues
        // For some reason using powers of 2 (512, 1024, 2048…) instead 1000 to raise precesion
        // does not yield very good results when testing the FFT of 440.4Hz
        let psg_millicycles = conf.frames_to_psg_cycles(1000) + self.remaining_millicycles as u32;
        let psg_cycles = psg_millicycles / 1000;
        self.remaining_millicycles = (psg_millicycles % 1000) as u16;
        psg_cycles
    }
    fn next_f32_frame(&mut self, dest: &mut [f32], conf: AudioConf) {
        let psg_cycles = self.consume_frame(conf.clone());
        self.update_state(psg_cycles);
        let sample = self.polarity as i32 as f32 * AUDIO_F32_CHANNEL_MAX * self.attenuation_mul();
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

#[derive(Default, Debug)]
enum NoiseType {
    #[default]
    Periodic = 0,
    White = 1,
}

#[derive(Default, Debug)]
enum NoiseToneSource {
    #[default]
    Inner,
    Tone2,
}

#[derive(Default)]
struct Noise {
    tone: Tone,
    state: u16,
    polarity: TonePolarity,
    typ: NoiseType,
    src: NoiseToneSource,
    tone2_polarity: TonePolarity,
}

impl Noise {
    fn update_level(&mut self, level: u8) {
        self.tone.attenuation = level & REG_DATA_MASK;
    }
    fn ctrl(&mut self, ctrl: u8) {
        //println!("Noise ctrl: {ctrl:08b}");
        self.typ = if ctrl & NOISE_TYPE_MASK == 0 {
            NoiseType::Periodic
        } else {
            NoiseType::White
        };
        self.state = NOISE_STATE_INITIAL;
        match ctrl & NOISE_CLOCK_MASK {
            0..=2 => {
                self.src = NoiseToneSource::Inner;
                self.tone.reg = NOISE_REG_BASE_VALUE << (ctrl & NOISE_CLOCK_MASK);
            }
            3 => {
                self.src = NoiseToneSource::Tone2;
                self.tone.reg = 0;
            }
            _ => unreachable!(),
        };
    }
    fn update_lfsr(&mut self) -> u16 {
        let out = self.state & NOISE_OUTPUT;
        self.state = (self.state >> 1)
            | ((((self.state >> NOISE_WHITE_TAPPED_BIT_1) & 1)
                ^ ((self.state >> NOISE_WHITE_TAPPED_BIT_0) & 1))
                << 15);
        out
    }
    fn update_shift(&mut self) -> u16 {
        let out = self.state & NOISE_OUTPUT;
        self.state = self.state.rotate_right(1);
        out
    }
    fn update(&mut self) {
        self.polarity = match self.typ {
            NoiseType::Periodic => self.update_shift(),
            NoiseType::White => self.update_lfsr(),
        }
        .into();
    }
    fn next_f32_frame(&mut self, tone2: &Tone, dest: &mut [f32], conf: AudioConf) {
        let psg_cycles = self.tone.consume_frame(conf.clone());
        match self.src {
            NoiseToneSource::Inner => {
                if self.tone.update_state(psg_cycles).is_some() {
                    if let TonePolarity::Pos = self.tone.polarity {
                        self.update()
                    }
                }
            }
            NoiseToneSource::Tone2 => {
                if self.tone2_polarity != tone2.polarity {
                    self.tone2_polarity = tone2.polarity;
                    if let TonePolarity::Pos = tone2.polarity {
                        self.update();
                    }
                }
            }
        }
        let sample =
            self.polarity as i32 as f32 * AUDIO_F32_CHANNEL_MAX * self.tone.attenuation_mul();
        /*
        println!(
            "Sample: {sample:1.9}, attenuation: {}, polarity: {:?}, src: {:?}, type: {:?}, tone2 pol: {}/{}, SR: {:016b}",
            self.tone.attenuation, self.polarity, self.src, self.typ, self.tone2_polarity as i32, tone2.polarity as i32, self.state,
        );
        */
        if conf.channels == 2 && dest.len() == 2 {
            dest[0] += if self.tone.left { sample } else { 0.0 };
            dest[1] += if self.tone.right { sample } else { 0.0 };
            return;
        }
        for s in dest.iter_mut() {
            *s += sample;
        }
    }
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
            (1, 44100) => Ok(Self {
                // for tests
                channels,
                sample_rate,
            }),
            _ => Err(format!(
                "Unsupported channels={channels}/sample_rate={sample_rate}"
            )),
        }
    }
    #[allow(dead_code)] // used in debug code
    const fn cycles_to_samples(&self, cycles: u64) -> usize {
        (cycles as usize * self.sample_rate as usize * self.channels as usize)
            / CPU_CLOCK_HZ as usize
    }
    const fn cycles_to_frames(&self, cycles: u32) -> usize {
        (cycles as usize * self.sample_rate as usize) / (CPU_CLOCK_HZ as usize)
    }
    const fn frames_to_samples(&self, frames: usize) -> usize {
        frames * self.channels as usize
    }
    fn _cycles_to_ms(&self, cycles: u64) -> f32 {
        (self.cycles_to_samples(cycles) as f32 * 1000.0) / self.sample_rate as f32
    }
    const fn samples_to_cycles(&self, samples: usize) -> u32 {
        ((samples as u64 * CPU_CLOCK_HZ) / ((self.sample_rate * self.channels as u32) as u64))
            as u32
    }
    const fn frames_to_psg_cycles(&self, frames: usize) -> u32 {
        ((frames as u64 * CPU_CLOCK_HZ) / (self.sample_rate as u64 * CPU_TO_PSG_CLOCK_DIVIDER))
            as u32
    }
    pub const fn display_frame_to_samples(&self, fps: usize) -> usize {
        self.sample_rate as usize * self.channels as usize / fps
    }
}

impl Synth {
    fn update_stereo(&mut self, channels: u8) {
        for (i, t) in self.tone.iter_mut().enumerate() {
            t.right = (channels & (1 << i)) != 0;
            t.left = (channels & (1 << (i + 4))) != 0;
        }
        self.noise.tone.right = (channels & (1 << 3)) != 0;
        self.noise.tone.left = (channels & (1 << (3 + 4))) != 0;
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
        if dest.len() % (audio_conf.channels as usize) != 0 {
            println!(
                "Wrong frame alignement, not multiple of channels: {} vs {} channels",
                dest.len(),
                audio_conf.channels
            )
        }
        // handrolled mixer; all channels have a hardcoded 0.25 maximum, so we use the full f32
        // range
        dest.fill(0.0);
        for frame in dest.chunks_mut(audio_conf.channels.into()) {
            self.tone
                .iter_mut()
                .for_each(|t| t.next_f32_frame(frame, audio_conf.clone()));
            self.noise
                .next_f32_frame(&self.tone[2], frame, audio_conf.clone());
        }
    }
}

impl PsgRenderState {
    fn synth_audio_f32(
        &mut self,
        cmds: &ChannelReceiver<Cmd>,
        dest: &mut [f32],
        audio_conf: AudioConf,
    ) {
        let mut current_sample = 0;
        for cmd in cmds.try_iter() {
            if !matches!(cmd, Cmd::Wait(_)) {
                /* Empty cycles are here to consume waits that might be added *before* a command;
                 * If we reach a non-wait command, then we need to reset empty_cycles.
                 */
                self.empty_cycles = 0;
            }
            match cmd {
                Cmd::Write(val) => self.synth.cmd(val),
                Cmd::WriteStereo(val) => self.synth.update_stereo(val),
                Cmd::Wait(cycles) => {
                    // First check that we didn't have any empty_cycles to consume
                    if self.empty_cycles > 0 && cycles <= self.empty_cycles {
                        /*
                        println!(
                            "consuming {cycles} empty cycles out of {}",
                            self.empty_cycles
                        );
                        */
                        self.empty_cycles -= cycles;
                        continue;
                    }
                    let cycles = cycles - self.empty_cycles + self.remaining_cycles;
                    // synth
                    let frames = audio_conf.cycles_to_frames(cycles);
                    let samples = audio_conf.frames_to_samples(frames);
                    let (end, remaining) = if current_sample + samples > dest.len() {
                        let remaining = samples - (dest.len() - current_sample);
                        (dest.len(), audio_conf.samples_to_cycles(remaining))
                    } else {
                        (current_sample + samples, 0)
                    };
                    self.remaining_cycles =
                        cycles - audio_conf.samples_to_cycles(samples) + remaining;
                    self.empty_cycles = 0;
                    self.synth
                        .audio_f32(&mut dest[current_sample..end], audio_conf.clone());
                    current_sample = end;
                }
            }
            if current_sample == dest.len() {
                //println!("Reached end of {} samples buffer", dest.len());
                break;
            }
        }
        if current_sample < dest.len() {
            /*println!(
                "filling {} samples or {}ms",
                dest.len() - current_sample,
                audio_conf
                    ._cycles_to_ms(audio_conf.samples_to_cycles(dest.len() - current_sample) as u64),
            );*/
            self.empty_cycles += audio_conf.samples_to_cycles(dest.len() - current_sample);
            self.synth
                .audio_f32(&mut dest[current_sample..], audio_conf.clone());
        }

        //println!("{} empty cycles", self.empty_cycles);
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
struct PsgRenderState {
    synth: Synth,
    empty_cycles: u32,
    remaining_cycles: u32, // basically the opposite of empty_cycles, maybe we should merge the two in a
                           // single signed value
}
pub struct PsgRender {
    state: RefCell<PsgRenderState>,
    cmds: AudioCmdReceiver,
    audio_conf: AudioConf,
}
impl PsgRender {
    pub fn new(cmds: AudioCmdReceiver, audio_conf: AudioConf) -> PsgRender {
        PsgRender {
            state: RefCell::new(PsgRenderState::default()),
            cmds,
            audio_conf,
        }
    }
    pub fn synth_audio_f32(&self, dest: &mut [f32]) -> Result<(), String> {
        let mut state = self
            .state
            .try_borrow_mut()
            .map_err(|e| format!("cannot lock state: {e}"))?;
        state.synth_audio_f32(&self.cmds.0, dest, self.audio_conf.clone());
        Ok(())
    }
}

pub struct AudioCmdReceiver(ChannelReceiver<Cmd>);
pub struct AudioCmdSender(ChannelSender<Cmd>);

pub fn cmds() -> (AudioCmdSender, AudioCmdReceiver) {
    let (tx, rx) = channel();

    (AudioCmdSender(tx), AudioCmdReceiver(rx))
}
pub struct PsgDevice {
    cmds: AudioCmdSender,
    prev_cycle: RefCell<u32>,
}
impl PsgDevice {
    pub fn new(cmds: AudioCmdSender) -> PsgDevice {
        PsgDevice {
            cmds,
            prev_cycle: RefCell::new(0_u32),
        }
    }
}
impl io::Device for PsgDevice {
    fn out(&self, addr: u16, val: u8, cycle: u32) -> Result<(), String> {
        match addr & 0xFF {
            PSG_CMD | PSG_STEREO => {
                //let mut prev_cycle = self.prev_cycle.borrow_mut();
                let mut prev_cycle = self.prev_cycle.borrow_mut();
                let elapsed_cycles = if cycle >= *prev_cycle {
                    cycle - *prev_cycle
                } else {
                    // overflow, or a huge wait (unlikely and ignored)
                    u32::MAX - (*prev_cycle - cycle)
                };
                *prev_cycle = cycle;
                self.cmds
                    .0
                    .send(Cmd::Wait(elapsed_cycles))
                    .map_err(|e| format!("cannot send wait cmd: {e}"))?;
                self.cmds
                    .0
                    .send(match addr & 0xFF {
                        PSG_CMD => Cmd::Write(val),
                        PSG_STEREO => Cmd::WriteStereo(val),
                        _ => unreachable!(),
                    })
                    .map_err(|e| format!("cannot send cmd: {e}"))?;
                //println!("PSG Write @{addr:04X}: <{elapsed_cycles}> --> [{val}]");
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
