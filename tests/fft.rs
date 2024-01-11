use std::sync::Arc;

use realfft::num_complex::Complex;
use realfft::RealFftPlanner;
use realfft::RealToComplex;

use gears::io::Device;
use gears::psg::{cmds, AudioConf, PsgDevice, PsgRender};

fn sine_wave(freq: f32, sample_rate: u32, duration_secs: u32) -> Vec<f32> {
    let mut buffer = Vec::new();
    let samples_per_period: f64 = sample_rate as f64 / freq as f64;
    for i in 0..(duration_secs * sample_rate) {
        let scale = 0.25;
        let x: f64 = std::f64::consts::PI * 2.0 * i as f64 / samples_per_period;

        let s = (x.sin() * scale) as f32;

        buffer.push(s);
    }
    buffer
}
fn square_wave(freq: f32, sample_rate: u32, duration_secs: u32) -> Vec<f32> {
    let mut buffer = Vec::new();
    let samples_per_period: f32 = sample_rate as f32 / freq;
    dbg!(samples_per_period);
    for i in 0..(duration_secs * sample_rate) {
        // Naive square wave
        if i as f32 % samples_per_period > (samples_per_period / 2.0) {
            buffer.push(0.25);
        } else {
            buffer.push(0.0);
        }
    }
    buffer
}

fn top_frequencies_iter(
    len: usize,
    sample_rate: u32,
    spectrum: &[Complex<f32>],
) -> impl Iterator<Item = (f32, f32)> + '_ {
    fn scale(f: f32, len: usize) -> f32 {
        f * (1.0 / (len as f32).sqrt())
    }
    spectrum
        .iter()
        .enumerate()
        .map(move |(i, num)| {
            let re = scale(num.re, len);
            let im = scale(num.im, len);
            let mag = (re * re + im * im).sqrt();
            let freq = i as f32 * sample_rate as f32 / len as f32;
            (freq, mag)
        })
        .filter(|num| num.1 > 1.0)
}
fn top_frequencies(len: usize, sample_rate: u32, spectrum: &[Complex<f32>]) -> Vec<(f32, f32)> {
    top_frequencies_iter(len, sample_rate, spectrum).collect()
}
fn top_freqs_sorted(len: usize, sample_rate: u32, spectrum: &[Complex<f32>]) -> Vec<(f32, f32)> {
    let mut freqs: Vec<(f32, f32)> = top_frequencies_iter(len, sample_rate, spectrum)
        .skip(1) // 0 bin
        .collect();
    freqs.sort_by(|a, b| b.1.partial_cmp(&a.1).expect("orderable floats"));
    freqs
}

fn sine_wave_fft(
    r2c: &Arc<dyn RealToComplex<f32>>,
    spectrum: &mut [Complex<f32>],
    scratch: &mut [Complex<f32>],
    freq: f32,
    sample_rate: u32,
    duration: u32,
) {
    let mut samples = sine_wave(freq, sample_rate, duration);

    // forward transform the signal
    r2c.process_with_scratch(&mut samples, spectrum, scratch)
        .unwrap();
}

fn square_wave_fft(
    r2c: &Arc<dyn RealToComplex<f32>>,
    spectrum: &mut [Complex<f32>],
    scratch: &mut [Complex<f32>],
    freq: f32,
    sample_rate: u32,
    duration: u32,
) {
    let mut samples = square_wave(freq, sample_rate, duration);

    // forward transform the signal
    r2c.process_with_scratch(&mut samples, spectrum, scratch)
        .unwrap();
}

#[test]
fn test_realfft() {
    // make a planner
    let mut real_planner = RealFftPlanner::<f32>::new();
    const SAMPLE_RATE: u32 = 44100;
    const DURATION_SECS: u32 = 10;
    const LEN: usize = (SAMPLE_RATE * DURATION_SECS) as usize;

    // create a FFT
    let r2c = real_planner.plan_fft_forward(LEN);
    // make a vector for storing the spectrum
    let mut spectrum = r2c.make_output_vec();
    let mut scratch = r2c.make_scratch_vec();

    sine_wave_fft(
        &r2c,
        &mut spectrum,
        &mut scratch,
        440.1,
        SAMPLE_RATE,
        DURATION_SECS,
    );
    let bin_size_hz = SAMPLE_RATE as f32 / LEN as f32;
    assert_eq!(bin_size_hz, 0.1);

    let top = top_frequencies(LEN, SAMPLE_RATE, &spectrum);
    assert_eq!(top.len(), 1);
    assert_eq!(top[0].0, 440.1);

    // Square
    square_wave_fft(
        &r2c,
        &mut spectrum,
        &mut scratch,
        441.0,
        SAMPLE_RATE,
        DURATION_SECS,
    );
    let top = top_frequencies(LEN, SAMPLE_RATE, &spectrum);
    assert_eq!(top[0].0, 0.0); // first 0Hz bin
    assert!(
        (top[1].0 - 441.0).abs() < 0.005,
        "441 Hz detected as {}, magnitude {}",
        top[1].0,
        top[1].1
    );

    // Precision test, mostly for verification
    for i in (1..20000).step_by(30) {
        // reduce step_by for more accurate testing
        sine_wave_fft(
            &r2c,
            &mut spectrum,
            &mut scratch,
            i as f32,
            SAMPLE_RATE,
            DURATION_SECS,
        );
        let top = top_frequencies(LEN, SAMPLE_RATE, &spectrum);
        assert_eq!(top.len(), 1);
        assert!(
            (top[0].0 - i as f32).abs() < 0.005,
            "{i} Hz detected as {}, magnitude {}",
            top[0].0,
            top[0].1
        );
    }
}

fn psgs(conf: AudioConf) -> (PsgDevice, PsgRender) {
    let (tx, rx) = cmds();
    (PsgDevice::new(tx), PsgRender::new(rx, conf))
}

#[test]
fn test_psg_single_freq() {
    const SAMPLE_RATE: u32 = 44100;
    const DURATION_SECS: u32 = 10;
    const LEN: usize = (SAMPLE_RATE * DURATION_SECS) as usize;
    const CPU_CLOCK_HZ: u32 = 3579545;

    let (psg_dev, psg_render) = psgs(AudioConf::new(1, SAMPLE_RATE).unwrap());
    const PSG_CMD: u16 = 0x7F;

    // Set ~440 Hz frequency
    psg_dev.out(PSG_CMD, 0x8E, 0).unwrap();
    psg_dev.out(PSG_CMD, 0x0F, 0).unwrap();
    // Set Tone level max (0 attenuation)
    psg_dev.out(PSG_CMD, 0x90, 0).unwrap();
    // wait 10 seconds
    psg_dev
        .out(PSG_CMD, 0x80, CPU_CLOCK_HZ * DURATION_SECS)
        .unwrap();

    let mut samples = vec![0.0; LEN];
    psg_render.synth_audio_f32(&mut samples).unwrap();
    let mut real_planner = RealFftPlanner::<f32>::new();
    // create a FFT
    let r2c = real_planner.plan_fft_forward(LEN);
    // make a vector for storing the spectrum
    let mut spectrum = r2c.make_output_vec();
    r2c.process(&mut samples, &mut spectrum).unwrap();
    let top = top_freqs_sorted(LEN, SAMPLE_RATE, &spectrum);
    dbg!(&top[..5]);
    assert!(top.len() > 1);
    assert!(
        (top[0].0 - 440.4).abs() < 0.005,
        "440 Hz detected as {}, magnitude {}",
        top[0].0,
        top[0].1
    );
}

#[test]
fn test_psg_ignored_bit() {
    const SAMPLE_RATE: u32 = 44100;
    const DURATION_SECS: u32 = 10;
    const LEN: usize = (SAMPLE_RATE * DURATION_SECS) as usize;
    const CPU_CLOCK_HZ: u32 = 3579545;

    let (psg_dev, psg_render) = psgs(AudioConf::new(1, SAMPLE_RATE).unwrap());
    const PSG_CMD: u16 = 0x7F;

    // Set ~440 Hz frequency
    psg_dev.out(PSG_CMD, 0x8E, 0).unwrap();
    psg_dev.out(PSG_CMD, 0x4F, 0).unwrap();
    // Set Tone level max (0 attenuation)
    psg_dev.out(PSG_CMD, 0x90, 0).unwrap();
    // wait 10 seconds
    psg_dev
        .out(PSG_CMD, 0x80, CPU_CLOCK_HZ * DURATION_SECS)
        .unwrap();

    let mut samples = vec![0.0; LEN];
    psg_render.synth_audio_f32(&mut samples).unwrap();
    let mut real_planner = RealFftPlanner::<f32>::new();
    // create a FFT
    let r2c = real_planner.plan_fft_forward(LEN);
    // make a vector for storing the spectrum
    let mut spectrum = r2c.make_output_vec();
    r2c.process(&mut samples, &mut spectrum).unwrap();
    let top = top_freqs_sorted(LEN, SAMPLE_RATE, &spectrum);
    dbg!(&top[..5]);
    assert!(top.len() > 1);
    assert!(
        (top[0].0 - 440.4).abs() < 0.005,
        "440 Hz detected as {}, magnitude {}",
        top[0].0,
        top[0].1
    );
}

#[test]
fn test_psg_duration() {
    const SAMPLE_RATE: u32 = 44100;
    const DURATION_SECS: u32 = 2;
    const LEN: usize = (SAMPLE_RATE * DURATION_SECS) as usize;
    const HALF_LEN: usize = LEN / 2;
    const CPU_CLOCK_HZ: u32 = 3579545;

    let (psg_dev, psg_render) = psgs(AudioConf::new(1, SAMPLE_RATE).unwrap());
    const PSG_CMD: u16 = 0x7F;

    // Set ~233 Hz frequency
    psg_dev.out(PSG_CMD, 0x80, 0).unwrap();
    psg_dev.out(PSG_CMD, 0x1E, 0).unwrap();
    // Set Tone level max (0 attenuation)
    psg_dev.out(PSG_CMD, 0x90, 0).unwrap();
    // wait 1 second and set the new ~1316 Hz frequency
    for i in (20..(CPU_CLOCK_HZ * DURATION_SECS / 2)).step_by(20) {
        psg_dev.out(PSG_CMD, 0x90, i).unwrap();
    }
    psg_dev
        .out(PSG_CMD, 0x85, CPU_CLOCK_HZ * DURATION_SECS / 2)
        .unwrap();
    psg_dev
        .out(PSG_CMD, 0x05, CPU_CLOCK_HZ * DURATION_SECS / 2)
        .unwrap();
    // Generate new frequency for 1 second
    psg_dev
        .out(PSG_CMD, 0x90, CPU_CLOCK_HZ * DURATION_SECS)
        .unwrap();

    let mut samples = vec![0.0; LEN];
    psg_render.synth_audio_f32(&mut samples).unwrap();
    let mut real_planner = RealFftPlanner::<f32>::new();
    // create a FFT
    let r2c = real_planner.plan_fft_forward(HALF_LEN);
    // make a vector for storing the spectrum
    let mut spectrum = r2c.make_output_vec();
    r2c.process(&mut samples[..HALF_LEN], &mut spectrum)
        .unwrap();
    let top = top_freqs_sorted(HALF_LEN, SAMPLE_RATE, &spectrum);
    dbg!(&top);
    assert!(top.len() > 1);
    assert!(
        (top[0].0 - 233.0).abs() < 0.005,
        "233 Hz detected as {}, magnitude {}",
        top[0].0,
        top[0].1
    );
}

#[test]
fn test_psg_noise_periodic() {
    const SAMPLE_RATE: u32 = 44100;
    const DURATION_SECS: u32 = 10;
    const LEN: usize = (SAMPLE_RATE * DURATION_SECS) as usize;
    const CPU_CLOCK_HZ: u32 = 3579545;

    let (psg_dev, psg_render) = psgs(AudioConf::new(1, SAMPLE_RATE).unwrap());
    const PSG_CMD: u16 = 0x7F;

    // Set ~92 Hz Noise
    // Set noise type periodic, source tone2
    psg_dev.out(PSG_CMD, 0xE3, 0).unwrap();
    // Set Tone2 freq to ~1472Hz ; divided by 16 it will be a noise of ~92Hz
    psg_dev.out(PSG_CMD, 0xCC, 0).unwrap();
    psg_dev.out(PSG_CMD, 0x04, 0).unwrap();
    // Set Tone 4 level max (0 attenuation)
    psg_dev.out(PSG_CMD, 0xF0, 0).unwrap();
    // wait 10 seconds
    psg_dev
        .out(PSG_CMD, 0x80, CPU_CLOCK_HZ * DURATION_SECS)
        .unwrap();

    let mut samples = vec![0.0; LEN];
    psg_render.synth_audio_f32(&mut samples).unwrap();
    let mut real_planner = RealFftPlanner::<f32>::new();
    // create a FFT
    let r2c = real_planner.plan_fft_forward(LEN);
    // make a vector for storing the spectrum
    let mut spectrum = r2c.make_output_vec();
    r2c.process(&mut samples, &mut spectrum).unwrap();
    let top = top_freqs_sorted(LEN, SAMPLE_RATE, &spectrum);
    //dbg!(&top[..5]);
    assert!(top.len() > 1);
    assert!(
        (top[0].0 - 92.0).abs() < 0.005,
        "92 Hz detected as {}, magnitude {}",
        top[0].0,
        top[0].1
    );
}
