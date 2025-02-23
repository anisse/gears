// Low pass audio filter to reduce artifacts of downsampling
//
// This is a second order biquad filter of Direct Form II, transposed:
// https://ccrma.stanford.edu/~jos/filters/Direct_Form_II.html
// https://en.wikipedia.org/wiki/Digital_biquad_filter#Direct_form_2
//
// Inspired by https://github.com/korken89/biquad-rs
//
use core::f32::consts::TAU;

pub struct Coefficients {
    // Unused
    //a0: f32,
    a1: f32,
    a2: f32,
    b0: f32,
    b1: f32,
    b2: f32,
}

const Q_LOWPASS_BUTTERWORTH: f32 = core::f32::consts::FRAC_1_SQRT_2;

// https://webaudio.github.io/Audio-EQ-Cookbook/audio-eq-cookbook.html#LPF-eqn
fn coefficients(sample_rate: f32, center_freq: f32) -> Coefficients {
    // this function could be const if we had a const cosinus and sinus
    let omega0 = TAU * center_freq / sample_rate;
    let cos_omega = omega0.cos();
    let alpha = omega0.sin() / (2.0 * Q_LOWPASS_BUTTERWORTH);

    // Normalize such that a0 = 1
    let a0 = 1.0 + alpha;
    Coefficients {
        //a0: 1.0,
        a1: (-2.0 * cos_omega) / a0,
        a2: (1.0 - alpha) / a0,
        b0: ((1.0 - cos_omega) / 2.0) / a0,
        b1: (1.0 - cos_omega) / a0,
        b2: ((1.0 - cos_omega) / 2.0) / a0,
    }
}

pub struct Filter {
    coeffs: Coefficients,
    z1: f32,
    z2: f32,
}

impl Filter {
    pub fn new(sample_rate: f32, center_freq: f32) -> Self {
        Self {
            coeffs: coefficients(sample_rate, center_freq),
            z1: 0.0,
            z2: 0.0,
        }
    }

    pub fn step(&mut self, x: f32) -> f32 {
        // https://en.wikipedia.org/wiki/Digital_biquad_filter#Transposed_direct_forms
        let y = self.coeffs.b0 * x + self.z1;
        self.z1 = self.z2 + self.coeffs.b1 * x - self.coeffs.a1 * y;
        self.z2 = self.coeffs.b2 * x - self.coeffs.a2 * y;
        y
    }
}
