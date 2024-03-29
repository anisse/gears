use gears::emu;

use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

pub fn main() {
    let args: Vec<String> = env::args().collect();
    let file = args.get(1).expect("needs an argument");
    let path = Path::new(file);

    let mut file = match File::open(path) {
        Err(why) => panic!("Cannot open {}: {}", path.display(), why),
        Ok(f) => f,
    };

    let mut data: Vec<u8> = vec![];
    if let Err(why) = file.read_to_end(&mut data) {
        panic!("Could not read {}: {}", path.display(), why);
    }

    let (mut emu, _) = emu::Emulator::init(data, false, emu::AudioConf::new(2, 44100).unwrap());
    let mut pixels = vec![0; 32 * 8 * 28 * 8 * 4];
    loop {
        emu.step(&mut pixels);
    }
}
