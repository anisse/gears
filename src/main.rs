mod cpu;
mod disas;

use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

pub fn main() {
    let args: Vec<String> = env::args().collect();
    let file = args.get(1).expect("needs an argument");
    let path = Path::new(file);

    let mut file = match File::open(&path) {
        Err(why) => panic!("Cannot open {}: {}", path.display(), why),
        Ok(f) => f,
    };

    let mut data: Vec<u8> = vec![];
    if let Err(why) = file.read_to_end(&mut data) {
        panic!("Could not read {}: {}", path.display(), why);
    }

    let mut cpu = cpu::init();
    cpu::run(&mut cpu, &data, 9999999999).unwrap();
}
