#![deny(clippy::all)]
mod core;

use std::env;
use std::error::Error;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use gears::emu::testcmd;

use core::run;

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let cmds = testcmd::TestCommand::new_vec(args.get(2).unwrap_or(&String::new()))?;
    let file = args.get(1).expect("needs an argument");
    let path = Path::new(file);

    let mut file = match File::open(path) {
        Err(why) => panic!("Cannot open {}: {}", path.display(), why),
        Ok(f) => f,
    };

    let mut data: Vec<u8> = vec![];
    file.read_to_end(&mut data)
        .map_err(|e| format!("Could not read {}: {e}", path.display()))?;
    pollster::block_on(run(&data, &cmds))
}
