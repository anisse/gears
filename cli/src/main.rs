use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

use gears::emu;
use gears::emu::testcmd::TestCommand;

fn run_up_to(filename: &Path, cmds: &[TestCommand]) -> Result<u32, String> {
    let path = Path::new(&filename);

    let mut file =
        File::open(path).map_err(|why| format!("Cannot open {}: {}", path.display(), why))?;

    let mut data: Vec<u8> = vec![];
    file.read_to_end(&mut data)
        .map_err(|why| format!("Could not read {}: {}", path.display(), why))?;
    let audio_conf = emu::AudioConf::new(2, 44100)?;
    let (mut emu, mut audio) = emu::Emulator::init(data, true, audio_conf.clone());
    let mut pixels = vec![0; emu::LCD_WIDTH * emu::LCD_HEIGHT * 4];
    let frame = emu.run_commands(&mut pixels, cmds, &mut audio, audio_conf);
    Ok(frame)
}

pub fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();
    let file = args.get(1).expect("needs a file argument");
    let path = Path::new(file);

    let cmds = args.get(2).expect("needs gears testcmd argument");
    let cmds = TestCommand::new_vec(cmds).expect("not a test command format");

    let frame = run_up_to(path, &cmds)?;
    println!("Ran {file} up to frame {frame}");
    Ok(())
}
