#![deny(clippy::all)]
use std::env;
use std::error::Error;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use log::{Level, Metadata, Record};

use gears::emu::testcmd;
use gears_ui_core::core::run;

fn main() -> Result<(), Box<dyn Error>> {
    init_log()?;
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
    pollster::block_on(run(&data, &cmds, |_| Ok(())))
}

struct SimpleLogger;
fn init_log() -> Result<(), Box<dyn Error>> {
    log::set_logger(&LOGGER)?;
    log::set_max_level(LOG_LEVEL.to_level_filter());
    Ok(())
}

impl log::Log for SimpleLogger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        metadata.level() <= LOG_LEVEL
    }

    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            println!("{} - {}", record.level(), record.args());
        }
    }

    fn flush(&self) {}
}

static LOGGER: SimpleLogger = SimpleLogger;
static LOG_LEVEL: Level = Level::Info;
