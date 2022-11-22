use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

use gears::emu;

fn common_test(filename: String, mut frame: u64, result: &[u8]) -> Result<bool, String> {
    let path = Path::new(&filename);

    let mut file =
        File::open(&path).map_err(|why| format!("Cannot open {}: {}", path.display(), why))?;

    let mut data: Vec<u8> = vec![];
    file.read_to_end(&mut data)
        .map_err(|why| format!("Could not read {}: {}", path.display(), why))?;
    let mut emu = emu::Emulator::init(data, false);
    let mut pixels = vec![0; 32 * 8 * 28 * 8 * 4];
    assert_eq!(pixels.len(), result.len());
    loop {
        if emu.step(&mut pixels) {
            frame -= 1;
        }
        if frame == 0 {
            return Ok(pixels.iter().eq(result.iter()));
        }
    }
}

#[test]
fn test_roms() {
    let pixels = vec![0; 32 * 8 * 28 * 8 * 4];
    assert_eq!(
        common_test(
            "../roms/Sonic The Hedgehog (World) (Rev 1).gg".to_string(),
            0,
            &pixels
        ),
        Ok(true)
    );
    assert_eq!(
        common_test(
            "../roms/Sonic The Hedgehog (World) (Rev 1).gg".to_string(),
            1,
            &pixels
        ),
        Ok(false)
    );
}
