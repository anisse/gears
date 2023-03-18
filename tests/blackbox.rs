use std::fs::{self, File};
use std::io::{self, prelude::*, BufWriter};
use std::path::{Path, PathBuf};

use gears::emu::{self, Button};

use crate::TestCommand::*;

#[derive(Debug, Clone)]
enum TestCommand {
    WaitFrames(u32),
    PressButton(emu::Button),
    ReleaseButton(emu::Button),
}
impl TestCommand {
    fn slice_str(s: &[TestCommand]) -> String {
        s.iter()
            .cloned()
            .map(String::from)
            .collect::<Vec<_>>()
            .join("-")
    }
}
impl TryFrom<&str> for TestCommand {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if let Some(c) = value.chars().next() {
            return Ok(match c {
                '0'..='9' => WaitFrames(value.parse().map_err(|e| format!("not int: {e}"))?),
                'S' => PressButton(Button::Start),
                's' => ReleaseButton(Button::Start),
                'A' => PressButton(Button::One),
                'a' => ReleaseButton(Button::One),
                'B' => PressButton(Button::Two),
                'b' => ReleaseButton(Button::Two),
                'U' => PressButton(Button::Up),
                'u' => ReleaseButton(Button::Up),
                'D' => PressButton(Button::Down),
                'd' => ReleaseButton(Button::Down),
                'L' => PressButton(Button::Left),
                'l' => ReleaseButton(Button::Left),
                'R' => PressButton(Button::Right),
                'r' => ReleaseButton(Button::Right),
                _ => return Err(format!("Unexpected char {c}")),
            });
        }
        Err("empty value".to_string())
    }
}
impl From<TestCommand> for String {
    fn from(value: TestCommand) -> String {
        match value {
            WaitFrames(f) => format!("{f}"),
            PressButton(b) => match b {
                Button::Start => "S",
                Button::One => "A",
                Button::Two => "B",
                Button::Up => "U",
                Button::Down => "D",
                Button::Left => "L",
                Button::Right => "R",
            }
            .to_string(),
            ReleaseButton(b) => match b {
                Button::Start => "s",
                Button::One => "a",
                Button::Two => "b",
                Button::Up => "u",
                Button::Down => "d",
                Button::Left => "l",
                Button::Right => "r",
            }
            .to_string(),
        }
    }
}

fn write_png_common(
    frame_data: &[u8],
    filename: &Path,
    width: u32,
    height: u32,
) -> Result<(), io::Error> {
    let file = File::create(filename)?;
    let mut w = BufWriter::new(file);
    let mut encoder = png::Encoder::new(&mut w, width, height);
    encoder.set_color(png::ColorType::Rgba);
    encoder.set_depth(png::BitDepth::Eight);
    let mut writer = encoder.write_header()?;
    writer.write_image_data(frame_data)?;
    Ok(())
}
fn write_png_lcd(frame_data: &[u8], filename: &Path) -> Result<(), io::Error> {
    write_png_common(
        frame_data,
        filename,
        emu::LCD_WIDTH as u32,
        emu::LCD_HEIGHT as u32,
    )
}

fn read_png(filename: &Path) -> Result<Vec<u8>, io::Error> {
    let decoder = png::Decoder::new(File::open(filename)?);
    let mut reader = decoder.read_info()?;
    // Allocate the output buffer.
    let mut buf = vec![0; reader.output_buffer_size()];
    // Read the next frame. An APNG might contain multiple frames.
    let info = reader.next_frame(&mut buf)?;
    // Only keep the first frame
    buf.truncate(info.buffer_size());
    Ok(buf)
}

fn common_test(filename: &Path, cmds: &[TestCommand], result: &[u8]) -> Result<(), String> {
    let path = Path::new(&filename);

    let mut file =
        File::open(path).map_err(|why| format!("Cannot open {}: {}", path.display(), why))?;

    let mut data: Vec<u8> = vec![];
    file.read_to_end(&mut data)
        .map_err(|why| format!("Could not read {}: {}", path.display(), why))?;
    let mut emu = emu::Emulator::init(data, true);
    let mut pixels = vec![0; emu::LCD_WIDTH * emu::LCD_HEIGHT * 4];
    assert_eq!(pixels.len(), result.len());
    let mut frame = 0;
    for cmd in cmds.iter() {
        match cmd {
            WaitFrames(f) => {
                for _ in 0..*f {
                    while !emu.step(&mut pixels) {}
                }
                frame += *f
            }
            PressButton(b) => emu.press(*b),
            ReleaseButton(b) => emu.release(*b),
        }
    }
    /*
    pixels
        .iter()
        .zip(result.iter())
        .enumerate()
        .filter(|(_, (a, b))| a != b)
        .for_each(|(i, (a, b))| {
            println!(
                "{} - {} {:03}x{:03}: color {} got {a} != {b} (expected)",
                filename.display(),
                TestCommand::slice_str(cmds),
                (i % (emu::LCD_WIDTH * 4)) / 4,
                i / (emu::LCD_WIDTH * 4),
                match i % 4 {
                    0 => "R",
                    1 => "G",
                    2 => "B",
                    3 => "A",
                    _ => unreachable!(),
                },
            )
        });
    */
    if !pixels.iter().eq(result.iter()) {
        let mut outfile = PathBuf::from(path.file_name().unwrap());
        outfile.set_extension(format!("{}.png", TestCommand::slice_str(cmds)));
        write_png_lcd(&pixels, &outfile)
            .map_err(|e| format!("error writing png to {}: {e}", outfile.to_string_lossy()))?;
        outfile.set_extension("TILESET.png");
        let mut tiles = vec![0; 512 * 8 * 8 * 4];
        emu.vdp_dump_tileset(&mut tiles);
        write_png_common(&tiles, &outfile, 16 * 8, 32 * 8)
            .map_err(|e| format!("error writing png to {}: {e}", outfile.to_string_lossy()))?;
        return Err(format!(
            "Result for frame {frame} of {} is not what expected; written to {}",
            filename.display(),
            outfile.display()
        ));
    }
    Ok(())
}

#[test]
fn test_roms() {
    let pixels = vec![0; emu::LCD_HEIGHT * emu::LCD_WIDTH * 4];
    common_test(
        Path::new("../roms/Sonic The Hedgehog (World) (Rev 1).gg"),
        &[WaitFrames(0)],
        &pixels,
    )
    .unwrap();
    let roms_dirs = Path::new("../roms");
    let png_dirs = Path::new("tests/roms-frames");
    let mut res = vec![];
    for path in fs::read_dir(png_dirs).expect("test roms frames") {
        let png_name = path.expect("a file name").path();
        let mut rom_name = PathBuf::from(roms_dirs);
        let basename: Vec<_> = png_name
            .file_name()
            .expect("basename")
            .to_str()
            .unwrap()
            .split('.')
            .collect();
        rom_name.push(basename[0]);
        rom_name.set_extension("gg");
        let rom_data = read_png(&png_name).expect("reading png");
        let frame: Vec<_> = basename[1]
            .split('-')
            .map(|c| c.try_into().expect("a test command"))
            .collect();
        res.push(common_test(&rom_name, &frame, &rom_data));
    }
    res.iter().for_each(|r| {
        if let Err(s) = r {
            println!("{s}");
        }
    });
    assert!(
        res.iter().all(Result::is_ok),
        "Not all test frames are correct, see above"
    );
}
