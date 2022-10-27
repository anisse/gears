use std::cell::RefCell;

use crate::io;

macro_rules! flag {
    ($enum_name:ident, $($name:ident => $num:literal,)+) =>{
        #[allow(non_camel_case_types, dead_code)]
        enum $enum_name {
            $($name = 1 << $num ,)+
        }
        $(
            #[allow(dead_code)]
            const $name :u8 = $enum_name::$name as u8;
        )+
    };
}

flag!(Status,
    ST_I => 7,
    ST_S9 => 6,
    ST_C => 5,
);

flag!(Reg0,
    REG0_MVS => 7,
    REG0_IE1 => 4,
    REG0_EC => 3,
);

flag!(Reg1,
    REG1_BLANK => 6,
    REG1_IE => 5,
    REG1_SIZE => 1,
);

flag!(PatternName,
    PNAME_RVH => 1,
    PNAME_RVV => 2,
    PNAME_CPT => 3,
    PNAME_PRI => 4,
);

const DEBUG: bool = false;

const SCROLL_SCREEN_WIDTH: usize = 32;
const SCROLL_SCREEN_HEIGHT: usize = 28;
const VISIBLE_AREA_WIDTH: usize = 20;
const VISIBLE_AREA_HEIGHT: usize = 18;
const CHAR_SIZE: u8 = 8;
const VISIBLE_AREA_START_X: usize =
    ((SCROLL_SCREEN_WIDTH - VISIBLE_AREA_WIDTH) / 2) * CHAR_SIZE as usize;
const VISIBLE_AREA_START_Y: usize = 4 * CHAR_SIZE as usize;
const VISIBLE_AREA_END_X: usize = VISIBLE_AREA_WIDTH * CHAR_SIZE as usize + VISIBLE_AREA_START_X;
const VISIBLE_AREA_END_Y: usize = VISIBLE_AREA_HEIGHT * CHAR_SIZE as usize + VISIBLE_AREA_START_Y;

#[derive(Debug, Clone, Copy)]
enum WriteDest {
    Vram,
    Cram,
}
#[derive(Debug)]
struct VDPState {
    status: u8,
    reg: [u8; 11],
    v_counter: u8,
    addr: u16,
    dest: Option<WriteDest>,
    vram: Vec<u8>,
    cram: Vec<u8>,
    cmd_byte1: Option<u8>,
    cram_byte1: Option<u8>,
    vertical_scroll: u8, // We need to save it at the beginning of the frame
}

#[derive(Debug)]
pub struct VDP {
    state: RefCell<VDPState>,
}
#[derive(Debug)]
pub enum VDPInt {
    NoInterrupt,
    InterruptGenerated,
}

#[derive(Debug)]
pub enum VDPDisplay {
    NoDisplay,
    ScreenDone,
}

struct CharSettings {
    char_num: u16,   // max value is 448; character number
    x: u8,           // x coord in destination buffer (in pixels [0; 256[ )
    y: u8,           // y coord in destination buffer (in pixels [0; 224[ )
    line_length: u8, // line length of destination buffer (unit: characters)
    // Rest of the struct should fit an u16, but we won't optimize for now
    src_line: u8, // line number to of char to draw (in pixels, [0; 8[ )
    x_start: u8,  // start x of character (in pixels, [0; 8[)
    x_end: u8,    // end x of character (in pixels, [0; 8] )
    sprite: bool, // whether this character is sprite or background pattern
    rvh: bool,    // character is reversed horizontally
    rvv: bool,    // character is reversed vertically
}

impl VDPState {
    fn character_line(&self, dest: &mut [u8], c: CharSettings) {
        assert!(c.x_start < CHAR_SIZE);
        assert!(c.x_end <= CHAR_SIZE);
        let base = match c.sprite {
            false => {
                if c.char_num >= 448 {
                    // TODO: dump those overflow chars
                    println!(
                        "char overflow: {}, x {} y {} visible ? {}",
                        c.char_num,
                        c.x,
                        c.y,
                        c.x + c.x_end >= VISIBLE_AREA_START_X as u8
                            && c.x + c.x_start < VISIBLE_AREA_END_X as u8
                            && c.y >= VISIBLE_AREA_START_Y as u8
                            && c.y < VISIBLE_AREA_END_Y as u8
                    );
                    //return;
                }
                assert!(c.char_num < 512);
                0
            }
            true => {
                assert!(c.char_num < 256);
                ((self.reg[6] as usize) & 0x4) << 11
            }
        };
        let pallette_base = match c.sprite {
            false => 0,
            true => 32,
        };
        if c.sprite && DEBUG {
            println!(
                "i{}: @{:04X} : {} {}x{}",
                c.char_num, base, pallette_base, c.x, c.y
            );
        }
        let src_line = if c.rvv {
            CHAR_SIZE - 1 - c.src_line
        } else {
            c.src_line
        } as usize;
        for pix in c.x_start..c.x_end {
            let addr: usize = base + c.char_num as usize * 32 + (src_line) * 4;
            let offset = if c.rvh {
                pix % CHAR_SIZE
            } else {
                CHAR_SIZE - 1 - (pix % CHAR_SIZE)
            };
            #[allow(clippy::identity_op)]
            let code = (((self.vram[addr + 3] >> offset) & 1) << 3)
                | (((self.vram[addr + 2] >> offset) & 1) << 2)
                | (((self.vram[addr + 1] >> offset) & 1) << 1)
                | (((self.vram[addr + 0] >> offset) & 1) << 0);
            if c.sprite && code == 0 {
                //transparent
                continue;
            }
            let color_r = (self.cram[pallette_base + code as usize * 2]) & 0xF;
            let color_g = (self.cram[pallette_base + code as usize * 2] >> 4) & 0xF;
            let color_b = (self.cram[pallette_base + code as usize * 2 + 1]) & 0xF;

            let col = pix as usize;
            let x = c.x as usize;
            let y = c.y as usize;
            let line_length = c.line_length as usize * CHAR_SIZE as usize;
            let pix_size = 4; // 4 bytes per pixels, one for each component
            dest[y * line_length * pix_size + (x + col) * pix_size] = color_r << 4;
            dest[y * line_length * pix_size + (x + col) * pix_size + 1] = color_g << 4;
            dest[y * line_length * pix_size + (x + col) * pix_size + 2] = color_b << 4;
            dest[y * line_length * pix_size + (x + col) * pix_size + 3] = 0xFF;
        }
    }
    fn character(
        &self,
        sprite: bool,
        i: usize,
        dest: &mut [u8],
        x: usize,
        y: usize,
        line_length: usize,
    ) {
        for src_line in 0..CHAR_SIZE {
            self.character_line(
                dest,
                CharSettings {
                    char_num: i as u16,
                    x: x as u8,
                    y: y as u8 + src_line,
                    line_length: line_length as u8,
                    src_line,
                    x_start: 0,
                    x_end: CHAR_SIZE,
                    sprite,
                    rvh: false,
                    rvv: false,
                },
            );
        }
    }
    fn render_background_line(&self, pixels: &mut [u8], line: u8, visible_only: bool) {
        // xxx
        let scroll_offset_x = if visible_only {
            self.reg[8] as usize + VISIBLE_AREA_START_X
        } else {
            0
        };
        let scroll_offset_y = if visible_only {
            self.vertical_scroll as usize + VISIBLE_AREA_START_Y
        } else {
            0
        };
        let line_length = if visible_only {
            VISIBLE_AREA_WIDTH as u8
        } else {
            SCROLL_SCREEN_WIDTH as u8
        };

        let pattern_base = ((self.reg[2] as usize) & 0x0E) << 10;

        let scroll_start_y =
            (line as usize + scroll_offset_y) % (SCROLL_SCREEN_HEIGHT * CHAR_SIZE as usize);
        let scroll_start_x = scroll_offset_x % (SCROLL_SCREEN_WIDTH * CHAR_SIZE as usize);

        const CHAR_DESC_LEN: usize = 2;
        let line_length_px = line_length as usize * CHAR_SIZE as usize;

        let mut x = 0;
        loop {
            let ch_start_y = scroll_start_y % CHAR_SIZE as usize;
            let ch_start_x = (scroll_start_x + x) % CHAR_SIZE as usize;
            let ch = ((scroll_start_y - ch_start_y) / CHAR_SIZE as usize * SCROLL_SCREEN_WIDTH
                + (scroll_start_x + x - ch_start_x) / CHAR_SIZE as usize)
                as u16;
            let addr = pattern_base + (ch as usize * CHAR_DESC_LEN);
            let b1 = self.vram[addr + 1];
            let character = self.vram[addr] as u16 | ((b1 as u16 & 0x1) << 8);
            let rvh = b1 & PNAME_RVH != 0;
            let rvv = b1 & PNAME_RVV != 0;
            let palette1 = b1 & PNAME_CPT != 0;
            let prio = b1 & PNAME_PRI != 0;
            if character != 0 && DEBUG {
                println!(
                    "Pattern {:03X}: character {:03X} revh {} revv {} pallette1 {} prio {}",
                    ch, character, rvh, rvv, palette1, prio
                );
            }
            let x_end = if (x + CHAR_SIZE as usize) <= line_length_px {
                CHAR_SIZE
            } else {
                (x % CHAR_SIZE as usize) as u8
            };
            self.character_line(
                pixels,
                CharSettings {
                    char_num: character,
                    x: x as u8,
                    y: line as u8,
                    line_length,
                    src_line: ch_start_y as u8,
                    x_start: ch_start_x as u8,
                    x_end,
                    sprite: false,
                    rvh,
                    rvv,
                },
            );
            x += x_end as usize - ch_start_x;
            if x >= line_length_px as usize {
                break;
            }
        }
    }
    fn render_screen(&self, pixels: &mut [u8]) {
        let pattern_base = ((self.reg[2] as usize) & 0x0E) << 10;
        let sprite_base = ((self.reg[5] as usize) & 0x7E) << 7;

        println!("Double size sprites: {}", self.reg[1] & REG1_SIZE != 0);
        println!("Scroll screen: Pat name @{:04X}", pattern_base);
        //let mut bg = vec![0x0F_u8; 32 * 28 * 8 * 8 * 3];
        let mut chars = [false; 448];
        for line in 0_u8..(SCROLL_SCREEN_HEIGHT as u8 * CHAR_SIZE) {
            self.render_background_line(pixels, line, false);
        }
        println!("Sprites attribute @{:04X}", sprite_base);
        for i in 0_usize..64 {
            let v = self.vram[sprite_base + i];
            let h = self.vram[sprite_base + 0x80 + i * 2];
            let ch = self.vram[sprite_base + 0x80 + i * 2 + 1] as usize;
            chars[ch] = true;
            if h == 0xE0 {
                break;
            }
            //if ch != 0 {
            println!("Sprite {}: {}x{} char {}", i, h, v, ch);
            //}
            if h < 248 && v < 220 {
                if self.reg[1] & REG1_SIZE != 0 {
                    println!("Sprite {}: {}x{} char {}", i, h, v, ch);
                    // double size
                    self.character(true, ch & (!0x1), pixels, h as usize, v as usize, 32);
                    self.character(true, ch | 0x1, pixels, h as usize, v as usize + 8, 32);
                } else {
                    self.character(true, ch, pixels, h as usize, v as usize, 32);
                }
            }
        }
        println!("Characters @0x0000");
        for i in 0_usize..448 {
            if chars[i] {
                //println!("Character {}", i);
                /*
                let mut data = vec![0; 8 * 8 * 3];
                Self::character(self, false, i, &mut data, 0, 0, 1);
                Self::serialize_ppm(format!("./char-{:02X}.ppm", i), 8, 8, &data);
                */
            }
        }
        //Self::serialize_ppm("bg.ppm".to_string(), 32 * 8, 28 * 8, &bg);

        println!("Color RAM: {:?}", self.cram);
    }
    fn write_cmd(&mut self, val: u8) -> Result<(), String> {
        match self.cmd_byte1.take() {
            Some(data) => match val & 0xC0 {
                0x80 => {
                    // This is a register write
                    let reg = val & !0x80;
                    if reg > 10 {
                        panic!("Unexpected high VDP register {:02X}", reg)
                    }
                    //dbg!("Writing to vdp reg", reg, data);
                    self.reg[reg as usize] = data;
                }
                0x00 | 0x40 => {
                    // TODO: differentiate read and write setup ? This is only useful for timings
                    // after setup
                    // VRAM address setup
                    self.addr = data as u16 | ((val as u16 & 0x3F) << 8);
                    self.dest = Some(WriteDest::Vram);
                    //dbg!("setup vram address", self.addr);
                }
                0xC0 => {
                    // cram address setup
                    assert_eq!(val, 0xC0);
                    assert!(data < 64);
                    self.addr = data as u16;
                    self.dest = Some(WriteDest::Cram);
                    //println!("setup cram address: {:02X}", data);
                }
                _ => {
                    panic!(
                        "Unexpected high bits in VDP register selection: {:02X} ({:02X})",
                        val,
                        val & 0xC0
                    )
                }
            },
            None => self.cmd_byte1 = Some(val),
        }
        Ok(())
    }
    fn reset_byte1(&mut self) {
        self.cmd_byte1.take();
    }

    fn write_ram(&mut self, val: u8) -> Result<(), String> {
        self.reset_byte1();
        let addr = self.addr as usize;
        let dest = self.dest;
        let cram_byte1 = self.cram_byte1;
        let ram = match dest {
            Some(WriteDest::Vram) => &mut self.vram,
            Some(WriteDest::Cram) => &mut self.cram,
            None => return Err("No VDP write dest".to_string()),
        };
        if addr > ram.len() {
            return Err(format!(
                "VDP access to {:?} address too high: {:04X} / {:04X}",
                dest,
                addr,
                ram.len()
            ));
        }
        match dest {
            Some(WriteDest::Vram) => {
                ram[addr] = val;
                self.addr = (self.addr + 1) & 0x3FFF;
            }
            Some(WriteDest::Cram) => {
                if addr & 1 == 0 {
                    //println!("Upper CRAM byte set: {:02X}", val);
                    self.cram_byte1 = Some(val);
                } else {
                    ram[addr - 1] = cram_byte1.unwrap();
                    ram[addr] = val & 0x0F;
                    //println!("Written to CRAM: {:02X}{:02X}", ram[addr], ram[addr - 1]);
                }
                self.addr = (self.addr + 1) & 0x3F;
            }
            None => unreachable!(),
        }
        Ok(())
    }
    fn read_ram(&mut self) -> Result<u8, String> {
        self.reset_byte1();
        let addr = self.addr as usize;
        let dest = self.dest;
        match dest {
            Some(WriteDest::Vram) => {}
            Some(WriteDest::Cram) => {
                return Err("Reading from Color RAM is not supported".to_string())
            }
            None => return Err("No VDP write dest".to_string()),
        };
        if addr > self.vram.len() {
            return Err(format!(
                "VDP access to {:?} address too high: {:04X} / {:04X}",
                dest,
                addr,
                self.vram.len()
            ));
        }
        let val = self.vram[addr];
        self.addr = (self.addr + 1) & 0x3FF;
        Ok(val)
    }
    fn read_status(&mut self) -> Result<u8, String> {
        self.reset_byte1();
        let st = self.status;
        self.status = 0; // clear all three flags
        Ok(st)
    }
}
impl VDP {
    pub fn new() -> Self {
        VDP {
            state: RefCell::new(VDPState::default()),
        }
    }

    fn _serialize_ppm(filename: String, width: usize, height: usize, data: &[u8]) {
        {
            let mut f = std::fs::File::create("temp.ppm").unwrap();
            use std::io::Write;
            writeln!(f, "P3").unwrap();
            writeln!(f, "{} {}", width, height).unwrap();
            writeln!(f, "15").unwrap();
            for (i, _) in data.iter().enumerate().step_by(3) {
                writeln!(f, "{} {} {}", data[i], data[i + 1], data[i + 2]).unwrap();
            }
        }
        std::fs::rename("temp.ppm", filename).unwrap();
    }
    pub fn step(&self, pixels: &mut [u8]) -> (VDPInt, VDPDisplay) {
        let mut state = self.state.borrow_mut();
        state.v_counter = state.v_counter.wrapping_add(1);
        let mut rendered = VDPDisplay::NoDisplay;
        if state.v_counter == 0 {
            state.vertical_scroll = state.reg[9];
        }
        if state.v_counter == 0xC0 {
            state.status |= ST_I;
            if state.reg[1] & REG1_BLANK != 0 {
                state.render_screen(pixels);
                rendered = VDPDisplay::ScreenDone;
            }
        }
        if state.reg[0] & REG0_IE1 != 0 {
            println!("Down counter interrupt should be enabled");
        }
        let interrupt = if state.status & ST_I != 0 && state.reg[1] & REG1_IE != 0 {
            VDPInt::InterruptGenerated
        } else {
            VDPInt::NoInterrupt
        };
        (interrupt, rendered)
    }
    /*
    fn reg_side_effect(state: &mut VDPState, reg: usize) {
        match reg {
            1 => {}
            _ => {}
        }
    }
    */
}

impl io::Device for VDP {
    fn out(&self, addr: u16, val: u8) -> Result<(), String> {
        let mut state = self.state.borrow_mut();
        match addr & 0xFF {
            0xBF => state.write_cmd(val),
            0xBE => state.write_ram(val),
            _ => Err(format!(
                "unknown VDP output address @{:04X} ({:02X} ",
                addr, val
            )),
        }
    }
    fn input(&self, addr: u16) -> Result<u8, String> {
        let mut state = self.state.borrow_mut();
        match addr & 0xFF {
            0x7E => {
                let vcounter = state.v_counter;
                Ok(vcounter)
            }
            0xBF => state.read_status(),
            0xBE => state.read_ram(),
            _ => Err(format!("unknown VDP input address @{:04X}", addr)),
        }
    }
}

impl Default for VDP {
    fn default() -> Self {
        Self::new()
    }
}
impl Default for VDPState {
    fn default() -> Self {
        VDPState {
            reg: [0, 0, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0, 0, 0, 1],
            v_counter: 0,
            addr: 0,
            dest: None,
            vram: vec![0; 16 * 1024],
            cram: vec![0; 32 * 2],
            status: 0,
            cmd_byte1: None,
            cram_byte1: None,
            vertical_scroll: 0,
        }
    }
}

#[cfg(test)]
#[test]
fn run_step() {
    use crate::io::Device;

    let vdp = VDP::default();
    let mut pixels = vec![0; 32 * 8 * 28 * 8 * 4];
    vdp.step(&mut pixels);
    assert_eq!(vdp.input(0x7E), Ok(1_u8),);
}
