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
    ST_9S => 6,
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

macro_rules! debugln {
    ($($rest:tt)*) => {
        if DEBUG {
            std::println!($($rest)*)
        }
    }
}
const DEBUG: bool = false;
const OVERFLOW_TRACK: bool = false;

const SCROLL_SCREEN_WIDTH: usize = 32;
const SCROLL_SCREEN_HEIGHT: usize = 28;
const VISIBLE_AREA_WIDTH: usize = 20;
const VISIBLE_AREA_HEIGHT: usize = 18;
const CHAR_SIZE: u8 = 8;
const VISIBLE_AREA_START_X: usize =
    ((SCROLL_SCREEN_WIDTH - VISIBLE_AREA_WIDTH) / 2) * CHAR_SIZE as usize;
const VISIBLE_AREA_START_Y: usize = 3 * CHAR_SIZE as usize;
const VISIBLE_AREA_END_X: usize = VISIBLE_AREA_WIDTH * CHAR_SIZE as usize + VISIBLE_AREA_START_X;
const VISIBLE_AREA_END_Y: usize = VISIBLE_AREA_HEIGHT * CHAR_SIZE as usize + VISIBLE_AREA_START_Y;

#[derive(Debug, Clone, Copy)]
enum WriteDest {
    Vram,
    Cram,
}

#[derive(Debug, Clone, Copy)]
pub enum RenderArea {
    VisibleOnly,
    EffectiveArea,
}

#[derive(Debug)]
struct VdpState {
    status: u8,
    reg: [u8; 11],
    v_counter: u8,
    v_counter_jumped: bool,
    addr: u16,
    dest: Option<WriteDest>,
    vram: Vec<u8>,
    cram: Vec<u8>,
    cmd_byte1: Option<u8>,
    cram_byte1: Option<u8>,
    vertical_scroll: u8, // We need to save it at the beginning of the frame
}

#[derive(Debug)]
pub struct Vdp {
    state: RefCell<VdpState>,
}
#[derive(Debug)]
pub enum VdpInt {
    NoInterrupt,
    InterruptGenerated,
}

#[derive(Debug)]
pub enum VdpDisplay {
    NoDisplay,
    ScreenDone,
}

#[derive(Debug, PartialEq)]
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
    #[cfg(feature = "pattern_debug")]
    bg_num: u16,
}
struct Bitmap<T, const N: usize> {
    bits: [T; N],
}
impl<T, const N: usize> Bitmap<T, N>
where
    T: std::ops::BitOrAssign
        + std::ops::Shl<usize, Output = T>
        + std::ops::Shr<usize, Output = T>
        + std::ops::BitOr<Output = T>
        + Default
        + Copy,
{
    const ITEM_SIZE: usize = std::mem::size_of::<T>() * 8;

    #[inline]
    fn new() -> Self {
        Self {
            bits: [T::default(); N],
        }
    }
    #[inline]
    fn set(&mut self, bit_offset: usize, value: T) {
        let offset = bit_offset % (N * Self::ITEM_SIZE);
        let shift = offset % Self::ITEM_SIZE;
        self.bits[offset / Self::ITEM_SIZE] |= value << shift;
        self.bits[(offset / Self::ITEM_SIZE + 1) % N] |=
            (value >> 1) >> (Self::ITEM_SIZE - shift - 1);
    }
    #[inline]
    fn get(&self, bit_offset: usize) -> T {
        let offset = bit_offset % (N * Self::ITEM_SIZE);
        let shift = offset % Self::ITEM_SIZE;
        let e1 = self.bits[offset / Self::ITEM_SIZE];
        let e2 = self.bits[(offset / Self::ITEM_SIZE + 1) % N];
        (e1 >> shift) | ((e2 << 1) << (Self::ITEM_SIZE - shift - 1))
    }
}
impl<T, const N: usize> std::fmt::Debug for Bitmap<T, N>
where
    T: std::fmt::UpperHex,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for i in self.bits.iter() {
            write!(f, "{i:0$X} ", std::mem::size_of::<T>() * 2)?;
        }
        Ok(())
    }
}

impl VdpState {
    #[inline]
    fn color_code(&self, addr: usize, offset: u8) -> u8 {
        #[allow(clippy::identity_op)]
        let c = (((self.vram[addr + 3] >> offset) & 1) << 3)
            | (((self.vram[addr + 2] >> offset) & 1) << 2)
            | (((self.vram[addr + 1] >> offset) & 1) << 1)
            | (((self.vram[addr + 0] >> offset) & 1) << 0);
        c
    }
    fn character_line_sprite_bitmap(&self, c: CharSettings) -> u8 {
        let base = ((self.reg[6] as usize) & 0x4) << 11;
        let src_line = c.src_line as usize;
        let mut char_bitmap: u8 = 0;
        for pix in c.x_start..c.x_end {
            let addr: usize = base + c.char_num as usize * 32 + (src_line) * 4;
            let offset = CHAR_SIZE - 1 - (pix % CHAR_SIZE);
            let code = self.color_code(addr, offset);
            if code == 0 {
                //transparent
                continue;
            }
            char_bitmap |= 1 << pix;
        }
        char_bitmap
    }
    #[inline]
    fn character_line(&self, dest: &mut [u8], c: CharSettings) -> u8 {
        assert!(c.x_start < CHAR_SIZE);
        assert!(c.x_end <= CHAR_SIZE);
        let mut overflow_pause = false;
        let base = match c.sprite {
            false => {
                if c.char_num >= 448 {
                    // TODO: dump those overflow chars
                    let visible = c.x + c.x_end > VISIBLE_AREA_START_X as u8
                        && c.x + c.x_start < VISIBLE_AREA_END_X as u8
                        && c.y >= VISIBLE_AREA_START_Y as u8
                        && c.y < VISIBLE_AREA_END_Y as u8;
                    if OVERFLOW_TRACK && visible {
                        println!(
                            "char overflow: {}, x {} y {} visible ? {}",
                            c.char_num, c.x, c.y, visible,
                        );
                        overflow_pause = true;
                    }
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
        if DEBUG {
            println!(
                "{} char {}: @{:04X} : {} {}x{} (char line length: {}) from ({}->{})x{}",
                if c.sprite { "Sprite" } else { "BG" },
                c.char_num,
                base,
                pallette_base,
                c.x,
                c.y,
                c.line_length,
                c.x_start,
                c.x_end,
                c.src_line
            );
        }
        let src_line = if c.rvv {
            CHAR_SIZE - 1 - c.src_line
        } else {
            c.src_line
        } as usize;
        let mut char_bitmap: u8 = 0;
        for pix in c.x_start..c.x_end {
            let addr: usize = base + c.char_num as usize * 32 + (src_line) * 4;
            let offset = if c.rvh {
                pix % CHAR_SIZE
            } else {
                CHAR_SIZE - 1 - (pix % CHAR_SIZE)
            };
            let code = self.color_code(addr, offset);
            if c.sprite && code == 0 {
                //transparent
                if overflow_pause {
                    println!("skipping sprite !");
                }
                continue;
            }
            char_bitmap |= 1 << pix;
            let color_r = (self.cram[pallette_base + code as usize * 2]) & 0xF;
            let color_g = (self.cram[pallette_base + code as usize * 2] >> 4) & 0xF;
            let color_b = (self.cram[pallette_base + code as usize * 2 + 1]) & 0xF;

            let col = pix as usize - c.x_start as usize;
            let x = c.x as usize;
            let y = c.y as usize;
            let line_length = c.line_length as usize * CHAR_SIZE as usize;
            let pix_size = 4; // 4 bytes per pixels, one for each component

            dest[y * line_length * pix_size + (x + col) * pix_size] = color_r << 4;
            dest[y * line_length * pix_size + (x + col) * pix_size + 1] = color_g << 4;
            dest[y * line_length * pix_size + (x + col) * pix_size + 2] = color_b << 4;
            dest[y * line_length * pix_size + (x + col) * pix_size + 3] = 0xFF;
            if overflow_pause && (col == 0 || col == 7 || src_line == 0 || src_line == 7) {
                dest[y * line_length * pix_size + (x + col) * pix_size] = 0xF3;
                dest[y * line_length * pix_size + (x + col) * pix_size + 1] = 0;
                dest[y * line_length * pix_size + (x + col) * pix_size + 2] = 0;
                dest[y * line_length * pix_size + (x + col) * pix_size + 3] = 0xFF;
            }
            #[cfg(feature = "pattern_debug")]
            if (col == 0 || src_line == 0) {
                dest[y * line_length * pix_size + (x + col) * pix_size] =
                    ((c.char_num >> 4) & 0xFF) as u8;
                dest[y * line_length * pix_size + (x + col) * pix_size + 1] =
                    ((c.char_num & 0xF) as u8) << 4 | (((c.bg_num >> 8) & 0xF) as u8);
                dest[y * line_length * pix_size + (x + col) * pix_size + 2] =
                    (c.bg_num & 0xFF) as u8;
            }
        }
        if overflow_pause {
            std::thread::sleep(std::time::Duration::from_millis(2));
        }
        char_bitmap
    }

    fn render_background_line(&self, pixels: &mut [u8], line: u8, visible_only: bool) {
        let scroll_offset_x = if visible_only {
            (SCROLL_SCREEN_WIDTH * CHAR_SIZE as usize) - (self.reg[8] as usize)
                + VISIBLE_AREA_START_X
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
            let src_x =
                (scroll_start_x + x - ch_start_x) % (SCROLL_SCREEN_WIDTH * CHAR_SIZE as usize);
            let ch = ((scroll_start_y - ch_start_y) / CHAR_SIZE as usize * SCROLL_SCREEN_WIDTH
                + src_x / CHAR_SIZE as usize) as u16;
            let addr = pattern_base + (ch as usize * CHAR_DESC_LEN);
            let b1 = self.vram[addr + 1];
            let character = self.vram[addr] as u16 | ((b1 as u16 & 0x1) << 8);
            let rvh = b1 & PNAME_RVH != 0;
            let rvv = b1 & PNAME_RVV != 0;
            let palette1 = b1 & PNAME_CPT != 0;
            let prio = b1 & PNAME_PRI != 0;
            if character != 0 && DEBUG {
                println!(
                    "BG src {}x{} dest {}x{} Pattern: {:03X} @{:04X} (base @{:04X} character {:03X} revh {} revv {} pallette1 {} prio {}",
                    src_x, scroll_start_y - ch_start_y, x, line,
                    ch, addr, pattern_base,
                    character, rvh, rvv, palette1, prio
                );
            }
            let x_end = if (x + CHAR_SIZE as usize) <= line_length_px {
                CHAR_SIZE
            } else {
                CHAR_SIZE - (x % CHAR_SIZE as usize) as u8
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
                    #[cfg(feature = "pattern_debug")]
                    bg_num: ch,
                },
            );
            x += x_end as usize - ch_start_x;
            if x >= line_length_px as usize {
                break;
            }
        }
    }
    fn sprite_double_size_src_line(double_size: bool, char_num: u16, ldiff: u8) -> (u16, u8) {
        if !double_size {
            // simple size
            (char_num, ldiff)
        } else {
            // double size
            if ldiff < CHAR_SIZE {
                //upper sprite
                (char_num & (!0x1), ldiff)
            } else {
                // lower sprite
                (char_num | 0x1, ldiff - CHAR_SIZE)
            }
        }
    }
    #[allow(clippy::too_many_arguments)]
    #[inline]
    fn sprite_line_settings_visible(
        double_size: bool,
        vline: u8,
        char_num: u16,
        v: u8,
        h: u8,
    ) -> CharSettings {
        let line_length = VISIBLE_AREA_WIDTH as u8;
        let x = std::cmp::max(h as i16 - VISIBLE_AREA_START_X as i16, 0) as u8;
        let x_start = std::cmp::max(VISIBLE_AREA_START_X as i16 - h as i16, 0) as u8;
        let x_end = std::cmp::min(
            ((VISIBLE_AREA_WIDTH as i16 * CHAR_SIZE as i16) - x as i16) as u8,
            CHAR_SIZE,
        );
        let ldiff = vline - v;
        let (char_num, src_line) = Self::sprite_double_size_src_line(double_size, char_num, ldiff);
        CharSettings {
            char_num,
            x,
            y: vline - VISIBLE_AREA_START_Y as u8,
            line_length,
            src_line,
            x_start,
            x_end,
            sprite: true,
            rvh: false,
            rvv: false,
            #[cfg(feature = "pattern_debug")]
            bg_num: 0,
        }
    }
    #[allow(clippy::too_many_arguments)]
    #[inline]
    fn sprite_line_settings_effective(
        double_size: bool,
        vline: u8,
        char_num: u16,
        v: u8,
        h: u8,
    ) -> CharSettings {
        let line_length = SCROLL_SCREEN_WIDTH as u8;
        let x = h as u8;
        let x_start = 0;
        let x_end = std::cmp::max(
            CHAR_SIZE as i16
                - (x as i16 + CHAR_SIZE as i16)
                - (SCROLL_SCREEN_WIDTH * CHAR_SIZE as usize) as i16,
            CHAR_SIZE as i16,
        ) as u8;
        let ldiff = vline - v;
        let (char_num, src_line) = Self::sprite_double_size_src_line(double_size, char_num, ldiff);
        CharSettings {
            char_num,
            x,
            y: vline,
            line_length,
            src_line,
            x_start,
            x_end,
            sprite: true,
            rvh: false,
            rvv: false,
            #[cfg(feature = "pattern_debug")]
            bg_num: 0,
        }
    }
    #[allow(clippy::too_many_arguments)]
    fn render_sprites_line(
        &mut self,
        pixels: &mut [u8],
        vcoord_line: u8,
        invisible: fn(u8) -> bool,
        sprite_settings: fn(bool, u8, u16, u8, u8) -> CharSettings,
    ) {
        let sprite_base = ((self.reg[5] as usize) & 0x7E) << 7;
        let sprite_height = if self.reg[1] & REG1_SIZE != 0 { 16 } else { 8 };
        let mut rendered_sprites = 0;
        let mut line_bitmap_collision = [0_u8; SCROLL_SCREEN_WIDTH]; // works because size of u8 == CHAR_SIZE

        // Compute collisions, priorities....
        for sprite in 0_usize..64 {
            // sprite number is priority; once we have rendered eight sprites, we stop
            let v = self.vram[sprite_base + sprite];
            if DEBUG {
                let h = self.vram[sprite_base + 0x80 + sprite * 2] as usize;
                println!(
                    "Sprite {}, on dest vline: {} v: {} h: {} sprite height: {}",
                    sprite, vcoord_line, v, h, sprite_height
                );
            }
            if v <= vcoord_line && (vcoord_line as u16) < (v as u16) + sprite_height {
                if rendered_sprites >= 8 {
                    // We have reached sprite 9
                    self.status |= ST_9S;
                    break; //nothing more to render
                }
                rendered_sprites += 1;
                let h = self.vram[sprite_base + 0x80 + sprite * 2];
                let char_num = self.vram[sprite_base + 0x80 + sprite * 2 + 1] as u16;
                let char_settings =
                    sprite_settings(self.reg[1] & REG1_SIZE != 0, vcoord_line, char_num, v, h);
                if invisible(h) {
                    // We only render the visible area, so we count the sprites for priority, and
                    // collision, but not rendering
                    let bitmap = self.character_line_sprite_bitmap(char_settings);
                    self.collision_check(&mut line_bitmap_collision, bitmap, h);
                    continue;
                }

                let bitmap = self.character_line(pixels, char_settings);
                self.collision_check(&mut line_bitmap_collision, bitmap, h);
            }
        }
    }
    fn collision_check(&mut self, line_bitmap_collision: &mut [u8; 32], bitmap: u8, h: u8) {
        if Self::collision_calc(line_bitmap_collision, bitmap, h) {
            self.status |= ST_C;
        }
        let shift = h % CHAR_SIZE;
        line_bitmap_collision[h as usize / CHAR_SIZE as usize] |= bitmap << shift;
        line_bitmap_collision[(h as usize / CHAR_SIZE as usize + 1) % SCROLL_SCREEN_WIDTH] |=
            bitmap >> ((CHAR_SIZE - shift) % CHAR_SIZE);
    }
    fn collision_calc(line_bitmap_collision: &[u8; 32], bitmap: u8, h: u8) -> bool {
        Bitmap {
            bits: *line_bitmap_collision,
        }
        .get(h as usize)
            & bitmap
            != 0
    }
    fn render_line_effective(&mut self, pixels: &mut [u8], line: u8) {
        // First render background, then sprites ; we could optimize here by only rendering what is
        // needed
        self.render_background_line(pixels, line, false);
        self.render_sprites_line(
            pixels,
            line,
            |_| false,
            Self::sprite_line_settings_effective,
        );
    }
    fn render_line_visible(&mut self, pixels: &mut [u8], line: u8) {
        // First render background, then sprites ; we could optimize here by only rendering what is
        // needed
        self.render_background_line(pixels, line, true);
        self.render_sprites_line(
            pixels,
            line + VISIBLE_AREA_START_Y as u8,
            |h| {
                (h as usize + CHAR_SIZE as usize) <= VISIBLE_AREA_START_X
                    || h as usize >= VISIBLE_AREA_END_X
            },
            Self::sprite_line_settings_visible,
        );
    }
    fn render_line(&mut self, pixels: &mut [u8], line: u8, render_area: RenderArea) {
        match render_area {
            RenderArea::VisibleOnly => self.render_line_visible(pixels, line),
            RenderArea::EffectiveArea => self.render_line_effective(pixels, line),
        }
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
                    if val != 0xC0 {
                        println!("VDP cram address setup: {} != 0xC0", val);
                    }
                    if data >= 64 {
                        println!("VDP cram data write: {} >= 64", val);
                    }
                    self.addr = data as u16 & 0x3F;
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
impl Vdp {
    pub fn new() -> Self {
        Vdp {
            state: RefCell::new(VdpState::default()),
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
    pub fn step(&self, pixels: &mut [u8], render_area: RenderArea) -> (VdpInt, VdpDisplay) {
        let mut state = self.state.borrow_mut();
        state.v_counter = state.v_counter.wrapping_add(1);
        let mut rendered = VdpDisplay::NoDisplay;
        if state.v_counter == 0 {
            state.vertical_scroll = state.reg[9];
            state.v_counter_jumped = false;
        }
        if state.v_counter == 0xDA && !state.v_counter_jumped {
            state.v_counter_jumped = true;
            state.v_counter = 0xD5
        }
        if let RenderArea::VisibleOnly = render_area {
            if state.v_counter >= VISIBLE_AREA_START_Y as u8
                && state.v_counter < VISIBLE_AREA_END_Y as u8
            {
                let line = state.v_counter - VISIBLE_AREA_START_Y as u8;
                state.render_line(pixels, line, render_area);
            }
        } else if state.v_counter < SCROLL_SCREEN_HEIGHT as u8 * CHAR_SIZE {
            let line = state.v_counter as u8;
            state.render_line(pixels, line, render_area);
        }
        if state.v_counter == 0xC0 {
            state.status |= ST_I;
            if state.reg[1] & REG1_BLANK != 0 {
                rendered = VdpDisplay::ScreenDone;
            }
        }
        if state.reg[0] & REG0_IE1 != 0 {
            println!("H counter interrupt (line completion) should be enabled");
        }
        let interrupt = if state.status & ST_I != 0 && state.reg[1] & REG1_IE != 0 {
            VdpInt::InterruptGenerated
        } else {
            VdpInt::NoInterrupt
        };
        (interrupt, rendered)
    }
}

impl io::Device for Vdp {
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

impl Default for Vdp {
    fn default() -> Self {
        Self::new()
    }
}
impl Default for VdpState {
    fn default() -> Self {
        VdpState {
            reg: [0, 0, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0, 0, 0, 1],
            v_counter: 0,
            v_counter_jumped: false,
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

    let vdp = Vdp::default();
    let mut pixels = vec![0; 32 * 8 * 28 * 8 * 4];
    vdp.step(&mut pixels, RenderArea::EffectiveArea);
    assert_eq!(vdp.input(0x7E), Ok(1_u8),);
}

#[cfg(test)]
#[test]
fn sprite_coord() {
    // random one
    assert_eq!(
        VdpState::sprite_line_settings_visible(false, 32, 0, 28, 50),
        CharSettings {
            char_num: 0,
            x: 2,
            y: 8,
            line_length: 20,
            src_line: 4,
            x_start: 0,
            x_end: 8,
            sprite: true,
            rvh: false,
            rvv: false,
        }
    );
    // double size, first sprite
    assert_eq!(
        VdpState::sprite_line_settings_visible(true, 37, 3, 30, 55),
        CharSettings {
            char_num: 2,
            x: 7,
            y: 13,
            line_length: 20,
            src_line: 7,
            x_start: 0,
            x_end: 8,
            sprite: true,
            rvh: false,
            rvv: false,
        }
    );
    // double size, second sprite
    assert_eq!(
        VdpState::sprite_line_settings_visible(true, 38, 3, 30, 55),
        CharSettings {
            char_num: 3,
            x: 7,
            y: 14,
            line_length: 20,
            src_line: 0,
            x_start: 0,
            x_end: 8,
            sprite: true,
            rvh: false,
            rvv: false,
        }
    );
    // partial x, left edge
    assert_eq!(
        VdpState::sprite_line_settings_visible(true, 38, 3, 30, 45),
        CharSettings {
            char_num: 3,
            x: 0,
            y: 14,
            line_length: 20,
            src_line: 0,
            x_start: 3,
            x_end: 8,
            sprite: true,
            rvh: false,
            rvv: false,
        }
    );
    // partial x, right edge
    assert_eq!(
        VdpState::sprite_line_settings_visible(false, 38, 42, 34, 203),
        CharSettings {
            char_num: 42,
            x: 155,
            y: 14,
            line_length: 20,
            src_line: 4,
            x_start: 0,
            x_end: 5,
            sprite: true,
            rvh: false,
            rvv: false,
        }
    );
}

#[cfg(test)]
#[test]
fn sprite_collisions() {
    let mut line_bitmap_collision = [0_u8; 32];
    line_bitmap_collision[8] = 0xF3;
    assert!(VdpState::collision_calc(&line_bitmap_collision, 0xF, 64));
    assert!(!VdpState::collision_calc(&line_bitmap_collision, 0xF, 72));
    // with a shift
    line_bitmap_collision[12] = 0x3C;
    assert!(VdpState::collision_calc(&line_bitmap_collision, 0xF, 95));
    assert!(!VdpState::collision_calc(&line_bitmap_collision, 0xF, 94));
    // with a shift
    line_bitmap_collision[10] = 0x1;
    assert!(VdpState::collision_calc(&line_bitmap_collision, 0xFF, 80));
    assert!(VdpState::collision_calc(&line_bitmap_collision, 0xFF, 73));
    assert!(!VdpState::collision_calc(&line_bitmap_collision, 0xFF, 81));
    assert!(!VdpState::collision_calc(&line_bitmap_collision, 0xFF, 72));
    line_bitmap_collision[10] = 0x80;
    assert!(VdpState::collision_calc(&line_bitmap_collision, 0xFF, 80));
    assert!(VdpState::collision_calc(&line_bitmap_collision, 0xFF, 87));
    assert!(!VdpState::collision_calc(&line_bitmap_collision, 0xFF, 79));
    assert!(!VdpState::collision_calc(&line_bitmap_collision, 0xFF, 88));
    line_bitmap_collision[31] = 0xFF;
    // edge
    assert!(VdpState::collision_calc(&line_bitmap_collision, 0xFF, 250));
    // TODO: should be a match or not ?. We wrap around, but should we ?
    line_bitmap_collision[31] = 0;
    line_bitmap_collision[0] = 0xFF;
    assert!(VdpState::collision_calc(&line_bitmap_collision, 0xFF, 250));
}
