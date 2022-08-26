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
}

#[derive(Debug)]
pub struct VDP {
    state: RefCell<VDPState>,
}

impl VDP {
    pub fn new() -> Self {
        VDP {
            state: RefCell::new(VDPState::default()),
        }
    }

    fn serialize_ppm(filename: String, width: usize, height: usize, data: &[u8]) {
        {
            let mut f = std::fs::File::create("temp.ppm").unwrap();
            use std::io::Write;
            write!(f, "P3\n").unwrap();
            write!(f, "{} {}\n", width, height).unwrap();
            write!(f, "15\n").unwrap();
            for (i, _) in data.iter().enumerate().step_by(3) {
                write!(f, "{} {} {}\n", data[i], data[i + 1], data[i + 2]).unwrap();
            }
        }
        std::fs::rename("temp.ppm", filename).unwrap();
    }
    fn character(
        state: &VDPState,
        sprite: bool,
        i: usize,
        dest: &mut [u8],
        x: usize,
        y: usize,
        line_length: usize,
    ) {
        let base = match sprite {
            false => {
                assert!(i < 448);
                0
            }
            true => {
                assert!(i < 256);
                ((state.reg[6] as usize) & 0x4) << 10
            }
        };
        let pallette_base = match sprite {
            false => 0,
            true => 32,
        };
        for pix in 0..64 {
            let addr: usize = base + i * 32 + (pix >> 3) * 4;
            let offset = 0x7 - (pix & 0x7);
            let code = (((state.vram[addr + 3] >> offset) & 1) << 3)
                | (((state.vram[addr + 2] >> offset) & 1) << 2)
                | (((state.vram[addr + 1] >> offset) & 1) << 1)
                | (((state.vram[addr + 0] >> offset) & 1) << 0);
            if sprite && code == 0 {
                //transparent
                continue;
            }
            /*
            if code != 0 {
                println!("@{:04X} : {}", addr, code);
            }
            */
            let color_r = (state.cram[pallette_base + code as usize * 2]) & 0xF;
            let color_g = (state.cram[pallette_base + code as usize * 2] >> 4) & 0xF;
            let color_b = (state.cram[pallette_base + code as usize * 2 + 1]) & 0xF;

            let line = pix >> 3;
            let col = pix & 0x7;
            dest[(y + line) * (line_length * 8 * 3) + (x + col) * 3] = color_r;
            dest[(y + line) * (line_length * 8 * 3) + (x + col) * 3 + 1] = color_g;
            dest[(y + line) * (line_length * 8 * 3) + (x + col) * 3 + 2] = color_b;
        }
    }
    fn debug_screen_state(state: &VDPState) {
        let pattern_base = ((state.reg[2] as usize) & 0x0E) << 10;
        let sprite_base = ((state.reg[5] as usize) & 0x7E) << 7;

        println!("Double size sprites: {}", state.reg[1] & REG1_SIZE != 0);
        println!("Scroll screen: Pat name @{:04X}", pattern_base);
        let mut bg = vec![0_u8; 32 * 28 * 8 * 8 * 3];
        let mut chars = [false; 448];
        for i in (0_usize..0x700).step_by(2) {
            let b1 = state.vram[pattern_base + i + 1];
            let character = state.vram[pattern_base + i] as u16 | ((b1 as u16 & 0x1) << 8);
            let rvh = b1 & PNAME_RVH != 0;
            let rvv = b1 & PNAME_RVV != 0;
            let palette1 = b1 & PNAME_CPT != 0;
            if i > 96 && i < 672 {
                chars[character as usize] = true;
            }
            if character != 0 && false {
                println!(
                    "Pattern {:03X}: character {:03X} revh {} revv {} pallette1 {}",
                    i / 2,
                    character,
                    rvh,
                    rvv,
                    palette1,
                );
            }
            let x = (i / 2) & 31; // col
            let y = i >> 6; // row
            Self::character(state, false, character as usize, &mut bg, x * 8, y * 8, 32);
        }
        println!("Sprites attribute @{:04X}", sprite_base);
        for i in 0_usize..64 {
            let v = state.vram[sprite_base + i];
            let h = state.vram[sprite_base + 0x80 + i * 2];
            let ch = state.vram[sprite_base + 0x80 + i * 2 + 1] as usize;
            chars[ch] = true;
            if h == 0xD0 {
                break;
            }
            //if ch != 0 {
            println!("Sprite {}: {}x{} char {}", i, h, v, ch);
            //}
            if h < (248) && v < (220) {
                if state.reg[1] & REG1_SIZE != 0 {
                    // double size
                    Self::character(
                        state,
                        true,
                        ch & (!0x1),
                        &mut bg,
                        h as usize,
                        v as usize,
                        32,
                    );
                    Self::character(
                        state,
                        true,
                        ch | 0x1,
                        &mut bg,
                        h as usize,
                        v as usize + 1,
                        32,
                    );
                } else {
                    Self::character(state, true, ch, &mut bg, h as usize, v as usize, 32);
                }
            }
        }
        println!("Characters @0x0000");
        for i in 0_usize..448 {
            if chars[i] {
                //println!("Character {}", i);
                /*
                let mut data = vec![0; 8 * 8 * 3];
                Self::character(state, false, i, &mut data, 0, 0, 1);
                Self::serialize_ppm(format!("./char-{:02X}.ppm", i), 8, 8, &data);
                */
            }
        }
        Self::serialize_ppm("bg.ppm".to_string(), 32 * 8, 28 * 8, &bg);

        println!("Color RAM: {:?}", state.cram);
    }
    pub fn step(&self) -> bool {
        let mut state = self.state.borrow_mut();
        state.v_counter = state.v_counter.wrapping_add(1);
        if state.v_counter == 0xC0 {
            state.status |= ST_I;
            if state.reg[1] & REG1_BLANK != 0 {
                Self::debug_screen_state(&state);
            }
        }
        if state.reg[0] & REG0_IE1 != 0 {
            println!("Down counter interrupt should be enabled");
        }
        return state.status & ST_I != 0 && state.reg[1] & REG1_IE != 0;
    }
    fn write_cmd(&self, val: u8) -> Result<(), String> {
        let mut state = self.state.borrow_mut();
        match state.cmd_byte1.take() {
            Some(data) => match val & 0xC0 {
                0x80 => {
                    // This is a register write
                    let reg = val & !0x80;
                    if reg > 10 {
                        panic!("Unexpected high VDP register {:02X}", reg)
                    }
                    //dbg!("Writing to vdp reg", reg, data);
                    state.reg[reg as usize] = data;
                }
                0x00 | 0x40 => {
                    // TODO: differentiate read and write setup ? This is only useful for timings
                    // after setup
                    // VRAM address setup
                    state.addr = data as u16 | ((val as u16 & 0x3F) << 8);
                    state.dest = Some(WriteDest::Vram);
                    //dbg!("setup vram address", state.addr);
                }
                0xC0 => {
                    // cram address setup
                    assert_eq!(val, 0xC0);
                    assert!(data < 64);
                    state.addr = data as u16;
                    state.dest = Some(WriteDest::Cram);
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
            None => state.cmd_byte1 = Some(val),
        }
        Ok(())
    }
    fn reset_byte1(&self) {
        let mut state = self.state.borrow_mut();
        state.cmd_byte1.take();
    }

    fn write_ram(&self, val: u8) -> Result<(), String> {
        self.reset_byte1();
        let mut state = self.state.borrow_mut();
        let addr = state.addr as usize;
        let dest = state.dest;
        let cram_byte1 = state.cram_byte1;
        let ram = match dest {
            Some(WriteDest::Vram) => &mut state.vram,
            Some(WriteDest::Cram) => &mut state.cram,
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
                state.addr = (state.addr + 1) & 0x3FFF;
            }
            Some(WriteDest::Cram) => {
                if addr & 1 == 0 {
                    //println!("Upper CRAM byte set: {:02X}", val);
                    state.cram_byte1 = Some(val);
                } else {
                    ram[addr - 1] = cram_byte1.unwrap();
                    ram[addr] = val & 0x0F;
                    //println!("Written to CRAM: {:02X}{:02X}", ram[addr], ram[addr - 1]);
                }
                state.addr = (state.addr + 1) & 0x3F;
            }
            None => unreachable!(),
        }
        Ok(())
    }
    fn read_ram(&self) -> Result<u8, String> {
        self.reset_byte1();
        let mut state = self.state.borrow_mut();
        let addr = state.addr as usize;
        let dest = state.dest;
        match dest {
            Some(WriteDest::Vram) => {}
            Some(WriteDest::Cram) => {
                return Err("Reading from Color RAM is not supported".to_string())
            }
            None => return Err("No VDP write dest".to_string()),
        };
        if addr > state.vram.len() {
            return Err(format!(
                "VDP access to {:?} address too high: {:04X} / {:04X}",
                dest,
                addr,
                state.vram.len()
            ));
        }
        let val = state.vram[addr];
        state.addr = (state.addr + 1) & 0x3FF;
        Ok(val)
    }
    fn read_status(&self) -> Result<u8, String> {
        self.reset_byte1();
        let mut state = self.state.borrow_mut();
        let st = state.status;
        state.status = 0; // clear all three flags
        Ok(st)
    }
}

impl io::Device for VDP {
    fn out(&self, addr: u16, val: u8) -> Result<(), String> {
        match addr & 0xFF {
            0xBF => self.write_cmd(val),
            0xBE => self.write_ram(val),
            _ => Err(format!(
                "unknown VDP output address @{:04X} ({:02X} ",
                addr, val
            )),
        }
    }
    fn input(&self, addr: u16) -> Result<u8, String> {
        match addr & 0xFF {
            0x7E => {
                let vcounter = self.state.borrow().v_counter;
                Ok(vcounter)
            }
            0xBF => self.read_status(),
            0xBE => self.read_ram(),
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
        }
    }
}

#[cfg(test)]
#[test]
fn run_step() {
    use crate::io::Device;

    let vdp = VDP::default();
    vdp.step();
    assert_eq!(vdp.input(0x7E), Ok(1_u8),);
}
