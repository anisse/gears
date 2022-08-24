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

    fn debug_screen_state(state: &VDPState) {
        let pattern_base = ((state.reg[2] as usize) & 0x0E) << 10;
        let sprite_base = ((state.reg[5] as usize) & 0x7E) << 7;

        println!("Scroll screen: Pat name @{:04X}", pattern_base);
        let mut chars = [false; 448];
        for i in (0_usize..0x700).step_by(2) {
            let b1 = state.vram[pattern_base + i + 1];
            let character = state.vram[pattern_base + i] as u16 | (b1 as u16 & 0x1 << 8);
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
        }
        println!("Sprites attribute @{:04X}", sprite_base);
        for i in 0_usize..64 {
            let v = state.vram[sprite_base + i];
            let h = state.vram[sprite_base + 0x80 + i];
            let ch = state.vram[sprite_base + 0x80 + i * 2 + 1];
            chars[ch as usize] = true;
            if ch != 0 {
                println!("Sprite {}: {}x{} char {}", i, h, v, ch);
            }
        }
        println!("Characters @0x0000");
        for i in 0_usize..448 {
            if chars[i] {
                use std::io::Write;
                let mut f = std::fs::File::create(format!("./char-{:02X}.ppm", i)).unwrap();
                println!("Character {}", i);
                write!(f, "P3\n").unwrap();
                write!(f, "8 8\n").unwrap();
                for pix in 0..64 {
                    let addr: usize = i * 32 + (pix >> 3) * 4;
                    let offset = 8 - (pix & 0x7);
                    let code = (((state.vram[addr + 3] >> offset) & 1) << 3)
                        | (((state.vram[addr + 2] >> offset) & 1) << 2)
                        | (((state.vram[addr + 1] >> offset) & 1) << 1)
                        | (((state.vram[addr + 0] >> offset) & 1) << 0);
                    /*
                    if code != 0 {
                        println!("@{:04X} : {}", addr, code);
                    }
                    */
                    let color_r = (state.cram[code as usize * 2]) & 0xF;
                    let color_g = (state.cram[code as usize * 2] >> 4) & 0xF;
                    let color_b = (state.cram[code as usize * 2 + 1]) & 0xF;
                    write!(f, "{} {} {}\n", color_r * 16, color_g * 16, color_b * 16).unwrap();
                }
            }
        }

        //println!("Color RAM: {:?}", state.cram);
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
                state.addr = (state.addr + 1) & 0x3FF;
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
