use std::cell::RefCell;

use crate::io;

enum StatusFlag {
    I = 1 << 7,
    S9 = 1 << 6,
    C = 1 << 5,
}

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
    pub fn step(&self) {
        let vcounter = self.state.borrow().v_counter.overflowing_add(1).0;
        self.state.borrow_mut().v_counter = vcounter;
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
                    //dbg!("setup cram address", state.addr);
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
                    state.cram_byte1 = Some(val);
                } else {
                    ram[addr - 1] = cram_byte1.unwrap();
                    ram[addr] = val & 0x0F;
                    state.addr = (state.addr + 1) & 0x3F;
                }
            }
            None => unreachable!(),
        }
        Ok(())
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
            0xBF => {
                let st = self.state.borrow().status;
                self.reset_byte1();
                Ok(st)
            }
            0xBE => {
                self.reset_byte1();
                panic!("Unsupported input on BE");
                Ok(0)
            }
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
