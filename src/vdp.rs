use std::cell::RefCell;

use crate::io;

enum StatusFlag {
    I = 1 << 7,
    S9 = 1 << 6,
    C = 1 << 5,
}

#[derive(Debug, Clone)]
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
    write_data: Option<u8>,
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
    fn write_data(&self, val: u8) -> Result<(), String> {
        let mut state = self.state.borrow_mut();
        match state.write_data.take() {
            Some(data) => match val & 0xC0 {
                0x80 => {
                    // This is a register write
                    let reg = val & !0x80;
                    if reg > 10 {
                        panic!("Unexpected high VDP register {:02X}", reg)
                    }
                    dbg!("Writing to vdp reg", reg, data);
                    state.reg[reg as usize] = data;
                }
                0x00 | 0x40 => {
                    // TODO: differentiate read and write setup ?
                    // VRAM address setup
                    state.addr = data as u16 | ((val as u16) << 8);
                    state.dest = Some(WriteDest::Vram);
                    dbg!("setup vram address", state.addr);
                }
                0xC0 => {
                    // cram address setup
                    assert_eq!(val, 0xC0);
                    assert!(data < 64);
                    state.addr = data as u16;
                    state.dest = Some(WriteDest::Cram);
                    dbg!("setup cram address", state.addr);
                }
                _ => {
                    panic!(
                        "Unexpected high bits in VDP register selection: {:02X} ({:02X})",
                        val,
                        val & 0xC0
                    )
                }
            },
            None => state.write_data = Some(val),
        }
        Ok(())
    }
    fn reset_status_write(&self) {
        let mut state = self.state.borrow_mut();
        state.write_data.take();
    }
}

impl io::Device for VDP {
    fn out(&self, addr: u16, val: u8) -> Result<(), String> {
        match addr & 0xFF {
            0xBF => self.write_data(val),
            _ => {
                dbg!("unknown VDP output address", addr);
                panic!();
                Ok(())
            }
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
                self.reset_status_write();
                Ok(st)
            }
            _ => {
                dbg!("unknown VDP input address", addr);
                panic!();
                Ok(0)
            }
        }
    }
}

impl Default for VDP {
    fn default() -> Self {
        Self::new()
    }
}
impl VDPState {
    fn default() -> VDPState {
        VDPState {
            reg: [0, 0, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0, 0, 0, 1],
            v_counter: 0,
            addr: 0,
            dest: None,
            vram: vec![0; 16 * 1024],
            cram: vec![0; 32 * 2],
            status: 0,
            write_data: None,
        }
    }
}
