use std::cell::RefCell;

use crate::io;

#[derive(Debug, Default)]

struct VDPState {
    v_counter: u8,
    vram: Vec<u8>,
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
}

impl io::Device for VDP {
    fn out(&self, addr: u16, val: u8) -> Result<(), String> {
        Ok(())
    }
    fn input(&self, addr: u16) -> Result<u8, String> {
        match addr & 0xFF {
            0x7E => {
                let vcounter = self.state.borrow().v_counter;
                dbg!(vcounter);
                Ok(vcounter)
            }
            _ => {
                dbg!("unknown VDP input address", addr);
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
