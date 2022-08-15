use std::cell::RefCell;

use crate::io;

#[derive(Debug, PartialEq, Eq, Default)]
pub struct System {
    start_button: RefCell<bool>,
}

impl System {
    fn set_start_button(&self, val: bool) {
        self.start_button.replace(val);
    }
}
impl io::Device for System {
    fn out(&self, addr: u16, val: u8) -> Result<(), String> {
        match addr & 0xFF {
            0x0 => {
                panic!("System write: @{:04X} {:02X}", addr, val);
            }
            _ => Err(format!(
                "unknown system port output write address @{:04X} {:02X}",
                addr, val
            )),
        }
    }

    fn input(&self, addr: u16) -> Result<u8, String> {
        match addr & 0xFF {
            0x0 => Ok((*self.start_button.borrow() as u8) << 7 | 0x6F),
            _ => Err(format!(
                "unknown system port output write address @{:04X}",
                addr
            )),
        }
    }
}
