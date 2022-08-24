use std::cell::RefCell;

use crate::io;

pub enum Button {
    Up = 0,
    Down = 1,
    Left = 2,
    Right = 3,
    One = 4,
    Two = 5,
}
#[derive(Debug)]
struct JoystickState {
    buttons: [bool; 6],
    buttons_ext: [bool; 6], // not really emulated
}

#[derive(Debug)]
pub struct Joystick {
    state: RefCell<JoystickState>,
}

impl Default for Joystick {
    fn default() -> Self {
        Joystick {
            state: RefCell::new(JoystickState {
                buttons: [true; 6],
                buttons_ext: [true; 6],
            }),
        }
    }
}

impl Joystick {
    fn set_button(&self, button: Button, val: bool) {
        let mut state = self.state.borrow_mut();
        state.buttons[button as usize] = val;
    }
    fn get_dc(&self) -> u8 {
        let state = self.state.borrow();
        let mut dc = 0xFF;
        for (i, &button) in state.buttons.iter().enumerate() {
            if button {
                dc &= !(1 << i);
            }
        }
        dc
    }
    fn get_dd(&self) -> u8 {
        0xFF
    }
}
impl io::Device for Joystick {
    fn out(&self, addr: u16, val: u8) -> Result<(), String> {
        match addr & 0xFF {
            0xDC => {
                panic!("Joystick write: @{:04X} {:02X}", addr, val);
            }
            _ => Err(format!(
                "unknown system port output write address @{:04X} {:02X}",
                addr, val
            )),
        }
    }

    fn input(&self, addr: u16) -> Result<u8, String> {
        match addr & 0xFF {
            0xDC => Ok(self.get_dc()),
            0xDD => Ok(self.get_dd()),
            _ => Err(format!(
                "unknown system port output write address @{:04X}",
                addr
            )),
        }
    }
}