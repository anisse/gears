use std::{fmt::Debug, rc::Rc};

pub trait Device {
    fn out(&self, addr: u16, val: u8, cycle: u32) -> Result<(), String>;
    fn input(&self, addr: u16, cycle: u32) -> Result<u8, String>;
}

pub struct RcDevice(Rc<dyn Device>);

impl RcDevice {
    pub fn new(d: impl Device + 'static) -> Self {
        Self(Rc::new(d))
    }
}

impl PartialEq for RcDevice {
    fn eq(&self, _: &Self) -> bool {
        // Nothing to compare here
        true
    }
}
impl Debug for RcDevice {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        f.write_str("")
    }
}

impl Default for RcDevice {
    fn default() -> Self {
        RcDevice(Rc::new(ErrIO {}))
    }
}

impl Clone for RcDevice {
    fn clone(&self) -> Self {
        RcDevice(self.0.clone())
    }
}

impl Device for RcDevice {
    fn out(&self, addr: u16, val: u8, cycle: u32) -> Result<(), String> {
        self.0.out(addr, val, cycle)
    }

    fn input(&self, addr: u16, cycle: u32) -> Result<u8, String> {
        self.0.input(addr, cycle)
    }
}

struct ErrIO {}
impl Device for ErrIO {
    fn out(&self, addr: u16, _val: u8, _cycle: u32) -> Result<(), String> {
        Err(format!("Out address {addr:04X} to err device"))
    }

    fn input(&self, addr: u16, _cycle: u32) -> Result<u8, String> {
        Err(format!("In address {addr:04X} to err device"))
    }
}
