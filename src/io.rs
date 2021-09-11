use std::{collections::HashMap, fmt::Debug};

pub trait Device {
    // XXX: maybe use upper 8 bits of address bus ? we don't need it for now
    fn out(&self, addr: u8, val: u8) -> Result<(), String>;
    fn input(&self, addr: u8) -> Result<u8, String>;
}

#[derive(Clone)]
pub struct IO<'a> {
    map: HashMap<u8, &'a dyn Device>,
}

impl<'a> IO<'a> {
    pub fn new() -> Self {
        IO {
            map: HashMap::new(),
        }
    }
    pub fn register(&mut self, addr: u8, dev: &'a mut dyn Device) {
        self.map.insert(addr, dev);
    }
    pub fn out(&self, addr: u8, val: u8) -> Result<(), String> {
        let dev = self.map.get(&addr).ok_or("Out address not found")?;
        dev.out(addr, val)
    }
    pub fn input(&self, addr: u8) -> Result<u8, String> {
        let dev = self.map.get(&addr).ok_or("In address not found")?;
        dev.input(addr)
    }
}

impl<'a> PartialEq for IO<'a> {
    fn eq(&self, _: &Self) -> bool {
        // Nothing to compare here
        true
    }
}

impl<'a> Default for IO<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Debug for IO<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        f.write_str("")
    }
}
