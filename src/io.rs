use std::{fmt::Debug, rc::Rc};

pub trait Device {
    fn out(&self, addr: u16, val: u8, cycle: u32) -> Result<(), String>;
    fn input(&self, addr: u16, cycle: u32) -> Result<u8, String>;
}

#[derive(Clone)]
struct Entry {
    start: u16,
    end: u16,
    /// ignored bits
    ignore_mask: u16,
    dev: Rc<dyn Device>,
}
#[derive(Clone)]
pub struct IO {
    devs: Vec<Entry>,
}

impl IO {
    pub fn new() -> Self {
        IO { devs: Vec::new() }
    }
    pub fn register(&mut self, start: u16, end: u16, ignore_mask: u16, dev: Rc<dyn Device>) {
        self.devs.push(Entry {
            start,
            end,
            ignore_mask,
            dev,
        });
    }
    pub fn out(&self, addr: u16, val: u8, cycle: u32) -> Result<(), String> {
        let dev = self
            .devs
            .iter()
            .find(|&x| addr & !x.ignore_mask >= x.start && addr & !x.ignore_mask <= x.end)
            .ok_or(format!("Out address {:04X} to unknown device", addr))?;
        dev.dev.out(addr, val, cycle)
    }
    pub fn input(&self, addr: u16, cycle: u32) -> Result<u8, String> {
        let dev = self
            .devs
            .iter()
            .find(|&x| addr & !x.ignore_mask >= x.start && addr & !x.ignore_mask <= x.end)
            .ok_or(format!("In address {:04X} to unknown device", addr))?;
        dev.dev.input(addr, cycle)
    }
}

impl PartialEq for IO {
    fn eq(&self, _: &Self) -> bool {
        // Nothing to compare here
        true
    }
}

impl Default for IO {
    fn default() -> Self {
        Self::new()
    }
}

impl Debug for IO {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        f.write_str("")
    }
}
