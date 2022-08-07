use std::fmt::Debug;

pub trait Device {
    // XXX: maybe use upper 8 bits of address bus ? we don't need it for now
    fn out(&self, addr: u16, val: u8) -> Result<(), String>;
    fn input(&self, addr: u16) -> Result<u8, String>;
}

#[derive(Clone)]
struct Entry<'a> {
    start: u16,
    end: u16,
    /// ignored bits
    ignore_mask: u16,
    dev: &'a dyn Device,
}
#[derive(Clone)]
pub struct IO<'a> {
    devs: Vec<Entry<'a>>,
}

impl<'a> IO<'a> {
    pub fn new() -> Self {
        IO { devs: Vec::new() }
    }
    pub fn register(&mut self, start: u16, end: u16, ignore_mask: u16, dev: &'a dyn Device) {
        self.devs.push(Entry {
            start,
            end,
            ignore_mask,
            dev,
        });
    }
    pub fn out(&self, addr: u16, val: u8) -> Result<(), String> {
        let dev = self
            .devs
            .iter()
            .find(|&x| addr & !x.ignore_mask >= x.start && addr & !x.ignore_mask <= x.end)
            .ok_or("Out address not found")?;
        dev.dev.out(addr, val)
    }
    pub fn input(&self, addr: u16) -> Result<u8, String> {
        let dev = self
            .devs
            .iter()
            .find(|&x| addr & !x.ignore_mask >= x.start && addr & !x.ignore_mask <= x.end)
            .ok_or("In address not found")?;
        dev.dev.input(addr)
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
