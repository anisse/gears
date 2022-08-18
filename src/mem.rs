use std::fmt;

#[derive(PartialEq, Clone, Debug)]
enum Dest {
    ROM { start: u16 },
    RAM { start: u16 },
    Panic,
}

pub enum Mapper {
    ZX64K, // useful for testing
    SegaGG,
    // Leave possibility to add codemasters mapper later
}

#[derive(PartialEq, Clone)]
pub struct Memory {
    ram: Vec<u8>,
    rom: Vec<u8>,
    map: [Dest; 8],
}

// TODO: mem errors
impl Memory {
    pub fn init(mapper: Mapper, rom: Option<&[u8]>) -> Memory {
        let mut mem = match mapper {
            Mapper::ZX64K => Memory {
                ram: vec![0; 0x10000],
                rom: Vec::new(),
                map: [
                    Dest::RAM { start: 0x0000 },
                    Dest::RAM { start: 0x2000 },
                    Dest::RAM { start: 0x4000 },
                    Dest::RAM { start: 0x6000 },
                    Dest::RAM { start: 0x8000 },
                    Dest::RAM { start: 0xA000 },
                    Dest::RAM { start: 0xC000 },
                    Dest::RAM { start: 0xE000 },
                ],
            },
            Mapper::SegaGG => Memory {
                ram: vec![0; 8192],
                rom: match rom {
                    Some(rom) => rom.to_vec(),
                    None => Vec::new(),
                },
                map: [
                    Dest::Panic,
                    Dest::Panic,
                    Dest::Panic,
                    Dest::Panic,
                    Dest::Panic,
                    Dest::Panic,
                    Dest::RAM { start: 0x0 },
                    Dest::RAM { start: 0x0 },
                ],
            },
        };
        if mem.rom.len() > 0 {
            mem.map[0] = Dest::ROM { start: 0x0000 };
            mem.map[1] = Dest::ROM { start: 0x2000 };
        } // addititional mappings will be done at runtime
        mem
    }
    pub fn fetch_u8(&self, addr: u16) -> u8 {
        match self.map[addr as usize >> 13] {
            Dest::ROM { start } => self.rom[((addr & !0xC000) + start) as usize],
            Dest::RAM { start } => self.ram[((addr & !0xC000).wrapping_add(start)) as usize],
            Dest::Panic => panic!("Unmapped memory read: @{:04X}", addr),
        }
    }
    pub fn fetch_u16(&self, addr: u16) -> u16 {
        self.fetch_u8(addr) as u16 | ((self.fetch_u8(addr.wrapping_add(1)) as u16) << 8)
    }

    pub fn fetch_range_safe(&self, addr: u16, len: u16) -> &[u8] {
        match self.map[addr as usize >> 13] {
            Dest::ROM { start } => {
                &self.rom
                    [((addr & !0xC000) + start) as usize..((addr & !0xC000) + start + len) as usize]
            }
            Dest::RAM { start } => {
                &self.ram
                    [((addr & !0xC000) + start) as usize..((addr & !0xC000) + start + len) as usize]
            }
            Dest::Panic => panic!("couldn't get a memory slice for @{:04X}+{:04X}", addr, len),
        }
    }

    pub fn set_u8(&mut self, addr: u16, val: u8) {
        match self.map[addr as usize >> 13] {
            Dest::ROM { start: _ } => panic!("Write to ROM address: {:04X}", addr),
            Dest::RAM { start } => self.ram[((addr & !0xC000).wrapping_add(start)) as usize] = val,
            Dest::Panic => panic!("Unmapped memory write: @{:04X}: {:02X}", addr, val),
        }
    }
    pub fn set_u16(&mut self, addr: u16, val: u16) {
        self.set_u8(addr, (val & 0xFF) as u8);
        self.set_u8(addr.wrapping_add(1), ((val >> 8) & 0xFF) as u8);
    }
}

impl From<Vec<u8>> for Memory {
    fn from(mut a: Vec<u8>) -> Self {
        let len = a.len();
        let newlen = (len as u16 + 3) & 0xFFFC; // this is used for testing, give us at least one instruction
        a.resize(newlen as usize, 0);
        Memory {
            ram: a,
            rom: Vec::new(),
            map: [
                // todo: proper implementation based on size
                Dest::RAM { start: 0x0000 },
                Dest::Panic,
                Dest::Panic,
                Dest::Panic,
                Dest::Panic,
                Dest::Panic,
                Dest::Panic,
                Dest::Panic,
            ],
        }
    }
}

impl Default for Memory {
    fn default() -> Self {
        Memory {
            ram: vec![0; 8192],
            rom: Vec::new(),
            map: [
                // todo: proper implementation based on size
                Dest::RAM { start: 0x0000 },
                Dest::Panic,
                Dest::Panic,
                Dest::Panic,
                Dest::Panic,
                Dest::Panic,
                Dest::Panic,
                Dest::Panic,
            ],
        }
    }
}

impl fmt::Debug for Memory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(format!("{:?}\n", self.map).as_str())?;
        let mut i = 0;
        let mut star = false; // line repeat marker
        while i < self.ram.len() {
            let v = self.ram[i];
            if i % 16 == 0 {
                if i > 0 {
                    if i >= 16 && self.ram.len() > i + 16 {
                        if self.ram[i - 16..i] == self.ram[i..i + 16] {
                            if !star {
                                f.write_str("\n*")?;
                            }
                            i += 16;
                            star = true;
                            continue;
                        }
                        star = false;
                    }
                    f.write_str("\n")?;
                }
                f.write_str(&format!("{:04X}", i))?;
            }
            if i % 8 == 0 {
                f.write_str(" ")?;
            }
            f.write_str(&format!("{:02X} ", v))?;
            i += 1;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::mem::*;
    #[test]
    fn basic_set() {
        let mut mem = Memory::init(Mapper::ZX64K, None);
        mem.set_u8(32768, 19);
        dbg!(&mem);
        assert_eq!(mem.fetch_u8(32768), 19);
    }
}
