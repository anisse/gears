use std::fmt;

#[derive(PartialEq, Clone, Debug)]
enum Dest {
    ROM { start: u32 },
    RAM { start: u16 },
    BRAM { start: u16 },
    Panic,
}

#[derive(PartialEq, Clone)]
pub enum Mapper {
    ZX64K, // useful for testing
    SegaGG {
        rom: Vec<u8>,
        backup_ram: Option<Vec<u8>>,
    },
    // Leave possibility to add codemasters mapper later
}

#[derive(PartialEq, Clone)]
pub struct Memory {
    ram: Vec<u8>,
    mapper: Mapper,
    map: [Dest; 8],
}

// TODO: mem errors
impl Memory {
    pub fn init(mapper: Mapper) -> Memory {
        match mapper {
            Mapper::ZX64K => Memory {
                ram: vec![0; 0x10000],
                mapper: match mapper {
                    Mapper::ZX64K => Mapper::ZX64K,
                    Mapper::SegaGG { rom, backup_ram } => Mapper::SegaGG { rom, backup_ram },
                },
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
            Mapper::SegaGG { .. } => Memory {
                ram: vec![0; 8192],
                mapper,
                map: [
                    Dest::ROM { start: 0x0000 },
                    Dest::ROM { start: 0x2000 },
                    Dest::ROM { start: 0x4000 },
                    Dest::ROM { start: 0x6000 },
                    Dest::ROM { start: 0x8000 },
                    Dest::ROM { start: 0xA000 },
                    Dest::RAM { start: 0x0 },
                    Dest::RAM { start: 0x0 },
                ],
            },
        }
    }
    pub fn fetch_u8(&self, addr: u16) -> u8 {
        match self.map[addr as usize >> 13] {
            Dest::ROM { start } => {
                if let Mapper::SegaGG { rom, .. } = &self.mapper {
                    rom[(addr & !0xE000) as usize + start as usize]
                } else {
                    unreachable!()
                }
            }
            Dest::RAM { start } => self.ram[((addr & !0xE000).wrapping_add(start)) as usize],
            Dest::BRAM { start } => {
                if let Mapper::SegaGG {
                    backup_ram: Some(bram),
                    ..
                } = &self.mapper
                {
                    bram[((addr & !0xE000).wrapping_add(start)) as usize]
                } else {
                    unreachable!()
                }
            }
            Dest::Panic => panic!("Unmapped memory read: @{:04X}", addr),
        }
    }
    pub fn fetch_u16(&self, addr: u16) -> u16 {
        self.fetch_u8(addr) as u16 | ((self.fetch_u8(addr.wrapping_add(1)) as u16) << 8)
    }

    pub fn fetch_range_safe(&self, addr: u16, len: u16) -> &[u8] {
        match self.map[addr as usize >> 13] {
            Dest::ROM { start } => {
                if let Mapper::SegaGG { rom, .. } = &self.mapper {
                    &rom[((addr & !0xE000) as usize + start as usize)
                        ..((addr & !0xE000) as usize + start as usize + len as usize)]
                } else {
                    unreachable!()
                }
            }
            Dest::RAM { start } => {
                &self.ram
                    [((addr & !0xE000) + start) as usize..((addr & !0xE000) + start + len) as usize]
            }
            Dest::BRAM { start } => {
                if let Mapper::SegaGG {
                    backup_ram: Some(bram),
                    ..
                } = &self.mapper
                {
                    &bram[((addr & !0xE000) + start) as usize
                        ..((addr & !0xE000) + start + len) as usize]
                } else {
                    unreachable!()
                }
            }
            Dest::Panic => panic!("couldn't get a memory slice for @{:04X}+{:04X}", addr, len),
        }
    }

    pub fn set_u8(&mut self, addr: u16, val: u8) {
        match self.map[addr as usize >> 13] {
            Dest::ROM { .. } => {
                println!(
                    "Ignoring write to ROM address: @{:04X} = {:02X} (old:{:02X})",
                    addr,
                    val,
                    self.fetch_u8(addr)
                );
            }
            Dest::RAM { start } => {
                if addr >= 0xFFFC {
                    if let Mapper::SegaGG { .. } = self.mapper {
                        self.rom_banking(addr, val);
                    }
                }
                self.ram[((addr & !0xE000).wrapping_add(start)) as usize] = val
            }
            Dest::BRAM { start } => {
                if let Mapper::SegaGG {
                    backup_ram: Some(bram),
                    ..
                } = &mut self.mapper
                {
                    bram[((addr & !0xE000).wrapping_add(start)) as usize] = val
                } else {
                    unreachable!()
                }
            }
            Dest::Panic => panic!("Unmapped memory write: @{:04X}: {:02X}", addr, val),
        }
    }
    pub fn set_u16(&mut self, addr: u16, val: u16) {
        self.set_u8(addr, (val & 0xFF) as u8);
        self.set_u8(addr.wrapping_add(1), ((val >> 8) & 0xFF) as u8);
    }

    fn set_bank(&mut self, num: usize, val: u8) {
        let _shift = self.ram[0x1FFC] & 0x3;
        let d1 = Dest::ROM {
            start: (val as u32) << 14,
        };
        let d2 = Dest::ROM {
            start: ((val as u32) << 14) + 0x2000,
        };
        //println!("setting bank {} to dest {:?}", num, &d1);
        self.map[num * 2] = d1;
        self.map[num * 2 + 1] = d2;
    }
    fn rom_banking(&mut self, addr: u16, val: u8) {
        match addr {
            0xFFFC => {
                /* BANK CONTROL REGISTER */
                if val & 0x08 != 0 {
                    /* Select Backup RAM */
                    if let Mapper::SegaGG { backup_ram, .. } = &mut self.mapper {
                        if backup_ram.is_none() {
                            backup_ram.replace(vec![0; 8192]);
                        }
                        if backup_ram.is_some() {
                            self.map[4] = Dest::BRAM { start: 0x0000 };
                            self.map[5] = Dest::BRAM { start: 0x0000 };
                        } else {
                            unreachable!();
                        }
                    } else {
                        unreachable!();
                    }
                }
                if val & 0x3 != 0 {
                    panic!("Unimplemented bank shifting: {:02X}", val);
                }
            }
            0xFFFD => {
                /* BANK REGISTER 0 */
                if val != 0 {
                    panic!(
                        "Unimplemented mapping of slot 0 (first 1K preserved interrupt vectors)"
                    );
                }
                self.set_bank(0, val);
            }
            0xFFFE => self.set_bank(1, val), /* BANK REGISTER 1 */
            0xFFFF => {
                /* BANK REGISTER 2 */
                if self.ram[0x1FFC] & 0x08 != 0 {
                    /* Backup RAM overrides this */
                    return;
                }
                self.set_bank(2, val);
            }
            _ => unreachable!(),
        }
    }
}

impl From<Vec<u8>> for Memory {
    fn from(mut a: Vec<u8>) -> Self {
        let len = a.len();
        let newlen = (len as u16 + 3) & 0xFFFC; // this is used for testing, give us at least one instruction
        a.resize(newlen as usize, 0);
        Memory {
            ram: a,
            mapper: Mapper::ZX64K,
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
            mapper: Mapper::ZX64K,
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
        let mut mem = Memory::init(Mapper::ZX64K);
        mem.set_u8(32768, 19);
        dbg!(&mem);
        assert_eq!(mem.fetch_u8(32768), 19);
    }
}
