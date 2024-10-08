use std::fmt;

#[derive(PartialEq, Eq, Clone, Debug)]
enum Dest {
    Rom { start: u32 },
    Ram { start: u16 },
    Bram { start: u16 },
    Panic,
}

#[derive(PartialEq, Eq, Clone)]
pub enum Mapper {
    ZX64K, // useful for testing
    SegaGG {
        rom: Vec<u8>,
        backup_ram: Option<Vec<u8>>,
    },
    // Leave possibility to add codemasters mapper later
}

#[derive(PartialEq, Eq, Clone)]
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
                mapper,
                map: [
                    Dest::Ram { start: 0x0000 },
                    Dest::Ram { start: 0x2000 },
                    Dest::Ram { start: 0x4000 },
                    Dest::Ram { start: 0x6000 },
                    Dest::Ram { start: 0x8000 },
                    Dest::Ram { start: 0xA000 },
                    Dest::Ram { start: 0xC000 },
                    Dest::Ram { start: 0xE000 },
                ],
            },
            Mapper::SegaGG { .. } => Memory {
                ram: vec![0; 8192],
                mapper,
                map: [
                    Dest::Rom { start: 0x0000 },
                    Dest::Rom { start: 0x2000 },
                    Dest::Rom { start: 0x4000 },
                    Dest::Rom { start: 0x6000 },
                    Dest::Rom { start: 0x8000 },
                    Dest::Rom { start: 0xA000 },
                    Dest::Ram { start: 0x0 },
                    Dest::Ram { start: 0x0 },
                ],
            },
        }
    }
    pub fn fetch_u8(&self, addr: u16) -> u8 {
        match self.map[addr as usize >> 13] {
            Dest::Rom { start } => {
                if let Mapper::SegaGG { rom, .. } = &self.mapper {
                    rom[(addr & !0xE000) as usize + start as usize]
                } else {
                    unreachable!()
                }
            }
            Dest::Ram { start } => self.ram[((addr & !0xE000).wrapping_add(start)) as usize],
            Dest::Bram { start } => {
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

    pub fn fetch_range_safe(&self, addr: u16, dest: &mut [u8]) {
        for (i, d) in dest.iter_mut().enumerate() {
            *d = self.fetch_u8(addr.wrapping_add(i as u16))
        }
    }

    pub fn set_u8(&mut self, addr: u16, val: u8) {
        match self.map[addr as usize >> 13] {
            Dest::Rom { .. } => {
                println!(
                    "Ignoring write to ROM address: @{:04X} = {:02X} (old:{:02X})",
                    addr,
                    val,
                    self.fetch_u8(addr)
                );
            }
            Dest::Ram { start } => {
                if addr >= 0xFFFC {
                    if let Mapper::SegaGG { .. } = self.mapper {
                        self.rom_banking(addr, val);
                    }
                }
                self.ram[((addr & !0xE000).wrapping_add(start)) as usize] = val
            }
            Dest::Bram { start } => {
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

    fn rom_bitmask(&self) -> u32 {
        if let Mapper::SegaGG { rom, .. } = &self.mapper {
            return match rom.len() {
                32768 => 0x00007FFF,
                65536 => 0x0000FFFF,
                131072 => 0x0001FFFF,
                262144 => 0x0003FFFF,
                524288 => 0x0007FFFF,
                1048576 => 0x000FFFFF,
                // Technically this should be unsupported, but keep it as-is for now
                _ => 0xFFFFFFFF,
            };
        }
        0xFFFFFFFF
    }

    fn set_bank(&mut self, num: usize, val: u8) {
        let _shift = self.ram[0x1FFC] & 0x3;
        let start = ((val as u32) << 14) & self.rom_bitmask();
        let d1 = Dest::Rom { start };
        let d2 = Dest::Rom {
            start: start + 0x2000,
        };
        //println!("setting bank {} to dest {:X?}", num, &d1);
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
                            self.map[4] = Dest::Bram { start: 0x0000 };
                            self.map[5] = Dest::Bram { start: 0x0000 };
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
                Dest::Ram { start: 0x0000 },
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
                Dest::Ram { start: 0x0000 },
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
