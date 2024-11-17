use std::fmt;

pub trait MemoryMapper: fmt::Debug {
    fn fetch_u8(&self, addr: u16) -> u8;
    fn fetch_range_safe(&self, addr: u16, dest: &mut [u8]) {
        for (i, d) in dest.iter_mut().enumerate() {
            *d = self.fetch_u8(addr.wrapping_add(i as u16))
        }
    }
    fn fetch_u16(&self, addr: u16) -> u16 {
        self.fetch_u8(addr) as u16 | ((self.fetch_u8(addr.wrapping_add(1)) as u16) << 8)
    }
    fn set_u8(&mut self, addr: u16, val: u8);
    fn set_u16(&mut self, addr: u16, val: u16) {
        self.set_u8(addr, (val & 0xFF) as u8);
        self.set_u8(addr.wrapping_add(1), ((val >> 8) & 0xFF) as u8);
    }
}

#[derive(PartialEq, Eq, Clone)]
pub struct ZX64kMapper {
    ram: [u8; 0x10000],
}
impl Default for ZX64kMapper {
    fn default() -> Self {
        Self { ram: [0; 0x10000] }
    }
}

impl MemoryMapper for ZX64kMapper {
    fn fetch_u8(&self, addr: u16) -> u8 {
        self.ram[addr as usize]
    }

    fn set_u8(&mut self, addr: u16, val: u8) {
        self.ram[addr as usize] = val;
    }
}

impl From<Vec<u8>> for ZX64kMapper {
    fn from(init: Vec<u8>) -> Self {
        let mut m = Self::default();
        //let range_dst = 0..init.len().max(m.ram.len());
        let range = 0..init.len().min(m.ram.len());
        m.ram[range.clone()].copy_from_slice(&init[range]);
        m
    }
}

#[derive(PartialEq, Eq, Clone)]
pub struct SegaGGMapper {
    ram: [u8; 8192],
    map: [Dest; 8],
    rom: Vec<u8>,
    backup_ram: Option<Vec<u8>>,
}

impl SegaGGMapper {
    pub fn new(rom: Vec<u8>, backup_ram: Option<Vec<u8>>) -> Self {
        Self {
            ram: [0; 8192],
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
            rom,
            backup_ram,
        }
    }
    fn rom_bitmask(&self) -> u32 {
        match self.rom.len() {
            32768 => 0x00007FFF,
            65536 => 0x0000FFFF,
            131072 => 0x0001FFFF,
            262144 => 0x0003FFFF,
            524288 => 0x0007FFFF,
            1048576 => 0x000FFFFF,
            // Technically this should be unsupported, but keep it as-is for now
            _ => 0xFFFFFFFF,
        }
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
                    if self.backup_ram.is_none() {
                        self.backup_ram.replace(vec![0; 8192]);
                    }
                    if self.backup_ram.is_some() {
                        self.map[4] = Dest::Bram { start: 0x0000 };
                        self.map[5] = Dest::Bram { start: 0x0000 };
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

impl MemoryMapper for SegaGGMapper {
    fn fetch_u8(&self, addr: u16) -> u8 {
        match self.map[addr as usize >> 13] {
            Dest::Rom { start } => self.rom[(addr & !0xE000) as usize + start as usize],
            Dest::Ram { start } => self.ram[((addr & !0xE000).wrapping_add(start)) as usize],
            Dest::Bram { start } => {
                if let Some(bram) = &self.backup_ram {
                    bram[((addr & !0xE000).wrapping_add(start)) as usize]
                } else {
                    // We should never put bram in the map if there is no backup ram
                    unreachable!()
                }
            }
            Dest::Panic => panic!("Unmapped memory read: @{:04X}", addr),
        }
    }

    fn set_u8(&mut self, addr: u16, val: u8) {
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
                    self.rom_banking(addr, val);
                }
                self.ram[((addr & !0xE000).wrapping_add(start)) as usize] = val
            }
            Dest::Bram { start } => {
                if let Some(bram) = &mut self.backup_ram {
                    bram[((addr & !0xE000).wrapping_add(start)) as usize] = val
                } else {
                    // We should never put bram in the map if there is no backup ram
                    unreachable!()
                }
            }
            Dest::Panic => panic!("Unmapped memory write: @{:04X}: {:02X}", addr, val),
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
#[allow(dead_code)]
enum Dest {
    Rom { start: u32 },
    Ram { start: u16 },
    Bram { start: u16 },
    Panic,
}

fn dbg_mem(block: &[u8], f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut i = 0;
    let mut star = false; // line repeat marker
    while i < block.len() {
        let v = block[i];
        if i % 16 == 0 {
            if i > 0 {
                if i >= 16 && block.len() > i + 16 {
                    if block[i - 16..i] == block[i..i + 16] {
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

impl fmt::Debug for ZX64kMapper {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        dbg_mem(&self.ram, f)
    }
}
impl fmt::Debug for SegaGGMapper {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(format!("{:?}\n", self.map).as_str())?;
        dbg_mem(&self.ram, f)
    }
}

#[cfg(test)]
mod tests {
    use crate::mem::*;
    #[test]
    fn basic_set() {
        let mut mem = ZX64kMapper::default();
        mem.set_u8(32768, 19);
        dbg!(&mem);
        assert_eq!(mem.fetch_u8(32768), 19);
    }
}
