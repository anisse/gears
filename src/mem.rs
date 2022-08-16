use std::fmt;

#[derive(PartialEq, Clone, Debug)]
enum Dest {
    ROM { start: u16, end: u16 },
    RAM,
}

pub enum Mapper {
    ZX64K, // useful for testing
    SegaGG,
    // Leave possibility to add codemasters mapper later
}

#[derive(PartialEq, Clone, Debug)]
struct Mapping {
    start: u16,
    end: u16,
    dest: Dest,
}

#[derive(PartialEq, Clone)]
pub struct Memory {
    ram: Vec<u8>,
    rom: Vec<u8>,
    mappings: Vec<Mapping>,
}

// TODO: mem errors
impl Memory {
    pub fn init(mapper: Mapper, rom: Option<&[u8]>) -> Memory {
        let mut mem = match mapper {
            Mapper::ZX64K => Memory {
                ram: vec![0; 0x10000],
                rom: Vec::new(),
                mappings: vec![Mapping {
                    start: 0,
                    end: 0xFFFF,
                    dest: Dest::RAM,
                }],
            },
            Mapper::SegaGG => Memory {
                ram: vec![0; 8192],
                rom: match rom {
                    Some(rom) => rom.to_vec(),
                    None => Vec::new(),
                },
                mappings: vec![
                    Mapping {
                        start: 0xC000,
                        end: 0xDFFF,
                        dest: Dest::RAM,
                    },
                    Mapping {
                        start: 0xE000,
                        end: 0xFFFF,
                        dest: Dest::RAM,
                    },
                ],
            },
        };
        if mem.rom.len() > 0 {
            mem.mappings.push(Mapping {
                start: 0,
                end: 0x3FFF,
                dest: Dest::ROM {
                    start: 0,
                    end: 0x3FFF,
                },
            });
        } // addititional mappings will be done at runtime
        mem
    }
    pub fn fetch_u8(&self, addr: u16) -> u8 {
        for mapping in self.mappings.iter() {
            if addr >= mapping.start && addr <= mapping.end {
                match mapping.dest {
                    Dest::ROM { start, end: _ } => {
                        return self.rom[(addr - start) as usize];
                    }
                    Dest::RAM => {
                        return self.ram[(addr - mapping.start) as usize];
                    }
                }
            }
        }
        panic!("Unmapped memory read: @{:04X}", addr);
    }
    pub fn fetch_u16(&self, addr: u16) -> u16 {
        self.fetch_u8(addr) as u16 | ((self.fetch_u8(addr.overflowing_add(1).0) as u16) << 8)
    }

    pub fn fetch_range_safe(&self, addr: u16, len: u16) -> &[u8] {
        for mapping in self.mappings.iter() {
            if addr >= mapping.start && addr + len - 1 <= mapping.end {
                match mapping.dest {
                    Dest::ROM { start, end: _ } => {
                        return &self.rom[(addr - start) as usize..(addr - start + len) as usize];
                    }
                    Dest::RAM => {
                        return &self.ram[(addr - mapping.start) as usize
                            ..(addr - mapping.start + len) as usize];
                    }
                }
            }
        }
        panic!("couldn't get a memory slice for @{:04X}+{:04X}", addr, len);
    }

    pub fn set_u8(&mut self, addr: u16, val: u8) {
        for mapping in self.mappings.iter() {
            if addr >= mapping.start && addr <= mapping.end {
                match mapping.dest {
                    Dest::ROM { start: _, end: _ } => {
                        panic!("Write to ROM address: {:04X}", addr);
                    }
                    Dest::RAM => {
                        self.ram[(addr - mapping.start) as usize] = val;
                        return;
                    }
                }
            }
        }
        panic!("Unmapped memory write: @{:04X}: {:02X}", addr, val);
    }
    pub fn set_u16(&mut self, addr: u16, val: u16) {
        self.set_u8(addr, (val & 0xFF) as u8);
        self.set_u8(addr.overflowing_add(1).0, ((val >> 8) & 0xFF) as u8);
    }
    pub fn len(&self) -> usize {
        self.ram.len()
    }
    pub fn is_empty(&self) -> bool {
        self.ram.is_empty()
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
            mappings: vec![Mapping {
                start: 0,
                end: newlen - 1,
                dest: Dest::RAM,
            }],
        }
    }
}

impl Default for Memory {
    fn default() -> Self {
        Memory {
            ram: vec![0; 8192],
            rom: Vec::new(),
            mappings: vec![Mapping {
                start: 0x0000,
                end: 0x1FFF,
                dest: Dest::RAM,
            }],
        }
    }
}

impl fmt::Debug for Memory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(format!("{:?}", self.mappings).as_str())?;
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
