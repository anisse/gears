use std::convert::From;
use std::fmt;

#[derive(PartialEq, Clone)]
pub struct Memory {
    mem: Vec<u8>,
}

// TODO: mem errors
impl Memory {
    fn init(size: usize) -> Memory {
        Memory { mem: vec![0; size] }
    }
    // TODO: implement memory map (e.g ROM read-only)
    pub fn fetch_u8(&self, addr: u16) -> u8 {
        self.mem[addr as usize]
    }

    pub fn fetch_range(&self, addr: u16, len: u16) -> &[u8] {
        &self.mem[addr as usize..(addr + len) as usize]
    }

    pub fn set_u8(&mut self, addr: u16, val: u8) {
        self.mem[addr as usize] = val
    }
    pub fn len(&self) -> usize {
        self.mem.len()
    }
    pub fn is_empty(&self) -> bool {
        self.mem.is_empty()
    }
}

impl From<&[u8]> for Memory {
    fn from(a: &[u8]) -> Self {
        Memory { mem: a.to_vec() }
    }
}
impl From<Vec<u8>> for Memory {
    fn from(a: Vec<u8>) -> Self {
        Memory { mem: a }
    }
}

impl Default for Memory {
    fn default() -> Self {
        Memory { mem: vec![0; 8192] }
    }
}

impl fmt::Debug for Memory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut i = 0;
        let mut star = false;
        while i < self.mem.len() {
            let v = self.mem[i];
            if i % 16 == 0 {
                if i > 0 {
                    if i >= 16 && self.mem.len() > i + 16 {
                        if self.mem[i - 16..i] == self.mem[i..i + 16] {
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
