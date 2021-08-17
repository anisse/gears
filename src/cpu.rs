use crate::disas;
use crate::mem;
use std::fmt;

enum Flag {
    S = 1 << 7,
    Z = 1 << 6,
    F5 = 1 << 5,
    H = 1 << 4,
    F3 = 1 << 3,
    PV = 1 << 2,
    N = 1 << 1,
    C = 1 << 0,
}
const S: u8 = Flag::S as u8;
const Z: u8 = Flag::Z as u8;
const F5: u8 = Flag::F5 as u8;
const H: u8 = Flag::H as u8;
const F3: u8 = Flag::F3 as u8;
const PV: u8 = Flag::PV as u8;
const N: u8 = Flag::N as u8;
const C: u8 = Flag::C as u8;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum RegPair {
    AF,
    BC,
    DE,
    HL,
    AFp,
    BCp,
    DEp,
    HLp,
    SP, // for convenience
}

impl From<disas::Reg16> for RegPair {
    fn from(r: disas::Reg16) -> Self {
        match r {
            disas::Reg16::AF => RegPair::AF,
            disas::Reg16::BC => RegPair::BC,
            disas::Reg16::DE => RegPair::DE,
            disas::Reg16::HL => RegPair::HL,
            disas::Reg16::AFp => RegPair::AFp,
            disas::Reg16::SP => RegPair::SP,
        }
    }
}

#[derive(Default, PartialEq, Clone, Copy)]
#[allow(non_snake_case)]
pub struct Regs {
    pub A: u8,
    pub F: u8,
    pub B: u8,
    pub C: u8,
    pub D: u8,
    pub E: u8,
    pub H: u8,
    pub L: u8,
    pub Ap: u8,
    pub Fp: u8,
    pub Bp: u8,
    pub Cp: u8,
    pub Dp: u8,
    pub Ep: u8,
    pub Hp: u8,
    pub Lp: u8,
    pub I: u8,
    pub R: u8,
    pub IX: u16,
    pub IY: u16,
    pub SP: u16,
    pub PC: u16,
    pub MEMPTR: u16,
    /* Interrupts */
    pub IM: u8, // TODO: enum ?
    pub IFF1: bool,
    pub IFF2: bool,
}

#[derive(Default, Debug, PartialEq, Clone)]
#[allow(non_snake_case)]
pub struct State {
    pub r: Regs,
    pub halted: bool,
    pub mem: mem::Memory,
}

impl Regs {
    pub fn set_regpair(&mut self, reg: RegPair, val: u16) {
        let r1 = ((val & 0xFF00) >> 8) as u8;
        let r2 = (val & 0x00FF) as u8;
        match reg {
            RegPair::AF => {
                self.A = r1;
                self.F = r2
            }
            RegPair::BC => {
                self.B = r1;
                self.C = r2
            }
            RegPair::DE => {
                self.D = r1;
                self.E = r2
            }
            RegPair::HL => {
                self.H = r1;
                self.L = r2
            }
            RegPair::AFp => {
                self.Ap = r1;
                self.Fp = r2
            }
            RegPair::BCp => {
                self.Bp = r1;
                self.Cp = r2
            }
            RegPair::DEp => {
                self.Dp = r1;
                self.Ep = r2
            }
            RegPair::HLp => {
                self.Hp = r1;
                self.Lp = r2
            }
            RegPair::SP => {
                self.SP = val;
            }
        }
    }
    pub fn get_regpair(&self, reg: RegPair) -> u16 {
        let r1;
        let r2;
        match reg {
            RegPair::AF => {
                r1 = self.A;
                r2 = self.F
            }
            RegPair::BC => {
                r1 = self.B;
                r2 = self.C
            }
            RegPair::DE => {
                r1 = self.D;
                r2 = self.E
            }
            RegPair::HL => {
                r1 = self.H;
                r2 = self.L
            }
            RegPair::AFp => {
                r1 = self.Ap;
                r2 = self.Fp
            }
            RegPair::BCp => {
                r1 = self.Bp;
                r2 = self.Cp
            }
            RegPair::DEp => {
                r1 = self.Dp;
                r2 = self.Ep
            }
            RegPair::HLp => {
                r1 = self.Hp;
                r2 = self.Lp
            }
            RegPair::SP => return self.SP,
        }
        (r1 as u16) << 8 | r2 as u16
    }
    fn set_flag(&mut self, f: Flag, val: bool) {
        let shift = match f {
            Flag::S => 7,
            Flag::Z => 6,
            Flag::F5 => 5,
            Flag::H => 4,
            Flag::F3 => 3,
            Flag::PV => 2,
            Flag::N => 1,
            Flag::C => 0,
        };
        if val {
            //set
            self.F |= 1 << shift;
        } else {
            self.F &= !(1 << shift);
        }
    }
}

fn flag(s: &str, f: u8) -> &str {
    if f != 0 {
        return s;
    }
    "-"
}

impl fmt::Display for Regs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "A: {:02X} ({}) F: {}{}{}{}{}{}{}{} \
             BC: {:X} ({}) \
             DE: {:X} ({}) \
             HL: {:X} ({}) \n\
             AF': {:X} ({}) F: {}{}{}{}{}{}{}{} \
             BC': {:X} ({}) \
             DE': {:X} ({}) \
             HL': {:X} ({}) \n\
             SP: {:X} PC: {:X} R: {:X} MEMPTR: {:X}",
            self.A,
            self.A as i8,
            flag("S", self.F & S),
            flag("Z", self.F & Z),
            flag("Y", self.F & F5),
            flag("H", self.F & H),
            flag("X", self.F & F3),
            flag("PV", self.F & PV),
            flag("N", self.F & N),
            flag("C", self.F & C),
            self.get_regpair(RegPair::BC),
            self.get_regpair(RegPair::BC) as i16,
            self.get_regpair(RegPair::DE),
            self.get_regpair(RegPair::DE) as i16,
            self.get_regpair(RegPair::HL),
            self.get_regpair(RegPair::HL) as i16,
            self.get_regpair(RegPair::AFp),
            self.get_regpair(RegPair::AFp) as i16,
            flag("S", self.Fp & S),
            flag("Z", self.Fp & Z),
            flag("Y", self.Fp & F5),
            flag("H", self.Fp & H),
            flag("X", self.Fp & F3),
            flag("PV", self.Fp & PV),
            flag("N", self.Fp & N),
            flag("C", self.Fp & C),
            self.get_regpair(RegPair::BCp),
            self.get_regpair(RegPair::BCp) as i16,
            self.get_regpair(RegPair::DEp),
            self.get_regpair(RegPair::DEp) as i16,
            self.get_regpair(RegPair::HLp),
            self.get_regpair(RegPair::HLp) as i16,
            self.SP,
            self.PC,
            self.R,
            self.MEMPTR
        )
    }
}
impl fmt::Debug for Regs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}
fn get_op8(s: &State, op: disas::Operand) -> u8 {
    // TODO: size
    match op {
        disas::Operand::Reg8(reg) => match reg {
            disas::Reg8::A => s.r.A,
            disas::Reg8::F => s.r.F,
            disas::Reg8::B => s.r.B,
            disas::Reg8::C => s.r.C,
            disas::Reg8::D => s.r.D,
            disas::Reg8::E => s.r.E,
            disas::Reg8::H => s.r.H,
            disas::Reg8::L => s.r.L,
        },
        disas::Operand::RegAddr(reg) => s.mem.fetch_u8(s.r.get_regpair(RegPair::from(reg))),
        disas::Operand::Address(addr) => s.mem.fetch_u8(addr),
        disas::Operand::Imm8(imm) => imm,
        _ => panic!("Unknown operand {:?} or size not 8", op),
    }
}

fn set_op8(s: &mut State, op: disas::Operand, val: u8) {
    // TODO: size
    match op {
        disas::Operand::Reg8(reg) => match reg {
            disas::Reg8::A => s.r.A = val,
            disas::Reg8::F => s.r.F = val,
            disas::Reg8::B => s.r.B = val,
            disas::Reg8::C => s.r.C = val,
            disas::Reg8::D => s.r.D = val,
            disas::Reg8::E => s.r.E = val,
            disas::Reg8::H => s.r.H = val,
            disas::Reg8::L => s.r.L = val,
        },
        disas::Operand::RegAddr(reg) => s.mem.set_u8(s.r.get_regpair(RegPair::from(reg)), val),
        disas::Operand::Address(addr) => s.mem.set_u8(addr, val),
        _ => panic!(
            "Unknown operand {:?} or size not 8, or writing unsupported",
            op
        ),
    }
}

fn get_op16(s: &State, op: disas::Operand) -> u16 {
    match op {
        disas::Operand::Imm16(x) => x,
        disas::Operand::Reg16(reg) => s.r.get_regpair(RegPair::from(reg)),
        disas::Operand::Address(addr) => s.mem.fetch_u16(addr),
        _ => panic!("Unknown operand {:?} or size not 16", op),
    }
}

fn set_op16(s: &mut State, op: disas::Operand, val: u16) {
    match op {
        disas::Operand::Reg16(reg) => s.r.set_regpair(RegPair::from(reg), val),
        disas::Operand::Address(addr) => s.mem.set_u16(addr, val),
        _ => panic!(
            "Unknown operand {:?} or size not 16, or writing unsupported",
            op
        ),
    }
}

fn set_conditions_add_8(r: &mut Regs, a: i8, b: i8) -> u8 {
    r.set_flag(Flag::C, (a as u8).overflowing_add(b as u8).1);

    set_conditions_add8_base(r, a, b)
}

fn set_conditions_inc8_dec8(r: &mut Regs, a: i8, b: i8) -> u8 {
    let res = set_conditions_add8_base(r, a, b);
    if b < 0 {
        // invert H to mean borrow
        r.set_flag(Flag::H, (r.F & H) ^ H != 0)
    }
    r.set_flag(Flag::N, b < 0);
    res
}

fn set_conditions_add8_base(r: &mut Regs, a: i8, b: i8) -> u8 {
    let real_res = a as i16 + b as i16; // easier for overflows, etc
    let res = (real_res & 0xFF) as i8;
    r.set_flag(Flag::S, res < 0);
    r.set_flag(Flag::Z, res == 0);
    r.set_flag(Flag::H, (a ^ b ^ res) & 0x10 != 0);
    r.set_flag(
        Flag::PV,
        a.signum() == b.signum() && a.signum() != res.signum(),
    );
    r.set_flag(Flag::N, false);
    copy_f53_res(res as u8, r);

    res as u8
}

fn set_conditions_add_16(r: &mut Regs, a: i16, b: i16) -> u16 {
    let real_res = a as i32 + b as i32; // easier for overflows, etc
    let res = (real_res & 0xFFFF) as i16;
    //println!("a = {:04X}, b = {:04X}, res = {:04X}, a ^ b = {:04X}, a ^ b ^ res = {:04X}, & 0x1000 = {:04X}", a, b, res, a ^b, a ^ b ^ res, (a ^ b ^res) & 0x1000);
    r.set_flag(Flag::H, (a ^ b ^ res) & 0x1000 != 0);
    r.set_flag(Flag::N, false);
    copy_f53_res((res >> 8) as u8, r);

    r.set_flag(Flag::C, (a as u16).overflowing_add(b as u16).1);

    res as u16
}

fn cond_valid(r: &Regs, fc: disas::FlagCondition) -> bool {
    match fc {
        disas::FlagCondition::NZ => r.F & Z == 0,
        disas::FlagCondition::Z => r.F & Z != 0,
        disas::FlagCondition::NC => r.F & C == 0,
        disas::FlagCondition::C => r.F & C != 0,
        disas::FlagCondition::PO => r.F & PV == 0,
        disas::FlagCondition::PE => r.F & PV != 0,
        disas::FlagCondition::P => r.F & S == 0,
        disas::FlagCondition::M => r.F & S != 0,
    }
}

fn daa_diff(c: u8, h: u8, lo: u8, hi: u8) -> Option<u8> {
    // Tables from Stefano Donati via Sean Young's Z80 Undocumented
    let diff_table = [
        vec![
            // CF = 0
            (
                // high nibble
                0..=9,
                [
                    // HF = 0
                    (
                        // low nibble
                        0..=9,
                        0_u8, // diff
                    ),
                    // HF = 1
                    (
                        // low nibble
                        0..=9,
                        6_u8, // diff
                    ),
                ],
            ),
            (
                0..=8,
                [
                    // HF = 0
                    (
                        // low nibble
                        0xa..=0xf,
                        6_u8, // diff
                    ),
                    // HF = 1
                    (
                        // low nibble
                        0xa..=0xf,
                        6_u8, // diff
                    ),
                ],
            ),
            (
                0xa..=0xf,
                [
                    // HF = 0
                    (
                        // low nibble
                        0..=9,
                        0x60_u8, // diff
                    ),
                    // HF = 1
                    (
                        // low nibble
                        0..=9,
                        0x66_u8, // diff
                    ),
                ],
            ),
            (
                0x9..=0xf,
                [
                    // HF = 0
                    (
                        // low nibble
                        0xa..=0xf,
                        0x66_u8, // diff
                    ),
                    // HF = 1
                    (
                        // low nibble
                        0xa..=0xf,
                        0x66_u8, // diff
                    ),
                ],
            ),
        ],
        vec![
            // CF = 1
            (
                // high nibble
                0..=0xf,
                [
                    // HF = 0
                    (
                        // low nibble
                        0..=9,
                        0x60_u8, // diff
                    ),
                    // HF = 1
                    (
                        // low nibble
                        0..=9,
                        0x66_u8, // diff
                    ),
                ],
            ),
            (
                0..=0xf,
                [
                    // HF = 0
                    (
                        // low nibble
                        0xa..=0xf,
                        0x66_u8, // diff
                    ),
                    // HF = 1
                    (
                        // low nibble
                        0xa..=0xf,
                        0x66_u8, // diff
                    ),
                ],
            ),
        ],
    ];
    for i in diff_table[c as usize].iter() {
        // c
        if (*i).0.contains(&hi) {
            // high nibble
            // h
            let arr = &i.1[h as usize];
            if arr.0.contains(&lo) {
                // low nibble
                return Some(arr.1);
            }
        }
    }
    None
}
fn daa_cf(c: u8, hi: u8, lo: u8) -> Option<bool> {
    if c != 0 {
        return Some(true);
    }
    let table = [
        (0..=9, 0..=9, false),
        (0..=8, 0xa..=0xf, false),
        (9..=0xf, 0xa..=0xf, true),
        (0xa..=0xf, 0..=9, true),
    ];
    for i in table.iter() {
        if i.0.contains(&hi) && i.1.contains(&lo) {
            return Some(i.2);
        }
    }
    None
}

fn daa_hf(n: u8, h: u8, lo: u8) -> Option<bool> {
    struct HTable {
        n: u8,
        h: u8,
        lo: std::ops::RangeInclusive<u8>,
        hp: bool,
    }
    let table = [
        HTable {
            n: 0,
            h: 0,
            lo: 0..=9,
            hp: false,
        },
        HTable {
            n: 0,
            h: 1,
            lo: 0..=9,
            hp: false,
        },
        HTable {
            n: 0,
            h: 0,
            lo: 0xa..=0xf,
            hp: true,
        },
        HTable {
            n: 0,
            h: 1,
            lo: 0xa..=0xf,
            hp: true,
        },
        HTable {
            n: 1,
            h: 0,
            lo: 0..=0xf,
            hp: false,
        },
        HTable {
            n: 1,
            h: 1,
            lo: 6..=0xf,
            hp: false,
        },
        HTable {
            n: 1,
            h: 1,
            lo: 0..=5,
            hp: true,
        },
    ];
    for l in table.iter() {
        if n == l.n && h == l.h && l.lo.contains(&lo) {
            return Some(l.hp);
        }
    }
    None
}
fn daa(r: &mut Regs) {
    let c = r.F & C;
    let h = (r.F & H) >> 4;
    let lo = r.A & 0xF;
    let hi = (r.A >> 4) & 0xF;
    let n = (r.F & N) >> 1;
    let diff = daa_diff(c, h, lo, hi).expect("DAA diff not found, should never happen");
    let cp = daa_cf(c, hi, lo).expect("DAA C' not found, should never happen");
    let hp = daa_hf(n, h, lo).expect("DAA H' not found, should never happen");
    if n == 0 {
        r.A += diff;
    } else {
        r.A -= diff;
    }
    r.set_flag(Flag::C, cp);
    r.set_flag(Flag::H, hp);
    r.set_flag(Flag::S, (r.A & 0x80) != 0);
    copy_f53_res(r.A, r);
}
fn copy_f53_res(res: u8, r: &mut Regs) {
    r.set_flag(Flag::F5, res & (1 << 5) != 0);
    r.set_flag(Flag::F3, res & (1 << 3) != 0);
}

pub fn init() -> State {
    State::default()
}

pub fn run_op(s: &mut State, op: &disas::OpCode) -> Result<usize, String> {
    let mut update_pc = true;
    let mut op_len: usize = op.tstates.iter().fold(0, |sum, x| sum + (*x as usize));
    match op.ins {
        disas::Instruction::ADD => {
            let op1 = op.op1.ok_or("add op1 missing")?;
            let op2 = op.op2.ok_or("add op2 missing")?;
            match op1.size().ok_or("unsupported add size")? {
                disas::OpSize::S1 => {
                    let a = get_op8(s, op1) as i8;
                    let b = get_op8(s, op2) as i8;
                    let res = set_conditions_add_8(&mut s.r, a, b);
                    set_op8(s, op1, res);
                }
                disas::OpSize::S2 => {
                    let a = get_op16(s, op1) as i16;
                    let b = get_op16(s, op2) as i16;
                    let res = set_conditions_add_16(&mut s.r, a, b);
                    s.r.MEMPTR = a.overflowing_add(1).0 as u16;
                    set_op16(s, op1, res);
                }
            }
        }
        disas::Instruction::NOP => {
            // nothing to do
        }
        disas::Instruction::LD => {
            let op1 = op.op1.ok_or("LD op1 missing")?;
            let op2 = op.op2.ok_or("LD op2 missing")?;
            // transfer size == min(op1, op1)
            let size = op2
                .size()
                .or_else(|| op1.size())
                .ok_or("unsupported ld size")?;
            match size {
                disas::OpSize::S1 => {
                    let val = get_op8(s, op2);
                    set_op8(s, op1, val);
                }
                disas::OpSize::S2 => {
                    let val = get_op16(s, op2);
                    set_op16(s, op1, val);
                }
            }
            // MEMPTR
            if op2 == disas::Operand::Reg8(disas::Reg8::A) {
                match op1 {
                    disas::Operand::Address(addr) => {
                        // MEMPTR_low = (addr + 1) & #FF,  MEMPTR_hi = A
                        s.r.MEMPTR = (addr + 1) & 0xFF | ((s.r.A as u16) << 8);
                    }
                    disas::Operand::RegAddr(x) => {
                        let r = match x {
                            disas::Reg16::BC => s.r.get_regpair(RegPair::BC),
                            disas::Reg16::DE => s.r.get_regpair(RegPair::DE),
                            _ => panic!("Unsupported MEMPTR LD {:?}", x),
                        };
                        // MEMPTR_low = (rp + 1) & #FF,  MEMPTR_hi = A
                        s.r.MEMPTR = (r + 1) & 0xFF | ((s.r.A as u16) << 8);
                    }
                    _ => {}
                }
            } else if op1 == disas::Operand::Reg8(disas::Reg8::A) || size == disas::OpSize::S2 {
                if let disas::Operand::RegAddr(reg) = op2 {
                    s.r.MEMPTR = s.r.get_regpair(RegPair::from(reg)) + 1
                } else if let disas::Operand::Address(addr) = op1 {
                    s.r.MEMPTR = addr + 1
                } else if let disas::Operand::Address(addr) = op2 {
                    s.r.MEMPTR = addr + 1
                }
            }
        }
        disas::Instruction::INC | disas::Instruction::DEC => {
            let op1 = op.op1.ok_or("INC op1 missing")?;
            let inc = if op.ins == disas::Instruction::INC {
                1
            } else {
                -1
            };
            let size = match op1 {
                disas::Operand::RegAddr(_) => disas::OpSize::S1,
                disas::Operand::RegI(_) => disas::OpSize::S1,
                _ => op1.size().ok_or("unsupported INC source size")?,
            };
            match size {
                disas::OpSize::S1 => {
                    // Sets conditions
                    let val = get_op8(s, op1) as i8;
                    let res = set_conditions_inc8_dec8(&mut s.r, val, inc);
                    set_op8(s, op1, res);
                }
                disas::OpSize::S2 => {
                    // Does not set conditions
                    let val = get_op16(s, op1);
                    let (res, _) = val.overflowing_add(inc as u16);
                    set_op16(s, op1, res);
                }
            }
        }
        disas::Instruction::RLCA => {
            s.r.A = s.r.A.rotate_left(1);
            s.r.set_flag(Flag::C, s.r.A & 0x1 != 0);
            s.r.set_flag(Flag::H, false);
            s.r.set_flag(Flag::N, false);
            copy_f53_res(s.r.A, &mut s.r);
        }
        disas::Instruction::RLA => {
            let a = s.r.A;
            let c = s.r.F & C;
            s.r.set_flag(Flag::C, s.r.A & 0x80 != 0);
            s.r.A = a << 1 | c;
            s.r.set_flag(Flag::H, false);
            s.r.set_flag(Flag::N, false);
            copy_f53_res(s.r.A, &mut s.r);
        }
        disas::Instruction::RRCA => {
            s.r.A = s.r.A.rotate_right(1);
            s.r.set_flag(Flag::C, s.r.A & 0x80 != 0);
            s.r.set_flag(Flag::H, false);
            s.r.set_flag(Flag::N, false);
            copy_f53_res(s.r.A, &mut s.r);
        }
        disas::Instruction::RRA => {
            let a = s.r.A;
            let c = s.r.F & C;
            s.r.set_flag(Flag::C, s.r.A & 0x1 != 0);
            s.r.A = a >> 1 | (c << 7);
            s.r.set_flag(Flag::H, false);
            s.r.set_flag(Flag::N, false);
            copy_f53_res(s.r.A, &mut s.r);
        }
        disas::Instruction::EX => {
            let op1 = op.op1.ok_or("EX op1 missing")?;
            let op2 = op.op2.ok_or("EX op2 missing")?;
            let a = get_op16(s, op1);
            let b = get_op16(s, op2);
            set_op16(s, op1, b);
            set_op16(s, op2, a);
        }
        disas::Instruction::DJNZ => {
            let op1 = op.op1.ok_or("No immediate arg for DJNZ")?;
            let jump = if let disas::Operand::RelAddr(j) = op1 {
                j
            } else {
                return Err(format!("Arg {} not rel addr for DJNZ", op1));
            };
            s.r.B = s.r.B.overflowing_sub(1).0;
            if s.r.B != 0 {
                s.r.PC = (s.r.PC as i32 + jump as i32) as u16;
                update_pc = false;
            } else {
                op_len = 8 // [5, 3]
            }
        }
        disas::Instruction::JR => {
            let op1 = op.op1.ok_or("No immediate arg for DJNZ")?;
            if let disas::Operand::RelAddr(j) = op1 {
                s.r.PC = (s.r.PC as i32 + j as i32) as u16;
                update_pc = false;
                s.r.MEMPTR = s.r.PC;
            } else {
                let op2 = op.op2.ok_or("No second rel addr operand for JR")?;
                if let disas::Operand::FlagCondition(cond) = op1 {
                    if let disas::Operand::RelAddr(j) = op2 {
                        if cond_valid(&s.r, cond) {
                            s.r.PC = (s.r.PC as i32 + j as i32) as u16;
                            update_pc = false;
                            s.r.MEMPTR = s.r.PC;
                        } else {
                            op_len = 7 // [4, 3]
                        }
                    } else {
                        return Err(format!("op2 {:?} should be rel addr for JR", op2));
                    }
                } else {
                    return Err(format!("op1 {:?} should be cond for JR", op1));
                }
            }
        }
        disas::Instruction::DAA => daa(&mut s.r),
        disas::Instruction::CPL => {
            s.r.A = !s.r.A;
            s.r.set_flag(Flag::H, true);
            s.r.set_flag(Flag::N, true);
            copy_f53_res(s.r.A, &mut s.r);
        }
        disas::Instruction::SCF => {
            s.r.set_flag(Flag::C, true);
            s.r.set_flag(Flag::H, false);
            s.r.set_flag(Flag::N, false);
            copy_f53_res(s.r.A, &mut s.r);
        }
        disas::Instruction::CCF => {
            s.r.set_flag(Flag::H, s.r.F & C != 0);
            s.r.set_flag(Flag::C, s.r.F & C == 0);
            s.r.set_flag(Flag::N, false);
            copy_f53_res(s.r.A, &mut s.r);
        }
        _ => return Err(format!("Unsupported opcode {:?}", op.ins)),
    }
    if update_pc {
        s.r.PC += op.length as u16;
    }
    s.r.R = (s.r.R + 1) & 0x7F;

    Ok(op_len)
}

pub fn run(s: &mut State, tstates_len: usize) -> Result<(), String> {
    let mut tstates_len = tstates_len;
    while tstates_len > 0 {
        // TODO: split into m states, fetch etc
        let disas_target = s.mem.fetch_range_safe(s.r.PC, 4);
        if let Some(op) = disas::disas(disas_target) {
            println!("{:04X}: {:?}", s.r.PC, op);
            let op_len = run_op(s, &op)?;
            tstates_len = usize::saturating_sub(tstates_len, op_len);
        } else {
            return Err(format!("Unknown instruction {:#X}", disas_target[0]));
        }
    }
    Ok(())
}
#[cfg(test)]
mod tests {
    use crate::cpu::*;
    #[test]
    fn run_add() {
        let mut s = init();
        s.r.A = 1;
        let default = Regs::default();
        s.mem = mem::Memory::from(vec![0x87]);
        run(&mut s, 1).unwrap();
        assert_eq!(
            s.r,
            Regs {
                A: 2,
                F: 0,
                PC: 1,
                R: 1,
                ..default
            },
        );
        s.r = default;
        s.r.A = 64;
        s.mem.set_u8(0, 0x87); // same instruction
        run(&mut s, 1).unwrap();
        assert_eq!(
            s.r,
            Regs {
                A: -128_i8 as u8,
                F: 0x84,
                PC: 1,
                R: 1,
                ..default
            },
        );
        s.r = default;
        s.r.A = -1_i8 as u8;
        s.mem.set_u8(0, 0x87); // same instruction
        run(&mut s, 1).unwrap();
        assert_eq!(
            s.r,
            Regs {
                A: -2_i8 as u8,
                F: 0xb9,
                PC: 1,
                R: 1,
                ..default
            },
        )
    }
}
