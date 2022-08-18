use crate::disas;
use crate::disas::{Instruction, OpSize, Operand};
use crate::io;
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
    IX,
    IY,
    SP, /* for convenience */
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
            disas::Reg16::IX => RegPair::IX,
            disas::Reg16::IY => RegPair::IY,
        }
    }
}
impl From<disas::RegI> for RegPair {
    fn from(r: disas::RegI) -> Self {
        match r {
            disas::RegI::IX(_) => RegPair::IX,
            disas::RegI::IY(_) => RegPair::IY,
        }
    }
}

#[derive(Default, Debug, Clone, Copy)]
struct FChanged {
    changed: bool, // No need to store Q itself, we only need to know if F was changed during the previous instruction cycle
}

impl PartialEq for FChanged {
    fn eq(&self, _: &Self) -> bool {
        true // always ignored
    }
}

#[derive(PartialEq, Clone, Copy)]
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
    pub IXh: u8,
    pub IXl: u8,
    pub IYh: u8,
    pub IYl: u8,
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
pub struct State<'a> {
    pub r: Regs,
    q: FChanged,
    pub halted: bool,
    pub mem: mem::Memory,
    pub io: io::IO<'a>,
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
            RegPair::IX => {
                self.IXh = r1;
                self.IXl = r2;
            }
            RegPair::IY => {
                self.IYh = r1;
                self.IYl = r2;
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
            RegPair::IX => {
                r1 = self.IXh;
                r2 = self.IXl
            }
            RegPair::IY => {
                r1 = self.IYh;
                r2 = self.IYl
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
    fn get_flag(&self, f: Flag) -> bool {
        self.F & f as u8 != 0
    }
}

impl Default for Regs {
    fn default() -> Self {
        Regs {
            A: 0xFF,
            F: 0xFF,
            B: 0,
            C: 0,
            D: 0,
            E: 0,
            H: 0,
            L: 0,
            Ap: 0,
            Fp: 0,
            Bp: 0,
            Cp: 0,
            Dp: 0,
            Ep: 0,
            Hp: 0,
            Lp: 0,
            I: 0,
            R: 0,
            IXh: 0,
            IXl: 0,
            IYh: 0,
            IYl: 0,
            SP: 0xFFFF,
            PC: 0,
            MEMPTR: 0,
            IM: 0,
            IFF1: false,
            IFF2: false,
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
             BC: {:04X} ({}) \
             DE: {:04X} ({}) \
             HL: {:04X} ({}) \n\
             AF': {:04X} ({}) F': {}{}{}{}{}{}{}{} \
             BC': {:04X} ({}) \
             DE': {:04X} ({}) \
             HL': {:04X} ({}) \
             IX: {:04X} ({}) IY: {:04X} ({}) \n\
             SP: {:04X} PC: {:04X} R: {:X} MEMPTR: {:04X} \
             I: {} IFF1: {} IFF2: {} IM: {}\n",
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
            self.get_regpair(RegPair::IX),
            self.get_regpair(RegPair::IX) as i16,
            self.get_regpair(RegPair::IY),
            self.get_regpair(RegPair::IY) as i16,
            self.SP,
            self.PC,
            self.R,
            self.MEMPTR,
            self.I,
            self.IFF1,
            self.IFF2,
            self.IM
        )
    }
}
impl fmt::Debug for Regs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}
fn get_op8(s: &State, op: Operand) -> u8 {
    // TODO: size
    match op {
        Operand::Reg8(reg) => match reg {
            disas::Reg8::A => s.r.A,
            disas::Reg8::B => s.r.B,
            disas::Reg8::C => s.r.C,
            disas::Reg8::D => s.r.D,
            disas::Reg8::E => s.r.E,
            disas::Reg8::H => s.r.H,
            disas::Reg8::L => s.r.L,
            disas::Reg8::IXh => s.r.IXh,
            disas::Reg8::IXl => s.r.IXl,
            disas::Reg8::IYh => s.r.IYh,
            disas::Reg8::IYl => s.r.IYl,
            disas::Reg8::I => s.r.I,
            disas::Reg8::R => s.r.R,
        },
        Operand::RegAddr(reg) => s.mem.fetch_u8(s.r.get_regpair(RegPair::from(reg))),
        Operand::Address(addr) => s.mem.fetch_u8(addr),
        Operand::Imm8(imm) => imm,
        Operand::RegI(reg) => s.mem.fetch_u8(reg_i_addr(&reg, &s.r)),
        _ => panic!("Unknown operand {:?} or size not 8", op),
    }
}

fn set_op8(s: &mut State, op: Operand, val: u8) {
    // TODO: size
    match op {
        Operand::Reg8(reg) => match reg {
            disas::Reg8::A => s.r.A = val,
            disas::Reg8::B => s.r.B = val,
            disas::Reg8::C => s.r.C = val,
            disas::Reg8::D => s.r.D = val,
            disas::Reg8::E => s.r.E = val,
            disas::Reg8::H => s.r.H = val,
            disas::Reg8::L => s.r.L = val,
            disas::Reg8::IXh => s.r.IXh = val,
            disas::Reg8::IXl => s.r.IXl = val,
            disas::Reg8::IYh => s.r.IYh = val,
            disas::Reg8::IYl => s.r.IYl = val,
            disas::Reg8::I => s.r.I = val,
            disas::Reg8::R => s.r.R = val,
        },
        Operand::RegAddr(reg) => s.mem.set_u8(s.r.get_regpair(RegPair::from(reg)), val),
        Operand::Address(addr) => s.mem.set_u8(addr, val),
        Operand::RegI(reg) => s.mem.set_u8(reg_i_addr(&reg, &s.r), val),
        _ => panic!(
            "Unknown operand {:?} or size not 8, or writing unsupported",
            op
        ),
    }
}

fn get_op16(s: &State, op: Operand) -> u16 {
    match op {
        Operand::Imm16(x) => x,
        Operand::Reg16(reg) => s.r.get_regpair(RegPair::from(reg)),
        Operand::RegAddr(reg) => s.mem.fetch_u16(s.r.get_regpair(RegPair::from(reg))),
        Operand::Address(addr) => s.mem.fetch_u16(addr),
        _ => panic!("Unknown operand {:?} or size not 16", op),
    }
}

fn set_op16(s: &mut State, op: Operand, val: u16) {
    match op {
        Operand::Reg16(reg) => s.r.set_regpair(RegPair::from(reg), val),
        Operand::RegAddr(reg) => s.mem.set_u16(s.r.get_regpair(RegPair::from(reg)), val),
        Operand::Address(addr) => s.mem.set_u16(addr, val),
        _ => panic!(
            "Unknown operand {:?} or size not 16, or writing unsupported",
            op
        ),
    }
}

fn set_conditions_add_8(r: &mut Regs, a: i8, b: i8) -> u8 {
    r.set_flag(Flag::C, (a as u8).overflowing_add(b as u8).1);

    set_conditions_add8_base(r, a, b, 0)
}

fn set_conditions_sub_8(r: &mut Regs, a: i8, b: i8) -> u8 {
    r.set_flag(Flag::C, (a as u8).overflowing_sub(b as u8).1);
    set_conditions_sub8_base(r, a, b, 0)
}
fn set_conditions_sub8_base(r: &mut Regs, a: i8, b: i8, c: i8) -> u8 {
    let real_res = a as i16 - b as i16 - c as i16; // easier for overflows, etc
    let res = (real_res & 0xFF) as i8;
    r.set_flag(Flag::S, res < 0);
    r.set_flag(Flag::Z, res == 0);
    r.set_flag(Flag::H, (a ^ b ^ res) & 0x10 != 0);
    r.set_flag(Flag::PV, (a < 0) != (b < 0) && (res < 0) != (a < 0));
    r.set_flag(Flag::N, true);
    copy_f53_res(res as u8, r);

    res as u8
}
fn set_conditions_sbc_8(r: &mut Regs, a: i8, b: i8) -> u8 {
    let c = r.F & C;
    let (tmp, ov1) = (a as u8).overflowing_sub(b as u8);
    let (_, ov2) = (tmp as u8).overflowing_sub(c as u8);
    r.set_flag(Flag::C, ov1 || ov2);

    set_conditions_sub8_base(r, a, b, c as i8)
}

fn set_conditions_adc_8(r: &mut Regs, a: i8, b: i8) -> u8 {
    let c = r.F & C;
    let (tmp, ov1) = (a as u8).overflowing_add(b as u8);
    let (_, ov2) = (tmp as u8).overflowing_add(c as u8);
    r.set_flag(Flag::C, ov1 || ov2);

    set_conditions_add8_base(r, a, b, c as i8)
}

fn set_conditions_inc8_dec8(r: &mut Regs, a: i8, b: i8) -> u8 {
    if b < 0 {
        set_conditions_sub8_base(r, a, 1, 0)
    } else {
        set_conditions_add8_base(r, a, 1, 0)
    }
}

fn set_conditions_add8_base(r: &mut Regs, a: i8, b: i8, c: i8) -> u8 {
    let real_res = a as i16 + b as i16 + c as i16; // easier for overflows, etc
    let res = (real_res & 0xFF) as i8;
    r.set_flag(Flag::S, res < 0);
    r.set_flag(Flag::Z, res == 0);
    r.set_flag(Flag::H, (a ^ b ^ res) & 0x10 != 0);
    r.set_flag(Flag::PV, (a < 0) == (b < 0) && (res < 0) != (a < 0));
    r.set_flag(Flag::N, false);
    copy_f53_res(res as u8, r);

    res as u8
}

fn set_conditions_adc_16(r: &mut Regs, a: i16, b: i16) -> u16 {
    let c = r.F & C;
    let res = set_conditions_add_16(r, a, b, c as i16) as i16;
    r.set_flag(Flag::S, res < 0);
    r.set_flag(Flag::Z, res == 0);
    r.set_flag(Flag::PV, (a < 0) == (b < 0) && (res < 0) != (a < 0));
    res as u16
}
fn set_conditions_add_16(r: &mut Regs, a: i16, b: i16, c: i16) -> u16 {
    let real_res = a as i32 + b as i32 + c as i32; // easier for overflows, etc
    let res = (real_res & 0xFFFF) as i16;
    //println!("a = {:04X}, b = {:04X}, res = {:04X}, a ^ b = {:04X}, a ^ b ^ res = {:04X}, & 0x1000 = {:04X}", a, b, res, a ^b, a ^ b ^ res, (a ^ b ^res) & 0x1000);
    r.set_flag(Flag::H, (a ^ b ^ res) & 0x1000 != 0);
    r.set_flag(Flag::N, false);
    copy_f53_res((res >> 8) as u8, r);

    let (tmp, ov1) = (a as u16).overflowing_add(b as u16);
    let (_, ov2) = (tmp as u16).overflowing_add(c as u16);
    r.set_flag(Flag::C, ov1 || ov2);

    res as u16
}

fn set_conditions_sbc_16(r: &mut Regs, a: i16, b: i16) -> u16 {
    let c = (r.F & C) as i16;
    let real_res = a as i32 - b as i32 - c as i32; // easier for overflows, etc
    let res = (real_res & 0xFFFF) as i16;
    r.set_flag(Flag::S, res < 0);
    r.set_flag(Flag::Z, res == 0);
    r.set_flag(Flag::H, (a ^ b ^ res) & 0x1000 != 0);
    r.set_flag(Flag::PV, (a < 0) != (b < 0) && (res < 0) != (a < 0));
    r.set_flag(Flag::N, true);
    copy_f53_res((res >> 8) as u8, r);

    let (tmp, ov1) = (a as u16).overflowing_sub(b as u16);
    let (_, ov2) = (tmp as u16).overflowing_sub(c as u16);
    r.set_flag(Flag::C, ov1 || ov2);

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
fn daa(r: &mut Regs) {
    let c = r.get_flag(Flag::C);
    let h = r.get_flag(Flag::H);
    let lo = r.A & 0xF;
    let n = r.get_flag(Flag::N);
    let diff_lo = lo > 9 || h;
    let diff_hi = c || r.A > 0x99;
    let diff = if diff_lo { 0x6 } else { 0 } + if diff_hi { 0x60 } else { 0 };
    if !n {
        r.A = r.A.wrapping_add(diff);
    } else {
        r.A = r.A.wrapping_sub(diff);
    }
    r.set_flag(Flag::C, diff_hi);
    let hp = (!n && lo > 9) || (n && h && lo < 6);
    r.set_flag(Flag::H, hp);
    r.set_flag(Flag::S, (r.A & 0x80) != 0);
    copy_f53_res(r.A, r);
    r.set_flag(Flag::PV, r.A.count_ones() & 1 == 0);
    r.set_flag(Flag::Z, r.A == 0);
}

fn copy_f53_res(res: u8, r: &mut Regs) {
    r.set_flag(Flag::F5, res & F5 != 0);
    r.set_flag(Flag::F3, res & F3 != 0);
}

fn set_bitops_flags(res: u8, r: &mut Regs) {
    r.set_flag(Flag::S, (res as i8) < 0);
    r.set_flag(Flag::Z, res == 0);
    r.set_flag(Flag::PV, res.count_ones() & 1 == 0);
    r.set_flag(Flag::N, false);
    copy_f53_res(res, r);
}
fn set_bitops_flags_c(res: u8, r: &mut Regs) {
    set_bitops_flags(res, r);
    r.set_flag(Flag::C, false);
}

fn reg_i_addr(reg: &disas::RegI, r: &Regs) -> u16 {
    (r.get_regpair(RegPair::from(*reg)) as i32 + reg.offset() as i32) as u16
}

fn memptr_idx(op: &Option<disas::Operand>, r: &mut Regs) {
    if let Some(Operand::RegI(reg)) = op {
        r.MEMPTR = reg_i_addr(reg, r);
    }
}
fn memptr_index(opcode: &disas::OpCode, r: &mut Regs) {
    memptr_idx(&opcode.op1, r);
    memptr_idx(&opcode.op2, r);
}

fn set_conditions_io_block_base(r: &mut Regs) {
    let res = (r.B as i8).wrapping_sub(1);
    r.set_flag(Flag::S, res < 0);
    r.set_flag(Flag::Z, res == 0);
    copy_f53_res(res as u8, r);

    r.B = res as u8;
}

fn set_conditions_io_block(r: &mut Regs, val: u8, lc: u8) {
    let k = (val as u16).wrapping_add(lc as u16);
    r.set_flag(Flag::H, k > 0xFF);
    r.set_flag(Flag::C, k > 0xFF);
    r.set_flag(Flag::PV, ((k & 0x7) as u8 ^ r.B).count_ones() & 1 == 0);
    r.set_flag(Flag::N, (val & 0x80) != 0);
}

fn input_block(s: &mut State, inc: i8) -> Result<(), String> {
    let hl = s.r.get_regpair(RegPair::HL);
    let addr = s.r.get_regpair(RegPair::BC);
    set_conditions_io_block_base(&mut s.r);
    let val = s.io.input(addr)?;
    s.mem.set_u8(hl, val);
    let c = s.r.C.wrapping_add(inc as u8);
    set_conditions_io_block(&mut s.r, val, c);

    s.r.set_regpair(RegPair::HL, hl.wrapping_add(inc as u16));
    // MEMPTR = BC_before_decrementing_B +/- 1
    s.r.MEMPTR = addr.wrapping_add(inc as u16);
    Ok(())
}

fn output_block(s: &mut State, inc: i8) -> Result<(), String> {
    let hl = s.r.get_regpair(RegPair::HL);
    let val = s.mem.fetch_u8(hl);
    let addr = s.r.get_regpair(RegPair::BC);
    s.io.out(addr, val)?;
    set_conditions_io_block_base(&mut s.r);
    s.r.set_regpair(RegPair::HL, hl.wrapping_add(inc as u16));
    let l = s.r.L;
    set_conditions_io_block(&mut s.r, val, l);
    // MEMPTR = BC_after_decrementing_B +/- 1
    let bc = s.r.get_regpair(RegPair::BC);
    s.r.MEMPTR = bc.wrapping_add(inc as u16);
    Ok(())
}

fn mem_block_copy(r: &mut Regs, mem: &mut mem::Memory, inc: i8) {
    let hl = r.get_regpair(RegPair::HL);
    let de = r.get_regpair(RegPair::DE);
    let bc = r.get_regpair(RegPair::BC).wrapping_sub(1);
    let val = mem.fetch_u8(hl);
    mem.set_u8(de, val);
    r.set_regpair(RegPair::DE, de.wrapping_add(inc as u16));
    r.set_regpair(RegPair::HL, hl.wrapping_add(inc as u16));
    r.set_regpair(RegPair::BC, bc);

    let n = val.wrapping_add(r.A);
    r.set_flag(Flag::F5, (n & (1 << 1)) != 0);
    r.set_flag(Flag::F3, (n & (1 << 3)) != 0);
    r.set_flag(Flag::H, false);
    r.set_flag(Flag::N, false);
    r.set_flag(Flag::PV, bc != 0);
}

fn mem_block_cmp(r: &mut Regs, mem: &mut mem::Memory, inc: i8) -> bool {
    let hl = r.get_regpair(RegPair::HL);
    let bc = r.get_regpair(RegPair::BC).wrapping_sub(1);
    let a = r.A as i8;
    let val = mem.fetch_u8(hl) as i8;
    set_conditions_sub8_base(r, a, val, 0);
    let n = a.wrapping_sub(val).wrapping_sub(((r.F & H) >> 4) as i8);
    r.set_flag(Flag::F5, n & (1 << 1) != 0);
    r.set_flag(Flag::F3, n & (1 << 3) != 0);
    r.set_flag(Flag::PV, bc != 0);
    r.set_regpair(RegPair::HL, hl.wrapping_add(inc as u16));
    r.set_regpair(RegPair::BC, bc);
    // MEMPTR = MEMPTR + 1
    r.MEMPTR = r.MEMPTR.wrapping_add(inc as u16);

    val == a
}

fn mem_block_copy_repeat(r: &mut Regs, op_len: &mut usize) -> bool {
    let bc = r.get_regpair(RegPair::BC);
    if bc == 0 {
        return true;
    }
    // add looping tstate
    *op_len += 5;
    r.MEMPTR = r.PC.wrapping_add(1);
    false
}

fn mem_block_cmp_repeat(r: &mut Regs, mem: &mut mem::Memory, inc: i8, op_len: &mut usize) -> bool {
    let found = mem_block_cmp(r, mem, inc);
    let bc = r.get_regpair(RegPair::BC);
    if bc == 0 || found {
        return true;
    }
    // add looping tstate
    *op_len += 5;
    r.MEMPTR = r.PC.wrapping_add(1);
    false
}

fn io_block_repeat(r: &Regs, op_len: &mut usize) -> bool {
    if r.B == 0 {
        return true;
    }
    *op_len += 5;
    false
}

pub fn init() -> State<'static> {
    State::default()
}

pub fn run_op(s: &mut State, op: &disas::OpCode) -> Result<usize, String> {
    let mut update_pc = true;
    let mut op_len: usize = op.tstates.iter().sum::<u8>() as usize;
    s.r.R = (((s.r.R & 0x7F) + 1) & 0x7F) | (s.r.R & 0x80);
    if op.length > 1 {
        match op.data[0] {
            0xCB | 0xDD | 0xFB | 0xED | 0xFD => {
                s.r.R = (((s.r.R & 0x7F) + 1) & 0x7F) | (s.r.R & 0x80)
            }
            _ => {}
        };
    }
    let start_f = s.r.F;
    let ret = |r: &mut Regs, mem: &mem::Memory, update_pc: &mut bool| {
        r.PC = mem.fetch_u16(r.SP);
        r.SP = r.SP.wrapping_add(2);
        r.MEMPTR = r.PC;
        *update_pc = false;
    };
    match op.ins {
        Instruction::ADD => {
            let op1 = op.op1.ok_or("add op1 missing")?;
            let op2 = op.op2.ok_or("add op2 missing")?;
            match op1.size().ok_or("unsupported add size")? {
                OpSize::S1 => {
                    let a = get_op8(s, op1) as i8;
                    let b = get_op8(s, op2) as i8;
                    let res = set_conditions_add_8(&mut s.r, a, b);
                    set_op8(s, op1, res);
                }
                OpSize::S2 => {
                    let a = get_op16(s, op1) as i16;
                    let b = get_op16(s, op2) as i16;
                    let res = set_conditions_add_16(&mut s.r, a, b, 0);
                    s.r.MEMPTR = a.wrapping_add(1) as u16;
                    set_op16(s, op1, res);
                }
            }
        }
        Instruction::ADC => {
            let op1 = op.op1.ok_or("adc op1 missing")?;
            let op2 = op.op2.ok_or("adc op2 missing")?;
            match op1.size().ok_or("unsupported add size")? {
                OpSize::S1 => {
                    let a = get_op8(s, op1) as i8;
                    let b = get_op8(s, op2) as i8;
                    let res = set_conditions_adc_8(&mut s.r, a, b);
                    set_op8(s, op1, res);
                }
                OpSize::S2 => {
                    let a = get_op16(s, op1) as i16;
                    let b = get_op16(s, op2) as i16;
                    let res = set_conditions_adc_16(&mut s.r, a, b);
                    s.r.MEMPTR = a.wrapping_add(1) as u16;
                    set_op16(s, op1, res);
                }
            }
        }
        Instruction::SUB => {
            let op1 = op.op1.ok_or("sub op1 missing")?;
            let a = s.r.A as i8;
            let b = get_op8(s, op1) as i8;
            let res = set_conditions_sub_8(&mut s.r, a, b);
            s.r.A = res;
        }
        Instruction::SBC => {
            let op1 = op.op1.ok_or("sbc op1 missing")?;
            let op2 = op.op2.ok_or("sbc op2 missing")?;
            match op1.size().ok_or("unsupported sbc size")? {
                OpSize::S1 => {
                    let a = get_op8(s, op1) as i8;
                    let b = get_op8(s, op2) as i8;
                    let res = set_conditions_sbc_8(&mut s.r, a, b);
                    set_op8(s, op1, res);
                }
                OpSize::S2 => {
                    let a = get_op16(s, op1) as i16;
                    let b = get_op16(s, op2) as i16;
                    let res = set_conditions_sbc_16(&mut s.r, a, b);
                    s.r.MEMPTR = a.wrapping_add(1) as u16;
                    set_op16(s, op1, res);
                }
            }
        }
        Instruction::NOP => {
            // nothing to do
        }
        Instruction::LD => {
            let op1 = op.op1.ok_or("LD op1 missing")?;
            let op2 = op.op2.ok_or("LD op2 missing")?;
            // transfer size == min(op1, op1)
            let size = op2
                .size()
                .or_else(|| op1.size())
                .ok_or("unsupported ld size")?;
            match size {
                OpSize::S1 => {
                    let val = get_op8(s, op2);
                    set_op8(s, op1, val);
                }
                OpSize::S2 => {
                    let val = get_op16(s, op2);
                    set_op16(s, op1, val);
                }
            }
            let a_reg = Operand::Reg8(disas::Reg8::A);
            // LD A, I and LD A, R set condition bits
            if op1 == a_reg
                && (op2 == Operand::Reg8(disas::Reg8::R) || op2 == Operand::Reg8(disas::Reg8::I))
            {
                s.r.set_flag(Flag::PV, s.r.IFF2);
                s.r.set_flag(Flag::S, (s.r.A as i8) < 0);
                s.r.set_flag(Flag::Z, s.r.A == 0);
                s.r.set_flag(Flag::H, false);
                s.r.set_flag(Flag::N, false);
                copy_f53_res(s.r.A, &mut s.r);
            }
            // MEMPTR
            if op2 == a_reg {
                match op1 {
                    Operand::Address(addr) => {
                        // MEMPTR_low = (addr + 1) & #FF,  MEMPTR_hi = A
                        s.r.MEMPTR = addr.wrapping_add(1) & 0xFF | ((s.r.A as u16) << 8);
                    }
                    Operand::RegAddr(x) => {
                        if x == disas::Reg16::BC || x == disas::Reg16::DE {
                            let r = s.r.get_regpair(RegPair::from(x));
                            // MEMPTR_low = (rp + 1) & #FF,  MEMPTR_hi = A
                            s.r.MEMPTR = (r + 1) & 0xFF | ((s.r.A as u16) << 8);
                        }
                    }
                    _ => {}
                }
            } else if op1 == a_reg || size == OpSize::S2 {
                if let Operand::RegAddr(reg) = op2 {
                    if op1 != a_reg
                        || op2 == Operand::RegAddr(disas::Reg16::BC)
                        || op2 == Operand::RegAddr(disas::Reg16::DE)
                    {
                        s.r.MEMPTR = s.r.get_regpair(RegPair::from(reg)) + 1
                    }
                } else if let Operand::Address(addr) = op1 {
                    s.r.MEMPTR = addr + 1
                } else if let Operand::Address(addr) = op2 {
                    s.r.MEMPTR = addr + 1
                }
            }
        }
        Instruction::INC | Instruction::DEC => {
            let op1 = op.op1.ok_or("INC op1 missing")?;
            let inc = if op.ins == Instruction::INC { 1 } else { -1 };
            let size = match op1 {
                Operand::RegAddr(_) => OpSize::S1,
                Operand::RegI(_) => OpSize::S1,
                _ => op1.size().ok_or("unsupported INC source size")?,
            };
            match size {
                OpSize::S1 => {
                    // Sets conditions
                    let val = get_op8(s, op1) as i8;
                    let res = set_conditions_inc8_dec8(&mut s.r, val, inc);
                    set_op8(s, op1, res);
                }
                OpSize::S2 => {
                    // Does not set conditions
                    let val = get_op16(s, op1);
                    let res = val.wrapping_add(inc as u16);
                    set_op16(s, op1, res);
                }
            }
        }
        Instruction::RLC => {
            let op1 = op.op1.ok_or("RLC missing op1")?;
            let val = get_op8(s, op1);
            let val = val.rotate_left(1);
            set_op8(s, op1, val);
            if let Some(op2) = op.op2 {
                // Copy result to op2 as well
                set_op8(s, op2, val);
            }
            set_bitops_flags(val, &mut s.r);
            s.r.set_flag(Flag::C, val & 0x1 != 0);
            s.r.set_flag(Flag::H, false);
        }
        Instruction::RLCA => {
            s.r.A = s.r.A.rotate_left(1);
            s.r.set_flag(Flag::C, s.r.A & 0x1 != 0);
            s.r.set_flag(Flag::H, false);
            s.r.set_flag(Flag::N, false);
            copy_f53_res(s.r.A, &mut s.r);
        }
        Instruction::RL => {
            let op1 = op.op1.ok_or("RL missing op1")?;
            let c = s.r.F & C;
            let val = get_op8(s, op1);
            let cp = val & 0x80 != 0;
            let val = val << 1 | c;
            set_op8(s, op1, val);
            if let Some(op2) = op.op2 {
                // Copy result to op2 as well
                set_op8(s, op2, val);
            }
            set_bitops_flags(val, &mut s.r);
            s.r.set_flag(Flag::C, cp);
            s.r.set_flag(Flag::H, false);
        }
        Instruction::RLA => {
            let a = s.r.A;
            let c = s.r.F & C;
            s.r.set_flag(Flag::C, s.r.A & 0x80 != 0);
            s.r.A = a << 1 | c;
            s.r.set_flag(Flag::H, false);
            s.r.set_flag(Flag::N, false);
            copy_f53_res(s.r.A, &mut s.r);
        }
        Instruction::RRC => {
            let op1 = op.op1.ok_or("RRC missing op1")?;
            let val = get_op8(s, op1);
            let val = val.rotate_right(1);
            set_op8(s, op1, val);
            if let Some(op2) = op.op2 {
                // Copy result to op2 as well
                set_op8(s, op2, val);
            }
            set_bitops_flags(val, &mut s.r);
            s.r.set_flag(Flag::C, val & 0x80 != 0);
            s.r.set_flag(Flag::H, false);
        }
        Instruction::RRCA => {
            s.r.A = s.r.A.rotate_right(1);
            s.r.set_flag(Flag::C, s.r.A & 0x80 != 0);
            s.r.set_flag(Flag::H, false);
            s.r.set_flag(Flag::N, false);
            copy_f53_res(s.r.A, &mut s.r);
        }
        Instruction::RR => {
            let op1 = op.op1.ok_or("RR missing op1")?;
            let c = s.r.F & C;
            let val = get_op8(s, op1);
            let cp = val & 0x1 != 0;
            let val = val >> 1 | (c << 7);
            set_op8(s, op1, val);
            if let Some(op2) = op.op2 {
                // Copy result to op2 as well
                set_op8(s, op2, val);
            }
            set_bitops_flags(val, &mut s.r);
            s.r.set_flag(Flag::C, cp);
            s.r.set_flag(Flag::H, false);
        }
        Instruction::RRA => {
            let a = s.r.A;
            let c = s.r.F & C;
            s.r.set_flag(Flag::C, s.r.A & 0x1 != 0);
            s.r.A = a >> 1 | (c << 7);
            s.r.set_flag(Flag::H, false);
            s.r.set_flag(Flag::N, false);
            copy_f53_res(s.r.A, &mut s.r);
        }
        Instruction::EX => {
            let op1 = op.op1.ok_or("EX op1 missing")?;
            let op2 = op.op2.ok_or("EX op2 missing")?;
            let a = get_op16(s, op1);
            let b = get_op16(s, op2);
            set_op16(s, op1, b);
            set_op16(s, op2, a);
            if let Operand::RegAddr(disas::Reg16::SP) = op1 {
                // MEMPTR = rp value after the operation
                s.r.MEMPTR = a;
            }
        }
        Instruction::DJNZ => {
            let op1 = op.op1.ok_or("No immediate arg for DJNZ")?;
            let jump = if let Operand::RelAddr(j) = op1 {
                j
            } else {
                return Err(format!("Arg {} not rel addr for DJNZ", op1));
            };
            s.r.B = s.r.B.wrapping_sub(1);
            if s.r.B != 0 {
                s.r.PC = (s.r.PC as i32 + jump as i32) as u16;
                s.r.MEMPTR = s.r.PC;
                update_pc = false;
            } else {
                op_len = 8 // [5, 3]
            }
        }
        Instruction::JR => {
            let op1 = op.op1.ok_or("No immediate arg for DJNZ")?;
            if let Operand::RelAddr(j) = op1 {
                s.r.PC = (s.r.PC as i32 + j as i32) as u16;
                update_pc = false;
                s.r.MEMPTR = s.r.PC;
            } else {
                let op2 = op.op2.ok_or("No second rel addr operand for JR")?;
                if let Operand::FlagCondition(cond) = op1 {
                    if let Operand::RelAddr(j) = op2 {
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
        Instruction::DAA => daa(&mut s.r),
        Instruction::CPL => {
            s.r.A = !s.r.A;
            s.r.set_flag(Flag::H, true);
            s.r.set_flag(Flag::N, true);
            copy_f53_res(s.r.A, &mut s.r);
        }
        Instruction::SCF => {
            s.r.set_flag(Flag::C, true);
            s.r.set_flag(Flag::H, false);
            s.r.set_flag(Flag::N, false);
            if s.q.changed {
                copy_f53_res(s.r.A, &mut s.r);
            } else {
                // OR instead
                s.r.set_flag(Flag::F5, ((s.r.A & F5) | (s.r.F & F5)) != 0);
                s.r.set_flag(Flag::F3, ((s.r.A & F3) | (s.r.F & F3)) != 0);
            }
        }
        Instruction::CCF => {
            s.r.set_flag(Flag::H, s.r.F & C != 0);
            s.r.set_flag(Flag::C, s.r.F & C == 0);
            s.r.set_flag(Flag::N, false);
            if s.q.changed {
                copy_f53_res(s.r.A, &mut s.r);
            } else {
                // OR instead
                s.r.set_flag(Flag::F5, ((s.r.A & F5) | (s.r.F & F5)) != 0);
                s.r.set_flag(Flag::F3, ((s.r.A & F3) | (s.r.F & F3)) != 0);
            }
        }
        Instruction::HALT => {
            s.halted = true;
            update_pc = false;
        }
        Instruction::AND => {
            let op1 = op.op1.ok_or("AND op1 missing")?;
            s.r.A &= get_op8(s, op1);
            s.r.set_flag(Flag::H, true);
            set_bitops_flags_c(s.r.A, &mut s.r);
        }
        Instruction::OR => {
            let op1 = op.op1.ok_or("AND op1 missing")?;
            s.r.A |= get_op8(s, op1);
            s.r.set_flag(Flag::H, false);
            set_bitops_flags_c(s.r.A, &mut s.r);
        }
        Instruction::XOR => {
            let op1 = op.op1.ok_or("AND op1 missing")?;
            s.r.A ^= get_op8(s, op1);
            s.r.set_flag(Flag::H, false);
            set_bitops_flags_c(s.r.A, &mut s.r);
        }
        Instruction::CP => {
            let op1 = op.op1.ok_or("CP op1 missing")?;
            let a = s.r.A as i8;
            let b = get_op8(s, op1) as i8;
            set_conditions_sub_8(&mut s.r, a, b);
            copy_f53_res(b as u8, &mut s.r);
        }
        Instruction::RET => {
            let mut jump = true;
            if let Some(Operand::FlagCondition(cond)) = op.op1 {
                if !cond_valid(&s.r, cond) {
                    op_len = 5;
                    jump = false;
                }
            }
            if jump {
                ret(&mut s.r, &s.mem, &mut update_pc);
            }
        }
        Instruction::POP => {
            let op1 = op.op1.ok_or("POP op1 missing")?;
            let val = s.mem.fetch_u16(s.r.SP);
            set_op16(s, op1, val);
            s.r.SP = s.r.SP.wrapping_add(2);
        }
        Instruction::PUSH => {
            let op1 = op.op1.ok_or("PUSH op1 missing")?;
            let arg = get_op16(s, op1);
            s.r.SP = s.r.SP.wrapping_sub(2);
            s.mem.set_u16(s.r.SP, arg);
        }
        Instruction::JP => {
            if let Some(Operand::FlagCondition(cond)) = op.op1 {
                let op2 = op.op2.ok_or("JP op2 missing")?;
                let addr = get_op16(s, op2);
                if cond_valid(&s.r, cond) {
                    s.r.PC = addr;
                    update_pc = false;
                }
                s.r.MEMPTR = addr;
            } else {
                let op1 = op.op1.ok_or("JP op1 missing")?;
                let addr = get_op16(s, op1);
                s.r.PC = addr;
                update_pc = false;
                if let Operand::Imm16(_) = op1 {
                    s.r.MEMPTR = addr;
                }
            }
        }
        Instruction::CALL => {
            let mut jump = true;
            let addr;
            if let Some(Operand::FlagCondition(cond)) = op.op1 {
                addr = get_op16(s, op.op2.ok_or("CALL missing op2")?);
                if !cond_valid(&s.r, cond) {
                    op_len = 10; // [4, 3, 3]
                    jump = false;
                }
            } else {
                addr = get_op16(s, op.op1.ok_or("CALL missing op1")?);
            }
            if jump {
                s.r.SP = s.r.SP.wrapping_sub(2);
                s.mem.set_u16(s.r.SP, s.r.PC.wrapping_add(op.length as u16));
                s.r.PC = addr;
                update_pc = false;
            }
            s.r.MEMPTR = addr;
        }
        Instruction::RST => {
            let op1 = op.op1.ok_or("RST op1 missing")?;
            s.r.SP = s.r.SP.wrapping_sub(2);
            s.mem.set_u16(s.r.SP, s.r.PC.wrapping_add(op.length as u16));
            let addr = get_op8(s, op1) as u16;
            s.r.PC = addr;
            s.r.MEMPTR = addr;
            update_pc = false;
        }
        Instruction::SLA => {
            let op1 = op.op1.ok_or("SLA missing op1")?;
            let val = get_op8(s, op1);
            let cp = val & 0x80 != 0;
            let val = val << 1;
            set_op8(s, op1, val);
            if let Some(op2) = op.op2 {
                // Copy result to op2 as well
                set_op8(s, op2, val);
            }
            set_bitops_flags(val, &mut s.r);
            s.r.set_flag(Flag::C, cp);
            s.r.set_flag(Flag::H, false);
        }
        Instruction::SRA => {
            let op1 = op.op1.ok_or("SRA missing op1")?;
            let val = get_op8(s, op1);
            let cp = val & 0x1 != 0;
            let hi = val & 0x80;
            let val = (val >> 1) | hi;
            set_op8(s, op1, val);
            if let Some(op2) = op.op2 {
                // Copy result to op2 as well
                set_op8(s, op2, val);
            }
            set_bitops_flags(val, &mut s.r);
            s.r.set_flag(Flag::C, cp);
            s.r.set_flag(Flag::H, false);
        }
        Instruction::SLL => {
            let op1 = op.op1.ok_or("SLL missing op1")?;
            let val = get_op8(s, op1);
            let cp = val & 0x80 != 0;
            let val = val << 1 | 1;
            set_op8(s, op1, val);
            if let Some(op2) = op.op2 {
                // Copy result to op2 as well
                set_op8(s, op2, val);
            }
            set_bitops_flags(val, &mut s.r);
            s.r.set_flag(Flag::C, cp);
            s.r.set_flag(Flag::H, false);
        }
        Instruction::SRL => {
            let op1 = op.op1.ok_or("SRL missing op1")?;
            let val = get_op8(s, op1);
            let cp = val & 0x1 != 0;
            let val = val >> 1;
            set_op8(s, op1, val);
            if let Some(op2) = op.op2 {
                // Copy result to op2 as well
                set_op8(s, op2, val);
            }
            set_bitops_flags(val, &mut s.r);
            s.r.set_flag(Flag::C, cp);
            s.r.set_flag(Flag::H, false);
        }
        Instruction::BIT => {
            let op1 = op.op1.ok_or("BIT missing op1")?;
            let op2 = op.op2.ok_or("BIT missing op2")?;
            let shift = get_op8(s, op1);
            let val = get_op8(s, op2);

            s.r.set_flag(Flag::H, true);
            set_bitops_flags(val & (1 << shift), &mut s.r);
            /* According to the FUSE test suite, real hardware uses all bits for F5 and F3,
             * opposite to what Sean Young's Z80 undocumented says; we'll need to verify this
             * later, but trust the more recent FUSE interpretation for now */
            copy_f53_res(val, &mut s.r);
            if let Operand::RegI(i) = op2 {
                let val = reg_i_addr(&i, &s.r);
                copy_f53_res(((val >> 8) & 0xFF) as u8, &mut s.r);
            } else if op2 == Operand::RegAddr(disas::Reg16::HL) {
                // infamous MEMPTR leaking
                copy_f53_res(((s.r.MEMPTR >> 8) & 0xFF) as u8, &mut s.r);
            }
        }
        Instruction::RES => {
            let op1 = op.op1.ok_or("RES missing op1")?;
            let op2 = op.op2.ok_or("RES missing op2")?;
            let shift = get_op8(s, op1);
            let val = get_op8(s, op2) & (!(1 << shift));
            set_op8(s, op2, val);
            if let Some(op3) = op.op3 {
                // Copy result to op3 as well
                set_op8(s, op3, val);
            }
        }
        Instruction::SET => {
            let op1 = op.op1.ok_or("SET missing op1")?;
            let op2 = op.op2.ok_or("SET missing op2")?;
            let shift = get_op8(s, op1);
            let val = get_op8(s, op2) | (1 << shift);
            set_op8(s, op2, val);
            if let Some(op3) = op.op3 {
                // Copy result to op3 as well
                set_op8(s, op3, val);
            }
        }
        Instruction::OUT => {
            let op1 = op.op1.ok_or("OUT missing op1")?;
            let op2 = op.op2.ok_or("OUT missing op2")?;
            if let Operand::IOAddress(addr) = op1 {
                if op2 != Operand::Reg8(disas::Reg8::A) {
                    return Err("Op2 should be A".to_string());
                }
                let val = match op.op3 {
                    Some(Operand::IgnoreIO) => 0,
                    _ => s.r.A,
                };
                let addr = addr as u16 | ((s.r.A as u16) << 8);
                s.io.out(addr, val)?;
                // MEMPTR_low = (port + 1) & #FF,  MEMPTR_hi = A
                let low = addr.wrapping_add(1) as u16;
                s.r.MEMPTR = low & 0xFF | ((val as u16) << 8);
            } else if Operand::RegIOAddr(disas::Reg8::C) == op1 {
                let val = get_op8(s, op2);
                let addr = s.r.get_regpair(RegPair::BC);
                s.io.out(addr, val)?;

                // MEMPTR = BC + 1
                s.r.MEMPTR = addr.wrapping_add(1);
            }
        }
        Instruction::IN => {
            let op1 = op.op1.ok_or("IN missing op1")?;
            let op2 = op.op2.ok_or("IN missing op2")?;
            if let Operand::IOAddress(addr) = op2 {
                if op1 != Operand::Reg8(disas::Reg8::A) {
                    return Err("IN op1 should be A".to_string());
                }
                // MEMPTR = (A_before_operation << 8) + port + 1
                let low = addr.wrapping_add(1) as u16;
                let addr = addr as u16 | ((s.r.A as u16) << 8);
                s.r.MEMPTR = low & 0xFF | ((s.r.A as u16) << 8);
                let val = s.io.input(addr)?;
                set_op8(s, op1, val);
            } else if Operand::RegIOAddr(disas::Reg8::C) == op2 {
                // MEMPTR = BC + 1
                let addr = s.r.get_regpair(RegPair::BC);
                s.r.MEMPTR = addr.wrapping_add(1);
                let val = s.io.input(addr)?;
                if op.op3 != Some(Operand::IgnoreIO) {
                    set_op8(s, op1, val);
                }
                //flags
                set_bitops_flags(val, &mut s.r);
                s.r.set_flag(Flag::H, false);
            }
        }
        Instruction::EXX => {
            // waiting for https://github.com/rust-lang/rust/issues/71126
            fn swap(a: &mut u8, b: &mut u8) {
                let tmp1 = *a;
                let tmp2 = *b;
                *a = tmp2;
                *b = tmp1;
            }
            swap(&mut s.r.B, &mut s.r.Bp);
            swap(&mut s.r.C, &mut s.r.Cp);
            swap(&mut s.r.D, &mut s.r.Dp);
            swap(&mut s.r.E, &mut s.r.Ep);
            swap(&mut s.r.H, &mut s.r.Hp);
            swap(&mut s.r.L, &mut s.r.Lp);
        }
        Instruction::NEG => {
            let a = s.r.A as i8;
            let res = set_conditions_sub_8(&mut s.r, 0, a);
            s.r.A = res;
        }
        Instruction::RETN => {
            ret(&mut s.r, &s.mem, &mut update_pc);
            s.r.IFF1 = s.r.IFF2;
        }
        Instruction::RETI => {
            ret(&mut s.r, &s.mem, &mut update_pc);
        }
        Instruction::IM => {
            let op1 = op.op1.ok_or("IM missing op1")?;
            s.r.IM = get_op8(s, op1);
        }
        Instruction::RRD => {
            let a = s.r.A;
            let addr = s.r.get_regpair(RegPair::HL);
            let b = s.mem.fetch_u8(addr);
            s.mem.set_u8(addr, b >> 4 | (a << 4));
            s.r.A = (a & 0xF0) | (b & 0x0F);
            s.r.set_flag(Flag::H, false);
            set_bitops_flags(s.r.A, &mut s.r);
            s.r.MEMPTR = addr.wrapping_add(1);
        }
        Instruction::RLD => {
            let a = s.r.A;
            let addr = s.r.get_regpair(RegPair::HL);
            let b = s.mem.fetch_u8(addr);
            s.mem.set_u8(addr, b << 4 | (a & 0x0F));
            s.r.A = (a & 0xF0) | (b >> 4);
            s.r.set_flag(Flag::H, false);
            set_bitops_flags(s.r.A, &mut s.r);
            s.r.MEMPTR = addr.wrapping_add(1);
        }
        Instruction::LDI => mem_block_copy(&mut s.r, &mut s.mem, 1),
        Instruction::LDIR => {
            mem_block_copy(&mut s.r, &mut s.mem, 1);
            update_pc = mem_block_copy_repeat(&mut s.r, &mut op_len);
        }
        Instruction::LDD => mem_block_copy(&mut s.r, &mut s.mem, -1),
        Instruction::LDDR => {
            mem_block_copy(&mut s.r, &mut s.mem, -1);
            update_pc = mem_block_copy_repeat(&mut s.r, &mut op_len);
        }
        Instruction::CPI => {
            mem_block_cmp(&mut s.r, &mut s.mem, 1);
        }
        Instruction::CPIR => {
            update_pc = mem_block_cmp_repeat(&mut s.r, &mut s.mem, 1, &mut op_len);
        }
        Instruction::CPD => {
            mem_block_cmp(&mut s.r, &mut s.mem, -1);
        }
        Instruction::CPDR => {
            update_pc = mem_block_cmp_repeat(&mut s.r, &mut s.mem, -1, &mut op_len);
        }
        Instruction::INI => input_block(s, 1)?,
        Instruction::INIR => {
            input_block(s, 1)?;
            update_pc = io_block_repeat(&s.r, &mut op_len);
        }
        Instruction::IND => input_block(s, -1)?,
        Instruction::INDR => {
            input_block(s, -1)?;
            update_pc = io_block_repeat(&s.r, &mut op_len);
        }
        Instruction::OUTI => output_block(s, 1)?,
        Instruction::OTIR => {
            output_block(s, 1)?;
            update_pc = io_block_repeat(&s.r, &mut op_len);
        }
        Instruction::OUTD => output_block(s, -1)?,
        Instruction::OTDR => {
            output_block(s, -1)?;
            update_pc = io_block_repeat(&s.r, &mut op_len);
        }
        Instruction::DI => {
            s.r.IFF1 = false;
            s.r.IFF2 = false;
        }
        Instruction::EI => {
            s.r.IFF1 = true;
            s.r.IFF2 = true;
        }
    }
    memptr_index(op, &mut s.r);
    s.q.changed = start_f != s.r.F || op.ins == Instruction::SCF;
    if update_pc {
        s.r.PC += op.length as u16;
    }

    Ok(op_len)
}

pub fn run(s: &mut State, tstates_len: usize, debug: bool) -> Result<(), String> {
    let mut tstates_len = tstates_len;
    while tstates_len > 0 {
        // TODO: split into m states, fetch etc
        let disas_target = s.mem.fetch_range_safe(s.r.PC, 4);
        if let Some(op) = disas::disas(disas_target) {
            if debug {
                println!("{:04X}: {:?}", s.r.PC, op);
                //println!("{}", s.r);
            }
            let op_len = run_op(s, &op)?;
            tstates_len = usize::saturating_sub(tstates_len, op_len);
        } else {
            return Err(format!("Unknown instruction(s)) {:02X?}", disas_target));
        }
    }
    Ok(())
}
pub use disas::DisasCache;
pub fn run_cached(
    c: &disas::DisasCache,
    s: &mut State,
    tstates_len: usize,
    debug: bool,
) -> Result<(), String> {
    let mut tstates_len = tstates_len;
    while tstates_len > 0 {
        // TODO: split into m states, fetch etc
        let disas_target = s.mem.fetch_range_safe(s.r.PC, 4);
        if let Some(op) = c.disas(disas_target) {
            if debug {
                println!("{:04X}: {:?}", s.r.PC, op);
            }
            let op_len = run_op(s, &op)?;
            tstates_len = usize::saturating_sub(tstates_len, op_len);
        } else {
            return Err(format!("Unknown instruction(s)) {:02X?}", disas_target));
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
        run(&mut s, 1, true).unwrap();
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
        run(&mut s, 1, true).unwrap();
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
        run(&mut s, 1, true).unwrap();
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
    #[test]
    fn run_adc() {
        let mut s = init();
        s.r.A = 1;
        //7FFF BBCC DDEE 4411 DD88 FD77 C000
        s.r.set_regpair(RegPair::AF, 0x7FFF);
        s.r.set_regpair(RegPair::BC, 0xBBCC);
        s.r.set_regpair(RegPair::DE, 0xDDEE);
        s.r.set_regpair(RegPair::HL, 0x4411);

        let default = s.r;
        dbg!(&default);
        s.mem = mem::Memory::from(vec![0xCE, 0x00]);
        run(&mut s, 1, true).unwrap();
        assert_eq!(
            s.r,
            Regs {
                A: 0x80,
                F: 0x94,
                PC: 2,
                R: 1,
                ..default
            },
        );

        s.r = default;
        //FFFF BBCC DDEE 4411 DD88 FD77 C000
        s.r.set_regpair(RegPair::AF, 0xFFFF);
        dbg!(&s.r);
        s.mem = mem::Memory::from(vec![0xCE, 0x80]);
        run(&mut s, 1, true).unwrap();
        assert_eq!(
            s.r,
            Regs {
                A: 0x80,
                F: 0x91,
                PC: 2,
                R: 1,
                ..default
            },
        );

        s.r = default;
        //7FFF BBCC DDEE 4411 DD88 FD77 C000
        dbg!(&s.r);
        s.mem = mem::Memory::from(vec![0xCE, 0x80]);
        run(&mut s, 1, true).unwrap();
        assert_eq!(
            s.r,
            Regs {
                A: 0,
                F: 0x51,
                PC: 2,
                R: 1,
                ..default
            },
        );
        s.r = default;
        //00FF BBCC DDEE 4411 DD88 FD77 C000
        s.r.set_regpair(RegPair::AF, 0x00FF);
        dbg!(&s.r);
        s.mem = mem::Memory::from(vec![0xCE, 0x00]);
        run(&mut s, 1, true).unwrap();
        assert_eq!(
            s.r,
            Regs {
                A: 0x01,
                F: 0x00,
                PC: 2,
                R: 1,
                ..default
            },
        );
        s.r = default;
        //FFFF BBCC DDEE 4411 DD88 FD77 C000
        s.r.set_regpair(RegPair::AF, 0xFFFF);
        dbg!(&s.r);
        s.mem = mem::Memory::from(vec![0xCE, 0x01]);
        run(&mut s, 1, true).unwrap();
        assert_eq!(
            s.r,
            Regs {
                A: 0x01,
                F: 0x11,
                PC: 2,
                R: 1,
                ..default
            },
        );
        s.r = default;
        //55FF BBCC DDEE 4411 DD88 FD77 C000
        s.r.set_regpair(RegPair::AF, 0x55FF);
        dbg!(&s.r);
        s.mem = mem::Memory::from(vec![0xCE, 0x80]);
        run(&mut s, 1, true).unwrap();
        assert_eq!(
            s.r,
            Regs {
                A: 0xD6,
                F: 0x80,
                PC: 2,
                R: 1,
                ..default
            },
        );
    }
}
