use crate::disas;
use crate::mem;
use std::fmt;

enum Flag {
    S,
    Z,
    H,
    PV,
    N,
    C,
}

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
}

#[derive(Default, Debug, PartialEq, Clone, Copy)]
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
        }
        (r1 as u16) << 8 | r2 as u16
    }
}

fn flag(s: &str, f: u8) -> &str {
    if f != 0 {
        return s;
    }
    ""
}

impl fmt::Display for Regs {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "AF: {:X} ({}) F: {} {} {} {} {} {} \n\
             BC: {:X} ({}) \n\
             DE: {:X} ({}) \n\
             HL: {:X} ({}) \n\
             AF': {:X} ({}) F: {} {} {} {} {} {} \n\
             BC': {:X} ({}) \n\
             DE': {:X} ({}) \n\
             HL': {:X} ({}) \n\
             SP: {:X}\n\
             PC: {:X}",
            self.get_regpair(RegPair::AF),
            self.get_regpair(RegPair::AF) as i8,
            flag("S", self.F & 0x80),
            flag("Z", self.F & 0x40),
            flag("H", self.F & 0x10),
            flag("PV", self.F & 0x04),
            flag("N", self.F & 0x02),
            flag("C", self.F & 0x01),
            self.get_regpair(RegPair::BC),
            self.get_regpair(RegPair::BC) as i8,
            self.get_regpair(RegPair::DE),
            self.get_regpair(RegPair::DE) as i8,
            self.get_regpair(RegPair::HL),
            self.get_regpair(RegPair::HL) as i8,
            self.get_regpair(RegPair::AFp),
            self.get_regpair(RegPair::AFp) as i8,
            flag("S", self.Fp & 0x80),
            flag("Z", self.Fp & 0x40),
            flag("H", self.Fp & 0x10),
            flag("PV", self.Fp & 0x04),
            flag("N", self.Fp & 0x02),
            flag("C", self.Fp & 0x01),
            self.get_regpair(RegPair::BCp),
            self.get_regpair(RegPair::BCp) as i8,
            self.get_regpair(RegPair::DEp),
            self.get_regpair(RegPair::DEp) as i8,
            self.get_regpair(RegPair::HLp),
            self.get_regpair(RegPair::HLp) as i8,
            self.SP,
            self.PC
        )
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
            disas::Reg8::Ap => s.r.Ap,
            disas::Reg8::Fp => s.r.Fp,
            disas::Reg8::Bp => s.r.Bp,
            disas::Reg8::Cp => s.r.Cp,
            disas::Reg8::Dp => s.r.Dp,
            disas::Reg8::Ep => s.r.Ep,
            disas::Reg8::Hp => s.r.Hp,
            disas::Reg8::Lp => s.r.Lp,
        },
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
            disas::Reg8::Ap => s.r.Ap = val,
            disas::Reg8::Fp => s.r.Fp = val,
            disas::Reg8::Bp => s.r.Bp = val,
            disas::Reg8::Cp => s.r.Cp = val,
            disas::Reg8::Dp => s.r.Dp = val,
            disas::Reg8::Ep => s.r.Ep = val,
            disas::Reg8::Hp => s.r.Hp = val,
            disas::Reg8::Lp => s.r.Lp = val,
        },
        disas::Operand::RegAddr(reg) => match reg {
            disas::Reg16::AF => s.mem.set_u8(s.r.get_regpair(RegPair::AF), val),
            disas::Reg16::BC => s.mem.set_u8(s.r.get_regpair(RegPair::BC), val),
            disas::Reg16::DE => s.mem.set_u8(s.r.get_regpair(RegPair::DE), val),
            disas::Reg16::HL => s.mem.set_u8(s.r.get_regpair(RegPair::HL), val),
            disas::Reg16::SP => s.mem.set_u8(s.r.SP, val),
        },
        _ => panic!(
            "Unknown operand {:?} or size not 8, or writing unsupported",
            op
        ),
    }
}

fn get_op16(s: &State, op: disas::Operand) -> u16 {
    match op {
        disas::Operand::Imm16(x) => x,
        disas::Operand::Reg16(reg) => match reg {
            disas::Reg16::AF => s.r.get_regpair(RegPair::AF),
            disas::Reg16::BC => s.r.get_regpair(RegPair::BC),
            disas::Reg16::DE => s.r.get_regpair(RegPair::DE),
            disas::Reg16::HL => s.r.get_regpair(RegPair::HL),
            disas::Reg16::SP => s.r.SP,
        },
        _ => panic!("Unknown operand {:?} or size not 16", op),
    }
}

fn set_op16(s: &mut State, op: disas::Operand, val: u16) {
    match op {
        disas::Operand::Reg16(reg) => match reg {
            disas::Reg16::AF => s.r.set_regpair(RegPair::AF, val),
            disas::Reg16::BC => s.r.set_regpair(RegPair::BC, val),
            disas::Reg16::DE => s.r.set_regpair(RegPair::DE, val),
            disas::Reg16::HL => s.r.set_regpair(RegPair::HL, val),
            disas::Reg16::SP => s.r.SP = val,
        },
        _ => panic!(
            "Unknown operand {:?} or size not 16, or writing unsupported",
            op
        ),
    }
}

fn set_flag(s: &mut Regs, f: Flag, val: bool) {
    let shift = match f {
        Flag::S => 7,
        Flag::Z => 6,
        Flag::H => 4,
        Flag::PV => 2,
        Flag::N => 1,
        Flag::C => 0,
    };
    if val {
        //set
        s.F |= 1 << shift;
    } else {
        s.F &= !(1 << shift);
    }
}

pub fn init() -> State {
    State::default()
}

pub fn run_op(s: &mut State, op: &disas::OpCode) -> Result<(), String> {
    match op.ins {
        disas::Instruction::ADD => {
            let op1 = op.op1.ok_or("add op1 missing")?;
            let op2 = op.op2.ok_or("add op2 missing")?;
            let a = get_op8(s, op1) as i8;
            let b: i8;
            b = get_op8(s, op2) as i8;
            let real_res = a as i16 + b as i16; // easier for overflows, etc
            let res = (real_res & 0xFF) as i8;
            set_op8(s, op1, res as u8);
            set_flag(&mut s.r, Flag::S, res < 0);
            set_flag(&mut s.r, Flag::Z, res == 0);
            set_flag(&mut s.r, Flag::H, ((a & 0xF) + (b & 0xF)) != res & 0xF);
            //dbg!(((a & 0xF) + (b & 0xF)) != res & 0xF);
            //set_flag(s, Flag::PV, real_res < i8::MIN as i16 || real_res > i8::MAX as i16);
            set_flag(
                &mut s.r,
                Flag::PV,
                a.signum() == b.signum() && a.signum() != res.signum(),
            );
            set_flag(&mut s.r, Flag::N, false);
            set_flag(&mut s.r, Flag::C, (a as u8).overflowing_add(b as u8).1);
        }
        disas::Instruction::NOP => {
            // nothing to do
        }
        disas::Instruction::LD => {
            let op1 = op.op1.ok_or("LD op1 missing")?;
            let op2 = op.op2.ok_or("LD op2 missing")?;
            match op2.size().ok_or("unsupported ld source size")? {
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
                    disas::Operand::Address(_) => {
                        // MEMPTR_low = (addr + 1) & #FF,  MEMPTR_hi = A
                        todo!()
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
                    _ => {
                        panic!("Unknown MEMPTR LD update op1 {:?}", op1)
                    }
                }
            }
        },
        disas::Instruction::INC => {
            let op1 = op.op1.ok_or("INC op1 missing")?;
            match op1.size().ok_or("unsupported INC source size")? {
                disas::OpSize::S1 => {
                    let val = get_op8(s, op1);
                    set_op8(s, op1, val + 1);
                }
                disas::OpSize::S2 => {
                    let val = get_op16(s, op1);
                    set_op16(s, op1, val + 1);
                }
            }
        },
        _ => return Err(format!("Unsupported opcode {:?}", op.ins)),
    }
    s.r.PC += op.length as u16;
    s.r.R = (s.r.R + 1) & 0x7F;

    Ok(())
}

pub fn run(s: &mut State, tstates_len: usize) -> Result<(), String> {
    let mut tstates_len = tstates_len;
    while tstates_len > 0 {
        // TODO: split into m states, fetch etc
        let disas_target = s.mem.fetch_range(s.r.PC, s.mem.len() as u16);
        if let Some(op) = disas::disas(disas_target) {
            run_op(s, &op)?;
            let op_len: usize = op.tstates.iter().fold(0, |sum, x| sum + (*x as usize));
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
                F: 0x91,
                PC: 1,
                R: 1,
                ..default
            },
        )
    }
}
