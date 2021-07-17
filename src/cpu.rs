use crate::disas;
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
pub struct State {
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
    pub halted: bool,
}

impl State {
    pub fn set_regpair(&mut self, reg: RegPair, val: u16) {
        let r1 = (val & 0xFF00 >> 8) as u8;
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
}

fn flag(s: &str, f: u8) -> &str {
    if f != 0 {
        return s;
    }
    ""
}

impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "A: {:X} ({}) F: {} {} {} {} {} {}",
            self.A,
            self.A as i8,
            flag("S", self.F & 0x80),
            flag("Z", self.F & 0x40),
            flag("H", self.F & 0x10),
            flag("PV", self.F & 0x04),
            flag("N", self.F & 0x02),
            flag("C", self.F & 0x01)
        )
    }
}
fn get_op8(s: &State, op: disas::Operand) -> u8 {
    // TODO: size
    match op {
        disas::Operand::Reg8(disas::Reg8::A) => s.A,
        _ => {
            panic!("Unknown operand")
        }
    }
}

fn set_op8(s: &mut State, op: disas::Operand, val: u8) {
    // TODO: size
    match op {
        disas::Operand::Reg8(disas::Reg8::A) => s.A = val,
        _ => {
            panic!("Unknown operand")
        }
    }
}

fn get_op16(s: &State, op: disas::Operand) -> u16 {
    match op {
        disas::Operand::Imm16(x) => x,
        _ => panic!("Unknown operand")
    }
}

fn set_op16(s: &mut State, op: disas::Operand, val: u16) {
    match op {
        disas::Operand::Reg16(reg) => match reg {
            disas::Reg16::AF => s.set_regpair(RegPair::AF, val),
            disas::Reg16::BC => s.set_regpair(RegPair::BC, val),
            disas::Reg16::DE => s.set_regpair(RegPair::DE, val),
            disas::Reg16::HL => s.set_regpair(RegPair::HL, val),
            disas::Reg16::SP => s.SP = val,
        }
        _ => panic!("Unknown operand")
    }
}

fn set_flag(s: &mut State, f: Flag, val: bool) {
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
            set_flag(s, Flag::S, res < 0);
            set_flag(s, Flag::Z, res == 0);
            set_flag(s, Flag::H, ((a & 0xF) + (b & 0xF)) != res & 0xF);
            //dbg!(((a & 0xF) + (b & 0xF)) != res & 0xF);
            //set_flag(s, Flag::PV, real_res < i8::MIN as i16 || real_res > i8::MAX as i16);
            set_flag(
                s,
                Flag::PV,
                a.signum() == b.signum() && a.signum() != res.signum(),
            );
            set_flag(s, Flag::N, false);
            set_flag(s, Flag::C, (a as u8).overflowing_add(b as u8).1);
        }
        disas::Instruction::NOP => {
            // nothing to do
        }
        disas::Instruction::LD => {
            let op1 = op.op1.ok_or("LD op1 missing")?;
            let op2 = op.op2.ok_or("LD op2 missing")?;
            let val = get_op16(s, op2);
            dbg!(op1);
            dbg!(op2);
            set_op16(s, op1, val);
        }
        _ => return Err(format!("Unsupported opcode {:?}", op.ins)),
    }
    s.PC += op.length as u16;
    s.R = (s.R + 1) & 0x7F;

    Ok(())
}

pub fn run(s: &mut State, mem: &[u8], tstates_len: usize) -> Result<(), String> {
    let mut tstates_len = tstates_len;
    while tstates_len > 0 {
        // TODO: split into m states, fetch etc
        let disas_target = &mem[s.PC as usize..mem.len()];
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
        s.A = 1;
        let default = State::default();
        run(&mut s, &[0x87], 1).unwrap();
        assert_eq!(
            s,
            State {
                A: 2,
                F: 0,
                PC: 1,
                R: 1,
                ..default
            }
        );
        s = default;
        s.A = 64;
        run(&mut s, &[0x87], 1).unwrap();
        assert_eq!(
            s,
            State {
                A: -128_i8 as u8,
                F: 0x84,
                PC: 1,
                R: 1,
                ..default
            }
        );
        s = default;
        s.A = -1_i8 as u8;
        run(&mut s, &[0x87], 1).unwrap();
        assert_eq!(
            s,
            State {
                A: -2_i8 as u8,
                F: 0x91,
                PC: 1,
                R: 1,
                ..default
            }
        )
    }
}
