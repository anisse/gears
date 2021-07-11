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

pub fn run_op(s: &mut State, op: &disas::OpCode) {
    match op.ins {
        disas::Instruction::ADD => {
            if let Some(op1) = &op.op1 {
                let a = get_op8(s, *op1) as i8;
                let b: i8;
                if let Some(op2) = op.op2 {
                    b = get_op8(s, op2) as i8;
                } else {
                    panic!("Add with no second operand")
                }
                let real_res = a as i16 + b as i16; // easier for overflows, etc
                let res = (real_res & 0xFF) as i8;
                set_op8(s, *op1, res as u8);
                set_flag(s, Flag::S, res < 0);
                set_flag(s, Flag::Z, res == 0);
                set_flag(s, Flag::H, ((a & 0xF) + (b & 0xF)) != res & 0xF);
                dbg!(((a & 0xF) + (b & 0xF)) != res & 0xF);
                //set_flag(s, Flag::PV, real_res < i8::MIN as i16 || real_res > i8::MAX as i16);
                set_flag(
                    s,
                    Flag::PV,
                    a.signum() == b.signum() && a.signum() != res.signum(),
                );
                set_flag(s, Flag::N, false);
                set_flag(s, Flag::C, (a as u8).overflowing_add(b as u8).1);
            }
        }
        _ => {}
    }
}

pub fn run(s: &mut State, ins: &[u8]) -> Result<(), String> {
    let mut i = 0;
    while !ins[i..ins.len()].is_empty() {
        if let Some(op) = disas::disas(&ins[i..ins.len()]) {
            run_op(s, &op);
            i += op.length as usize;
        } else {
            return Err(format!("Unknown instruction {:#X}", ins[i]));
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
        run(&mut s, &[0x87]).unwrap();
        assert_eq!(s, State { A: 2, F: 0, ..default });
        s.A = 64;
        run(&mut s, &[0x87]).unwrap();
        assert_eq!(
            s,
            State {
                A: -128_i8 as u8,
                F: 0x84,
                ..default
            }
        );
        s.A = -1_i8 as u8;
        run(&mut s, &[0x87]).unwrap();
        assert_eq!(
            s,
            State {
                A: -2_i8 as u8,
                F: 0x91,
                ..default
            }
        )
    }
}
