use crate::disas;

#[derive(Default, Debug, PartialEq, Clone, Copy)]
pub struct State {
    A: u8,
    F: u8,
}

fn get_op(s: &State, op: disas::Operand) -> u8 { // TODO: size
    match op {
        disas::Operand::Reg8(disas::Reg8::A) => s.A,
        _ => { 0 },
    }
}
fn set_op(s: &mut State, op: disas::Operand, val: u8) { // TODO: size
    match op {
        disas::Operand::Reg8(disas::Reg8::A) => s.A = val,
        _ => {},
    }
}

pub fn init() -> State {
    let mut s = State::default();
    s
}

pub fn run_op(s: &mut State, op: &disas::OpCode) {
    match op.ins {
        disas::Instruction::ADD => {
            if let Some(op1) = &op.op1 {
                let mut res = get_op(s, *op1);
                if let Some(op2) = op.op2 {
                    res += get_op(s, op2);
                }
                set_op(s, *op1, res);
                //TODO: flags
            }

        },
        _ => {}
    }
}

pub fn run(s: &mut State, ins: &[u8]) {
    let mut i = 0;
    while ins[i.. ins.len()].len() > 0 {
        if let Some(op) = disas::disas(&ins[i..ins.len()]) {
            run_op(s, &op);
            i += op.length as usize;

        }
    }
}

#[cfg(test)]
mod tests {
    use crate::cpu::*;
    #[test]
    fn run_add() {
        let mut s = init();
        s.A = 1;
        run(&mut s, &[0x87]);
        assert_eq!(s, State{
            A: 2,
            F: 0,
        })
    }
}
