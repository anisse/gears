#[derive(Debug, PartialEq)]
pub enum Reg8 {
    A,
    F,
    B,
    C,
    D,
    E,
    H,
    L,
    Ap,
    Fp,
    Bp,
    Cp,
    Dp,
    Ep,
    Hp,
    Lp,
}
#[derive(Debug, PartialEq)]
pub enum RegSpe {
    I,
    R,
    PC,
}

#[derive(Debug, PartialEq)]
pub enum Reg16 {
    AF,
    BC,
    DE,
    HL,
    SP,
}
#[derive(Debug, PartialEq)]
pub enum RegI {
    IX,
    IY,
}


#[derive(Debug, PartialEq)]
pub enum Operand {
    Reg8(Reg8), // 8 bit register
    Reg16(Reg16), // 16 bit register
    RegSpe(RegSpe), // special register
    Address(u16), // extended addressing
    RelAddr(i8), // Relative addressing
    Imm8(u8), // immediate addressing
    Imm16(u16), // immediate extended adressing
    RegI(u8), // indexed addressing
}

#[derive(Debug, PartialEq)]
pub struct OpCode {
    data: Vec<u8>,
    length: u8,
    op1: Option<Operand>,
    op2: Option<Operand>,
    mcycles: u8,
    tstates: Vec<u8>,
    tstates_total: u8,
}

// An instruction can be one to four bytes
pub fn disas(ins: &[u8]) -> Option<OpCode> {
    match ins[0] >> 4 {
        0x8 => {
            // Add ?
            let opcode = OpCode{
                data: vec!(ins[0]),
                length: 1,
                op1: Some(Operand::Reg8(Reg8::A)),
                op2: match ins[0] & 0x7 {
                    0x7 => Some(Operand::Reg8(Reg8::A)),
                    0x0 => Some(Operand::Reg8(Reg8::B)),
                    0x1 => Some(Operand::Reg8(Reg8::C)),
                    0x2 => Some(Operand::Reg8(Reg8::D)),
                    0x3 => Some(Operand::Reg8(Reg8::E)),
                    0x4 => Some(Operand::Reg8(Reg8::H)),
                    0x5 => Some(Operand::Reg8(Reg8::L)),
                    _ => None,
                },
                mcycles: 1,
                tstates: vec!(4),
                tstates_total: 4,
            };
            Some(opcode)
        }
        _ => {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::*;
    #[test]
    fn disas_add() {
        let data = vec!(0x87); // ADD a, a
        let res = disas(&data);
        assert_eq!(res , Some(OpCode{
            data: vec!(0x87),
            length: 1,
            op1: Some(Operand::Reg8(Reg8::A)),
            op2: Some(Operand::Reg8(Reg8::A)),
            mcycles: 1,
            tstates: vec!(4),
            tstates_total: 4,
        }));
    }
}
