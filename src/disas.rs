#[derive(Debug, PartialEq, Clone, Copy)]
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
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum RegSpe {
    I,
    R,
    PC,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Reg16 {
    AF,
    BC,
    DE,
    HL,
    SP,
}
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum RegI {
    IX,
    IY,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Operand {
    Reg8(Reg8),     // 8 bit register
    Reg16(Reg16),   // 16 bit register
    RegSpe(RegSpe), // special register
    Address(u16),   // extended addressing
    RegAddr(Reg16),
    RelAddr(i8), // Relative addressing
    Imm8(u8),    // immediate addressing
    Imm16(u16),  // immediate extended adressing
    RegI(u8),    // indexed addressing
}

#[derive(Debug, PartialEq, Clone)]
pub enum Instruction {
    ADD,
    ADC,
}

#[derive(Debug, PartialEq, Clone)]
pub struct OpCode {
    pub data: Vec<u8>,
    pub length: u8,
    pub ins: Instruction,
    pub op1: Option<Operand>,
    pub op2: Option<Operand>,
    pub mcycles: u8,
    pub tstates: Vec<u8>,
}

fn add_reg_operand(arg: u8) -> Option<Operand> {
    match arg {
        0x7 => Some(Operand::Reg8(Reg8::A)),
        0x0 => Some(Operand::Reg8(Reg8::B)),
        0x1 => Some(Operand::Reg8(Reg8::C)),
        0x2 => Some(Operand::Reg8(Reg8::D)),
        0x3 => Some(Operand::Reg8(Reg8::E)),
        0x4 => Some(Operand::Reg8(Reg8::H)),
        0x5 => Some(Operand::Reg8(Reg8::L)),
        0x6 => Some(Operand::RegAddr(Reg16::HL)),
        _ => None,
    }
}

// An instruction can be one to four bytes
pub fn disas(ins: &[u8]) -> Option<OpCode> {
    match ins[0] >> 3 {
        0x10 => {
            // ADD a, r
            // ADD a, (HL)
            let arg = ins[0] & 0x7;
            let opcode = OpCode {
                data: vec![ins[0]],
                length: 1,
                ins: Instruction::ADD,
                op1: Some(Operand::Reg8(Reg8::A)),
                op2: add_reg_operand(arg),
                mcycles: match arg {
                    0x6 => 2,
                    _ => 1,
                },
                tstates: match arg {
                    0x6 => vec![4, 3],
                    _ => vec![4],
                },
            };
            return Some(opcode);
        }
        0x11 => {
            //ADC a, r
            //ADC a, (HL)
            let arg = ins[0] & 0x7;
            let opcode = OpCode {
                data: vec![ins[0]],
                length: 1,
                ins: Instruction::ADC,
                op1: Some(Operand::Reg8(Reg8::A)),
                op2: add_reg_operand(arg),
                mcycles: match arg {
                    0x6 => 2,
                    _ => 1,
                },
                tstates: match arg {
                    0x6 => vec![4, 3],
                    _ => vec![4],
                },
            };
            return Some(opcode);
        }
        _ => {}
    }
    match ins[0] {
        0xC6 => {
            // ADD a, n
            let arg = ins[1];
            let opcode = OpCode {
                data: vec![ins[0], ins[1]],
                length: 2,
                ins: Instruction::ADD,
                op1: Some(Operand::Reg8(Reg8::A)),
                op2: Some(Operand::Imm8(arg)),
                mcycles: 2,
                tstates: vec![4, 3],
            };
            return Some(opcode);
        }
        0xCE => {
            // ADC a, n
            let arg = ins[1];
            let opcode = OpCode {
                data: vec![ins[0], ins[1]],
                length: 2,
                ins: Instruction::ADC,
                op1: Some(Operand::Reg8(Reg8::A)),
                op2: Some(Operand::Imm8(arg)),
                mcycles: 2,
                tstates: vec![4, 3],
            };
            return Some(opcode);
        }
        0xDD => {}
        _ => {}
    }
    None
}

#[cfg(test)]
mod tests {
    use crate::disas::*;
    #[test]
    fn disas_add() {
        // ADD a, a
        assert_eq!(
            disas(&[0x87]),
            Some(OpCode {
                data: vec!(0x87),
                length: 1,
                ins: Instruction::ADD,
                op1: Some(Operand::Reg8(Reg8::A)),
                op2: Some(Operand::Reg8(Reg8::A)),
                mcycles: 1,
                tstates: vec!(4),
            })
        );
        // ADD A, HL
        assert_eq!(
            disas(&[0x86]),
            Some(OpCode {
                data: vec!(0x86),
                length: 1,
                ins: Instruction::ADD,
                op1: Some(Operand::Reg8(Reg8::A)),
                op2: Some(Operand::RegAddr(Reg16::HL)),
                mcycles: 2,
                tstates: vec!(4, 3),
            })
        );
        // ADC A, HL
        assert_eq!(
            disas(&[0x8E]),
            Some(OpCode {
                data: vec!(0x8E),
                length: 1,
                ins: Instruction::ADC,
                op1: Some(Operand::Reg8(Reg8::A)),
                op2: Some(Operand::RegAddr(Reg16::HL)),
                mcycles: 2,
                tstates: vec!(4, 3),
            })
        );
        // ADD A, n
        assert_eq!(
            disas(&[0xC6, 0x42]),
            Some(OpCode {
                data: vec!(0xC6, 0x42),
                length: 2,
                ins: Instruction::ADD,
                op1: Some(Operand::Reg8(Reg8::A)),
                op2: Some(Operand::Imm8(0x42)),
                mcycles: 2,
                tstates: vec!(4, 3),
            })
        );
        // ADD A, n
        assert_eq!(
            disas(&[0xCE, 0x55]),
            Some(OpCode {
                data: vec!(0xCE, 0x55),
                length: 2,
                ins: Instruction::ADC,
                op1: Some(Operand::Reg8(Reg8::A)),
                op2: Some(Operand::Imm8(0x55)),
                mcycles: 2,
                tstates: vec!(4, 3),
            })
        );
    }
}
