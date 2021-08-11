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
/*
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum RegSpe {
    I,
    R,
    PC,
}
*/

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
    Imm8(u8),     // immediate addressing
    Imm16(u16),   // immediate extended adressing
    RelAddr(i8),  // Relative addressing
    Address(u16), // extended addressing
    RegI(u8),     // indexed addressing
    Reg8(Reg8),   // 8 bit register
    Reg16(Reg16), // 16 bit register
    RegAddr(Reg16), //register indirect addressing

                  //RegSpe(RegSpe), // special register ? XXX ?
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum OpSize {
    S1,
    S2,
}

impl Operand {
    pub fn size(&self) -> Option<OpSize> {
        match self {
            Operand::Imm8(_) => Some(OpSize::S1),
            Operand::Imm16(_) => Some(OpSize::S2),
            Operand::RelAddr(_) => None,
            Operand::Address(_) => None,
            Operand::RegI(_) => None,
            Operand::Reg8(_) => Some(OpSize::S1),
            Operand::Reg16(_) => Some(OpSize::S2),
            Operand::RegAddr(_) => None,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Instruction {
    ADD,
    ADC,
    NOP,
    LD,
    INC,
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

fn decode_operand_reg_ddss(arg: u8) -> Reg16 {
    match arg & 0x3 {
        0 => Reg16::BC,
        1 => Reg16::DE,
        2 => Reg16::HL,
        3 => Reg16::SP,
        _ => panic!("unreachable"),
    }
}

fn decode_operand_reg_r(arg: u8) -> Reg8 {
    match arg & 0x7 {
        0 => Reg8::B,
        1 => Reg8::C,
        2 => Reg8::D,
        3 => Reg8::E,
        4 => Reg8::H,
        5 => Reg8::L,
        7 => Reg8::A,
        _ => panic!("unknown reg arg {}", arg),
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
        0x00 => {
            return Some(OpCode {
                data: vec![ins[0]],
                length: 1,
                ins: Instruction::NOP,
                op1: None,
                op2: None,
                mcycles: 1,
                tstates: vec![4],
            });
        }
        0x02 | 0x12 => {
            // LD (BC), A
            // LD (DE), A
            let reg = match ins[0] & 0x10 {
                0x10 => Reg16::DE,
                _ => Reg16::BC,
            };
            return Some(OpCode {
                data: vec![ins[0]],
                length: 1,
                ins: Instruction::LD,
                op1: Some(Operand::RegAddr(reg)),
                op2: Some(Operand::Reg8(Reg8::A)),
                mcycles: 2,
                tstates: vec![4, 3],
            });
        }
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
    if (ins[0] & 0xCF) == 0x01 {
        // LD dd, nn
        let arg = (ins[2] as u16) << 8 | ins[1] as u16;
        let reg = decode_operand_reg_ddss((ins[0] >> 4) & 0x3);
        let opcode = OpCode {
            data: vec![ins[0], ins[1], ins[2]],
            length: 3,
            ins: Instruction::LD,
            op1: Some(Operand::Reg16(reg)),
            op2: Some(Operand::Imm16(arg)),
            mcycles: 3, // error in datasheet page 99 ?
            tstates: vec![4, 3, 3],
        };
        return Some(opcode);
    }
    if (ins[0] & 0xCF) == 0x03 {
        // INC ss
        let reg = decode_operand_reg_ddss((ins[0] >> 4) & 0x3);
        let opcode = OpCode {
            data: vec![ins[0]],
            length: 1,
            ins: Instruction::INC,
            op1: Some(Operand::Reg16(reg)),
            op2: None,
            mcycles: 1,
            tstates: vec![6],
        };
        return Some(opcode);
    }
    if (ins[0] & 0xC7) == 0x04 {
        // INC r
        let op;
        let tstates;
        let opraw = (ins[0] >> 3) & 0x7;
        if opraw == 0x6 {
            op = Operand::RegAddr(Reg16::HL);
            tstates = vec![4, 4, 3];
        } else {
            let reg = decode_operand_reg_r(opraw);
            op = Operand::Reg8(reg);
            tstates = vec![4];
        }
        let opcode = OpCode {
            data: vec![ins[0]],
            length: 1,
            ins: Instruction::INC,
            op1: Some(op),
            op2: None,
            mcycles: tstates.len() as u8,
            tstates,
        };
        return Some(opcode);
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
        // LD dd, nn
        assert_eq!(
            disas(&[0x01, 0x42, 0x10]),
            Some(OpCode {
                data: vec!(0x01, 0x42, 0x10),
                length: 3,
                ins: Instruction::LD,
                op1: Some(Operand::Reg16(Reg16::BC)),
                op2: Some(Operand::Imm16(0x1042)),
                mcycles: 3,
                tstates: vec!(4, 3, 3),
            })
        );
        // LD dd, nn
        assert_eq!(
            disas(&[0x21, 0x42, 0x10]),
            Some(OpCode {
                data: vec!(0x21, 0x42, 0x10),
                length: 3,
                ins: Instruction::LD,
                op1: Some(Operand::Reg16(Reg16::HL)),
                op2: Some(Operand::Imm16(0x1042)),
                mcycles: 3,
                tstates: vec!(4, 3, 3),
            })
        );
        // LD (DE), A
        assert_eq!(
            disas(&[0x12]),
            Some(OpCode {
                data: vec!(0x12),
                length: 1,
                ins: Instruction::LD,
                op1: Some(Operand::RegAddr(Reg16::DE)),
                op2: Some(Operand::Reg8(Reg8::A)),
                mcycles: 2,
                tstates: vec!(4, 3),
            })
        );
        // INC SP
        assert_eq!(
            disas(&[0x33]),
            Some(OpCode {
                data: vec!(0x33),
                length: 1,
                ins: Instruction::INC,
                op1: Some(Operand::Reg16(Reg16::SP)),
                op2: None,
                mcycles: 1,
                tstates: vec!(6),
            })
        );
        // INC D
        assert_eq!(
            disas(&[0x14]),
            Some(OpCode {
                data: vec!(0x14),
                length: 1,
                ins: Instruction::INC,
                op1: Some(Operand::Reg8(Reg8::D)),
                op2: None,
                mcycles: 1,
                tstates: vec!(4),
            })
        );
    }
}
