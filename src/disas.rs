use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Reg8 {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    IXh,
    IXl,
    IYh,
    IYl,
    I,
    R,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Reg16 {
    AF,
    BC,
    DE,
    HL,
    SP,
    AFp,
    IX,
    IY,
}
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum RegI {
    IX(i8),
    IY(i8),
}
impl RegI {
    pub fn offset(&self) -> i8 {
        match self {
            RegI::IX(d) => *d,
            RegI::IY(d) => *d,
        }
    }
}
impl fmt::Display for RegI {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            RegI::IX(d) => write!(f, "(IX{:+})", d),
            RegI::IY(d) => write!(f, "(IY{:+})", d),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum FlagCondition {
    NZ,
    Z,
    NC,
    C,
    PO,
    PE,
    P,
    M,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Operand {
    Imm8(u8),        // immediate addressing
    Imm16(u16),      // immediate extended adressing
    RelAddr(i16),    // Relative addressing
    Address(u16),    // extended addressing
    IOAddress(u8),   // extended addressing
    RegI(RegI),      // indexed addressing
    Reg8(Reg8),      // 8 bit register
    Reg16(Reg16),    // 16 bit register
    RegAddr(Reg16),  //register indirect addressing
    RegIOAddr(Reg8), //register indirect addressing
    FlagCondition(FlagCondition),
    IgnoreIO,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
            Operand::IOAddress(_) => None,
            Operand::RegI(_) => None,
            Operand::Reg8(_) => Some(OpSize::S1),
            Operand::Reg16(_) => Some(OpSize::S2),
            Operand::RegAddr(_) => None,
            Operand::RegIOAddr(_) => None,
            Operand::FlagCondition(_) => None,
            Operand::IgnoreIO => None,
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Operand::Imm8(x) => write!(f, "{:02X}", x),
            Operand::Imm16(x) => write!(f, "{:04X}", x),
            Operand::RelAddr(x) => write!(f, "{:+}", x),
            Operand::Address(x) => write!(f, "({:04X})", x),
            Operand::IOAddress(x) => write!(f, "({:02X})", x),
            Operand::RegI(x) => write!(f, "{}", x),
            Operand::Reg8(x) => write!(f, "{:?}", x),
            Operand::Reg16(x) => write!(f, "{:?}", x),
            Operand::RegAddr(x) => write!(f, "({:?})", x),
            Operand::RegIOAddr(x) => write!(f, "({:?})", x),
            Operand::FlagCondition(x) => write!(f, "{:?}", x),
            Operand::IgnoreIO => write!(f, "ignored"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[allow(clippy::upper_case_acronyms)]
pub enum Instruction {
    ADC,
    ADD,
    AND,
    BIT,
    CALL,
    CCF,
    CP,
    CPD,
    CPDR,
    CPI,
    CPIR,
    CPL,
    DAA,
    DEC,
    DI,
    DJNZ,
    EI,
    EX,
    EXX,
    HALT,
    IM,
    IN,
    INC,
    IND,
    INDR,
    INI,
    INIR,
    JP,
    JR,
    LD,
    LDD,
    LDDR,
    LDI,
    LDIR,
    NEG,
    NOP,
    OR,
    OTDR,
    OTIR,
    OUT,
    OUTD,
    OUTI,
    POP,
    PUSH,
    RES,
    RET,
    RETI,
    RETN,
    RL,
    RLA,
    RLC,
    RLCA,
    RLD,
    RR,
    RRA,
    RRC,
    RRCA,
    RRD,
    RST,
    SBC,
    SCF,
    SET,
    SLA,
    SLL,
    SRA,
    SRL,
    SUB,
    XOR,
}

#[derive(PartialEq, Eq, Clone)]
pub struct OpCode {
    pub length: u8,
    pub ins: Instruction,
    pub op1: Option<Operand>,
    pub op2: Option<Operand>,
    pub op3: Option<Operand>,
    pub data: [u8; 4],
    pub tstates: [u8; 6],
    pub mcycles: u8,
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op1 = if let Some(x) = self.op1 {
            format!("{}", x)
        } else {
            "".to_string()
        };
        let op2 = if let Some(x) = self.op2 {
            format!(", {}", x)
        } else {
            "".to_string()
        };
        let op3 = if let Some(x) = self.op3 {
            format!(", {}", x)
        } else {
            "".to_string()
        };
        write!(
            f,
            "{:?}\t{}{}{} {:?}",
            self.ins, op1, op2, op3, self.tstates
        )
    }
}

impl fmt::Debug for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for i in self.data.iter().take(self.length as usize) {
            write!(f, "{:02X} ", i)?
        }
        write!(f, "\t\t{}", self)
    }
}

impl OpCode {
    fn tstates_push(&mut self, t: u8) {
        self.tstates[self.mcycles as usize] = t;
        self.mcycles += 1;
    }
    fn tstates_extend_from_slice(&mut self, s: &[u8]) {
        let len = self.mcycles as usize;
        self.tstates[len..(len + s.len())].copy_from_slice(s);
        self.mcycles += s.len() as u8;
    }
    fn tstates_insert(&mut self, pos: usize, s: &[u8]) {
        assert!(self.mcycles as usize + s.len() <= self.tstates.len());

        //let end = std::cmp::min(s.len(), self.tstates.len() - pos - s.len());
        let end = self.tstates.len() - pos - s.len();
        self.tstates[pos..].copy_within(0..end, s.len());
        self.tstates[pos..(pos + s.len())].copy_from_slice(s);
        self.mcycles += s.len() as u8;
    }
    fn tstates_remove(&mut self, pos: usize) {
        self.tstates[pos..].copy_within(1.., 0);
        self.mcycles -= 1;
        self.tstates[self.mcycles as usize] = 0;
    }
}

fn decode_operand_reg_ddss(arg: u8) -> Reg16 {
    match arg & 0x3 {
        0 => Reg16::BC,
        1 => Reg16::DE,
        2 => Reg16::HL,
        3 => Reg16::SP,
        _ => unreachable!(),
    }
}

fn decode_operand_reg_qq(arg: u8) -> Reg16 {
    match arg & 0x3 {
        0 => Reg16::BC,
        1 => Reg16::DE,
        2 => Reg16::HL,
        3 => Reg16::AF,
        _ => unreachable!(),
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

fn decode_operand_reg_r_in(arg: u8) -> Operand {
    if arg & 0x7 == 6 {
        return Operand::Reg8(Reg8::A);
    }
    Operand::Reg8(decode_operand_reg_r(arg))
}

fn decode_operand_reg_r_hladdr(arg: u8) -> Operand {
    if arg & 0x7 == 6 {
        return Operand::RegAddr(Reg16::HL);
    }
    Operand::Reg8(decode_operand_reg_r(arg))
}

fn decode_operand_cond_cc(arg: u8) -> FlagCondition {
    match arg & 0x7 {
        0 => FlagCondition::NZ,
        1 => FlagCondition::Z,
        2 => FlagCondition::NC,
        3 => FlagCondition::C,
        4 => FlagCondition::PO,
        5 => FlagCondition::PE,
        6 => FlagCondition::P,
        7 => FlagCondition::M,
        _ => unreachable!(),
    }
}

#[inline]
fn ins_to_data(ins: &[u8]) -> [u8; 4] {
    let mut data = [0; 4];
    let len = std::cmp::min(data.len(), ins.len());
    data[..len].copy_from_slice(ins);
    data
}
// An instruction can be one to four bytes
pub fn disas(ins: &[u8]) -> Option<OpCode> {
    if ins.is_empty() {
        return None;
    }
    let mut opcode = OpCode {
        data: ins_to_data(ins),
        length: 1,
        ins: Instruction::NOP,
        op1: None,
        op2: None,
        op3: None,
        tstates: [0; 6],
        mcycles: 0,
    };
    if disas_ref(ins, &mut opcode) {
        Some(opcode)
    } else {
        None
    }
}
fn disas_ref(ins: &[u8], opcode: &mut OpCode) -> bool {
    if ins.is_empty() {
        return false;
    }
    if disas_one_byte_shift3(ins[0], opcode) {
        return true;
    }
    if disas_one_byte_mask(ins[0], opcode) {
        return true;
    }
    if disas_one_byte(ins[0], opcode) {
        return true;
    }
    if ins.len() < 2 {
        return false;
    }
    if disas_two_bytes_mask(ins[0], ins[1], opcode) {
        return true;
    }
    if disas_two_bytes(&ins[..2], opcode) {
        return true;
    }
    if ins.len() < 3 {
        return false;
    }
    if disas_three_bytes_mask(ins, opcode) {
        return true;
    }
    if disas_three_bytes(ins, opcode) {
        return true;
    }
    if ins.len() < 4 {
        return false;
    }
    if disas_dd_fd_prefix(ins, opcode) {
        // TODO: dd and fd prefix length instructions can go from 2 to 4 bytes, we might miss
        // instructions by disassembling only for 4 here
        return true;
    }
    if disas_ddcb_fdcb_prefix(ins, opcode) {
        return true;
    }
    if disas_four_bytes_mask(ins, opcode) {
        return true;
    }
    /* By default the CPU act as NOPs */
    opcode.length = 1;
    opcode.ins = Instruction::NOP;
    if opcode.mcycles == 0 {
        opcode.tstates_push(4);
    }
    true
}

fn disas_one_byte_shift3(ins: u8, opcode: &mut OpCode) -> bool {
    let arg = ins & 0x7;
    fn alu8_base(opcode: &mut OpCode, ins: Instruction, arg: u8) -> bool {
        opcode.length = 1;
        opcode.ins = ins;
        opcode.tstates_extend_from_slice(match arg {
            0x6 => &[4, 3],
            _ => &[4],
        });
        true
    }
    let alu8 = |opcode: &mut OpCode, ins: Instruction| -> bool {
        opcode.op1 = Some(Operand::Reg8(Reg8::A));
        opcode.op2 = Some(decode_operand_reg_r_hladdr(arg));
        alu8_base(opcode, ins, arg)
    };
    let alu8a = |opcode: &mut OpCode, ins: Instruction| -> bool {
        opcode.op1 = Some(decode_operand_reg_r_hladdr(arg));
        alu8_base(opcode, ins, arg)
    };
    match ins >> 3 {
        0x10 => {
            // ADD a, r
            // ADD a, (HL)
            alu8(opcode, Instruction::ADD)
        }
        0x11 => {
            //ADC a, r
            //ADC a, (HL)
            alu8(opcode, Instruction::ADC)
        }
        0x12 => {
            // SUB A, r
            // SUB A, (HL)
            alu8a(opcode, Instruction::SUB)
        }
        0x13 => {
            // SBC A, r
            // SBC A, (HL)
            alu8(opcode, Instruction::SBC)
        }
        0x14 => {
            // AND A, r
            // AND A, (HL)
            alu8a(opcode, Instruction::AND)
        }
        0x16 => {
            // OR A, r
            // OR A, (HL)
            alu8a(opcode, Instruction::OR)
        }
        0x15 => {
            // XOR A, r
            // XOR A, (HL)
            alu8a(opcode, Instruction::XOR)
        }
        0x17 => {
            // CP A, r
            // CP A, (HL)
            alu8a(opcode, Instruction::CP)
        }
        _ => false,
    }
}
fn disas_one_byte_mask(ins: u8, opcode: &mut OpCode) -> bool {
    let reg = decode_operand_reg_ddss((ins >> 4) & 0x3);
    opcode.length = 1;
    match ins & 0xCF {
        0x03 => {
            // INC ss
            opcode.ins = Instruction::INC;
            opcode.op1 = Some(Operand::Reg16(reg));
            opcode.tstates_push(6);
            return true;
        }
        0x0B => {
            // DEC ss
            opcode.ins = Instruction::DEC;
            opcode.op1 = Some(Operand::Reg16(reg));
            opcode.tstates_push(6);
            return true;
        }
        0x09 => {
            // ADD HL, ss
            opcode.ins = Instruction::ADD;
            opcode.op1 = Some(Operand::Reg16(Reg16::HL));
            opcode.op2 = Some(Operand::Reg16(reg));
            opcode.tstates_extend_from_slice(&[4, 4, 3]);
            return true;
        }
        0xC1 => {
            // POP qq
            let reg = decode_operand_reg_qq((ins >> 4) & 0x3);
            opcode.ins = Instruction::POP;
            opcode.op1 = Some(Operand::Reg16(reg));
            opcode.tstates_extend_from_slice(&[4, 3, 3]);
            return true;
        }
        0xC5 => {
            // PUSH qq
            let reg = decode_operand_reg_qq((ins >> 4) & 0x3);
            opcode.ins = Instruction::PUSH;
            opcode.op1 = Some(Operand::Reg16(reg));
            opcode.tstates_extend_from_slice(&[5, 3, 3]);
            return true;
        }
        _ => {}
    }
    match ins & 0xC7 {
        0x04 | 0x05 /* INC | DEC */ => {
            // INC r
            // INC (HL)
            // DEC r
            // DEC (HL)
            let typ = if (ins & 0xC7) == 0x04 {
                Instruction::INC
            } else {
                Instruction::DEC
            };
            let opraw = (ins >> 3) & 0x7;
            let op = decode_operand_reg_r_hladdr(opraw);
            opcode.ins = typ;
            opcode.op1 = Some(op);
            if opraw == 0x6 {
                opcode.tstates_extend_from_slice(&[4, 4, 3]);
            } else {
                opcode.tstates_push(4);
            }
            return true;
        }
        0xC0 => {
            // RET cc
            let cond = decode_operand_cond_cc(ins >> 3 & 0x7);
            opcode.ins = Instruction::RET;
            opcode.op1 = Some(Operand::FlagCondition(cond));
            opcode.tstates_extend_from_slice(&[5, 3, 3]); // warning: varies
            return true;
        }
        0xC7 => {
            // RST p
            let p =  ((ins >> 3) & 0x7) << 3;
            opcode.ins = Instruction::RST;
            opcode.op1 = Some(Operand::Imm8(p));
            opcode.tstates_extend_from_slice(&[5, 3, 3]);
            return true;
        }
        _ => {}
    }
    if ins >> 6 == 0x1 {
        // LD r, r'
        // LD (HL), (HL)
        let op1 = Some(decode_operand_reg_r_hladdr((ins >> 3) & 0x7));
        let op2 = Some(decode_operand_reg_r_hladdr(ins & 0x7));
        /* We detect HALT here as well so that caller does not depend on ordering of this function
         * with the other one */
        if op1 == Some(Operand::RegAddr(Reg16::HL)) && op1 == op2 {
            opcode.ins = Instruction::HALT;
            opcode.tstates_push(4);
            return true;
        }
        opcode.ins = Instruction::LD;
        opcode.op1 = op1;
        opcode.op2 = op2;
        if op1 == Some(Operand::RegAddr(Reg16::HL)) || op2 == Some(Operand::RegAddr(Reg16::HL)) {
            opcode.tstates_extend_from_slice(&[4, 3]);
        } else {
            opcode.tstates_push(4);
        }
        return true;
    }
    false
}

fn disas_one_byte(ins: u8, opcode: &mut OpCode) -> bool {
    opcode.length = 1;
    match ins {
        0x00 => {
            // NOP
            opcode.ins = Instruction::NOP;
            opcode.tstates_push(4);
            true
        }
        0x02 | 0x12 => {
            // LD (BC), A
            // LD (DE), A
            let reg = match ins & 0x10 {
                0x10 => Reg16::DE,
                _ => Reg16::BC,
            };
            opcode.ins = Instruction::LD;
            opcode.op1 = Some(Operand::RegAddr(reg));
            opcode.op2 = Some(Operand::Reg8(Reg8::A));
            opcode.tstates_extend_from_slice(&[4, 3]);
            true
        }
        0x07 => {
            // RLCA
            opcode.ins = Instruction::RLCA;
            opcode.tstates_push(4);
            true
        }
        0x17 => {
            // RLA
            opcode.ins = Instruction::RLA;
            opcode.tstates_push(4);
            true
        }
        0x0F => {
            // RRCA
            opcode.ins = Instruction::RRCA;
            opcode.tstates_push(4);
            true
        }
        0x1F => {
            // RRA
            opcode.ins = Instruction::RRA;
            opcode.tstates_push(4);
            true
        }
        0x08 | 0xEB | 0xE3 => {
            // EX AF, AF'
            // EX DE, HL
            // EX (SP), HL
            opcode.ins = Instruction::EX;
            match ins {
                0x08 => {
                    opcode.op1 = Some(Operand::Reg16(Reg16::AF));
                    opcode.op2 = Some(Operand::Reg16(Reg16::AFp));
                    opcode.tstates_push(4);
                }
                0xEB => {
                    opcode.op1 = Some(Operand::Reg16(Reg16::DE));
                    opcode.op2 = Some(Operand::Reg16(Reg16::HL));
                    opcode.tstates_push(4);
                }
                0xE3 => {
                    opcode.op1 = Some(Operand::RegAddr(Reg16::SP));
                    opcode.op2 = Some(Operand::Reg16(Reg16::HL));
                    opcode.tstates_extend_from_slice(&[4, 3, 4, 3, 5]);
                }
                _ => unreachable!(),
            }
            true
        }
        0x0A | 0x1A => {
            // LD A, (BC)
            // LD A, (DE)
            opcode.ins = Instruction::LD;
            opcode.op1 = Some(Operand::Reg8(Reg8::A));
            opcode.op2 = if ins == 0x0A {
                Some(Operand::RegAddr(Reg16::BC))
            } else {
                Some(Operand::RegAddr(Reg16::DE))
            };
            opcode.tstates_extend_from_slice(&[4, 3]);
            true
        }
        0x27 => {
            // DAA
            opcode.ins = Instruction::DAA;
            opcode.tstates_push(4);
            true
        }
        0x2F => {
            // CPL
            opcode.ins = Instruction::CPL;
            opcode.tstates_push(4);
            true
        }
        0x37 => {
            // SCF
            opcode.ins = Instruction::SCF;
            opcode.tstates_push(4);
            true
        }
        0x3F => {
            // CCF
            opcode.ins = Instruction::CCF;
            opcode.tstates_push(4);
            true
        }
        0x76 => {
            // HALT
            opcode.ins = Instruction::HALT;
            opcode.tstates_push(4);
            true
        }
        0xC9 => {
            //RET
            opcode.ins = Instruction::RET;
            opcode.tstates_extend_from_slice(&[4, 3, 3]);
            true
        }
        0xD9 => {
            // EXX
            opcode.ins = Instruction::EXX;
            opcode.tstates_push(4);
            true
        }
        0xE9 => {
            // JP HL (no, not writing it JP (HL))
            opcode.ins = Instruction::JP;
            opcode.op1 = Some(Operand::Reg16(Reg16::HL));
            opcode.tstates_push(4);
            true
        }
        0xF3 => {
            opcode.ins = Instruction::DI;
            opcode.tstates_push(4);
            true
        }
        0xF9 => {
            // LD SP, HL
            opcode.ins = Instruction::LD;
            opcode.op1 = Some(Operand::Reg16(Reg16::SP));
            opcode.op2 = Some(Operand::Reg16(Reg16::HL));
            opcode.tstates_push(6);
            true
        }
        0xFB => {
            opcode.ins = Instruction::EI;
            opcode.tstates_push(4);
            true
        }
        _ => false,
    }
}

fn replace_hl_addr(op: &mut OpCode, reg: Reg16, d: i8) -> bool {
    let mut o1 = false;
    let mut o2 = false;
    if let Some(ref mut op1) = op.op1 {
        o1 = replace_hl_addr_op(op1, reg, d);
    }
    if let Some(ref mut op2) = op.op2 {
        o2 = replace_hl_addr_op(op2, reg, d);
    }
    o1 || o2
}

fn replace_hl_addr_op(op: &mut Operand, reg: Reg16, d: i8) -> bool {
    if let Operand::RegAddr(Reg16::HL) = op {
        *op = Operand::RegI(match reg {
            Reg16::IX => RegI::IX(d),
            Reg16::IY => RegI::IY(d),
            _ => unreachable!(),
        });
        true
    } else {
        false
    }
}
fn replace_hl(op: &mut OpCode, reg: Reg16) -> bool {
    let mut o1 = false;
    let mut o2 = false;
    if let Some(ref mut op1) = op.op1 {
        o1 = replace_hl_op(op1, reg);
    }
    if let Some(ref mut op2) = op.op2 {
        o2 = replace_hl_op(op2, reg);
    }
    o1 || o2
}
fn replace_hl_op(op: &mut Operand, reg: Reg16) -> bool {
    match op {
        Operand::Reg16(Reg16::HL) => {
            *op = Operand::Reg16(reg);
            true
        }
        Operand::Reg8(Reg8::H) => {
            *op = Operand::Reg8(match reg {
                Reg16::IX => Reg8::IXh,
                Reg16::IY => Reg8::IYh,
                _ => unreachable!(),
            });
            true
        }
        Operand::Reg8(Reg8::L) => {
            *op = Operand::Reg8(match reg {
                Reg16::IX => Reg8::IXl,
                Reg16::IY => Reg8::IYl,
                _ => unreachable!(),
            });
            true
        }
        _ => false,
    }
}

fn disas_dd_fd_prefix(ins: &[u8], opcode: &mut OpCode) -> bool {
    if (ins[0] == 0xDD || ins[0] == 0xFD) &&
        ins[1] != 0xCB /* DDCB */ &&
            ins[1] != 0xEB
    /* EX DE, HL */
    {
        let reg = match ins[0] {
            0xDD => Reg16::IX,
            0xFD => Reg16::IY,
            _ => unreachable!(),
        };
        // Disas by looking ahead and replacing an eventual HL
        if disas_ref(&ins[1..ins.len()], opcode) {
            if replace_hl_addr(opcode, reg, ins[2] as i8) {
                // update timings
                opcode.length += 2;
                opcode.tstates_insert(1, &[4, 3, 5]);
                /* Manual fix for LD (IX+d), n */
                if let Some(Operand::Imm8(ref mut v)) = opcode.op2 {
                    *v = ins[3];
                    opcode.tstates_remove(5);
                    //opcode.data[3] = ins[3];
                }
                return true;
            } else if replace_hl(opcode, reg) {
                // update timings
                opcode.length += 1;
                opcode.tstates_insert(1, &[4]);
                return true;
            }
        }
    }
    false
}

fn disas_two_bytes_mask(ins1: u8, ins2: u8, opcode: &mut OpCode) -> bool {
    match ins1 & 0xC7 {
        0x06 => {
            // LD r, n
            // LD (HL), n
            let opraw = (ins1 >> 3) & 0x7;
            let op = decode_operand_reg_r_hladdr(opraw);
            opcode.length = 2;
            opcode.ins = Instruction::LD;
            opcode.op1 = Some(op);
            opcode.op2 = Some(Operand::Imm8(ins2));
            if opraw == 0x6 {
                opcode.tstates_extend_from_slice(&[4, 3, 3]);
            } else {
                opcode.tstates_extend_from_slice(&[4, 3]);
            }
            true
        }
        _ => false,
    }
}
fn disas_two_bytes(ins: &[u8], opcode: &mut OpCode) -> bool {
    fn alu8imm(data: &[u8], opcode: &mut OpCode, ins: Instruction) -> bool {
        opcode.ins = ins;
        opcode.op1 = Some(Operand::Imm8(data[1]));
        opcode.tstates_extend_from_slice(&[4, 3]);
        true
    }
    fn alu8aimm(data: &[u8], opcode: &mut OpCode, ins: Instruction) -> bool {
        opcode.ins = ins;
        opcode.op1 = Some(Operand::Reg8(Reg8::A));
        opcode.op2 = Some(Operand::Imm8(data[1]));
        opcode.tstates_extend_from_slice(&[4, 3]);
        true
    }

    opcode.length = 2;
    match ins[0] {
        0x10 => {
            // DJNZ, e
            opcode.ins = Instruction::DJNZ;
            opcode.op1 = Some(Operand::RelAddr((ins[1] as i8) as i16 + 2));
            opcode.tstates_extend_from_slice(&[5, 3, 5]); // Warning: varies
            return true;
        }
        0x18 => {
            // JR e
            opcode.ins = Instruction::JR;
            opcode.op1 = Some(Operand::RelAddr((ins[1] as i8) as i16 + 2));
            opcode.tstates_extend_from_slice(&[4, 3, 5]); // Warning: varies
            return true;
        }
        0x38 | 0x30 | 0x28 | 0x20 => {
            // JR C, e
            // JR NC, e
            // JR Z, e
            // JR NZ, e
            let cond = match ins[0] {
                0x38 => FlagCondition::C,
                0x30 => FlagCondition::NC,
                0x28 => FlagCondition::Z,
                0x20 => FlagCondition::NZ,
                _ => unreachable!(),
            };
            opcode.ins = Instruction::JR;
            opcode.op1 = Some(Operand::FlagCondition(cond));
            opcode.op2 = Some(Operand::RelAddr((ins[1] as i8) as i16 + 2));
            opcode.tstates_extend_from_slice(&[4, 3, 5]); // Warning: varies
            return true;
        }
        0xC6 => return alu8aimm(ins, opcode, Instruction::ADD), // ADD A, n
        0xCE => return alu8aimm(ins, opcode, Instruction::ADC), // ADC A, n
        0xDE => return alu8aimm(ins, opcode, Instruction::SBC), // SBC A, n
        0xD3 => {
            // OUT (n), A
            opcode.ins = Instruction::OUT;
            opcode.op1 = Some(Operand::IOAddress(ins[1]));
            opcode.op2 = Some(Operand::Reg8(Reg8::A));
            opcode.tstates_extend_from_slice(&[4, 3, 4]);
            return true;
        }
        0xDB => {
            // IN A, (n)
            opcode.ins = Instruction::IN;
            opcode.op1 = Some(Operand::Reg8(Reg8::A));
            opcode.op2 = Some(Operand::IOAddress(ins[1]));
            opcode.tstates_extend_from_slice(&[4, 3, 4]);
            return true;
        }
        0xD6 => return alu8imm(ins, opcode, Instruction::SUB), // SUB n
        0xE6 => return alu8imm(ins, opcode, Instruction::AND), // AND n
        0xF6 => return alu8imm(ins, opcode, Instruction::OR),  // OR n
        0xEE => return alu8imm(ins, opcode, Instruction::XOR), // XOR n
        0xFE => return alu8imm(ins, opcode, Instruction::CP),  // CP n
        _ => {}
    }
    // two bytes opcodes
    let insw = (ins[0] as u16) << 8 | ins[1] as u16;
    fn ldria(opcode: &mut OpCode, r1: Reg8, r2: Reg8) -> bool {
        opcode.ins = Instruction::LD;
        opcode.op1 = Some(Operand::Reg8(r1));
        opcode.op2 = Some(Operand::Reg8(r2));
        opcode.tstates_extend_from_slice(&[4, 5]);
        true
    }
    fn memblock(opcode: &mut OpCode, ins: Instruction) -> bool {
        opcode.ins = ins;
        // warning: tstates may vary
        opcode.tstates_extend_from_slice(&[4, 4, 3, 5]);
        true
    }
    fn ioblock(opcode: &mut OpCode, ins: Instruction) -> bool {
        opcode.ins = ins;
        // warning: tstates may vary
        opcode.tstates_extend_from_slice(&[4, 5, 3, 4]);
        true
    }
    fn im(opcode: &mut OpCode, num: u8) -> bool {
        opcode.ins = Instruction::IM;
        opcode.op1 = Some(Operand::Imm8(num));
        opcode.tstates_extend_from_slice(&[4, 4]);
        true
    }
    match insw {
        0xED44 | 0xED4C | 0xED54 | 0xED5C | 0xED64 | 0xED6C | 0xED74 | 0xED7C => {
            // NEG
            opcode.ins = Instruction::NEG;
            opcode.tstates_extend_from_slice(&[4, 4]);
            return true;
        }
        0xED45 | 0xED55 | 0xED5D | 0xED65 | 0xED6D | 0xED75 | 0xED7D => {
            // RETN
            opcode.ins = Instruction::RETN;
            opcode.tstates_extend_from_slice(&[4, 4, 3, 3]);
            return true;
        }
        0xED4D => {
            //RETI
            opcode.ins = Instruction::RETI;
            opcode.tstates_extend_from_slice(&[4, 4, 3, 3]);
            return true;
        }
        // IM 0
        // IM 1
        // IM 2
        0xED46 | 0xED4E | 0xED66 | 0xED6E => return im(opcode, 0),
        0xED56 | 0xED76 => return im(opcode, 1),
        0xED5E | 0xED7E => return im(opcode, 2),
        0xED47 => return ldria(opcode, Reg8::I, Reg8::A), // LD I, A
        0xED4F => return ldria(opcode, Reg8::R, Reg8::A), // LD R, A
        0xED57 => return ldria(opcode, Reg8::A, Reg8::I), // LD A, I
        0xED5F => return ldria(opcode, Reg8::A, Reg8::R), // LD A, R
        0xED67 => {
            // RRD
            opcode.ins = Instruction::RRD;
            opcode.tstates_extend_from_slice(&[4, 4, 3, 4, 3]);
            return true;
        }
        0xED6F => {
            // RLD
            opcode.ins = Instruction::RLD;
            opcode.tstates_extend_from_slice(&[4, 4, 3, 4, 3]);
            return true;
        }
        0xEDA0 => return memblock(opcode, Instruction::LDI),
        0xEDB0 => return memblock(opcode, Instruction::LDIR),
        0xEDA1 => return memblock(opcode, Instruction::CPI),
        0xEDB1 => return memblock(opcode, Instruction::CPIR),
        0xEDA8 => return memblock(opcode, Instruction::LDD),
        0xEDB8 => return memblock(opcode, Instruction::LDDR),
        0xEDA9 => return memblock(opcode, Instruction::CPD),
        0xEDB9 => return memblock(opcode, Instruction::CPDR),
        0xEDA2 => return ioblock(opcode, Instruction::INI),
        0xEDB2 => return ioblock(opcode, Instruction::INIR),
        0xEDAA => return ioblock(opcode, Instruction::IND),
        0xEDBA => return ioblock(opcode, Instruction::INDR),
        0xEDA3 => return ioblock(opcode, Instruction::OUTI),
        0xEDB3 => return ioblock(opcode, Instruction::OTIR),
        0xEDAB => return ioblock(opcode, Instruction::OUTD),
        0xEDBB => return ioblock(opcode, Instruction::OTDR),
        _ => {}
    }
    match insw & 0xFFCF {
        0xED42 => {
            // SBC HL, ss
            let reg = decode_operand_reg_ddss((ins[1] >> 4) & 0x3);
            opcode.ins = Instruction::SBC;
            opcode.op1 = Some(Operand::Reg16(Reg16::HL));
            opcode.op2 = Some(Operand::Reg16(reg));
            opcode.tstates_extend_from_slice(&[4, 4, 4, 3]);
            return true;
        }
        0xED4A => {
            // ADC HL, ss
            let reg = decode_operand_reg_ddss((ins[1] >> 4) & 0x3);
            opcode.ins = Instruction::ADC;
            opcode.op1 = Some(Operand::Reg16(Reg16::HL));
            opcode.op2 = Some(Operand::Reg16(reg));
            opcode.tstates_extend_from_slice(&[4, 4, 4, 3]);
            return true;
        }
        _ => {}
    }
    match insw & 0xFFF8 {
        0xCB00 | 0xCB08 | 0xCB10 | 0xCB18 | 0xCB20 | 0xCB28 | 0xCB30 | 0xCB38 => {
            //RLC r
            //RRC r
            //RL r
            //RR r
            //SLA r
            //SRA r
            //SRL r
            opcode.op1 = Some(decode_operand_reg_r_hladdr(ins[1] & 0x7));
            opcode.tstates_extend_from_slice(if opcode.op1 == Some(Operand::RegAddr(Reg16::HL)) {
                &[4, 4, 4, 3]
            } else {
                &[4, 4]
            });
            opcode.ins = match insw & 0xFFF8 {
                0xCB00 => Instruction::RLC,
                0xCB08 => Instruction::RRC,
                0xCB10 => Instruction::RL,
                0xCB18 => Instruction::RR, // error in manual page 228 (pd 242)
                0xCB20 => Instruction::SLA,
                0xCB28 => Instruction::SRA,
                0xCB30 => Instruction::SLL,
                0xCB38 => Instruction::SRL,
                _ => unreachable!(),
            };
            return true;
        }
        _ => {}
    }
    match insw & 0xFFC0 {
        0xCB40 | 0xCBC0 | 0xCB80 => {
            // BIT b, r
            // SET b, r
            // RES b, r
            opcode.op1 = Some(Operand::Imm8((ins[1] >> 3) & 0x7));
            opcode.op2 = Some(decode_operand_reg_r_hladdr(ins[1] & 0x7));
            opcode.ins = match insw & 0xFFC0 {
                0xCB40 => Instruction::BIT,
                0xCBC0 => Instruction::SET,
                0xCB80 => Instruction::RES,
                _ => unreachable!(),
            };
            opcode.tstates_extend_from_slice(if opcode.op2 == Some(Operand::RegAddr(Reg16::HL)) {
                if insw & 0xFFC0 == 0xCB40 {
                    &[4, 4, 4]
                } else {
                    &[4, 4, 4, 3]
                }
            } else {
                &[4, 4]
            });
            return true;
        }
        _ => {}
    }
    match insw & 0xFFC7 {
        0xED40 => {
            // IN r, (C)
            opcode.ins = Instruction::IN;
            opcode.op1 = Some(decode_operand_reg_r_in(ins[1] >> 3));
            opcode.op2 = Some(Operand::RegIOAddr(Reg8::C));
            if insw == 0xED70 {
                opcode.op3 = Some(Operand::IgnoreIO);
            }
            opcode.tstates_extend_from_slice(&[4, 4, 4]);
            true
        }
        0xED41 => {
            // OUT (C), r
            opcode.ins = Instruction::OUT;
            opcode.op1 = Some(Operand::RegIOAddr(Reg8::C));
            opcode.op2 = Some(decode_operand_reg_r_in(ins[1] >> 3));
            if insw == 0xED71 {
                opcode.op3 = Some(Operand::IgnoreIO);
            }
            opcode.tstates_extend_from_slice(&[4, 4, 4]);
            true
        }
        _ => false,
    }
}

fn disas_three_bytes_mask(ins: &[u8], opcode: &mut OpCode) -> bool {
    let ins1 = ins[0];
    opcode.length = 3;
    if ins1 & 0xCF == 0x01 {
        // LD dd, nn
        let arg = (ins[2] as u16) << 8 | ins[1] as u16;
        let reg = decode_operand_reg_ddss((ins1 >> 4) & 0x3);
        opcode.ins = Instruction::LD;
        opcode.op1 = Some(Operand::Reg16(reg));
        opcode.op2 = Some(Operand::Imm16(arg));
        opcode.tstates_extend_from_slice(&[4, 3, 3]); // error in datasheet page 99 ?
        return true;
    }
    match ins1 & 0xC7 {
        0xC2 => {
            // JP cc, nn
            let cond = decode_operand_cond_cc(ins1 >> 3 & 0x7);
            opcode.ins = Instruction::JP;
            opcode.op1 = Some(Operand::FlagCondition(cond));
            opcode.op2 = Some(Operand::Imm16(ins[1] as u16 | ((ins[2] as u16) << 8)));
            opcode.tstates_extend_from_slice(&[4, 3, 3]); // Warning: varies
            true
        }
        0xC4 => {
            // CALL cc, nn
            let cond = decode_operand_cond_cc(ins1 >> 3 & 0x7);
            opcode.ins = Instruction::CALL;
            opcode.op1 = Some(Operand::FlagCondition(cond));
            opcode.op2 = Some(Operand::Imm16(ins[1] as u16 | ((ins[2] as u16) << 8)));
            opcode.tstates_extend_from_slice(&[4, 3, 4, 3, 3]); // Warning: varies
            true
        }
        _ => false,
    }
}
fn disas_three_bytes(ins: &[u8], opcode: &mut OpCode) -> bool {
    opcode.length = 3;
    match ins[0] {
        0x22 => {
            // LD (nn), HL
            opcode.ins = Instruction::LD;
            opcode.op1 = Some(Operand::Address(ins[1] as u16 | (ins[2] as u16) << 8));
            opcode.op2 = Some(Operand::Reg16(Reg16::HL));
            opcode.tstates_extend_from_slice(&[4, 3, 3, 3, 3]);
            true
        }
        0x2A => {
            // LD HL, (nn)
            opcode.ins = Instruction::LD;
            opcode.op1 = Some(Operand::Reg16(Reg16::HL));
            opcode.op2 = Some(Operand::Address(ins[1] as u16 | (ins[2] as u16) << 8));
            opcode.tstates_extend_from_slice(&[4, 3, 3, 3, 3]);
            true
        }
        0x32 => {
            // LD (nn), A
            opcode.ins = Instruction::LD;
            opcode.op1 = Some(Operand::Address(ins[1] as u16 | (ins[2] as u16) << 8));
            opcode.op2 = Some(Operand::Reg8(Reg8::A));
            opcode.tstates_extend_from_slice(&[4, 3, 3, 3]);
            true
        }
        0x3A => {
            // LD A, (nn)
            opcode.ins = Instruction::LD;
            opcode.op1 = Some(Operand::Reg8(Reg8::A));
            opcode.op2 = Some(Operand::Address(ins[1] as u16 | (ins[2] as u16) << 8));
            opcode.tstates_extend_from_slice(&[4, 3, 3, 3]);
            true
        }
        0xC3 => {
            // JP nn
            opcode.ins = Instruction::JP;
            opcode.op1 = Some(Operand::Imm16(ins[1] as u16 | ((ins[2] as u16) << 8)));
            opcode.tstates_extend_from_slice(&[4, 3, 3]); // Warning: varies
            true
        }
        0xCD => {
            // CALL nn
            opcode.ins = Instruction::CALL;
            opcode.op1 = Some(Operand::Imm16(ins[1] as u16 | ((ins[2] as u16) << 8)));
            opcode.tstates_extend_from_slice(&[4, 3, 4, 3, 3]); // Warning: varies
            true
        }
        _ => false,
    }
}

fn decode_operand_reg_ddcb(arg: u8) -> Option<Operand> {
    if arg & 0x7 == 6 {
        return None;
    }
    Some(Operand::Reg8(decode_operand_reg_r(arg)))
}

fn disas_ddcb_fdcb_prefix(ins: &[u8], opcode: &mut OpCode) -> bool {
    if (ins[0] != 0xDD && ins[0] != 0xFD) || ins[1] != 0xCB {
        return false;
    }
    let opidx = Some(Operand::RegI(match ins[0] {
        0xDD => RegI::IX(ins[2] as i8),
        0xFD => RegI::IY(ins[2] as i8),
        _ => unreachable!(),
    }));
    let opcp = decode_operand_reg_ddcb(ins[3]);
    opcode.length = 4;
    let mut bitshift = |ins: Instruction| -> bool {
        opcode.ins = ins;
        opcode.op1 = opidx;
        opcode.op2 = opcp;
        opcode.tstates_extend_from_slice(&[4, 4, 3, 5, 4, 3]);
        true
    };
    match ins[3] & 0xF8 {
        // RLC (IX+d), r
        0x00 => bitshift(Instruction::RLC),
        // RRC (IX+d), r
        0x08 => bitshift(Instruction::RRC),
        // RL (IX+d), r
        0x10 => bitshift(Instruction::RL),
        // RR (IX+d), r
        0x18 => bitshift(Instruction::RR),
        // SLA (IX+d), r
        0x20 => bitshift(Instruction::SLA),
        // SRA (IX+d), r
        0x28 => bitshift(Instruction::SRA),
        // SLL (IX+d), r
        0x30 => bitshift(Instruction::SLL),
        // SRL (IX+d), r
        0x38 => bitshift(Instruction::SRL),
        // BIT n, (IX+d)
        0x40 | 0x48 | 0x50 | 0x58 | 0x60 | 0x68 | 0x70 | 0x78 => {
            opcode.ins = Instruction::BIT;
            opcode.op1 = Some(Operand::Imm8((ins[3] >> 3) & 0x7));
            opcode.op2 = opidx;
            opcode.tstates_extend_from_slice(&[4, 4, 3, 5, 4]);
            true
        }
        // RES n, (IX+d), r
        0x80 | 0x88 | 0x90 | 0x98 | 0xA0 | 0xA8 | 0xB0 | 0xB8 => {
            opcode.ins = Instruction::RES;
            opcode.op1 = Some(Operand::Imm8((ins[3] >> 3) & 0x7));
            opcode.op2 = opidx;
            opcode.op3 = opcp;
            opcode.tstates_extend_from_slice(&[4, 4, 3, 5, 4, 3]);
            true
        }
        // SET n, (IX+d), r
        0xC0 | 0xC8 | 0xD0 | 0xD8 | 0xE0 | 0xE8 | 0xF0 | 0xF8 => {
            opcode.ins = Instruction::SET;
            opcode.op1 = Some(Operand::Imm8((ins[3] >> 3) & 0x7));
            opcode.op2 = opidx;
            opcode.op3 = opcp;
            opcode.tstates_extend_from_slice(&[4, 4, 3, 5, 4, 3]);
            true
        }
        _ => false,
    }
}
fn disas_four_bytes_mask(ins: &[u8], opcode: &mut OpCode) -> bool {
    let insw = (ins[0] as u16) << 8 | ins[1] as u16;
    let reg = decode_operand_reg_ddss((ins[1] >> 4) & 0x3);
    let nn = (ins[3] as u16) << 8 | ins[2] as u16;
    opcode.length = 4;
    match insw & 0xFFCF {
        // LD (nn), dd
        0xED43 => {
            opcode.ins = Instruction::LD;
            opcode.op1 = Some(Operand::Address(nn));
            opcode.op2 = Some(Operand::Reg16(reg));
            opcode.tstates_extend_from_slice(&[4, 4, 3, 3, 3, 3]);
            true
        }
        // LD dd, (nn)
        0xED4B => {
            opcode.ins = Instruction::LD;
            opcode.op1 = Some(Operand::Reg16(reg));
            opcode.op2 = Some(Operand::Address(nn));
            opcode.tstates_extend_from_slice(&[4, 4, 3, 3, 3, 3]);
            true
        }
        _ => false,
    }
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
                length: 1,
                ins: Instruction::ADD,
                op1: Some(Operand::Reg8(Reg8::A)),
                op2: Some(Operand::Reg8(Reg8::A)),
                op3: None,
                data: [0x87, 0, 0, 0],
                tstates: [4, 0, 0, 0, 0, 0],
                mcycles: 1,
            })
        );
        // ADD A, HL
        assert_eq!(
            disas(&[0x86]),
            Some(OpCode {
                data: [0x86, 0, 0, 0],
                length: 1,
                ins: Instruction::ADD,
                op1: Some(Operand::Reg8(Reg8::A)),
                op2: Some(Operand::RegAddr(Reg16::HL)),
                op3: None,
                tstates: [4, 3, 0, 0, 0, 0],
                mcycles: 2,
            })
        );
        // ADC A, HL
        assert_eq!(
            disas(&[0x8E]),
            Some(OpCode {
                data: [0x8E, 0, 0, 0],
                length: 1,
                ins: Instruction::ADC,
                op1: Some(Operand::Reg8(Reg8::A)),
                op2: Some(Operand::RegAddr(Reg16::HL)),
                op3: None,
                tstates: [4, 3, 0, 0, 0, 0],
                mcycles: 2,
            })
        );
        // ADD A, n
        assert_eq!(
            disas(&[0xC6, 0x42]),
            Some(OpCode {
                data: [0xC6, 0x42, 0, 0],
                length: 2,
                ins: Instruction::ADD,
                op1: Some(Operand::Reg8(Reg8::A)),
                op2: Some(Operand::Imm8(0x42)),
                op3: None,
                tstates: [4, 3, 0, 0, 0, 0],
                mcycles: 2,
            })
        );
        // ADD A, n
        assert_eq!(
            disas(&[0xCE, 0x55]),
            Some(OpCode {
                data: [0xCE, 0x55, 0, 0],
                length: 2,
                ins: Instruction::ADC,
                op1: Some(Operand::Reg8(Reg8::A)),
                op2: Some(Operand::Imm8(0x55)),
                op3: None,
                tstates: [4, 3, 0, 0, 0, 0],
                mcycles: 2,
            })
        );
        // LD dd, nn
        assert_eq!(
            disas(&[0x01, 0x42, 0x10]),
            Some(OpCode {
                data: [0x01, 0x42, 0x10, 0],
                length: 3,
                ins: Instruction::LD,
                op1: Some(Operand::Reg16(Reg16::BC)),
                op2: Some(Operand::Imm16(0x1042)),
                op3: None,
                tstates: [4, 3, 3, 0, 0, 0],
                mcycles: 3,
            })
        );
        // LD dd, nn
        assert_eq!(
            disas(&[0x21, 0x42, 0x10]),
            Some(OpCode {
                data: [0x21, 0x42, 0x10, 0],
                length: 3,
                ins: Instruction::LD,
                op1: Some(Operand::Reg16(Reg16::HL)),
                op2: Some(Operand::Imm16(0x1042)),
                op3: None,
                tstates: [4, 3, 3, 0, 0, 0],
                mcycles: 3,
            })
        );
        // LD (DE), A
        assert_eq!(
            disas(&[0x12]),
            Some(OpCode {
                data: [0x12, 0, 0, 0],
                length: 1,
                ins: Instruction::LD,
                op1: Some(Operand::RegAddr(Reg16::DE)),
                op2: Some(Operand::Reg8(Reg8::A)),
                op3: None,
                tstates: [4, 3, 0, 0, 0, 0],
                mcycles: 2,
            })
        );
        // LD r, n
        assert_eq!(
            disas(&[0x3E, 0x42]),
            Some(OpCode {
                data: [0x3E, 0x42, 0, 0],
                length: 2,
                ins: Instruction::LD,
                op1: Some(Operand::Reg8(Reg8::A)),
                op2: Some(Operand::Imm8(0x42)),
                op3: None,
                tstates: [4, 3, 0, 0, 0, 0],
                mcycles: 2,
            })
        );
        // LD (HL), n
        assert_eq!(
            disas(&[0x36, 0x33]),
            Some(OpCode {
                data: [0x36, 0x33, 0, 0],
                length: 2,
                ins: Instruction::LD,
                op1: Some(Operand::RegAddr(Reg16::HL)),
                op2: Some(Operand::Imm8(0x33)),
                op3: None,
                tstates: [4, 3, 3, 0, 0, 0],
                mcycles: 3,
            })
        );
        // INC SP
        assert_eq!(
            disas(&[0x33]),
            Some(OpCode {
                data: [0x33, 0, 0, 0],
                length: 1,
                ins: Instruction::INC,
                op1: Some(Operand::Reg16(Reg16::SP)),
                op2: None,
                op3: None,
                tstates: [6, 0, 0, 0, 0, 0],
                mcycles: 1,
            })
        );
        // INC D
        assert_eq!(
            disas(&[0x14]),
            Some(OpCode {
                data: [0x14, 0, 0, 0],
                length: 1,
                ins: Instruction::INC,
                op1: Some(Operand::Reg8(Reg8::D)),
                op2: None,
                op3: None,
                tstates: [4, 0, 0, 0, 0, 0],
                mcycles: 1,
            })
        );
        // DEC A
        assert_eq!(
            disas(&[0x3D]),
            Some(OpCode {
                data: [0x3D, 0, 0, 0],
                length: 1,
                ins: Instruction::DEC,
                op1: Some(Operand::Reg8(Reg8::A)),
                op2: None,
                op3: None,
                tstates: [4, 0, 0, 0, 0, 0],
                mcycles: 1,
            })
        );
        // LD A, (DE)
        assert_eq!(
            disas(&[0x1A]),
            Some(OpCode {
                data: [0x1A, 0, 0, 0],
                length: 1,
                ins: Instruction::LD,
                op1: Some(Operand::Reg8(Reg8::A)),
                op2: Some(Operand::RegAddr(Reg16::DE)),
                op3: None,
                tstates: [4, 3, 0, 0, 0, 0],
                mcycles: 2,
            })
        );
    }
}
