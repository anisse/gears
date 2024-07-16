use gears::{cpu, io, mem};

// Heavily inspired by iz80's test
// https://github.com/ivanizag/iz80/blob/396dc918fc46a85e200f154d5728f9733671870f/tests/zexall.rs
// MIT-licensed by Ivan Izaguirre

#[test]
#[ignore]
fn zexall() {
    let prog = include_bytes!("zexall/zexall.com");
    zex(prog);
}
#[test]
#[ignore]
fn zexdoc() {
    let prog = include_bytes!("zexall/zexdoc.com");
    zex(prog);
}
fn zex(prog: &[u8]) {
    let mut state = cpu::init();
    state.mem = mem::Memory::init(mem::Mapper::ZX64K); // This test suite is for machines with more RAM
    state.io = io::RcDevice::new(ZxSpectrumIODevice {});

    let load_addr = 0x100;

    for (addr, val) in prog.iter().enumerate() {
        state.mem.set_u8(addr as u16 + load_addr, *val);
    }

    // replace syscall 5
    state.mem.set_u8(5, 0xD3);
    state.mem.set_u8(6, 0x00);
    state.mem.set_u8(7, 0xC9);

    state.r.PC = load_addr;
    let mut msg = String::new();
    loop {
        cpu::run(&mut state, 1, false).unwrap();
        /*
        println!(
            "{:04X}: {:04X} | {:04X} {:04X} {:04X} {:04X} {:04X} {:04X} {:04X}",
            state.r.PC,
            state.mem.fetch_u16(state.r.PC),
            state.r.get_regpair(cpu::RegPair::AF),
            state.r.get_regpair(cpu::RegPair::BC),
            state.r.get_regpair(cpu::RegPair::DE),
            state.r.get_regpair(cpu::RegPair::HL),
            state.r.get_regpair(cpu::RegPair::IX),
            state.r.get_regpair(cpu::RegPair::IY),
            state.r.get_regpair(cpu::RegPair::SP),
        );
        */

        if state.r.PC == 0x0000 {
            println!();
            break; // we reached the end
        }

        // Handle print routine
        if state.r.PC == 0x0005 {
            match state.r.C {
                2 => {
                    let ch = state.r.E as char;
                    msg.push(ch);
                    print!("{}", ch);
                }
                9 => {
                    let mut addr = state.r.get_regpair(cpu::RegPair::DE);
                    loop {
                        let ch = state.mem.fetch_u8(addr) as char;
                        addr += 1;
                        if ch == '$' {
                            break;
                        }
                        print!("{}", ch);
                        msg.push(ch);
                    }
                }
                _ => unreachable!(),
            }
        }
    }
    assert_eq!(msg.matches("OK").count(), 67);
}

struct ZxSpectrumIODevice {}

impl io::Device for ZxSpectrumIODevice {
    fn out(&self, _: u16, _: u8, _: u32) -> std::result::Result<(), std::string::String> {
        Ok(())
    }
    fn input(&self, addr: u16, _: u32) -> std::result::Result<u8, std::string::String> {
        Ok((addr >> 8) as u8)
    }
}
