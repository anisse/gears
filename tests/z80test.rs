use std::rc::Rc;

use gears::{cpu, io, mem};

// Heavily inspired by iz80's test
// https://github.com/ivanizag/iz80/blob/396dc918fc46a85e200f154d5728f9733671870f/tests/z80test.rs
// MIT-licensed by Ivan Izaguirre

#[test]
#[ignore]
fn z80full() {
    z80common(include_bytes!("z80test/z80full.tap"))
}
#[test]
#[ignore]
fn z80ccf() {
    z80common(include_bytes!("z80test/z80ccf.tap"))
}
#[test]
#[ignore]
fn z80memptr() {
    z80common(include_bytes!("z80test/z80memptr.tap"))
}
fn z80common(prog: &[u8]) {
    let cache = gears::cpu::DisasCache::init();

    let mut state = cpu::init();
    state.mem = mem::Memory::init(mem::Mapper::ZX64K); // This test suite is for machines with more RAM
    let dev = Rc::new(ZxSpectrumIODevice {});
    state.io.register(0, 0xFFFF, 0, dev);

    let skip = 0x5B; // do not parse .tap header
    let load_addr = 0x8000;

    for (addr, val) in prog.iter().enumerate().skip(skip) {
        state
            .mem
            .set_u8(addr as u16 + load_addr - skip as u16, *val);
    }
    //ignore RST 0x10 and 0x1601
    state.mem.set_u8(0x10, 0xC9); // RET
    state.mem.set_u8(0x1601, 0xC9); // RET

    let run_single_test = false;
    let single_test = 10;
    if run_single_test {
        state.mem.set_u16(0x802b, single_test); // ld bc, 0 to ld bc, test
        let mut test_start = state.mem.fetch_u16(0x802e);
        println!("Test table {:x}", test_start);
        test_start += single_test * 2;
        println!("Test table {:x}", test_start);
        state.mem.set_u16(0x802e, test_start); // Move start
        state.mem.set_u16(test_start + 2, 0); // NUL terminate test
    }

    state.r.PC = load_addr;
    let mut msg = String::new();
    loop {
        cpu::run_cached(&cache, &mut state, 1, false).unwrap();
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
        if state.r.PC == 0x0010 {
            let mut ch = state.r.A as char;
            if ch == '\r' {
                ch = '\n'
            } else if ch as u8 == 23 || ch as u8 == 26 {
                ch = ' '
            }
            //print!("{}[{}]", ch, ch as u8);
            print!("{}", ch);
            msg.push(ch);
        }
    }
    assert!(msg.contains("CPU TESTS OK"));
}
struct ZxSpectrumIODevice {}

impl io::Device for ZxSpectrumIODevice {
    fn out(&self, _: u16, _: u8) -> std::result::Result<(), std::string::String> {
        Ok(())
    }
    fn input(&self, addr: u16) -> std::result::Result<u8, std::string::String> {
        Ok((addr >> 8) as u8)
    }
}
