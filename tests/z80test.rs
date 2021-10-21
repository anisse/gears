use gears::{cpu, mem};

// Heavily inspired by iz80's test
// https://github.com/ivanizag/iz80/blob/396dc918fc46a85e200f154d5728f9733671870f/tests/z80test.rs
// MIT-licensed by Ivan Izaguirre

#[test]
#[ignore]
fn z80full() {
    let prog = include_bytes!("z80test/z80full.tap");
    let cache = gears::cpu::DisasCache::init();

    let mut state = cpu::init();
    state.mem = mem::Memory::init(64 * 1024); // This test suite is for machines with more RAM

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

    state.r.PC = load_addr;
    let mut msg = String::new();
    loop {
        cpu::run_cached(&cache, &mut state, 1, false).unwrap();

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
