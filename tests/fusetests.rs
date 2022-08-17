use gears::cpu::RegPair;
use gears::{cpu, io, mem};

#[derive(Debug, Clone, Copy)]
enum EventType {
    MemoryRead { value: u8 },
    MemoryWrite { value: u8 },
    PortRead { value: u8 },
    PortWrite { value: u8 },
    MemoryContention,
    PortContention,
}

#[derive(Debug, Clone, Copy)]
struct Event {
    time: u16,
    address: u16,
    typ: EventType,
}

#[derive(Debug, Clone)]
struct MemValues {
    base_addr: u16,
    values: Vec<u8>,
}

#[derive(Debug, Clone, Default)]
struct Test {
    //tests.in
    desc: String,
    start_regs: cpu::Regs,
    start_halted: bool,
    tstate_to_run: u16,
    memory_values: Vec<MemValues>,
    //tests.expected
    end_regs: cpu::Regs,
    end_halted: bool,
    events: Vec<Event>,
    tstate_ran: u16,
    changed_mem_values: Vec<MemValues>,
}

fn parse_cpu_regs(input: Vec<&str>, r: &mut cpu::Regs, halted: &mut bool) -> Result<u16, String> {
    let regs: Vec<&str> = input[0].split(' ').collect();
    for (i, (name, reg)) in vec![
        ("AF", RegPair::AF),
        ("BC", RegPair::BC),
        ("DE", RegPair::DE),
        ("HL", RegPair::HL),
        ("AF'", RegPair::AFp),
        ("BC'", RegPair::BCp),
        ("DE'", RegPair::DEp),
        ("HL'", RegPair::HLp),
        ("IX", RegPair::IX),
        ("IY", RegPair::IY),
    ]
    .iter()
    .enumerate()
    {
        let val: u16 = match u16::from_str_radix(regs[i], 16) {
            Ok(x) => x,
            Err(_) => return Err(format!("bad {} at line {}: {}", name, input[0], regs[i])),
        };
        r.set_regpair(*reg, val);
    }

    for (i, (name, reg)) in vec![
        ("SP", &mut r.SP),
        ("PC", &mut r.PC),
        ("MEMPTR", &mut r.MEMPTR),
    ]
    .into_iter()
    .enumerate()
    {
        *reg = match u16::from_str_radix(regs[i + 10], 16) {
            Ok(x) => x,
            Err(_) => {
                return Err(format!(
                    "bad {} at line {}: {}",
                    name,
                    input[0],
                    regs[i + 8]
                ))
            }
        };
    }

    let regs: Vec<&str> = input[1].split_ascii_whitespace().collect();
    for (name, reg, offset) in vec![
        ("I", &mut r.I, 0),
        ("R", &mut r.R, 1),
        ("Int Mode", &mut r.IM, 4),
    ]
    .into_iter()
    {
        *reg = match u8::from_str_radix(regs[offset], 16) {
            Ok(x) => x,
            Err(_) => {
                return Err(format!(
                    "bad {} at line {}: {}",
                    name, input[1], regs[offset]
                ))
            }
        };
    }
    for (name, reg, offset) in vec![
        ("IFF1", &mut r.IFF1, 2),
        ("IFF2", &mut r.IFF2, 3),
        ("halted ", halted, 5),
    ]
    .into_iter()
    {
        *reg = match u8::from_str_radix(regs[offset], 16) {
            Ok(x) => x != 0,
            Err(_) => {
                return Err(format!(
                    "bad {} at line {}: {}",
                    name, input[1], regs[offset]
                ))
            }
        };
    }
    match regs[6].parse() {
        /* return expression is number of tstates to run */
        Ok(x) => Ok(x),
        Err(_) => Err(format!(
            "bad tstate_len at end of {}: {}",
            input[1], regs[6]
        )),
    }
}
fn parse_memory_values(input: Vec<&str>, memory_values: &mut Vec<MemValues>) -> Result<(), String> {
    for l in input.iter().filter(|l| **l != "-1") {
        let mem: Vec<&str> = l.split_ascii_whitespace().collect();
        //parse address and all value
        let base_addr = match u16::from_str_radix(mem[0], 16) {
            Err(_) => return Err(format!("bad mem addr {}", mem[0])),
            Ok(a) => a,
        };
        let values: Result<Vec<_>, _> = mem
            .iter()
            .skip(1)
            .filter(|s| **s != "-1")
            .map(|s| match u8::from_str_radix(s, 16) {
                Err(_) => Err(format!("bad mem val {}", s)),
                Ok(x) => Ok(x),
            })
            .collect();
        let values = values?;
        memory_values.push(MemValues { base_addr, values })
    }
    Ok(())
}

fn parse_tests(input: &str, expected: &str) -> Option<Vec<Test>> {
    let mut tests = Vec::new();
    let inputs: Vec<&str> = input.trim().split("\n\n").collect();
    let results: Vec<&str> = expected.trim().split("\n\n").collect();
    if inputs.len() != results.len() {
        panic!(
            "Inconsistent test len in {} vs res {}",
            inputs.len(),
            results.len()
        )
    }
    for (i, l) in inputs.iter().enumerate() {
        let lines: Vec<&str> = l.lines().collect();
        let res: Vec<&str> = results[i].lines().collect();
        let mut t = Test::default();
        if lines.len() < 5 {
            panic!("Not enough lines {:?}", lines)
        }
        if lines[0] != res[0] {
            panic!("Mismatch test desc {} vs {}", lines[0], res[0])
        }
        t.desc = lines[0].to_string();
        let len = parse_cpu_regs(
            lines[1..=2].to_vec(),
            &mut t.start_regs,
            &mut t.start_halted,
        )
        .unwrap_or_else(|s| panic!("test {} ({}): {}", t.desc, i, s));
        t.tstate_to_run = len;
        parse_memory_values(lines[3..lines.len()].to_vec(), &mut t.memory_values)
            .unwrap_or_else(|e| panic!("test {} ({}): {}", t.desc, i, e));

        t.events = res
            .iter()
            .skip(1)
            .filter(|l| l.starts_with(' '))
            .map(|l| {
                let event: Vec<&str> = l.split_ascii_whitespace().collect();
                let time = event[0].parse().unwrap_or_else(|_| {
                    panic!(
                        "test result {} ({}): bad event time {}",
                        t.desc, i, event[0]
                    )
                });
                let address = u16::from_str_radix(event[2], 16).unwrap_or_else(|_| {
                    panic!(
                        "test result {} ({}): bad event addr {}",
                        t.desc, i, event[2]
                    )
                });
                let mut value: u8 = 0;
                if event.len() == 4 {
                    value = u8::from_str_radix(event[3], 16).unwrap_or_else(|_| {
                        panic!(
                            "test result {} ({}): bad event value {}",
                            t.desc, i, event[3]
                        )
                    });
                } else {
                    assert!(event[1] == "MC" || event[1] == "PC");
                }
                let typ = match event[1] {
                    "MC" => EventType::MemoryContention,
                    "PC" => EventType::PortContention,
                    "MR" => EventType::MemoryRead { value },
                    "MW" => EventType::MemoryWrite { value },
                    "PR" => EventType::PortRead { value },
                    "PW" => EventType::PortWrite { value },
                    _ => panic!("Unknown event type {}", event[1]),
                };
                Event { time, address, typ }
            })
            .collect();
        let cpuval = res
            .iter()
            .skip(1)
            .filter(|l| !l.starts_with("  "))
            .take(2)
            .cloned()
            .collect();

        let len = parse_cpu_regs(cpuval, &mut t.end_regs, &mut t.end_halted)
            .unwrap_or_else(|e| panic!("test res {} ({}): end state {}", t.desc, i, e));
        t.tstate_ran = len;

        let memval = res
            .into_iter()
            .skip(1)
            .filter(|l| !l.starts_with("  "))
            .skip(2)
            .collect();
        parse_memory_values(memval, &mut t.changed_mem_values)
            .unwrap_or_else(|e| panic!("test res {} ({}): {}", t.desc, i, e));
        tests.push(t)
    }
    Some(tests)
}

// TODO can panic ?
fn setup_memory(values: &[MemValues], mem: &mut mem::Memory) {
    //let mut mem = Vec::new();
    for m in values.iter() {
        for (j, v) in m.values.iter().enumerate() {
            mem.set_u8(m.base_addr + j as u16, *v)
        }
    }
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

#[test]
fn fuse_tests() {
    let tests = parse_tests(
        include_str!("fusetests/tests.in"),
        include_str!("fusetests/tests.expected"),
    )
    .unwrap();

    for t in tests.iter() {
        let mut state = cpu::State::default();
        state.r = t.start_regs.clone();
        state.halted = t.start_halted;
        let mut data = mem::Memory::init(mem::Mapper::ZX64K, None); // This test suite is for machines with more RAM
        setup_memory(&t.memory_values, &mut data);
        let mut end_state = cpu::State::default();
        end_state.r = t.end_regs.clone();
        end_state.halted = t.end_halted;
        end_state.mem = data.clone();
        setup_memory(&t.changed_mem_values, &mut end_state.mem);
        state.mem = data;
        state.io = io::IO::default();
        end_state.io = io::IO::default();
        let dev = ZxSpectrumIODevice {};
        state.io.register(0, 0xFFFF, 0, &dev);

        dbg!(t);
        cpu::run(&mut state, t.tstate_to_run as usize, true).unwrap();
        assert_eq!(state, end_state, "test {}", t.desc);
    }
}
