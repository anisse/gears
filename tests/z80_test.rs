use gears::cpu;

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
    start_state: cpu::State,
    tstate_to_run: u16,
    memory_values: Vec<MemValues>,
    //tests.expected
    end_state: cpu::State,
    events: Vec<Event>,
    tstate_ran: u16,
    changed_mem_values: Vec<MemValues>,
}

fn parse_cpu_regs(input: Vec<&str>, st: &mut cpu::State) -> Result<u16, String> {
    let regs: Vec<&str> = input[0].split(' ').collect();
    let af: u16 = match u16::from_str_radix(regs[0], 16) {
        Ok(x) => x,
        Err(_) => return Err(format!("bad af at line {}", input[0])),
    };

    st.A = (af & 0xFF00 >> 8) as u8;
    st.F = (af & 0xFF) as u8;

    let len = match input[1].split_ascii_whitespace().collect::<Vec<&str>>()[6].parse() {
        Ok(x) => x,
        Err(_) => return Err(format!("bad tstate_len at end of {}", input[1])),
    };

    Ok(len)
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
            .map(|s| {
                match u8::from_str_radix(s, 16) {
                    Err(_) => Err(format!("bad mem val {}", s)),
                    Ok(x) => Ok(x),
                }
            })
        .collect();
        let values = values?;
        memory_values.push(MemValues{base_addr, values})
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
        let len = parse_cpu_regs(lines[1..=2].to_vec(), &mut t.start_state)
            .unwrap_or_else(|s| panic!("test {} ({}): {}", t.desc, i, s));
        t.tstate_to_run = len;
        parse_memory_values(lines[3..lines.len()].to_vec(), &mut t.memory_values).unwrap_or_else(|e| {
                     panic!("test {} ({}): {}", t.desc, i, e)
        });

        t.events = res
            .iter()
            .skip(1)
            .filter(|l| l.starts_with(" "))
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

        let len = parse_cpu_regs(cpuval, &mut t.end_state)
            .unwrap_or_else(|e| panic!("test res {} ({}): end state {}", t.desc, i, e));
        t.tstate_ran = len;

        let memval = res
            .into_iter()
            .skip(1)
            .filter(|l| !l.starts_with("  "))
            .skip(2)
            .collect();
        parse_memory_values(memval, &mut t.memory_values).unwrap_or_else(|e| {
                     panic!("test res {} ({}): {}", t.desc, i, e)
        });
        tests.push(t)
    }
    Some(tests)
}

fn setup_memory(values: &[MemValues]) -> Vec<u8> {
    let mut mem = Vec::new();
    let mut offset: usize = 0;
    for i in values.iter() {
        if i.base_addr as usize > offset {
            mem.append(&mut vec!(0; i.base_addr as usize - offset));
        }
        offset+=i.values.len();
        mem.append(&mut i.values.clone());
    }

    mem
}

#[test]
fn run_instructions() {
    let mut cpu = cpu::init();

    let tests = parse_tests(
        include_str!("z80-tests/tests.in"),
        include_str!("z80-tests/tests.expected"),
    )
    .unwrap();

    for t in tests.iter() {
        let mut state = t.start_state;
        let mut data = setup_memory(&t.memory_values);

        dbg!(t);
        cpu::run(&mut state, &data).unwrap();
        assert_eq!(t.end_state, state);
    }
}
