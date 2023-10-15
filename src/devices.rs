use std::rc::Rc;

use crate::io;
use crate::joystick;
use crate::psg;
use crate::system;
use crate::vdp;

pub(crate) struct Devices {
    dbg_io: DebugIO,
    pub(crate) sys: system::System,
    pub(crate) joy: joystick::Joystick,
    pub(crate) pov: PsgOrVdp,
    // TODO: two IOMaps (R/W) instead ? Would remove the need for PsgOrVdp
    iomap: IOMap,
}

#[derive(Clone)]
struct Map {
    start: u16,
    end: u16,
    /// ignored bits
    ignore_mask: u16,
}

#[derive(Clone)]
struct Entry {
    dev: Device,
    addrs: Vec<Map>,
}

type IOMap = Vec<Entry>;

#[derive(Clone)]
enum Device {
    Vdp,
    System,
    Joystick,
    Psg,
    PsgOrVdp,
    Debug,
}

impl Devices {
    pub fn new(audio_cmds: psg::AudioCmdSender) -> Self {
        Self {
            dbg_io: DebugIO {},
            sys: system::System::default(),
            joy: joystick::Joystick::default(),
            pov: PsgOrVdp {
                psg: psg::PsgDevice::new(audio_cmds),
                vdp: vdp::Vdp::default(),
            },
            iomap: vec![
                Entry {
                    dev: Device::Vdp,
                    addrs: vec![
                        Map {
                            start: 0x7E,
                            end: 0x7E,
                            ignore_mask: 0xFF00,
                        },
                        Map {
                            start: 0xBE,
                            end: 0xBF,
                            ignore_mask: 0xFF01,
                        },
                    ],
                },
                Entry {
                    dev: Device::System,
                    addrs: vec![Map {
                        start: 0,
                        end: 5,
                        ignore_mask: 0xFF00,
                    }],
                },
                Entry {
                    dev: Device::Joystick,
                    addrs: vec![Map {
                        start: 0xDC,
                        end: 0xDD,
                        ignore_mask: 0xFF00,
                    }],
                },
                Entry {
                    dev: Device::PsgOrVdp,
                    addrs: vec![Map {
                        start: 0x7F,
                        end: 0x7F,
                        ignore_mask: 0xFF00,
                    }],
                },
                Entry {
                    dev: Device::Psg,
                    addrs: vec![Map {
                        start: 0x06,
                        end: 0x06,
                        ignore_mask: 0xFF00,
                    }],
                },
                Entry {
                    dev: Device::Debug,
                    addrs: vec![Map {
                        start: 0,
                        end: 0,
                        ignore_mask: 0xFFFF,
                    }],
                },
            ],
        }
    }
    fn dev_to_dev(&self, dev: Device) -> &dyn io::Device {
        match dev {
            Device::Vdp => &self.pov.vdp,
            Device::System => &self.sys,
            Device::Joystick => &self.joy,
            Device::Psg => &self.pov.psg,
            Device::PsgOrVdp => &self.pov,
            Device::Debug => &self.dbg_io,
        }
    }
    fn addr_to_dev(&self, addr: u16) -> Result<Device, String> {
        self.iomap
            .iter()
            .flat_map(|dev| dev.addrs.iter().map(|m| (dev.dev.clone(), m)))
            .find(|&(_, map)| {
                addr & !map.ignore_mask >= map.start && addr & !map.ignore_mask <= map.end
            })
            .map(|(dev, _)| dev)
            .ok_or(format!("Address {:04X} to unknown device", addr))
    }
}

impl io::Device for Rc<Devices> {
    fn out(&self, addr: u16, val: u8, cycle: u32) -> Result<(), String> {
        self.dev_to_dev(self.addr_to_dev(addr)?)
            .out(addr, val, cycle)
    }

    fn input(&self, addr: u16, cycle: u32) -> Result<u8, String> {
        self.dev_to_dev(self.addr_to_dev(addr)?).input(addr, cycle)
    }
}
struct DebugIO {}
impl io::Device for DebugIO {
    fn out(&self, addr: u16, val: u8, _: u32) -> Result<(), String> {
        println!("Unknown I/O write: @{:04X} {:02X} ", addr, val);
        //panic!();
        Ok(())
    }
    fn input(&self, addr: u16, _: u32) -> Result<u8, String> {
        println!("Unknown I/O read: @{:04X}, sending 0", addr);
        //panic!();
        Ok(0)
    }
}

pub(crate) struct PsgOrVdp {
    pub(crate) psg: psg::PsgDevice,
    pub(crate) vdp: vdp::Vdp,
}
impl io::Device for PsgOrVdp {
    fn out(&self, addr: u16, val: u8, cycle: u32) -> Result<(), String> {
        self.psg.out(addr, val, cycle)
    }
    fn input(&self, addr: u16, cycle: u32) -> Result<u8, String> {
        self.vdp.input(addr, cycle)
    }
}
