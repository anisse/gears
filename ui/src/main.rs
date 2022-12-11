#![deny(clippy::all)]
#![forbid(unsafe_code)]

use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use gilrs::Gilrs;
use pixels::{Pixels, SurfaceTexture};
use winit::dpi::LogicalSize;
use winit::event::{ElementState, Event, VirtualKeyCode, WindowEvent};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::WindowBuilder;

use gears::emu;

const WIDTH: u32 = 20 * 8;
const HEIGHT: u32 = 18 * 8;

fn main() -> Result<(), String> {
    let event_loop = EventLoop::new();
    let window = {
        let size = LogicalSize::new(WIDTH as f64, HEIGHT as f64);
        WindowBuilder::new()
            .with_title("Gears")
            .with_inner_size(size)
            .with_min_inner_size(size)
            .build(&event_loop)
            .unwrap()
    };

    let mut pixels = {
        let window_size = window.inner_size();
        let surface_texture = SurfaceTexture::new(window_size.width, window_size.height, &window);
        Pixels::new(WIDTH, HEIGHT, surface_texture)
            .map_err(|e| format!("pixels buffer init: {e}"))?
    };

    let mut gilrs =
        Gilrs::new().map_err(|e| format!("cannot init gilrs gamepad library: {}", e))?;

    let args: Vec<String> = env::args().collect();
    let file = args.get(1).expect("needs an argument");
    let path = Path::new(file);

    let mut file = match File::open(&path) {
        Err(why) => panic!("Cannot open {}: {}", path.display(), why),
        Ok(f) => f,
    };

    let mut data: Vec<u8> = vec![];
    if let Err(why) = file.read_to_end(&mut data) {
        panic!("Could not read {}: {}", path.display(), why);
    }

    let mut emu = emu::Emulator::init(data, true);

    let mut run = true;
    event_loop.run(move |event, _, control_flow| {
        // Draw the current frame
        match event {
            Event::RedrawRequested(_) => {
                //emulator.draw(pixels.get_frame());
                if pixels
                    .render()
                    .map_err(|e| format!("pixels.render() failed: {e}"))
                    .is_err()
                {
                    *control_flow = ControlFlow::Exit;
                    return;
                }
            }
            Event::WindowEvent { event: ref wev, .. } => match wev {
                WindowEvent::CloseRequested | WindowEvent::Destroyed => {
                    *control_flow = ControlFlow::Exit;
                    return;
                }
                WindowEvent::KeyboardInput { input, .. } => match input.virtual_keycode {
                    Some(VirtualKeyCode::Escape) => {
                        *control_flow = ControlFlow::Exit;
                        return;
                    }
                    Some(VirtualKeyCode::Space) => {
                        if input.state == ElementState::Pressed {
                            run = !run;
                        }
                    }
                    _ => handle_key(&mut emu, input),
                },
                WindowEvent::Resized(size) => {
                    pixels.resize_surface(size.width, size.height);
                }
                _ => {}
            },
            Event::MainEventsCleared => {
                // Update internal state and request a redraw
                //println!("Stepping");
                while let Some(gilrs::Event { event, .. }) = gilrs.next_event() {
                    handle_joystick_event(&mut emu, event);
                }

                if run {
                    loop {
                        if emu.step(pixels.get_frame()) {
                            break;
                        }
                    }
                }
                window.request_redraw();
            }
            _ => {}
        }
        print!("");
    });
}

fn handle_joystick_event(emu: &mut emu::Emulator, ev: gilrs::EventType) {
    let (emu_action, btn): (fn(&mut emu::Emulator, emu::Button), _) = match ev {
        gilrs::EventType::ButtonPressed(button, _) => (emu::Emulator::press, button),
        gilrs::EventType::ButtonReleased(button, _) => (emu::Emulator::release, button),
        //gilrs::EventType::AxisValueChanged(button, val, _) => todo!(),
        _ => return,
    };
    println!("{:?}", btn);
    let button = match btn {
        gilrs::Button::DPadUp => emu::Button::Up,
        gilrs::Button::DPadDown => emu::Button::Down,
        gilrs::Button::DPadLeft => emu::Button::Left,
        gilrs::Button::DPadRight => emu::Button::Right,
        gilrs::Button::South => emu::Button::One,
        gilrs::Button::East => emu::Button::Two,
        gilrs::Button::West => emu::Button::One,
        gilrs::Button::North => emu::Button::Two,
        gilrs::Button::Start => emu::Button::Start,
        _ => return,
    };
    emu_action(emu, button);
}

fn handle_key(emu: &mut emu::Emulator, input: &winit::event::KeyboardInput) {
    let button = match input.virtual_keycode {
        Some(VirtualKeyCode::Up) => emu::Button::Up,
        Some(VirtualKeyCode::Down) => emu::Button::Down,
        Some(VirtualKeyCode::Left) => emu::Button::Left,
        Some(VirtualKeyCode::Right) => emu::Button::Right,
        Some(VirtualKeyCode::Back) => emu::Button::One,
        Some(VirtualKeyCode::Return) => emu::Button::Two,
        Some(VirtualKeyCode::LShift) => emu::Button::Start,
        _ => return,
    };
    let emu_action = match input.state {
        ElementState::Pressed => emu::Emulator::press,
        ElementState::Released => emu::Emulator::release,
    };
    emu_action(emu, button)
}
