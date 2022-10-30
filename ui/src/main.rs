#![deny(clippy::all)]
#![forbid(unsafe_code)]

use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use pixels::{Pixels, SurfaceTexture};
use winit::dpi::LogicalSize;
use winit::event::{Event, VirtualKeyCode, WindowEvent};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::WindowBuilder;

use gears::emu;

const WIDTH: u32 = 32 * 8;
const HEIGHT: u32 = 28 * 8;

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

    let mut emu = emu::Emulator::init(data);

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
                    _ => {}
                },
                WindowEvent::Resized(size) => {
                    pixels.resize_surface(size.width, size.height);
                }
                _ => {}
            },
            Event::MainEventsCleared => {
                // Update internal state and request a redraw
                println!("Stepping");
                loop {
                    if emu.step(pixels.get_frame()) {
                        break;
                    }
                }
                window.request_redraw();
            }
            _ => {}
        }
    });
}
