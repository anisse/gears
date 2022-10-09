#![deny(clippy::all)]
#![forbid(unsafe_code)]

use pixels::{Pixels, SurfaceTexture};
use winit::dpi::LogicalSize;
use winit::event::{Event, VirtualKeyCode, WindowEvent};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::WindowBuilder;

const WIDTH: u32 = 320;
const HEIGHT: u32 = 240;
const BOX_SIZE: i16 = 64;

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
    let mut emulator = Emulator::new();

    event_loop.run(move |event, _, control_flow| {
        // Draw the current frame
        match event {
            Event::RedrawRequested(_) => {
                emulator.draw(pixels.get_frame());
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
                emulator.update();
                window.request_redraw();
            }
            _ => {}
        }
    });
}

/// Representation of the application state. In this example, a box will bounce around the screen.
struct Emulator {
    box_x: i16,
    box_y: i16,
    velocity_x: i16,
    velocity_y: i16,
}

impl Emulator {
    fn new() -> Self {
        Self {
            box_x: 24,
            box_y: 16,
            velocity_x: 1,
            velocity_y: 1,
        }
    }

    fn update(&mut self) {
        if self.box_x <= 0 || self.box_x + BOX_SIZE > WIDTH as i16 {
            self.velocity_x *= -1;
        }
        if self.box_y <= 0 || self.box_y + BOX_SIZE > HEIGHT as i16 {
            self.velocity_y *= -1;
        }

        self.box_x += self.velocity_x;
        self.box_y += self.velocity_y;
    }

    /// Assumes the default texture format: `wgpu::TextureFormat::Rgba8UnormSrgb`
    fn draw(&self, frame: &mut [u8]) {
        for (i, pixel) in frame.chunks_exact_mut(4).enumerate() {
            let x = (i % WIDTH as usize) as i16;
            let y = (i / WIDTH as usize) as i16;

            let inside_the_box = x >= self.box_x
                && x < self.box_x + BOX_SIZE
                && y >= self.box_y
                && y < self.box_y + BOX_SIZE;

            let rgba = if inside_the_box {
                [0x5e, 0x48, 0xe8, 0xff]
            } else {
                [0x48, 0xb2, 0xe8, 0xff]
            };

            pixel.copy_from_slice(&rgba);
        }
    }
}
