#![deny(clippy::all)]
#![forbid(unsafe_code)]

use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use gilrs::Gilrs;
use pixels::{Pixels, SurfaceTexture};
use winit::dpi::LogicalSize;
use winit::event::{ElementState, Event, VirtualKeyCode, WindowEvent};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::WindowBuilder;

use gears::emu;
use gears::emu::testcmd;

fn main() -> Result<(), String> {
    let event_loop = EventLoop::new();
    let window = {
        let size = LogicalSize::new(emu::LCD_WIDTH as f64, emu::LCD_HEIGHT as f64);
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
        pixels::PixelsBuilder::new(
            emu::LCD_WIDTH as u32,
            emu::LCD_HEIGHT as u32,
            surface_texture,
        )
        .present_mode(wgpu::PresentMode::AutoVsync)
        .build()
        .map_err(|e| format!("pixels buffer init: {e}"))?
    };

    let mut gilrs = Gilrs::new().map_err(|e| format!("cannot init gilrs gamepad library: {e}"))?;

    let args: Vec<String> = env::args().collect();
    let file = args.get(1).expect("needs an argument");
    let cmds = testcmd::TestCommand::new_vec(args.get(2).unwrap_or(&String::new()))?;
    let path = Path::new(file);

    let mut file = match File::open(path) {
        Err(why) => panic!("Cannot open {}: {}", path.display(), why),
        Ok(f) => f,
    };

    let mut data: Vec<u8> = vec![];
    if let Err(why) = file.read_to_end(&mut data) {
        panic!("Could not read {}: {}", path.display(), why);
    }

    let (audio_device, stream_config) = audio_init()?;
    let mut emu = emu::Emulator::init(
        data,
        true,
        emu::AudioConf::new(stream_config.channels, stream_config.sample_rate.0)?,
    );
    emu.run_commands(pixels.frame_mut(), &cmds);
    let audio_stream = audio_init_stream(audio_device, stream_config, emu.audio_callback())?;
    audio_stream
        .play()
        .map_err(|e| format!("cannot play stream: {e}"))?;

    let mut state = EmuLoop {
        running: true,
        stepping: false,
    };
    event_loop.run(move |event, _, control_flow| {
        // Draw the current frame
        match event {
            Event::RedrawRequested(_) => {
                if pixels
                    .render()
                    .map_err(|e| println!("pixels.render() failed: {e}"))
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
                            state.running = !state.running;
                        }
                    }
                    Some(VirtualKeyCode::LControl) => {
                        if input.state == ElementState::Pressed {
                            state.stepping = true;
                        }
                    }
                    _ => {
                        handle_key(&mut emu, input);
                    }
                },
                WindowEvent::Resized(size) => {
                    pixels
                        .resize_surface(size.width, size.height)
                        .unwrap_or_else(|e| print!("Unable to resize pixel surface: {e}"));
                }
                _ => {}
            },
            Event::MainEventsCleared => {
                // Update internal state and request a redraw
                joystick_events(&mut emu, &mut gilrs);
                emu.run(state.running || state.stepping);
                if state.main_events_cleared(&mut emu, &mut pixels) {
                    window.request_redraw();
                }
            }
            _ => {}
        }
        print!("");
    });
}

fn joystick_events(emu: &mut emu::Emulator, gilrs: &mut Gilrs) {
    while let Some(gilrs::Event { event, .. }) = gilrs.next_event() {
        handle_joystick_event(emu, event);
    }
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

fn audio_init() -> Result<(cpal::Device, cpal::StreamConfig), String> {
    let audio_host = cpal::default_host();
    let audio_device = audio_host
        .default_output_device()
        .ok_or_else(|| "cannot get default audio output device".to_string())?;
    /*
    let audio_configs = audio_device.supported_output_configs().map_err(|e| {
        format!(
            "no supported config for device {:?}: {e}",
            audio_device.name()
        )
    })?;
    println!(
        "default out conf: {:?}",
        audio_device.default_output_config()
    );
    */
    let audio_config = audio_device
        .default_output_config()
        .map_err(|e| format!("output config error: {e}"))?;
    let channels = audio_config.channels() as usize;
    let sample_rate = audio_config.sample_rate().0 as usize;
    if sample_rate != 44100 {
        return Err(format!("Unsupported sample_rate: {sample_rate}"));
    }
    if channels != 2 {
        return Err(format!("Only two channels are supported, not: {channels}"));
    }
    let supported_buffer_size = audio_config.buffer_size().clone();
    println!("Possible sizes: {supported_buffer_size:?}");
    let mut stream_config: cpal::StreamConfig = audio_config.into();
    let default_buffer_size = 1472; // ~ 16ms buffer, about a frame. This is a manually rounded-up
                                    // buffer
    stream_config.buffer_size = cpal::BufferSize::Fixed(match supported_buffer_size {
        cpal::SupportedBufferSize::Range { min, .. } => {
            if min > default_buffer_size {
                min
            } else {
                default_buffer_size
            }
        }
        cpal::SupportedBufferSize::Unknown => default_buffer_size,
    } as cpal::FrameCount);
    Ok((audio_device, stream_config))
}
fn audio_init_stream(
    audio_device: cpal::Device,
    stream_config: cpal::StreamConfig,
    mut audio_cb: emu::AudioCallback,
) -> Result<cpal::Stream, String> {
    let audio_stream = audio_device
        .build_output_stream(
            &stream_config,
            move |data: &mut [f32], _: &cpal::OutputCallbackInfo| {
                audio_cb(data);
            },
            move |_err| {},
            None,
        )
        .map_err(|e| format!("could not start stream: {e}"))?;

    Ok(audio_stream)
}

struct EmuLoop {
    running: bool,
    stepping: bool,
}

impl EmuLoop {
    fn main_events_cleared(&mut self, emu: &mut emu::Emulator, pixels: &mut Pixels) -> bool {
        if self.running || self.stepping {
            loop {
                if emu.step(pixels.frame_mut()) {
                    break;
                }
            }
            self.stepping = false;
            return true;
        }
        false
    }
}
