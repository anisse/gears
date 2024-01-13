#![deny(clippy::all)]
#![forbid(unsafe_code)]

use std::error::Error;
use std::rc::Rc;

#[cfg(target_arch = "wasm32")]
use instant::{Duration, Instant};
#[cfg(not(target_arch = "wasm32"))]
use std::time::{Duration, Instant};

use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use gilrs::Gilrs;
use log::{debug, info, trace};
use pixels::{Pixels, SurfaceTexture};
use winit::dpi::LogicalSize;
use winit::event::{ElementState, Event, VirtualKeyCode, WindowEvent};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::{Window, WindowBuilder};

use gears::emu;
use gears::emu::testcmd;

pub async fn run(data: &[u8], cmds: &[testcmd::TestCommand]) -> Result<(), Box<dyn Error>> {
    let event_loop = EventLoop::new();
    let window = Rc::new({
        let size = LogicalSize::new(emu::LCD_WIDTH as f64, emu::LCD_HEIGHT as f64);
        WindowBuilder::new()
            .with_title("Gears")
            .with_inner_size(size)
            .with_min_inner_size(size)
            .build(&event_loop)
            .map_err(|e| format!("cannot create gears window: {e}"))?
    });

    web_init(Rc::clone(&window))?;

    let mut pixels = {
        let window_size = window.inner_size();
        let surface_texture =
            SurfaceTexture::new(window_size.width, window_size.height, window.as_ref());
        pixels::PixelsBuilder::new(
            emu::LCD_WIDTH as u32,
            emu::LCD_HEIGHT as u32,
            surface_texture,
        )
        .present_mode(pixels::wgpu::PresentMode::AutoNoVsync)
        .build_async()
        .await
        .map_err(|e| format!("pixels buffer init: {e}"))?
    };

    let mut gilrs = Gilrs::new().map_err(|e| format!("cannot init gilrs gamepad library: {e}"))?;

    let (audio_device, stream_config) = audio_init()?;
    let audio_conf = emu::AudioConf::new(stream_config.channels, stream_config.sample_rate.0)?;
    let (mut emu, mut audio_callback) = emu::Emulator::init(
        data.to_vec(), /* TODO: remove copy */
        true,
        audio_conf.clone(),
    );
    emu.run_commands(pixels.frame_mut(), cmds, &mut audio_callback, audio_conf);
    let audio_stream = audio_init_stream(audio_device, stream_config, audio_callback)?;
    audio_stream
        .play()
        .map_err(|e| format!("cannot play stream: {e}"))?;

    let mut state = EmuLoop::default();
    event_loop.run(move |event, _, control_flow| {
        // Draw the current frame
        match event {
            Event::RedrawRequested(_) => {
                debug!("Sending render");
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
                WindowEvent::Focused(focus) => {
                    info!("got focus event {focus}");
                    if state.running_focus != *focus {
                        state.running_focus = *focus;
                        emu.run(state.should_run())
                    }
                }
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
                emu.run(state.should_run());
                trace!("running emu after events cleared");
                if state.main_events_cleared(&mut emu, &mut pixels) {
                    debug!("requesting window redraw");
                    window.request_redraw();
                }
            }
            _ => {}
        }
        print!("");
    });
}

fn web_init(_win: Rc<Window>) -> Result<(), Box<dyn Error>> {
    #[cfg(target_arch = "wasm32")]
    {
        use crate::web;
        return web::web_init(_win);
    }
    #[cfg(not(target_arch = "wasm32"))]
    Ok(())
}

fn joystick_events(emu: &mut emu::Emulator, gilrs: &mut Gilrs) {
    while let Some(gilrs::Event { event, .. }) = gilrs.next_event() {
        handle_joystick_event(emu, event);
    }
}

fn handle_joystick_event(emu: &mut emu::Emulator, ev: gilrs::EventType) {
    trace!("joy ev: {:?}", ev);
    let (emu_action, btn): (fn(&mut emu::Emulator, emu::Button), _) = match ev {
        gilrs::EventType::ButtonPressed(button, _) => (emu::Emulator::press, button),
        gilrs::EventType::ButtonReleased(button, _) => (emu::Emulator::release, button),
        //gilrs::EventType::AxisValueChanged(button, val, _) => todo!(),
        _ => return,
    };
    debug!("joy btn: {:?}", btn);
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
    let audio_device = audio_host.default_output_device().ok_or_else(|| {
        format!(
            "cannot get default audio output device; available: {}",
            match audio_host.output_devices() {
                Ok(devs) => devs
                    .map(|d| d.name().unwrap_or("nameless device".to_string() + ", "))
                    .collect::<String>(),

                Err(e) => e.to_string(),
            }
        )
    })?;
    trace!(
        "Audio device default output config: {:?}",
        audio_device.default_output_config()
    );
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
    trace!("Audio config possible buffer sizes: {supported_buffer_size:?}");
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

#[derive(Debug)]
struct EmuLoop {
    running: bool,
    running_focus: bool,
    stepping: bool,
    skip_next: bool,
    current_time: Instant,
    accumulator: Duration,
}

impl Default for EmuLoop {
    fn default() -> Self {
        Self {
            running: true,
            running_focus: true,
            stepping: false,
            skip_next: false,
            current_time: Instant::now(),
            accumulator: Duration::from_secs(0),
        }
    }
}

impl EmuLoop {
    const FRAME_DURATION: Duration = Duration::from_nanos((1.0 / 60.0 * 1_000_000_000.0) as u64);
    fn main_events_cleared(&mut self, emu: &mut emu::Emulator, pixels: &mut Pixels) -> bool {
        let (start_elapsed, start_acc) = (self.current_time.elapsed(), self.accumulator);
        let single_step = self.stepping;
        self.accumulator += start_elapsed;
        let mut frames = 0;
        while self.accumulator >= Self::FRAME_DURATION {
            self.current_time = Instant::now();
            frames += 1;
            self.run(emu, pixels);
            self.accumulator -= Self::FRAME_DURATION;
            self.accumulator += self.current_time.elapsed();
        }
        self.current_time = Instant::now();
        trace!(
            "events cleared elapsed since last call: {start_elapsed:#?}, acc: {start_acc:#?}, => elapsed: {:#?}, acc: {:#?}, skip_next: {}, ran {frames} frames",
            self.current_time.elapsed(),
            self.accumulator,
            self.skip_next
        );
        frames > 0 && !self.skip_next && (self.should_run() || single_step)
    }
    fn should_run(&self) -> bool {
        (self.running && self.running_focus) || self.stepping
    }
    fn run(&mut self, emu: &mut emu::Emulator, pixels: &mut Pixels) {
        if !self.should_run() {
            return;
        }
        let buffer = pixels.frame_mut();
        loop {
            match emu.step(buffer) {
                emu::DisplayRefresh::NoDisplay => {}
                emu::DisplayRefresh::ScreenDone => {
                    self.skip_next = false;
                    break;
                }
                emu::DisplayRefresh::ScreenDoneNoRefresh => {
                    self.skip_next = true;
                    break;
                }
            }
        }
        self.stepping = false;
    }
}
