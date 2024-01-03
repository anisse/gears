#![deny(clippy::all)]
#![forbid(unsafe_code)]

use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::rc::Rc;

#[cfg(target_arch = "wasm32")]
use instant::{Duration, Instant};
#[cfg(not(target_arch = "wasm32"))]
use std::time::{Duration, Instant};

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
    #[cfg(target_arch = "wasm32")]
    {
        std::panic::set_hook(Box::new(console_error_panic_hook::hook));
        wasm_bindgen_futures::spawn_local(run_web());
        Ok(())
    }

    #[cfg(not(target_arch = "wasm32"))]
    {
        pollster::block_on(run())
    }
}
#[cfg(target_arch = "wasm32")]
async fn run_web() {
    run().await.unwrap();
}
async fn run() -> Result<(), String> {
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

    let window = Rc::new(window);

    #[cfg(target_arch = "wasm32")]
    {
        use wasm_bindgen::JsCast;
        use winit::platform::web::WindowExtWebSys;

        // Retrieve current width and height dimensions of browser client window
        let get_window_size = || {
            let client_window = web_sys::window().unwrap();
            LogicalSize::new(
                client_window.inner_width().unwrap().as_f64().unwrap(),
                client_window.inner_height().unwrap().as_f64().unwrap(),
            )
        };

        let window = Rc::clone(&window);

        // Initialize winit window with current dimensions of browser client
        window.set_inner_size(get_window_size());

        let client_window = web_sys::window().unwrap();

        // Attach winit canvas to body element
        web_sys::window()
            .and_then(|win| win.document())
            .and_then(|doc| doc.body())
            .and_then(|body| {
                body.append_child(&web_sys::Element::from(window.canvas()))
                    .ok()
            })
            .expect("couldn't append canvas to document body");

        // Listen for resize event on browser client. Adjust winit window dimensions
        // on event trigger
        let closure = wasm_bindgen::closure::Closure::wrap(Box::new(move |_e: web_sys::Event| {
            let size = get_window_size();
            window.set_inner_size(size)
        }) as Box<dyn FnMut(_)>);
        client_window
            .add_event_listener_with_callback("resize", closure.as_ref().unchecked_ref())
            .unwrap();
        closure.forget();
    }

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
    let audio_conf = emu::AudioConf::new(stream_config.channels, stream_config.sample_rate.0)?;
    let (mut emu, mut audio_callback) = emu::Emulator::init(data, true, audio_conf.clone());
    emu.run_commands(pixels.frame_mut(), &cmds, &mut audio_callback, audio_conf);
    let audio_stream = audio_init_stream(audio_device, stream_config, audio_callback)?;
    audio_stream
        .play()
        .map_err(|e| format!("cannot play stream: {e}"))?;

    let mut state = EmuLoop::default();
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
    //println!("Possible sizes: {supported_buffer_size:?}");
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
    stepping: bool,
    state: RunState,
    skip_next: bool,

    current_time: Instant,
    accumulator: Duration,
}

impl Default for EmuLoop {
    fn default() -> Self {
        Self {
            running: true,
            stepping: false,
            skip_next: false,
            state: RunState::Run,

            current_time: Instant::now(),
            accumulator: Duration::from_secs(0),
        }
    }
}

#[derive(Debug)]
enum RunState {
    Run,
    Render,
    Sleep,
}

impl EmuLoop {
    const FRAME_DURATION: Duration = Duration::from_nanos((1.0 / 60.0 * 1_000_000_000.0) as u64);
    fn main_events_cleared(&mut self, emu: &mut emu::Emulator, pixels: &mut Pixels) -> bool {
        /*
        print!(
            "{:#?} {:#?} state {:?} => ",
            self.current_time.elapsed(),
            self.accumulator,
            self.state
        );
        */
        self.state = self.should_run_or_sleep();
        /*
        println!(
            "{:?} {:#?} {:#?}",
            self.state,
            self.current_time.elapsed(),
            self.accumulator
        );
        */
        match self.state {
            RunState::Run => {
                if !self.running && !self.stepping {
                    return false;
                }
                loop {
                    match emu.step(pixels.frame_mut()) {
                        emu::DisplayRefresh::NoDisplay => {}
                        emu::DisplayRefresh::ScreenDone => break,
                        emu::DisplayRefresh::ScreenDoneNoRefresh => {
                            self.skip_next = true;
                            break;
                        }
                    }
                }
                self.stepping = false;
            }
            RunState::Render => {
                if self.skip_next {
                    self.skip_next = false;
                    return false;
                }
                return true;
            }
            RunState::Sleep => {
                const SLEEP_TIME: Duration = Duration::from_millis(1);
                const EPOLL_TIMEOUT: Duration = SLEEP_TIME;
                // Workaround winit/mio polling using epoll with a timeout of 1ms, so take that
                // into account for sleeping in case there is no event (most often)
                if self.accumulator + SLEEP_TIME + EPOLL_TIMEOUT < Self::FRAME_DURATION {
                    std::thread::sleep(SLEEP_TIME);
                }
            }
        }
        false
    }

    fn should_run_or_sleep(&mut self) -> RunState {
        match self.state {
            RunState::Run => {
                if self.accumulator >= Self::FRAME_DURATION {
                    self.accumulator -= Self::FRAME_DURATION;
                }
                if self.accumulator >= Self::FRAME_DURATION {
                    return RunState::Run; // frame skip
                }
                RunState::Render
            }
            RunState::Render => RunState::Sleep,
            RunState::Sleep => {
                let new_time = Instant::now();
                let elapsed_time = new_time.duration_since(self.current_time);
                self.current_time = new_time;
                self.accumulator += elapsed_time;
                if self.accumulator >= Self::FRAME_DURATION {
                    return RunState::Run;
                }
                RunState::Sleep
            }
        }
    }
}
