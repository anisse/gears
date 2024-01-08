#![deny(clippy::all)]
#![forbid(unsafe_code)]

use std::env;
use std::error::Error;
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
use winit::window::{Window, WindowBuilder};

use gears::emu;
use gears::emu::testcmd;

#[cfg(not(target_arch = "wasm32"))]
fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    let cmds = testcmd::TestCommand::new_vec(args.get(2).unwrap_or(&String::new()))?;
    let file = args.get(1).expect("needs an argument");
    let path = Path::new(file);

    let mut file = match File::open(path) {
        Err(why) => panic!("Cannot open {}: {}", path.display(), why),
        Ok(f) => f,
    };

    let mut data: Vec<u8> = vec![];
    file.read_to_end(&mut data)
        .map_err(|e| format!("Could not read {}: {e}", path.display()))?;
    pollster::block_on(run(&data, &cmds))
}
#[cfg(target_arch = "wasm32")]
fn main() {
    web::web_main().unwrap();
}
#[cfg(target_arch = "wasm32")]
mod web {
    use std::error::Error;
    use std::rc::Rc;

    use js_sys::Uint8Array;
    use wasm_bindgen::closure::Closure;
    use wasm_bindgen::{JsCast, JsValue};
    use web_sys::{Document, Element, HtmlInputElement};
    use winit::window::Window;

    pub fn web_main() -> Result<(), JsValue> {
        std::panic::set_hook(Box::new(console_error_panic_hook::hook));
        console_log::init_with_level(log::Level::Info).expect("error initializing logger");
        setup_dom()
    }
    fn setup_dom() -> Result<(), JsValue> {
        let client_window = web_sys::window().ok_or("cannot get JS DOM window")?;
        let document = client_window.document().ok_or("no document in window")?;
        let body = document.body().ok_or("no body in document")?;
        let button = document
            .create_element("button")
            .map_err(|e| format!("cannot create button: {e:?}"))?;
        body.set_inner_text(
            "D-Pad: ⬆️⬇️⬅️➡️ , Start: [SHIFT], 1: [BACKSPACE], 2: [ENTER], Pause emulation: [SPACE]\n",
        );
        button.set_text_content(Some("Start"));
        body.append_child(&button)
            .map_err(|e| format!("couldn't insert start button in document body: {e:?}"))?;
        let closure = Closure::wrap(Box::new(move |_e: web_sys::Event| {
            wasm_bindgen_futures::spawn_local(run_noerr(
                include_bytes!("../../../roms/Sonic The Hedgehog (World) (Rev 1).gg").into(),
            ));
        }) as Box<dyn FnMut(_)>);
        button
            .add_event_listener_with_callback("click", closure.as_ref().unchecked_ref())
            .map_err(|e| format!("cannot attach callback to button: {e:?}"))?;
        closure.forget();
        body.append_child(&select_rom_btn(document)?.into())
            .map_err(|e| format!("couldn't insert select button in document body: {e:?}"))?;

        Ok(())
    }
    fn select_rom_btn(doc: Document) -> Result<Element, JsValue> {
        let button = doc
            .create_element("button")
            .map_err(|e| format!("cannot create button: {e:?}"))?;
        button.set_text_content(Some("Open File..."));
        let input = doc.create_element("input").unwrap();
        let input_html: &HtmlInputElement = input.unchecked_ref();
        let input_html = input_html.clone();
        input_html.set_attribute("type", "file")?;
        input_html.set_attribute("accept", ".gg")?;
        let file_reader = web_sys::FileReader::new().expect("cannot create file reader");
        let open_file_dialog = Closure::wrap(Box::new(move |_e: web_sys::Event| {
            let input_html = input_html.clone();
            let ip1 = input_html.clone();
            let file_reader = file_reader.clone();
            let load_file = Closure::wrap(Box::new(move |_e: web_sys::Event| {
                let file_reader = file_reader.clone();
                let fr1 = file_reader.clone();
                let file_loaded = Closure::wrap(Box::new(move |_e: web_sys::Event| {
                    let array = fr1.result().unwrap();
                    let u8array = Uint8Array::new(&array);
                    wasm_bindgen_futures::spawn_local(run_noerr(u8array.to_vec()));
                }) as Box<dyn FnMut(_)>);
                let files = ip1.files().expect("a file list");
                let file = files.get(0).expect("first file");
                file_reader.set_onloadend(Some(file_loaded.as_ref().unchecked_ref()));
                file_reader
                    .read_as_array_buffer(&file.into())
                    .expect("cannot read file");
                file_loaded.forget();
            }) as Box<dyn FnMut(_)>);
            input_html
                .add_event_listener_with_callback("change", load_file.as_ref().unchecked_ref())
                .expect("cannot attach callback onchange input");
            load_file.forget();
            input_html.click();
        }) as Box<dyn FnMut(_)>);
        button
            .add_event_listener_with_callback("click", open_file_dialog.as_ref().unchecked_ref())
            .map_err(|e| format!("cannot attach callback to button: {e:?}"))?;
        open_file_dialog.forget();
        Ok(button)
    }
    async fn run_noerr(data: Vec<u8>) {
        super::run(&data, &[]).await.unwrap();
    }
    pub fn web_init(window: Rc<Window>) -> Result<(), Box<dyn Error>> {
        use winit::dpi::LogicalSize;
        use winit::platform::web::WindowExtWebSys;

        // Retrieve current width and height dimensions of browser client window
        let get_window_size = || {
            let client_window = web_sys::window().expect("cannot get JS DOM window");
            LogicalSize::new(
                client_window
                    .inner_width()
                    .expect("Cannot get JS Window width")
                    .as_f64()
                    .expect("width not f64"),
                client_window
                    .inner_height()
                    .expect("Cannot get JS Window height")
                    .as_f64()
                    .expect("height not f64")
                    - 45.0,
            )
        };

        // Initialize winit window with current dimensions of browser client
        window.set_inner_size(get_window_size());

        let client_window = web_sys::window().ok_or("cannot get JS DOM window")?;

        let canvas = window.canvas();
        // Attach winit canvas to body element
        client_window
            .document()
            .ok_or("no document in window")?
            .body()
            .ok_or("no body in document")?
            .append_child(&canvas)
            .map_err(|e| format!("couldn't insert winit canvas in document body: {e:?}"))?;

        canvas
            .focus()
            .map_err(|e| format!("Could not focus canvas: {e:?}"))?;
        // Listen for resize event on browser client. Adjust winit window dimensions
        // on event trigger
        let closure = wasm_bindgen::closure::Closure::wrap(Box::new(move |_e: web_sys::Event| {
            let size = get_window_size();
            window.set_inner_size(size)
        }) as Box<dyn FnMut(_)>);
        client_window
            .add_event_listener_with_callback("resize", closure.as_ref().unchecked_ref())
            .map_err(|e| format!("cannot add resize event listener to window: {e:?}"))?;
        closure.forget();
        Ok(())
    }
}
async fn run(data: &[u8], cmds: &[testcmd::TestCommand]) -> Result<(), Box<dyn Error>> {
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
                if state.main_events_cleared(&mut emu, &mut pixels) {
                    window.request_redraw();
                }
            }
            _ => {}
        }
        print!("");
    });
}

fn web_init(win: Rc<Window>) -> Result<(), Box<dyn Error>> {
    #[cfg(target_arch = "wasm32")]
    return web::web_init(win);
    #[cfg(not(target_arch = "wasm32"))]
    Ok(())
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
        /*
        let start_msg = format!(
            "elapsed since last call: {:#?}, acc: {:#?}, => ",
            self.current_time.elapsed(),
            self.accumulator,
        );
        */
        let single_step = self.stepping;
        self.accumulator += self.current_time.elapsed();
        let mut frames = 0;
        while self.accumulator >= Self::FRAME_DURATION {
            self.current_time = Instant::now();
            frames += 1;
            self.run(emu, pixels);
            self.accumulator -= Self::FRAME_DURATION;
            self.accumulator += self.current_time.elapsed();
        }
        self.current_time = Instant::now();
        /*
        println!(
            "{start_msg} elapsed: {:#?}, acc: {:#?}, skip_next: {}, ran {frames} frames",
            self.current_time.elapsed(),
            self.accumulator,
            self.skip_next
        );
        */
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
