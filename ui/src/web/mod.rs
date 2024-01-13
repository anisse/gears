#![deny(clippy::all)]

use crate::core::run;

use std::error::Error;
use std::rc::Rc;

use js_sys::Uint8Array;
use wasm_bindgen::closure::Closure;
use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::{JsCast, JsValue};
use web_sys::{Document, Element, Event, FileReader, HtmlInputElement};
use winit::window::Window;

#[wasm_bindgen(start)]
pub(crate) fn web_main() -> Result<(), JsValue> {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));
    console_log::init_with_level(log::Level::Info).expect("error initializing logger");
    setup_dom()
}
fn setup_dom() -> Result<(), JsValue> {
    let client_window = web_sys::window().ok_or("cannot get JS DOM window")?;
    let document = client_window.document().ok_or("no document in window")?;
    let body = document.body().ok_or("no body in document")?;
    body.set_inner_text(
        "D-Pad: ⬆️⬇️⬅️➡️ , Start: [SHIFT], 1: [BACKSPACE], 2: [ENTER], Pause emulation: [SPACE]\n",
    );
    body.append_child(
        &append_embedded_rom(
            document.clone(),
            "<a href=\"https://louistheseganerd.itch.io/monaco-master\">Monaco Master</a>:",
            include_bytes!("../../resources/Monaco Master GG V1.1.gg").into(),
        )?
        .into(),
    )
    .map_err(|e| format!("couldn't insert Monaco play span in document body: {e:?}"))?;
    body.append_child(
        &append_embedded_rom(
            document.clone(),
            "<a href=\"https://github.com/sverx/GGTestSuite\">GG Test Suite</a>:",
            include_bytes!("../../resources/GGTestSuite.gg").into(),
        )?
        .into(),
    )
    .map_err(|e| format!("couldn't insert GGT play span in document body: {e:?}"))?;
    body.append_child(&select_rom_btn(document)?.into())
        .map_err(|e| format!("couldn't insert select file in document body: {e:?}"))?;

    Ok(())
}
fn append_embedded_rom(doc: Document, html: &str, rom_data: Vec<u8>) -> Result<Element, JsValue> {
    let span = doc.create_element("span")?;
    span.set_inner_html(html);
    let button = doc
        .create_element("button")
        .map_err(|e| format!("cannot create button: {e:?}"))?;
    button.set_text_content(Some("Play"));
    let closure = Closure::wrap(Box::new(move |_e: Event| {
        wasm_bindgen_futures::spawn_local(run_noerr(
            rom_data.clone(), /* TODO: remove useless clone */
        ));
    }) as Box<dyn FnMut(_)>);
    button
        .add_event_listener_with_callback("click", closure.as_ref().unchecked_ref())
        .map_err(|e| format!("cannot attach callback to button: {e:?}"))?;
    closure.forget();
    span.append_child(&button)
        .map_err(|e| format!("couldn't insert play button in span: {e:?}"))?;
    Ok(span)
}
fn select_rom_btn(doc: Document) -> Result<Element, JsValue> {
    let button = doc
        .create_element("button")
        .map_err(|e| format!("cannot create button: {e:?}"))?;
    button.set_text_content(Some("Open File..."));
    let input = doc
        .create_element("input")
        .map_err(|e| format!("cannot create input: {e:?}"))?;
    let input_html: &HtmlInputElement = input.unchecked_ref();
    let input_html = input_html.clone();
    input_html.set_attribute("type", "file")?;
    input_html.set_attribute("accept", ".gg")?;
    let file_reader = FileReader::new().map_err(|e| format!("cannot create file reader: {e:?}"))?;
    let open_file_dialog = Closure::wrap(Box::new(move |_e: Event| {
        let input_html = input_html.clone();
        let input_html_clone = input_html.clone();
        let file_reader = file_reader.clone();
        let load_file = Closure::wrap(Box::new(move |_e: Event| {
            let file_reader_clone = file_reader.clone();
            let file_loaded = Closure::wrap(Box::new(move |_e: Event| {
                let array = file_reader_clone
                    .result()
                    .expect("cannot get file reader result");
                let u8array = Uint8Array::new(&array);
                wasm_bindgen_futures::spawn_local(run_noerr(u8array.to_vec()));
            }) as Box<dyn FnMut(_)>);
            let files = input_html_clone.files().expect("a file list");
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
    run(&data, &[]).await.unwrap();
}
pub(crate) fn web_init(window: Rc<Window>) -> Result<(), Box<dyn Error>> {
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
    let closure = wasm_bindgen::closure::Closure::wrap(Box::new(move |_e: Event| {
        let size = get_window_size();
        window.set_inner_size(size)
    }) as Box<dyn FnMut(_)>);
    client_window
        .add_event_listener_with_callback("resize", closure.as_ref().unchecked_ref())
        .map_err(|e| format!("cannot add resize event listener to window: {e:?}"))?;
    closure.forget();
    Ok(())
}
