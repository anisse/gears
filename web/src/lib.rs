use gears_ui_core::web;
use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::JsValue;

#[wasm_bindgen(start)]
pub(crate) fn web_main() -> Result<(), JsValue> {
    web::web_main()
}
