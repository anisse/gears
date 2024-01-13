pub mod core;
#[cfg(target_arch = "wasm32")]
pub mod web;
#[cfg(not(target_arch = "wasm32"))]
pub mod web {
    use wasm_bindgen::JsValue;
    pub fn web_main() -> Result<(), JsValue> {
        Ok(())
    }
}
