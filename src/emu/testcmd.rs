use crate::emu::{self, Button};

use crate::emu::testcmd::TestCommand::*;

#[derive(Debug, Clone)]
pub enum TestCommand {
    WaitFrames(u32),
    PressButton(emu::Button),
    ReleaseButton(emu::Button),
}
impl TestCommand {
    pub fn slice_str(s: &[TestCommand]) -> String {
        s.iter()
            .cloned()
            .map(String::from)
            .collect::<Vec<_>>()
            .join("-")
    }
    pub fn new_vec(s: &str) -> Result<Vec<Self>, String> {
        if s.is_empty() {
            return Ok(Vec::new());
        }
        s.split('-').map(|c| c.try_into()).collect()
    }
}
impl TryFrom<&str> for TestCommand {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if let Some(c) = value.chars().next() {
            return Ok(match c {
                '0'..='9' => WaitFrames(value.parse().map_err(|e| format!("not int: {e}"))?),
                'S' => PressButton(Button::Start),
                's' => ReleaseButton(Button::Start),
                'A' => PressButton(Button::One),
                'a' => ReleaseButton(Button::One),
                'B' => PressButton(Button::Two),
                'b' => ReleaseButton(Button::Two),
                'U' => PressButton(Button::Up),
                'u' => ReleaseButton(Button::Up),
                'D' => PressButton(Button::Down),
                'd' => ReleaseButton(Button::Down),
                'L' => PressButton(Button::Left),
                'l' => ReleaseButton(Button::Left),
                'R' => PressButton(Button::Right),
                'r' => ReleaseButton(Button::Right),
                _ => return Err(format!("Unexpected char {c}")),
            });
        }
        Err("empty value".to_string())
    }
}
impl From<TestCommand> for String {
    fn from(value: TestCommand) -> String {
        match value {
            WaitFrames(f) => format!("{f}"),
            PressButton(b) => match b {
                Button::Start => "S",
                Button::One => "A",
                Button::Two => "B",
                Button::Up => "U",
                Button::Down => "D",
                Button::Left => "L",
                Button::Right => "R",
            }
            .to_string(),
            ReleaseButton(b) => match b {
                Button::Start => "s",
                Button::One => "a",
                Button::Two => "b",
                Button::Up => "u",
                Button::Down => "d",
                Button::Left => "l",
                Button::Right => "r",
            }
            .to_string(),
        }
    }
}
