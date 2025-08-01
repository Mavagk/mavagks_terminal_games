use std::io::Stdout;

use crossterm::event::{Event, MouseButton};

pub trait Game {
	fn draw(&mut self, _writer: &mut Stdout);
	fn event(&mut self, _event: &Event);
	fn click(&mut self, _pos: (u16, u16), _button: MouseButton);
}