use crossterm::event::{Event, MouseButton};

use crate::console_writer::ConsoleWriter;

pub trait Game {
	fn draw(&mut self, _writer: &mut ConsoleWriter) {}
	fn first_draw(&mut self, _writer: &mut ConsoleWriter) {}
	fn event(&mut self, _event: &Event) {}
	fn click(&mut self, _pos: (u16, u16), _button: MouseButton) {}
	fn should_close(&self) -> bool {
		false
	}
	fn should_redraw(&self) -> bool {
		false
	}
}