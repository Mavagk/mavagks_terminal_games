use crossterm::event::{Event, MouseButton};

use crate::console::Console;

pub trait Game {
	/// Called after an event is handled if `should_redraw()` returns `true`.
	fn draw(&mut self, _writer: &mut Console) -> Result<(), String> { Ok(()) }
	/// Called once when the game starts
	fn first_draw(&mut self, _writer: &mut Console) -> Result<(), String> { Ok(()) }
	fn event(&mut self, _event: &Event) -> Result<(), String> { Ok(()) }
	fn mouse_moved_in_game_mouse_zone(&mut self, _pos_in_mouse_zone: Option<(u16, u16)>, _event: &Event) -> Result<(), String> { Ok(()) }
	fn mouse_click_in_game_mouse_zone(&mut self, _pos_in_mouse_zone: (u16, u16), _button: MouseButton, _event: &Event) -> Result<(), String> { Ok(()) }
	/// Called after an event is handled. If this returns true, the game will terminate, otherwise it won't.
	fn should_close(&self) -> bool {
		false
	}
	/// Called after an event is handled. If this returns true, `draw()` will be called, otherwise it won't.
	fn should_redraw(&self) -> bool {
		false
	}
}