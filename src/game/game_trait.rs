use std::time::Duration;

use crossterm::event::{Event, KeyCode, KeyModifiers, MouseButton};

use crate::console::Console;

pub trait Game {
	/// Called after an event is handled if `should_redraw()` returns `true`.
	fn draw(&mut self, _writer: &mut Console) -> Result<(), String> { Ok(()) }
	/// Called once when the game starts
	fn first_draw(&mut self, _writer: &mut Console) -> Result<(), String> { Ok(()) }
	fn event(&mut self, _event: &Event) -> Result<(), String> { Ok(()) }
	fn mouse_moved_in_game_mouse_zone(&mut self, _pos_in_mouse_zone: Option<(u16, u16)>, _event: &Event) -> Result<(), String> { Ok(()) }
	fn mouse_start_click_in_game_mouse_zone(&mut self, _pos_in_mouse_zone: (u16, u16), _button: MouseButton, _event: &Event) -> Result<(), String> { Ok(()) }
	fn mouse_drag_in_game_mouse_zone(&mut self, _pos_in_mouse_zone: (u16, u16), _button: MouseButton, _event: &Event) -> Result<(), String> { Ok(()) }
	fn keypress(&mut self, _key: KeyCode, _modifiers: KeyModifiers, _event: &Event) -> Result<(), String> { Ok(()) }
	/// Called after an event is handled. If this returns true, the game will terminate, otherwise it won't.
	fn should_close(&self) -> bool {
		false
	}
	/// Called after an event is handled. If this returns true, `draw()` will be called, otherwise it won't.
	fn should_redraw(&self) -> bool {
		false
	}
	fn tick_frequency(&self) -> Option<Duration> { None }
	fn tick(&mut self, _since_last_tick: Option<Duration>) -> Result<(), String> { Ok(()) }
	fn before_poll(&mut self) -> Result<(), String> { Ok(()) }
}