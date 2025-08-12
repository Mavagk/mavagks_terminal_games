use crossterm::event::{Event, KeyCode};

use crate::game::game_trait::Game;

pub struct Paint {
	should_redraw: bool,
	should_close: bool,
}

impl Paint {
	pub fn new() -> Result<Self, String> {
		Ok(Self {
			should_close: false,
			should_redraw: false,
		})
	}
}

impl Game for Paint {
	fn should_close(&self) -> bool {
		self.should_close
	}

	fn should_redraw(&self) -> bool {
		self.should_redraw
	}

	fn keypress(&mut self, key: KeyCode, _event: &Event) -> Result<(), String> {
		match key {
			KeyCode::Esc => self.should_close = true,
			_ => {}
		}
		Ok(())
	}
}