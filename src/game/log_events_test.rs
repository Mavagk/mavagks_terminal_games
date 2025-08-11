use crossterm::event::{self, Event, KeyCode};

use crate::{console::Console, Game};

pub struct LogEventsTest {
	should_close: bool,
}

impl Game for LogEventsTest {
	fn first_draw(&mut self, writer: &mut Console) -> Result<(), String> {
		writer.enable_mouse_capture();
		Ok(())
	}

	fn event(&mut self, event: &event::Event) -> Result<(), String> {
		if let Event::Key(key_event) = event {
			if key_event.code == KeyCode::Esc && key_event.is_release() {
				self.should_close = true;
			}
		}
		println!("{event:?}");
		Ok(())
	}

	fn should_close(&self) -> bool {
		self.should_close
	}
}

impl LogEventsTest {
	pub fn new() -> Self {
		Self {
			should_close: false,
		}
	}
}