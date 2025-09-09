use std::{io::{self, Write}, time::Duration};

use crossterm::{cursor::MoveToColumn, event::{self, Event, KeyCode, KeyModifiers}, execute};

use crate::{console::{enable_raw_mode_on_unix, Console}, Game};

pub struct LogEventsTest {
	should_close: bool,
	time_sum: Duration,
}

impl Game for LogEventsTest {
	fn first_draw(&mut self, writer: &mut Console) -> Result<(), String> {
		enable_raw_mode_on_unix();
		writer.enable_mouse_capture();
		Ok(())
	}

	fn keypress(&mut self, key: KeyCode, _modifiers: KeyModifiers, _event: &Event) -> Result<(), String> {
		if key == KeyCode::Esc {
			self.should_close = true;
		}
		Ok(())
	}

	fn event(&mut self, event: &event::Event) -> Result<(), String> {
		println!("{event:?}");
		io::stdout().flush().unwrap();
		execute!(io::stdout(), MoveToColumn(0)).unwrap();
		Ok(())
	}

	fn should_close(&self) -> bool {
		self.should_close
	}

	fn tick_frequency(&self) -> Option<Duration> {
		Some(Duration::from_secs(1))
	}

	fn tick(&mut self, since_last_tick: Option<Duration>) -> Result<(), String> {
		if let Some(since_last_tick) = since_last_tick {
			self.time_sum += since_last_tick;
		}
		println!("{since_last_tick:?}, Sum: {:?}", self.time_sum);
		io::stdout().flush().unwrap();
		execute!(io::stdout(), MoveToColumn(0)).unwrap();
		Ok(())
	}
}

impl LogEventsTest {
	pub fn new() -> Self {
		Self {
			should_close: false,
			time_sum: Duration::from_nanos(0),
		}
	}
}