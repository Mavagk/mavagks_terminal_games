use std::{fmt::Display, io::{stdout, Stdout}};

use crossterm::{cursor::MoveTo, execute, style::{ContentStyle, PrintStyledContent, StyledContent}, terminal::{Clear, ClearType, EnterAlternateScreen, LeaveAlternateScreen, SetSize}};

pub struct ConsoleWriter {
	stdout: Stdout,
	is_in_alternate_screen: bool,
}

impl ConsoleWriter {
	pub fn new() -> Self {
		Self {
			stdout: stdout(),
			is_in_alternate_screen: false,
		}
	}

	pub fn new_game_screen(&mut self, width: u16, height: u16) {
		execute!(self.stdout, EnterAlternateScreen, SetSize(width, height), Clear(ClearType::Purge)).unwrap();
		self.is_in_alternate_screen = true;
	}

	pub fn exit_game_screen(&mut self) {
		execute!(self.stdout, LeaveAlternateScreen).unwrap();
	}

	pub fn move_cursor_to(&mut self, to: (u16, u16)) {
		execute!(self.stdout, MoveTo(to.0, to.1)).unwrap();
	}

	pub fn write<T>(&mut self, to_write: T, style: ContentStyle) where T: Display {
		execute!(self.stdout, PrintStyledContent(StyledContent::new(style, to_write))).unwrap();
	}

	pub fn write_at<T>(&mut self, to_write: T, style: ContentStyle, at: (u16, u16)) where T: Display {
		execute!(self.stdout, MoveTo(at.0, at.1), PrintStyledContent(StyledContent::new(style, to_write))).unwrap();
	}
}