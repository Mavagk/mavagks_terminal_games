use std::{fmt::Display, io::{stdout, Stdout}};

use crossterm::{cursor::{position, Hide, MoveTo, Show}, event::{DisableMouseCapture, EnableMouseCapture}, execute, style::{ContentStyle, PrintStyledContent, StyledContent}, terminal::{size, Clear, ClearType, EnterAlternateScreen, LeaveAlternateScreen}};

pub struct Console {
	stdout: Stdout,
	is_in_alternate_screen: bool,
	has_mouse_capture: bool,
	main_screen_size: (u16, u16),
	main_screen_cursor_pos: (u16, u16),
	game_mouse_zone_top_left: (u16, u16),
	game_mouse_zone_size: (u16, u16),
}

#[cfg(target_os = "windows")]
pub fn enable_raw_mode_on_unix() {
	
}

#[cfg(unix)]
pub fn enable_raw_mode_on_unix() {
	use crossterm::terminal::enable_raw_mode;
	enable_raw_mode().unwrap();
}

#[cfg(target_os = "windows")]
pub fn disable_raw_mode_on_unix() {
	
}

#[cfg(unix)]
pub fn disable_raw_mode_on_unix() {
	use crossterm::terminal::disable_raw_mode;
	disable_raw_mode().unwrap();
}

impl Console {
	pub fn new() -> Self {
		Self {
			stdout: stdout(),
			is_in_alternate_screen: false,
			main_screen_cursor_pos: (0, 0),
			main_screen_size: (0, 0),
			game_mouse_zone_size: (0, 0),
			game_mouse_zone_top_left: (0, 0),
			has_mouse_capture: false,
		}
	}

	pub fn new_game_screen(&mut self, size: (u16, u16)) {
		enable_raw_mode_on_unix();
		if !self.is_in_alternate_screen {
			self.main_screen_size = self.get_size();
			self.main_screen_cursor_pos = self.get_cursor_pos();
		}
		execute!(self.stdout, EnterAlternateScreen).unwrap();
		self.set_size(size);
		self.clear();
		self.is_in_alternate_screen = true;
	}

	pub fn clear(&mut self) {
		match self.is_in_alternate_screen {
			false => execute!(self.stdout, Clear(ClearType::Purge)).unwrap(),
			true => execute!(self.stdout, EnterAlternateScreen).unwrap(),
		}
	}

	pub fn exit_game_screen(&mut self) {
		if self.is_in_alternate_screen {
			execute!(self.stdout, LeaveAlternateScreen).unwrap();
			self.show_cursor();
			self.set_size(self.main_screen_size);
			self.move_cursor_to(self.main_screen_cursor_pos);
		}
		self.is_in_alternate_screen = false;
		disable_raw_mode_on_unix();
	}

	pub fn on_game_close(&mut self) {
		self.exit_game_screen();
		self.disable_mouse_capture();
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

	pub fn enable_mouse_capture(&mut self) {
		execute!(self.stdout, EnableMouseCapture).unwrap();
		self.has_mouse_capture = true;
	}

	pub fn disable_mouse_capture(&mut self) {
		if self.has_mouse_capture {
			execute!(self.stdout, DisableMouseCapture).unwrap();
		}
		self.has_mouse_capture = false;
	}

	pub fn show_cursor(&mut self) {
		execute!(self.stdout, Show).unwrap();
	}

	pub fn hide_cursor(&mut self) {
		execute!(self.stdout, Hide).unwrap();
	}

	pub fn get_cursor_pos(&self) -> (u16, u16) {
		position().unwrap()
	}

	pub fn get_size(&self) -> (u16, u16) {
		size().unwrap()
	}

	pub fn set_size(&mut self, size: (u16, u16)) {
		//execute!(self.stdout, SetSize(size.0, size.1)).unwrap();
		self.game_mouse_zone_top_left = (0, 0);
		self.game_mouse_zone_size = size;
	}

	pub fn set_game_mouse_zone(&mut self, game_mouse_zone_top_left: (u16, u16), game_mouse_zone_size: (u16, u16)) {
		self.game_mouse_zone_top_left = game_mouse_zone_top_left;
		self.game_mouse_zone_size = game_mouse_zone_size;
	}

	pub fn get_game_mouse_zone(&mut self) -> ((u16, u16), (u16, u16)) {
		(self.game_mouse_zone_top_left, self.game_mouse_zone_size)
	}

	pub fn mouse_zone_pos_to_screen_pos(&self, game_mouse_zone_pos: (u16, u16)) -> (u16, u16) {
		(game_mouse_zone_pos.0 + self.game_mouse_zone_top_left.0, game_mouse_zone_pos.1 + self.game_mouse_zone_top_left.1)
	}
}