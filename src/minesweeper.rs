use std::io::{stdin, stdout, Write};

use array2d::Array2D;
use crossterm::event::{Event, KeyCode};

use crate::game::Game;

pub struct Minesweeper {
	board: Board,
	should_redraw: bool,
	should_close: bool,
}

impl Game for Minesweeper {
	fn should_close(&self) -> bool {
		self.should_close
	}
	
	fn should_redraw(&self) -> bool {
		self.should_redraw
	}

	fn event(&mut self, event: &Event) {
		match event {
			Event::Key(key_event) => {
				if key_event.is_press() && key_event.code == KeyCode::Esc {
					self.should_close = true;
				}
			}
			_ => {}
		}
	}
}

impl Minesweeper {
	pub fn new() -> Option<Self> {
		// Get width
		print!("Width (blank for 20): ");
		stdout().flush().unwrap();
		let mut text_entered = String::new();
		stdin().read_line(&mut text_entered).unwrap();
		let text_entered = text_entered.trim_end();
		let width = match text_entered {
			"" => 20,
			_ => match text_entered.parse() {
				Ok(width) => width,
				Err(_) => {
					println!("Error: invalid number.");
					return None;
				}
			}
		};
		// Get height
		print!("Height (blank for a square board): ");
		stdout().flush().unwrap();
		let mut text_entered = String::new();
		stdin().read_line(&mut text_entered).unwrap();
		let text_entered = text_entered.trim_end();
		let height = match text_entered {
			"" => width,
			_ => match text_entered.parse() {
				Ok(height) => height,
				Err(_) => {
					println!("Error: invalid number.");
					return None;
				}
			}
		};
		// Get mine count
		print!("Mine count (blank for 20): ");
		stdout().flush().unwrap();
		let mut text_entered = String::new();
		stdin().read_line(&mut text_entered).unwrap();
		let text_entered = text_entered.trim_end();
		let mine_count = match text_entered {
			"" => 20,
			_ => match text_entered.parse() {
				Ok(mine_count) => mine_count,
				Err(_) => {
					println!("Error: invalid number.");
					return None;
				}
			}
		};
		// Create game
		Some(Self {
			board: Board::Ungenerated { size: (width, height), mine_count },
			should_close: false,
			should_redraw: false,
		})
	}
}

pub enum Clue {
	None,
	Number(u8),
}

pub enum Tile {
	Uncleared { mined: bool, flagged: bool },
	Cleared(Clue),
	Exploded,
}

pub enum Board {
	Generated(Array2D<Tile>),
	Ungenerated { size: (u16, u16), mine_count: usize },
}