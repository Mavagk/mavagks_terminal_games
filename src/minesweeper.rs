use std::io::{stdin, stdout, Write};

use array2d::Array2D;
use crossterm::{event::{Event, KeyCode}, style::{Color, ContentStyle}};

use crate::{console::Console, game::Game};

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

	fn first_draw(&mut self, writer: &mut Console) {
		// Setup screen size and game mouse zone
		let board_size = self.board.size();
		let mut screen_size = board_size;
		screen_size.0 += 1;
		screen_size.1 += 1;
		writer.new_game_screen(screen_size);
		writer.set_game_mouse_zone((1, 1), board_size);
		// Draw
		self.draw(writer);
	}

	fn draw(&mut self, writer: &mut Console) {
		let board_size = self.board.size();
		writer.move_cursor_to((1, 1));
		for y in 0..board_size.1 {
			writer.move_cursor_to((1, y));
			for x in 0..board_size.0 {
				let (chr, style) = self.board.get_tile((x, y)).get_char_and_style();
				writer.write(chr, style);
			}
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
			board: Board {
				tiles: Array2D::filled_with(Tile::Uncleared { mined: false, flagged: false }, height, width),
				mine_count,
				is_generated: false
			},
			should_close: false,
			should_redraw: false,
		})
	}
}

#[derive(Clone, Copy)]
enum Clue {
	None,
	Number(u8),
}

impl Clue {
	pub fn get_char_and_color(self) -> (char, Color) {
		match self {
			Self::None => (' ', Color::Black),
			Self::Number(number) => (('0' as u8 + number) as char, Color::Grey),
		}
	}
}

#[derive(Clone)]
enum Tile {
	Uncleared { mined: bool, flagged: bool },
	Cleared(Clue),
	Exploded,
}

impl Tile {
	fn get_char_and_style(&self) -> (char, ContentStyle) {
		match self {
			Self::Uncleared { mined, flagged } => match flagged {
				false => (' ', ContentStyle { background_color: Some(Color::DarkGrey), ..Default::default() }),
				true => ('F', ContentStyle { background_color: Some(Color::Red), ..Default::default() }),
			},
			Self::Cleared(clue) => {
				let (chr, color) = clue.get_char_and_color();
				(chr, ContentStyle { foreground_color: Some(color), background_color: Some(Color::Black), ..Default::default() })
			}
			Self::Exploded => ('B', ContentStyle { foreground_color: Some(Color::White), background_color: Some(Color::Red), ..Default::default() }),
		}
	}
}

#[derive(Clone)]
struct Board {
	tiles: Array2D<Tile>,
	mine_count: usize,
	is_generated: bool,
}

impl Board {
	fn size(&self) -> (u16, u16) {
		(self.tiles.num_columns() as u16, self.tiles.num_rows() as u16)
	}

	fn get_tile(&self, pos: (u16, u16)) -> &Tile {
		&self.tiles[(pos.0 as usize, pos.1 as usize)]
	}
}