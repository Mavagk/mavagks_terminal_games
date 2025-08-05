use std::io::{stdout, Write};

use array2d::Array2D;
use crossterm::{event::{Event, KeyCode, MouseButton}, style::{Color, ContentStyle}};
use rand::random_range;

use crate::{console::Console, game::Game, get_input_value_or_default};

pub struct Minesweeper {
	board: Board,
	should_redraw: bool,
	should_close: bool,
	game_state: GameState,
}

impl Game for Minesweeper {
	fn should_close(&self) -> bool {
		self.should_close
	}
	
	fn should_redraw(&self) -> bool {
		self.should_redraw
	}

	fn event(&mut self, event: &Event) -> Result<(), String> {
		match event {
			Event::Key(key_event) => {
				if key_event.is_press() && key_event.code == KeyCode::Esc {
					self.should_close = true;
				}
			}
			_ => {}
		}
		Ok(())
	}

	fn first_draw(&mut self, writer: &mut Console) -> Result<(), String> {
		// Setup screen size and game mouse zone
		let board_size = self.board.size();
		let mut screen_size = board_size;
		screen_size.0 += 1;
		screen_size.1 += 1;
		writer.new_game_screen(screen_size);
		writer.set_game_mouse_zone((1, 1), board_size);
		writer.enable_mouse_capture();
		writer.hide_cursor();
		// Draw
		self.draw(writer)?;
		Ok(())
	}

	fn draw(&mut self, writer: &mut Console) -> Result<(), String> {
		let board_size = self.board.size();
		writer.move_cursor_to((1, 1));
		for y in 0..board_size.1 {
			writer.move_cursor_to((1, y + 1));
			for x in 0..board_size.0 {
				let (chr, style) = self.board.get_tile((x, y)).get_char_and_style();
				writer.write(chr, style);
			}
		}
		self.should_redraw = false;
		Ok(())
	}

	fn mouse_click_in_game_mouse_zone(&mut self, pos_in_mouse_zone: (u16, u16), button: MouseButton, _event: &Event) -> Result<(), String> {
		if self.game_state == GameState::Normal {
			match button {
				MouseButton::Left => {
					if !self.board.is_generated {
						if !self.board.generate(pos_in_mouse_zone) {
							self.should_close = true;
							return Err("Unable to generate board".into());
						}
					}
					let result = self.board.clear_tile(pos_in_mouse_zone);
					self.should_redraw |= result.0;
					self.game_state = result.1;
				}
				MouseButton::Right => self.should_redraw |= self.board.flag_unflag_tile(pos_in_mouse_zone),
				_ => {}
			}
		}
		Ok(())
	}
}

impl Minesweeper {
	pub fn new() -> Result<Self, String> {
		// Get width
		print!("Width (blank for 20): ");
		stdout().flush().unwrap();
		let width = get_input_value_or_default(20).ok_or("Error: invalid width.")?;
		// Get height
		print!("Height (blank for a square board): ");
		stdout().flush().unwrap();
		let height = get_input_value_or_default(width).ok_or("Error: invalid height.")?;
		// Get mine count
		print!("Mine count (blank for 75): ");
		stdout().flush().unwrap();
		let mine_count = get_input_value_or_default(75).ok_or("Error: invalid mine count.")?;
		// Create game
		Ok(Self {
			board: Board {
				tiles: Array2D::filled_with(Tile::Uncleared { mined: false, flagged: false }, height, width),
				mine_count,
				is_generated: false
			},
			should_close: false,
			should_redraw: false,
			game_state: GameState::Normal,
		})
	}
}

#[derive(Clone, Copy)]
enum Clue {
	None,
	Number(u8),
}

impl Clue {
	fn get_char_and_color(self) -> (char, Color) {
		match self {
			Self::None => (' ', Color::Black),
			Self::Number(number) => (('0' as u8 + number) as char, Color::Grey),
		}
	}

	fn calculate(neighbors: &[[bool; 3]; 3]) -> Self {
		let mines_in_neighbors = neighbors.iter().map(|row| row.iter().map(|is_mine| (*is_mine) as u8).sum::<u8>()).sum();
		if mines_in_neighbors == 0 {
			return Self::None;
		}
		return Self::Number(mines_in_neighbors);
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
			Self::Uncleared { flagged, .. } => match flagged {
				false => (' ', ContentStyle { background_color: Some(Color::DarkGrey), ..Default::default() }),
				true => ('F', ContentStyle { background_color: Some(Color::DarkGrey), foreground_color: Some(Color::Red), ..Default::default() }),
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

	fn get_tile_mut(&mut self, pos: (u16, u16)) -> &mut Tile {
		&mut self.tiles[(pos.0 as usize, pos.1 as usize)]
	}

	/// Returns `(should_redraw, new_game_state)`.
	fn clear_tile(&mut self, pos: (u16, u16)) -> (bool, GameState) {
		// Get neighboring tiles have bombs
		let mut neighbors_that_have_bombs = [[false; 3]; 3];
		let mut do_cascade = false;
		for y_offset in 0..=2 {
			let y = match (pos.1 + y_offset).checked_sub(1) {
				Some(y) => y,
				None => continue,
			};
			if y >= self.size().1 {
				continue;
			}
			for x_offset in 0..=2 {
				let x = match (pos.0 + x_offset).checked_sub(1) {
					Some(x) => x,
					None => continue,
				};
				if x >= self.size().0 {
					continue;
				}
				neighbors_that_have_bombs[y_offset as usize][x_offset as usize] = self.is_mine_in_tile((x, y));
			}
		}
		// Change tile
		let tile = self.get_tile_mut(pos);
		let mut should_redraw = false;
		let mut new_game_state = GameState::Normal;
		match tile {
			Tile::Uncleared { mined, .. } => {
				match mined {
					true => {
						*tile = Tile::Exploded;
						new_game_state = GameState::GameOver;
					}
					false => {
						let clue = Clue::calculate(&neighbors_that_have_bombs);
						*tile = Tile::Cleared(clue);
						if matches!(clue, Clue::None) {
							do_cascade = true;
						}
					}
				}
				should_redraw = true;
			}
			_ => {}
		}
		// Cascade
		if do_cascade {
			for y in pos.1.saturating_sub(1)..(pos.1 + 2).min(self.size().1) {
				for x in pos.0.saturating_sub(1)..(pos.0 + 2).min(self.size().0) {
					self.clear_tile((x, y));
				}
			}
		}
		// Return
		(should_redraw, new_game_state)
	}

	/// Returns if the board should be redrawn
	fn flag_unflag_tile(&mut self, pos: (u16, u16)) -> bool {
		let tile = self.get_tile_mut(pos);
		let mut should_redraw = false;
		match tile {
			Tile::Uncleared { flagged, .. } => {
				*flagged = !*flagged;
				should_redraw = true;
			}
			_ => {}
		}
		should_redraw
	}

	fn is_mine_in_tile(&self, pos: (u16, u16)) -> bool {
		match self.get_tile(pos) {
			Tile::Cleared(..) => false,
			Tile::Exploded => true,
			Tile::Uncleared { mined, .. } => *mined,
		}
	}

	/// Returns `false` if generation failed.
	fn generate(&mut self, safe_tile: (u16, u16)) -> bool {
		self.is_generated = true;
		let board_size = self.size();
		let mut was_successful = false;
		let mut mines_placed = 0;
		for _ in 0..self.mine_count.saturating_mul(1000) {
			let pos = (random_range(0..board_size.0), random_range(0..board_size.1));
			if (pos.0.abs_diff(safe_tile.0) < 2 && pos.1.abs_diff(safe_tile.1) < 2) || self.is_mine_in_tile(pos) {
				continue;
			}
			match self.get_tile_mut(pos) {
				Tile::Uncleared { mined, .. } => *mined = true,
				_ => unreachable!(),
			}
			mines_placed += 1;
			if mines_placed == self.mine_count {
				was_successful = true;
				break;
			}
		}
		was_successful
	}
}

#[derive(PartialEq, Eq)]
enum GameState {
	Normal,
	GameOver,
	GameWon,
}