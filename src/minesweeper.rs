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
		// Write game score info
		writer.move_cursor_to((0, 0));
		writer.write(format!("Flagged: {}/{}. ",self.board.mines_flagged, self.board.mine_count), ContentStyle { ..Default::default() });
		match self.game_state {
			GameState::GameOver => writer.write(format!("Game over!"), ContentStyle { foreground_color: Some(Color::Red), ..Default::default() }),
			GameState::GameWon => writer.write(format!("Game won!"), ContentStyle { foreground_color: Some(Color::Green), ..Default::default() }),
			_ => {}
		}
		// Draw board
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
		// Don't allow the board to be clicked on if we have won or lost
		if self.game_state != GameState::Normal {
			return Ok(());
		}
		match button {
			// Left click to clear a tile
			MouseButton::Left => {
				// Generate the board if it has not yet been generated
				if !self.board.is_generated {
					if !self.board.generate(pos_in_mouse_zone) {
						self.should_close = true;
						return Err("Unable to generate board".into());
					}
				}
				// Clear tile
				let result = self.board.clear_tile(pos_in_mouse_zone);
				self.should_redraw |= result.0;
				self.game_state = result.1;
			}
			// Right click to flag/unflag a tile
			MouseButton::Right => self.should_redraw |= self.board.flag_unflag_tile(pos_in_mouse_zone),
			// Middle click does nothing
			_ => {}
		}
		Ok(())
	}
}

impl Minesweeper {
	pub fn new() -> Result<Self, String> {
		// Get width
		print!("Width (blank for 20): ");
		let width = get_input_value_or_default(20).ok_or("Error: invalid width.")?;
		// Get height
		print!("Height (blank for a square board): ");
		let height = get_input_value_or_default(width).ok_or("Error: invalid height.")?;
		// Get mine count
		print!("Mine count (blank for 75): ");
		let mine_count = get_input_value_or_default(75).ok_or("Error: invalid mine count.")?;
		// Create game
		Ok(Self {
			board: Board {
				tiles: Array2D::filled_with(Tile::Uncleared { mined: false, flagged: false }, height, width),
				mine_count,
				is_generated: false,
				mines_flagged: 0,
				tiles_cleared: 0,
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

	/// Given a 3x3 grid centered on a clicked tile, gives the clue that the cleared tile should display.
	fn calculate(neighbors_with_mines: &[[bool; 3]; 3]) -> Self {
		let mine_count = neighbors_with_mines.iter().map(|row| row.iter().map(|is_mine| (*is_mine) as u8).sum::<u8>()).sum();
		if mine_count == 0 {
			return Self::None;
		}
		return Self::Number(mine_count);
	}
}

#[derive(Clone)]
enum Tile {
	Uncleared { mined: bool, flagged: bool },
	Cleared(Clue),
	Exploded,
}

impl Tile {
	/// Gives the char and style that should be displayed in the terminal cell that this tile is in.
	fn get_char_and_style(&self) -> (char, ContentStyle) {
		match self {
			Self::Uncleared { flagged, .. } => match flagged {
				false => (' ', ContentStyle { background_color: Some(Color::DarkGrey), ..Default::default() }),
				true => ('⚑', ContentStyle { background_color: Some(Color::DarkGrey), foreground_color: Some(Color::Red), ..Default::default() }),
			},
			Self::Cleared(clue) => {
				let (chr, color) = clue.get_char_and_color();
				(chr, ContentStyle { foreground_color: Some(color), background_color: Some(Color::Black), ..Default::default() })
			}
			Self::Exploded => ('●', ContentStyle { foreground_color: Some(Color::White), background_color: Some(Color::Red), ..Default::default() }),
		}
	}
}

#[derive(Clone)]
struct Board {
	tiles: Array2D<Tile>,
	mine_count: u32,
	mines_flagged: u32,
	tiles_cleared: u32,
	is_generated: bool,
}

impl Board {
	/// Gives the width and height of the board.
	fn size(&self) -> (u16, u16) {
		(self.tiles.num_columns() as u16, self.tiles.num_rows() as u16)
	}

	/// Gives the width and height of the board.
	fn tile_count(&self) -> u32 {
		self.tiles.num_columns() as u32 * self.tiles.num_rows() as u32
	}

	/// Get a immutable reference to a tile on the board.
	fn get_tile(&self, pos: (u16, u16)) -> &Tile {
		&self.tiles[(pos.0 as usize, pos.1 as usize)]
	}

	/// Get a mutable reference to a tile on the board.
	fn get_tile_mut(&mut self, pos: (u16, u16)) -> &mut Tile {
		&mut self.tiles[(pos.0 as usize, pos.1 as usize)]
	}

	/// Clears a tile on the board.
	/// Returns `(should_redraw, new_game_state)`.
	fn clear_tile(&mut self, pos: (u16, u16)) -> (bool, GameState) {
		// Get neighboring tiles have bombs
		let mut neighbors_that_have_mines = [[false; 3]; 3];
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
				neighbors_that_have_mines[y_offset as usize][x_offset as usize] = self.is_mine_in_tile((x, y));
			}
		}
		// Change tile
		let tile = self.get_tile_mut(pos);
		let mut should_redraw = false;
		let mut new_game_state = GameState::Normal;
		match tile {
			Tile::Uncleared { mined, flagged } => {
				// Don't clear a flagged tile
				if !*flagged {
					match mined {
						// If the tile has a mine, game over
						true => {
							*tile = Tile::Exploded;
							new_game_state = GameState::GameOver;
						}
						// Else clear the tile and calculate the clue that should appear in the tile
						false => {
							let clue = Clue::calculate(&neighbors_that_have_mines);
							*tile = Tile::Cleared(clue);
							if matches!(clue, Clue::None) {
								do_cascade = true;
							}
							self.tiles_cleared += 1;
						}
					}
					should_redraw = true;
				}
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
		// Check if game won
		if self.is_game_won() {
			new_game_state = GameState::GameWon
		}
		// Return
		(should_redraw, new_game_state)
	}

	/// Flags or unflags a tile, returns if the board should be redrawn.
	fn flag_unflag_tile(&mut self, pos: (u16, u16)) -> bool {
		let tile = self.get_tile_mut(pos);
		let mut should_redraw = false;
		match tile {
			Tile::Uncleared { flagged, .. } => {
				*flagged = !*flagged;
				should_redraw = true;
				match *flagged {
					true => self.mines_flagged += 1,
					false => self.mines_flagged -= 1,
				}
			}
			_ => {}
		}
		should_redraw
	}

	fn is_game_won(&self) -> bool {
		self.tiles_cleared == self.tile_count() - self.mine_count
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