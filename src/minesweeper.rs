use std::iter::once;

use array2d::Array2D;
use crossterm::{event::{Event, KeyCode, MouseButton}, style::{Color, ContentStyle}};
use rand::random_range;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::{console::Console, game::Game, get_input, get_input_value_or_default};

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
				if key_event.is_press() {
					match key_event.code {
						KeyCode::Esc => self.should_close = true,
						KeyCode::Char('r') => *self = Self {
							board: Board {
								tiles: Array2D::filled_with(Tile::Uncleared { mined: false, flagged: false }, self.board.tiles.num_rows(), self.board.tiles.num_columns()),
								mine_count: self.board.mine_count,
								ensured_solvable_difficulty: self.board.ensured_solvable_difficulty,
								is_generated: false,
								mines_flagged: 0,
								tiles_cleared: 0,
							},
							should_close: false,
							should_redraw: true,
							game_state: GameState::Normal,
						},
						KeyCode::Char('e' | 's') => {
							self.single_solve_step(Difficulty::Easy);
						}
						_ => {}
					}
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
			_ => writer.write(format!("                    "), ContentStyle { ..Default::default() }),
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
				if !self.board.is_generated {
					// Generate board
					let mut was_successful = false;
					for _ in 0..100 {
						if !self.board.generate(pos_in_mouse_zone) {
							return Err("Unable to generate board".into());
						}
						// Clear tile
						self.board.clear_tile_and_cascade(pos_in_mouse_zone);
						if self.board.is_solvable() {
							was_successful = true;
							break;
						}
						self.board.reset();
					}
					if !was_successful {
						return Err("Unable to generate board".into());
					}
					self.should_redraw = true;
					if self.board.is_game_won() {
						self.game_state = GameState::GameWon;
					}
					if self.game_state == GameState::GameWon {
						self.board.flag_all_uncleared();
					}
				}
				else {
					// Clear tile
					let result = self.board.clear_tile_and_cascade(pos_in_mouse_zone);
					self.should_redraw |= result.0;
					self.game_state = result.1;
					if self.game_state == GameState::GameWon {
						self.board.flag_all_uncleared();
					}
					if self.game_state == GameState::GameOver {
						self.board.explode_all_mines();
					}
				}
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
		print!("Width (blank for 40): ");
		let width = get_input_value_or_default(40).ok_or("Invalid width.")?;
		if width < 4 {
			return Err("Width too small".into());
		}
		if width > 10000 {
			return Err("Width too large".into());
		}
		// Get height
		print!("Height (blank for half width): ");
		let height = get_input_value_or_default((width / 2).max(4)).ok_or("Invalid height.")?;
		if height < 4 {
			return Err("height too small".into());
		}
		if height > 10000 {
			return Err("Height too large".into());
		}
		// Get mine count
		print!("Mine count (blank for 75): ");
		let mine_count = get_input_value_or_default(75).ok_or("Invalid mine count.")?;
		if mine_count > width as u32 * height as u32 - 9 {
			return Err("Too many mines".into());
		}
		// Difficulty
		print!("Ensure solvable using techniques (easy, unchecked, e, u) (blank for easy): ");
		let ensured_solvable_difficulty_text = get_input();
		let ensured_solvable_difficulty = match &*ensured_solvable_difficulty_text {
			"" | "e" | "easy" => Some(Difficulty::Easy),
			"u" | "unchecked" => None,
			_ => return Err("Invalid input".into()),
		};
		// Create game
		Ok(Self {
			board: Board {
				tiles: Array2D::filled_with(Tile::Uncleared { mined: false, flagged: false }, height, width),
				mine_count,
				is_generated: false,
				mines_flagged: 0,
				tiles_cleared: 0,
				ensured_solvable_difficulty,
			},
			should_close: false,
			should_redraw: false,
			game_state: GameState::Normal,
		})
	}

	fn single_solve_step(&mut self, difficulty: Difficulty) {
		let was_tile_solved = self.board.single_solve_step(difficulty);
		if was_tile_solved {
			self.should_redraw = true;
			if self.board.is_game_won() {
				self.game_state = GameState::GameWon;
				self.board.flag_all_uncleared();
			}
		}
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
			Self::Number(number) => (('0' as u8 + number) as char, match number {
				1 => Color::Blue,
				2 => Color::Green,
				3 => Color::Red,
				4 => Color::DarkBlue,
				5 => Color::DarkRed,
				6 => Color::DarkCyan,
				7 => Color::DarkGrey,
				8 => Color::Grey,
				_ => unreachable!()
			}),
		}
	}

	/// Given a board and pos, gives the clue that the cleared tile should display.
	fn calculate(board: &Board, pos: (u16, u16)) -> Self {
		let mine_count = OffsetDirection::iter()
			.map(|offset| board.get_offset_tile(pos, offset))
			.filter_map(|tile| tile)
			.map(|tile| matches!(tile, Tile::Uncleared { mined: true, .. }) as u8)
			.sum();
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
				false => ('•', ContentStyle { background_color: Some(Color::DarkGrey), foreground_color: Some(Color::Black), ..Default::default() }),
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
	ensured_solvable_difficulty: Option<Difficulty>,
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
		&self.tiles[(pos.1 as usize, pos.0 as usize)]
	}

	fn get_offset_tile(&self, pos: (u16, u16), offset: OffsetDirection) -> Option<&Tile> {
		let adjusted_pos = offset.adjust_to_offset(pos)?;
		self.tiles.get(adjusted_pos.1 as usize, adjusted_pos.0 as usize)
	}

	fn get_offset_tile_mut(&mut self, pos: (u16, u16), offset: OffsetDirection) -> Option<&mut Tile> {
		let adjusted_pos = offset.adjust_to_offset(pos)?;
		self.tiles.get_mut(adjusted_pos.1 as usize, adjusted_pos.0 as usize)
	}

	/// Get a mutable reference to a tile on the board.
	fn get_tile_mut(&mut self, pos: (u16, u16)) -> &mut Tile {
		&mut self.tiles[(pos.1 as usize, pos.0 as usize)]
	}

	/// Clears a tile on the board.
	/// Returns `(should_redraw, new_game_state)`.
	fn clear_tile_and_cascade(&mut self, pos: (u16, u16)) -> (bool, GameState) {
		let mut to_clear: Vec<(u16, u16)> = once(pos).collect();
		let mut out = None;
		while !to_clear.is_empty() {
			let element_to_clear_pos = to_clear.pop().unwrap();
			let result = self.clear_tile(element_to_clear_pos, &mut to_clear);
			if out.is_none() {
				out = Some(result)
			}
			if result.1 == GameState::GameWon {
				out = Some((true, GameState::GameWon));
			}
		}
		out.unwrap()
	}

	/// Clears a tile on the board.
	/// Returns `(should_redraw, new_game_state)`.
	fn clear_tile(&mut self, pos: (u16, u16), cascade_to: &mut Vec<(u16, u16)>) -> (bool, GameState) {
		// Get neighboring tiles have bombs
		let mut do_cascade = false;
		// Change tile
		let tile = self.get_tile_mut(pos);
		let mut should_redraw = false;
		let mut new_game_state = GameState::Normal;
		match tile {
			Tile::Uncleared { mined, flagged: false } => {
				match mined {
					// If the tile has a mine, game over
					true => {
						*tile = Tile::Exploded;
						new_game_state = GameState::GameOver;
					}
					// Else clear the tile and calculate the clue that should appear in the tile
					false => {
						let clue = Clue::calculate(self, pos);
						*self.get_tile_mut(pos) = Tile::Cleared(clue);
						if matches!(clue, Clue::None) {
							do_cascade = true;
						}
						self.tiles_cleared += 1;
					}
				}
				should_redraw = true;
			}
			_ => {}
		}
		// Cascade
		if do_cascade {
			cascade_to.extend(OffsetDirection::iter().map(|direction| direction.adjust_to_offset_in_board(pos, self)).filter_map(|pos| pos));
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

	/// Generates the board, returns `false` if generation failed.
	fn generate(&mut self, safe_tile: (u16, u16)) -> bool {
		self.is_generated = true;
		if self.mine_count == 0 {
			return true;
		}
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

	fn reset(&mut self) {
		self.is_generated = false;
		self.mines_flagged = 0;
		self.tiles_cleared = 0;
		for index in self.tiles.indices_column_major() {
			self.tiles[index] = Tile::Uncleared { mined: false, flagged: false };
		}
	}

	fn is_solvable(&self) -> bool {
		let ensured_solvable_difficulty = match self.ensured_solvable_difficulty {
			Some(ensured_solvable_difficulty) => ensured_solvable_difficulty,
			None => return true,
		};
		let mut test_board = self.clone();
		loop {
			let tile_solved = test_board.single_solve_step(ensured_solvable_difficulty);
			if !tile_solved {
				return test_board.is_game_won();
			}
		}
	}

	fn flag_all_uncleared(&mut self) {
		for y in 0..self.tiles.column_len() {
			for x in 0..self.tiles.row_len() {
				let tile = self.get_tile_mut((x as u16, y as u16));
				if let Tile::Uncleared { flagged, .. } = tile {
					*flagged = true;
				}
			}
		}
		self.mines_flagged = self.mine_count;
	}

	fn explode_all_mines(&mut self) {
		for y in 0..self.tiles.column_len() {
			for x in 0..self.tiles.row_len() {
				let tile = self.get_tile_mut((x as u16, y as u16));
				let mut was_unflagged = false;
				if let Tile::Uncleared { mined: true, flagged } = tile {
					if *flagged {
						was_unflagged = true;
					}
					*tile = Tile::Exploded;
				}
				if was_unflagged {
					self.mines_flagged -= 1;
				}
			}
		}
	}

	fn single_solve_step(&mut self, _difficulty: Difficulty) -> bool {
		for y in 0..self.tiles.column_len() {
			for x in 0..self.tiles.row_len() {
				let pos = (x as u16, y as u16);
				let tile = self.get_tile(pos);
				// Skip tiles that are not cleared
				let clue = match tile {
					Tile::Cleared(clue) => *clue,
					_ => continue,
				};
				// Count neighboring flags and cleared tiles
				let mut neighboring_flags = 0;
				let mut neighboring_uncleared_tiles = 0;
				for neighbor_tile in OffsetDirection::iter()
					.map(|offset| self.get_offset_tile(pos, offset))
					.filter_map(|tile| tile) {
					match neighbor_tile {
						Tile::Uncleared { flagged, .. } => {
							if *flagged {
								neighboring_flags += 1;
							}
							neighboring_uncleared_tiles += 1;
						}
						_ => {},
					}
				}
				// Try solve
				match clue {
					Clue::None => {}
					Clue::Number(neighboring_mines) => {
						// If all mines are flagged, clear non-flagged
						if neighboring_mines == neighboring_flags && neighboring_flags != 0 {
							let mut was_change = false;
							for neighbor_tile_direction in OffsetDirection::iter() {
								let neighbor_tile = match self.get_offset_tile_mut(pos, neighbor_tile_direction) {
									Some(neighbor_tile) => neighbor_tile,
									None => continue,
								};
								if let Tile::Uncleared { flagged: false, .. } = neighbor_tile {
									self.clear_tile_and_cascade(neighbor_tile_direction.adjust_to_offset(pos).unwrap());
									was_change = true;
								}
							}
							if was_change {
								return true;
							}
						}
						// If the mine count is equal to the uncleared tile count, flag all uncleared
						if neighboring_mines == neighboring_uncleared_tiles && neighboring_flags != neighboring_mines {
							let mut was_change = false;
							for neighbor_tile_direction in OffsetDirection::iter() {
								let neighbor_tile = match self.get_offset_tile_mut(pos, neighbor_tile_direction) {
									Some(neighbor_tile) => neighbor_tile,
									None => continue,
								};
								if let Tile::Uncleared { flagged: false, .. } = neighbor_tile {
									self.flag_unflag_tile(neighbor_tile_direction.adjust_to_offset(pos).unwrap());
									was_change = true;
								}
							}
							if was_change {
								return true;
							}
						}
					}
				}
			}
		}
		false
	}
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum GameState {
	Normal,
	GameOver,
	GameWon,
}

#[derive(EnumIter, Clone, Copy)]
enum OffsetDirection {
	TopLeft,
	Top,
	TopRight,
	Left,
	Right,
	BottomLeft,
	Bottom,
	BottomRight,
}

impl OffsetDirection {
	fn get_offset(self) -> (i8, i8) {
		match self {
			Self::TopLeft => (-1, -1),
			Self::Top => (0, -1),
			Self::TopRight => (1, -1),
			Self::Left => (-1, 0),
			Self::Right => (1, 0),
			Self::BottomLeft => (-1, 1),
			Self::Bottom => (0, 1),
			Self::BottomRight => (1, 1),
		}
	}

	fn adjust_to_offset(self, unadjusted: (u16, u16)) -> Option<(u16, u16)> {
		let offset = self.get_offset();
		Some((unadjusted.0.checked_add_signed(offset.0 as i16)?, unadjusted.1.checked_add_signed(offset.1 as i16)?))
	}

	fn adjust_to_offset_in_board(self, unadjusted: (u16, u16), board: &Board) -> Option<(u16, u16)> {
		let offset = self.get_offset();
		let board_size = board.size();
		let out = (unadjusted.0.checked_add_signed(offset.0 as i16)?, unadjusted.1.checked_add_signed(offset.1 as i16)?);
		match out.0 >= board_size.0 || out.1 >= board_size.1 {
			true => None,
			false => Some(out),
		}
	}
}

#[derive(Clone, Copy)]
enum Difficulty {
	Easy,
}