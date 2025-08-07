use std::iter::once;

use array2d::Array2D;
use crossterm::{event::{Event, KeyCode, MouseButton}, style::{Color, ContentStyle}};
use rand::{random, rngs::SmallRng, Rng, SeedableRng};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::{console::Console, game::Game, get_input, get_input_value_or_default};

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

	fn event(&mut self, event: &Event) -> Result<(), String> {
		match event {
			Event::Key(key_event) => {
				if key_event.is_press() {
					match key_event.code {
						KeyCode::Esc => self.should_close = true,
						KeyCode::Char('r') => {
							self.board.reset();
							self.should_redraw = true;
						}
						KeyCode::Char('e' | 's') => self.single_solve_step(Difficulty::Easy),
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
		writer.write(format!("Flagged: {}/{}. ",self.board.tiles_flagged, self.board.mine_count), ContentStyle { ..Default::default() });
		match self.board.game_state {
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
		match button {
			// Left click to clear a tile
			MouseButton::Left => self.clear_tile(pos_in_mouse_zone)?,
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
		// Clue type
		print!("Clue type (orthodox, extended, o, e) (blank for orthodox): ");
		let clue_type_text = get_input();
		let clue_type = match &*clue_type_text {
			"" | "o" | "orthodox" => ClueType::Orthodox,
			"e" | "extended" => ClueType::Extended,
			_ => return Err("Invalid input".into()),
		};
		// Create game
		Ok(Self {
			board: Board {
				tiles: Array2D::filled_with(Tile::Uncleared { mined: false, flagged: false }, height, width),
				mine_count,
				is_generated: false,
				tiles_flagged: 0,
				tiles_cleared: 0,
				ensured_solvable_difficulty,
				game_state: GameState::Normal,
				clue_type,
				seed: random(),
			},
			should_close: false,
			should_redraw: false,
		})
	}

	/// Will clear a tile.
	fn clear_tile(&mut self, pos: (u16, u16)) -> Result<(), String> {
		// If the board is not yet generated
		if !self.board.is_generated {
			let mut was_successful = false;
			for _ in 0..100 {
				// Generate
				if !self.board.generate(pos) {
					return Err("Unable to generate board".into());
				}
				// Clear tile
				self.board.clear_tile_and_cascade(pos);
				// Check if the board can be solved
				if self.board.is_solvable() {
					was_successful = true;
					break;
				}
				// Reset the board and try again if the board cannot be solved
				self.board.reset();
			}
			if !was_successful {
				return Err("Unable to generate board".into());
			}
			self.should_redraw = true;
		}
		// If the board is generated
		else {
			self.should_redraw |= self.board.clear_tile_and_cascade(pos);
		}
		// Return
		Ok(())
	}

	/// Execute a single solve step using techniques of the given difficulty.
	fn single_solve_step(&mut self, difficulty: Difficulty) {
		self.should_redraw |= self.board.single_solve_step(difficulty);
	}
}

#[derive(Clone, Copy)]
enum Clue {
	None,
	Number(u8),
	Question,
	Even,
	Odd,
	Small,
	Medium,
	Large,
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
			Self::Question => ('?', Color::DarkGrey),
			Self::Even => ('E', Color::Yellow),
			Self::Odd => ('O', Color::Cyan),
			Self::Small => ('S', Color::Magenta),
			Self::Medium => ('M', Color::DarkGreen),
			Self::Large => ('L', Color::DarkYellow),
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
		if board.clue_type == ClueType::Orthodox {
			return Self::Number(mine_count);
		}
		let mut rng = SmallRng::seed_from_u64(board.seed ^ ((pos.0 as u64) << 16) ^ ((pos.1 as u64) << 32));
		if rng.random_bool(0.2) {
			return Clue::Question;
		}
		if rng.random_bool(0.2) {
			match mine_count {
				..=2 => return Clue::Small,
				3..=4 => return Clue::Medium,
				5.. => return Clue::Large,
			}
		}
		if rng.random_bool(0.2) {
			match mine_count % 2 {
				0 => return Clue::Even,
				1 => return Clue::Odd,
				_ => unreachable!(),
			}
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
	tiles_flagged: u32,
	tiles_cleared: u32,
	is_generated: bool,
	ensured_solvable_difficulty: Option<Difficulty>,
	game_state: GameState,
	clue_type: ClueType,
	seed: u64,
}

impl Board {
	/// Gives the width and height of the board.
	fn size(&self) -> (u16, u16) {
		(self.width(), self.height())
	}

	/// Gives the width of the board.
	fn width(&self) -> u16 {
		self.tiles.num_columns() as u16
	}

	/// Gives the height of the board.
	fn height(&self) -> u16 {
		self.tiles.num_rows() as u16
	}

	/// Gives the count of board tiles..
	fn tile_count(&self) -> u32 {
		self.width() as u32 * self.height() as u32
	}

	/// Get a immutable reference to a tile on the board, MUST be inside the board.
	fn get_tile(&self, pos: (u16, u16)) -> &Tile {
		&self.tiles[(pos.1 as usize, pos.0 as usize)]
	}

	/// Get a mutable reference to a tile on the board, MUST be inside the board.
	fn get_tile_mut(&mut self, pos: (u16, u16)) -> &mut Tile {
		&mut self.tiles[(pos.1 as usize, pos.0 as usize)]
	}

	/// Get a immutable reference to a tile on the board given a pos and an offset to adjust it by.
	/// Returns `None` if the adjusted pos is outside the board.
	fn get_offset_tile(&self, unadjusted_pos: (u16, u16), offset: OffsetDirection) -> Option<&Tile> {
		let adjusted_pos = offset.adjust_to_offset(unadjusted_pos)?;
		self.tiles.get(adjusted_pos.1 as usize, adjusted_pos.0 as usize)
	}

	/// Get a mutable reference to a tile on the board given a pos and an offset to adjust it by.
	/// Returns `None` if the adjusted pos is outside the board.
	fn get_offset_tile_mut(&mut self, unadjusted_pos: (u16, u16), offset: OffsetDirection) -> Option<&mut Tile> {
		let adjusted_pos = offset.adjust_to_offset(unadjusted_pos)?;
		self.tiles.get_mut(adjusted_pos.1 as usize, adjusted_pos.0 as usize)
	}

	/// Clears a tile on the board, cascading if necessary.
	/// Returns if the board should be redrawn.
	fn clear_tile_and_cascade(&mut self, pos: (u16, u16)) -> bool {
		// Cannot clear a tile if the game is won or lost
		if self.game_state != GameState::Normal {
			return false;
		}
		// Clear the tile and the tiles to cascade to
		let mut to_clear: Vec<(u16, u16)> = once(pos).collect();
		let mut should_redraw = false;
		while !to_clear.is_empty() {
			let element_to_clear_pos = to_clear.pop().unwrap();
			should_redraw |= self.clear_tile(element_to_clear_pos, &mut to_clear);
		}
		// Check if the game was won or lost
		self.check_that_game_is_won();
		// Clear / explode all if the game is won or lost
		match self.game_state {
			GameState::GameWon => self.flag_all_uncleared(),
			GameState::GameOver => self.explode_all_mines(),
			GameState::Normal => {}
		}
		// Return
		should_redraw
	}

	/// Clears a tile on the board, writes tiles to cascade to to `cascade_to`.
	/// Returns if the board should be redrawn.
	fn clear_tile(&mut self, pos: (u16, u16), cascade_to: &mut Vec<(u16, u16)>) -> bool {
		let mut do_cascade = false;
		let mut should_redraw = false;
		// Change tile
		let tile = self.get_tile_mut(pos);
		match tile {
			Tile::Uncleared { mined, flagged: false } => {
				match mined {
					// If the tile has a mine, game over
					true => {
						*tile = Tile::Exploded;
						self.game_state = GameState::GameOver;
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
		// Add tiles to cascade to to the list.
		if do_cascade {
			cascade_to.extend(OffsetDirection::iter().map(|direction| direction.adjust_to_offset_in_board(pos, self)).filter_map(|pos| pos));
		}
		// Return
		should_redraw
	}

	/// Flags or unflags a tile, returns if the board should be redrawn.
	fn flag_unflag_tile(&mut self, pos: (u16, u16)) -> bool {
		// Can't flag if the game is won or lost
		if self.game_state != GameState::Normal {
			return false;
		}
		let mut should_redraw = false;
		// Flag tile
		let tile = self.get_tile_mut(pos);
		match tile {
			Tile::Uncleared { flagged, .. } => {
				*flagged = !*flagged;
				should_redraw = true;
				match *flagged {
					true => self.tiles_flagged += 1,
					false => self.tiles_flagged -= 1,
				}
			}
			_ => {}
		}
		// Return
		should_redraw
	}

	fn check_that_game_is_won(&mut self) {
		if self.tiles_cleared == self.tile_count() - self.mine_count {
			self.game_state = GameState::GameWon;
		}
	}

	/// Returns if a tile contains a mine, exploded or unexploded.
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
		// Do nothing if the board has 0 mines
		if self.mine_count == 0 {
			return true;
		}
		// Place all the mines
		let mut rng = SmallRng::seed_from_u64(self.seed);
		let board_size = self.size();
		let mut mines_placed = 0;
		for _ in 0..self.mine_count.saturating_mul(1000) {
			// Get the pos for the mine to place
			let pos = (rng.random_range(0..board_size.0), rng.random_range(0..board_size.1));
			// Do not place the mine inside a 3x3 box centered on the safe tile or in a tile that already has a mine
			if (pos.0.abs_diff(safe_tile.0) < 2 && pos.1.abs_diff(safe_tile.1) < 2) || self.is_mine_in_tile(pos) {
				continue;
			}
			// Place mine
			match self.get_tile_mut(pos) {
				Tile::Uncleared { mined, .. } => *mined = true,
				_ => unreachable!(),
			}
			mines_placed += 1;
			// Return if we have placed all mines
			if mines_placed == self.mine_count {
				return true;
			}
		}
		// If we failed to place all the mines
		false
	}

	/// Clears the board back to the starting ungenerated state.
	fn reset(&mut self) {
		self.is_generated = false;
		self.tiles_flagged = 0;
		self.tiles_cleared = 0;
		self.game_state = GameState::Normal;
		for index in self.tiles.indices_column_major() {
			self.tiles[index] = Tile::Uncleared { mined: false, flagged: false };
		}
		self.seed = random();
	}

	/// Returns true if the board can be solved using techniques of the games difficulty.
	fn is_solvable(&self) -> bool {
		// Return that we can solve the board if the difficulty is set to unchecked
		let ensured_solvable_difficulty = match self.ensured_solvable_difficulty {
			Some(ensured_solvable_difficulty) => ensured_solvable_difficulty,
			None => return true,
		};
		// Clone the board and try to solve it
		let mut test_board = self.clone();
		loop {
			let was_tile_solved = test_board.single_solve_step(ensured_solvable_difficulty);
			match (test_board.game_state, was_tile_solved) {
				(GameState::GameWon, _) => return true,
				(GameState::GameOver, _) => panic!("Something is wrong with the solver, it lost the game"),
				(GameState::Normal, false) => return false,
				(GameState::Normal, true) => {}
			}
		}
	}

	/// Flags all tiles that are uncleared
	fn flag_all_uncleared(&mut self) {
		for y in 0..self.height() {
			for x in 0..self.width() {
				if let Tile::Uncleared { flagged, .. } = self.get_tile_mut((x, y)) {
					*flagged = true;
				}
			}
		}
		self.tiles_flagged = self.mine_count;
	}

	/// Explode all the mines on the board.
	fn explode_all_mines(&mut self) {
		for y in 0..self.height() {
			for x in 0..self.width() {
				let tile = self.get_tile_mut((x, y));
				let mut was_unflagged = false;
				if let Tile::Uncleared { mined: true, flagged } = tile {
					if *flagged {
						was_unflagged = true;
					}
					*tile = Tile::Exploded;
				}
				if was_unflagged {
					self.tiles_flagged -= 1;
				}
			}
		}
	}

	/// Execute a single solve step using techniques of the given difficulty.
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
					Clue::None | Clue::Question => {}
					Clue::Even | Clue::Odd | Clue::Small | Clue::Medium | Clue::Large => {} // TODO
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
									was_change |= self.flag_unflag_tile(neighbor_tile_direction.adjust_to_offset(pos).unwrap());
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
	/// The offset of the direction.
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

	/// Adjusts a pos by moving it in a direction. Returns `None` if the pos x or y underflows.
	fn adjust_to_offset(self, unadjusted: (u16, u16)) -> Option<(u16, u16)> {
		let offset = self.get_offset();
		Some((unadjusted.0.checked_add_signed(offset.0 as i16)?, unadjusted.1.checked_add_signed(offset.1 as i16)?))
	}

	/// Adjusts a pos by moving it in a direction. Returns `None` if the pos is outside the given board.
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

#[derive(Clone, Copy, PartialEq, Eq)]
enum ClueType {
	Orthodox,
	Extended,
}