pub mod game;
pub mod test_demo;
pub mod console_writer;

use std::{collections::HashMap, io::{stdin, stdout, Write}};

use strum::IntoEnumIterator;
use strum_macros::{EnumIter, FromRepr};
use crossterm::event::read;

use crate::{console_writer::ConsoleWriter, game::Game, test_demo::TestDemo};

#[derive(Copy, Clone, EnumIter, FromRepr)]
pub enum GameType {
	TestDemo,
}

impl GameType {
	pub fn info(self) -> (&'static str, &'static str, &'static str) {
		match self {
			Self::TestDemo => ("Test Demo", "A test demo for testing out engine features.", "td"),
		}
	}

	pub fn new(self) -> Box<dyn Game> {
		match self {
			Self::TestDemo => Box::new(TestDemo::new()),
		}
	}
}

fn main() {
	// Print out game list
	println!("Games:");
	let mut short_name_to_game = HashMap::new();
	for game in GameType::iter() {
		let (game_name, game_info, game_short_name) = game.info();
		short_name_to_game.insert(game_short_name, game);
		println!("{}, {game_short_name}: {game_name}, {game_info}", game as usize);
	}

	loop {
		// Get user input
		print!("Enter game to play: ");
		stdout().flush().unwrap();
		let mut text_entered = String::new();
		stdin().read_line(&mut text_entered).unwrap();
		let text_entered = text_entered.trim_end();
		// Decide the game to play from the entered text
		if text_entered.is_empty() {
			break;
		}
		let mut game_to_play = None;
		if text_entered.chars().all(|chr| chr.is_ascii_digit()) {
			let game_id = match text_entered.parse::<usize>() {
				Ok(game_id) => game_id,
				Err(_) => {
					println!("Invalid game id.");
					continue;
				}
			};
			match GameType::from_repr(game_id) {
				Some(game) => game_to_play = Some(game),
				None => {
					println!("Invalid game id.");
					continue;
				}
			}
		}
		if let Some(game) = short_name_to_game.get(text_entered) {
			game_to_play = Some(*game);
		}
		// Start the game
		let mut game = match game_to_play {
			None => {
				println!("Invalid game entered.");
				continue;
			}
			Some(game_to_play) => game_to_play.new(),
		};
		let mut console_writer = ConsoleWriter::new();
		game.first_draw(&mut console_writer);
		// Enter game loop
		loop {
			if let Ok(event) = read() {
				game.event(&event);
			}
			if game.should_close() {
				break;
			}
			if game.should_redraw() {
				game.draw(&mut console_writer);
			}
		}
		// Switch back to main screen once game has finished
		console_writer.exit_game_screen();
	}
}