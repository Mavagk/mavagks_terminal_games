pub mod game;
pub mod test_demo;
pub mod console;
pub mod log_events_test;
pub mod minesweeper;

use std::{collections::HashMap, io::{stdin, stdout, Write}, str::FromStr};

use strum::IntoEnumIterator;
use strum_macros::{EnumIter, FromRepr};
use crossterm::event::{read, Event, MouseEventKind};

use crate::{console::Console, game::Game, log_events_test::LogEventsTest, minesweeper::Minesweeper, test_demo::TestDemo};

#[derive(Copy, Clone, EnumIter, FromRepr)]
pub enum GameType {
	Minesweeper,
	TestDemo,
	LogEventsTest,
}

impl GameType {
	pub fn info(self) -> (&'static str, &'static str, &'static str) {
		match self {
			Self::Minesweeper => ("Minesweeper", "The mine game.", "ms"),
			Self::TestDemo => ("Test Demo", "A test demo for testing out engine features.", "td"),
			Self::LogEventsTest => ("Log Events Test", "Lists all console events.", "le"),
		}
	}

	pub fn new(self) -> Result<Box<dyn Game>, String> {
		Ok(match self {
			Self::Minesweeper => Box::new(Minesweeper::new()?),
			Self::TestDemo => Box::new(TestDemo::new()),
			Self::LogEventsTest => Box::new(LogEventsTest::new()),
		})
	}
}

fn get_input() -> Box<str> {
	let mut text_entered = String::new();
	stdin().read_line(&mut text_entered).unwrap();
	text_entered.trim_end().into()
}

//fn get_input_value<T: FromStr>() -> Option<T> {
//	let mut text_entered = String::new();
//	stdin().read_line(&mut text_entered).unwrap();
//	let text_entered = text_entered.trim_end();
//	text_entered.parse().ok()
//}

fn get_input_value_or_default<T: FromStr>(default: T) -> Option<T> {
	let mut text_entered = String::new();
	stdin().read_line(&mut text_entered).unwrap();
	let text_entered = text_entered.trim_end();
	if text_entered.is_empty() {
		return Some(default);
	}
	text_entered.parse().ok()
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
	//
	loop {
		// Get user input
		print!("Enter game to play: ");
		stdout().flush().unwrap();
		let text_entered = get_input();
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
		if let Some(game) = short_name_to_game.get(&*text_entered) {
			game_to_play = Some(*game);
		}
		// Start the game
		let game = match game_to_play {
			None => {
				println!("Invalid game entered.");
				continue;
			}
			Some(game_to_play) => game_to_play.new(),
		};
		let mut game = match game {
			Ok(game) => game,
			Err(error) => {
				println!("Error: {error}.");
				continue;
			}
		};
		let mut console_writer = Console::new();
		if let Err(error) = game.first_draw(&mut console_writer) {
			console_writer.on_game_close();
			println!("Error: {error}.");
			continue;
		}
		let mut mouse_pos_in_mouse_zone = None;
		// Enter game loop
		loop {
			if let Ok(event) = read() {
				if let Err(error) = game.event(&event) {
					console_writer.on_game_close();
					println!("Error: {error}.");
					break;
				}
				match event {
					Event::Mouse(mouse_event) => {
						// Mouse movement
						let (game_mouse_zone_top_left, game_mouse_zone_size) = console_writer.get_game_mouse_zone();
						let mut new_mouse_pos_in_mouse_zone = Some((mouse_event.column, mouse_event.row));
						if new_mouse_pos_in_mouse_zone.unwrap().0 >= game_mouse_zone_top_left.0 && new_mouse_pos_in_mouse_zone.unwrap().1 >= game_mouse_zone_top_left.1 {
							new_mouse_pos_in_mouse_zone = Some((new_mouse_pos_in_mouse_zone.unwrap().0 - game_mouse_zone_top_left.0, new_mouse_pos_in_mouse_zone.unwrap().1 - game_mouse_zone_top_left.1));
						}
						else {
							new_mouse_pos_in_mouse_zone = None;
						}
						if let Some(new_mouse_pos_in_mouse_zone_some) = new_mouse_pos_in_mouse_zone {
							if new_mouse_pos_in_mouse_zone_some.0 >= game_mouse_zone_size.0 || new_mouse_pos_in_mouse_zone_some.1 >= game_mouse_zone_size.1 {
								new_mouse_pos_in_mouse_zone = None;
							}
						}
						if new_mouse_pos_in_mouse_zone != mouse_pos_in_mouse_zone {
							mouse_pos_in_mouse_zone = new_mouse_pos_in_mouse_zone;
							if let Err(error) = game.mouse_moved_in_game_mouse_zone(mouse_pos_in_mouse_zone, &event) {
								console_writer.on_game_close();
								println!("Error: {error}.");
								break;
							}
						}
						// Mouse clicks
						let mut new_mouse_pos_in_mouse_zone = Some((mouse_event.column, mouse_event.row));
						if new_mouse_pos_in_mouse_zone.unwrap().0 >= game_mouse_zone_top_left.0 && new_mouse_pos_in_mouse_zone.unwrap().1 >= game_mouse_zone_top_left.1 {
							new_mouse_pos_in_mouse_zone = Some((new_mouse_pos_in_mouse_zone.unwrap().0 - game_mouse_zone_top_left.0, new_mouse_pos_in_mouse_zone.unwrap().1 - game_mouse_zone_top_left.1));
						}
						else {
							new_mouse_pos_in_mouse_zone = None;
						}
						if let Some(new_mouse_pos_in_mouse_zone_some) = new_mouse_pos_in_mouse_zone {
							if new_mouse_pos_in_mouse_zone_some.0 >= game_mouse_zone_size.0 || new_mouse_pos_in_mouse_zone_some.1 >= game_mouse_zone_size.1 {
								new_mouse_pos_in_mouse_zone = None;
							}
						}
						if let Some(new_mouse_pos_in_mouse_zone) = new_mouse_pos_in_mouse_zone {
							match mouse_event.kind {
								MouseEventKind::Up(button) => {
									if let Err(error) = game.mouse_click_in_game_mouse_zone(new_mouse_pos_in_mouse_zone, button, &event) {
										console_writer.on_game_close();
										println!("Error: {error}.");
										break;
									}
								}
								_ => {}
							}
						}
					}
					_ => {}
				}
			}
			if game.should_close() {
				break;
			}
			if game.should_redraw() {
				if let Err(error) = game.draw(&mut console_writer) {
					console_writer.on_game_close();
					println!("Error: {error}.");
					break;
				}
			}
		}
		// Switch back to main screen once game has finished
		console_writer.on_game_close();
	}
}