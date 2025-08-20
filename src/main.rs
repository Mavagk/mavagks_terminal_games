pub mod game;
pub mod console;
pub mod mavagk_basic;

use std::{collections::HashMap, io::{stdin, stdout, Write}, str::FromStr, thread::yield_now, time::{Duration, Instant}};

use strum::IntoEnumIterator;
use strum_macros::{EnumIter, FromRepr};
use crossterm::event::{poll, read, Event, MouseEventKind};

use crate::{console::Console, game::{game_trait::Game, log_events_test::LogEventsTest, minesweeper::Minesweeper, paint::Paint, test_demo::TestDemo}};

#[derive(Copy, Clone, EnumIter, FromRepr)]
pub enum GameVariant {
	Minesweeper,
	Paint,
	TestDemo,
	LogEventsTest,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum GameClass {
	Game,
	NonGame,
	Debug,
}

impl GameVariant {
	pub fn info(self) -> (&'static str, &'static str, &'static str, GameClass) {
		match self {
			Self::Minesweeper => ("Minesweeper", "The mine game.", "ms", GameClass::Game),
			Self::Paint => ("Paint", "Paint on the console.", "pt", GameClass::NonGame),
			Self::TestDemo => ("Test Demo", "A test demo for testing out engine features.", "td", GameClass::Debug),
			Self::LogEventsTest => ("Log Events Test", "Lists all console events.", "le", GameClass::Debug),
		}
	}

	pub fn new(self) -> Result<Box<dyn Game>, String> {
		Ok(match self {
			Self::Minesweeper => Box::new(Minesweeper::new()?),
			Self::Paint => Box::new(Paint::new()?),
			Self::TestDemo => Box::new(TestDemo::new()),
			Self::LogEventsTest => Box::new(LogEventsTest::new()),
		})
	}
}

/// Will flush the stdin buffer, request a string and block until it receives it and then return it with the trailing whitespaces trimmed off.
fn get_input() -> Box<str> {
	stdout().flush().unwrap();
	let mut text_entered = String::new();
	stdin().read_line(&mut text_entered).unwrap();
	text_entered.trim_end().into()
}

/// Will flush the stdin buffer, request a string and block until it receives, trim off the trailing whitespaces then try to parse it into a value of type `T`.
/// Entering nothing will result in the default value being returned.
fn get_input_value_or_default<T: FromStr>(default: T) -> Option<T> {
	stdout().flush().unwrap();
	let mut text_entered = String::new();
	stdin().read_line(&mut text_entered).unwrap();
	let text_entered = text_entered.trim_end();
	if text_entered.is_empty() {
		return Some(default);
	}
	text_entered.parse().ok()
}

fn main() {
	println!("Mavagk's Terminal Games. Version {}", env!("CARGO_PKG_VERSION"));
	// Print out game list
	println!("--- Games ---");
	let mut short_name_to_game = HashMap::new();
	for game in GameVariant::iter() {
		let (game_name, game_info, game_short_name, game_class) = game.info();
		short_name_to_game.insert(game_short_name, game);
		if game_class != GameClass::Game {
			continue;
		}
		println!("{}, {game_short_name}: {game_name}, {game_info}", game as usize);
	}
	println!("--- Non-Games ---");
	for game in GameVariant::iter() {
		let (game_name, game_info, game_short_name, game_class) = game.info();
		if game_class != GameClass::NonGame {
			continue;
		}
		println!("{}, {game_short_name}: {game_name}, {game_info}", game as usize);
	}
	println!("--- Debug ---");
	for game in GameVariant::iter() {
		let (game_name, game_info, game_short_name, game_class) = game.info();
		if game_class != GameClass::Debug {
			continue;
		}
		println!("{}, {game_short_name}: {game_name}, {game_info}", game as usize);
	}
	//
	loop {
		// Get user input
		print!("Enter option to run: ");
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
			match GameVariant::from_repr(game_id) {
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
		let mut last_tick_time = None;
		let mut time_overflow = Duration::from_micros(0);
		// Enter game loop
		loop {
			while matches!(poll(Duration::from_secs(0)), Ok(true)) {
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
										if let Err(error) = game.mouse_start_click_in_game_mouse_zone(new_mouse_pos_in_mouse_zone, button, &event) {
											console_writer.on_game_close();
											println!("Error: {error}.");
											break;
										}
									}
									MouseEventKind::Drag(button) => {
										if let Err(error) = game.mouse_drag_in_game_mouse_zone(new_mouse_pos_in_mouse_zone, button, &event) {
											console_writer.on_game_close();
											println!("Error: {error}.");
											break;
										}
									}
									_ => {}
								}
							}
						}
						Event::Key(key_event) => {
							if key_event.is_press() {
								if let Err(error) = game.keypress(key_event.code, key_event.modifiers, &event) {
									console_writer.on_game_close();
									println!("Error: {error}.");
									break;
								}
							}
						}
						_ => {}
					}
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
			if let Some(tick_frequency) = game.tick_frequency() {
				let now = Instant::now();
				let (should_tick, next_tick_time) = match last_tick_time {
					None => (true, None),
					Some(last_tick_time) => {
						let next_tick_time: Instant = last_tick_time + tick_frequency;
						let next_tick_time = next_tick_time.checked_sub(time_overflow).unwrap_or(next_tick_time);
						(now >= next_tick_time, Some(next_tick_time))
					}
				};
				if should_tick {
					let time_since_last_tick = last_tick_time.map(|last_tick_time|now - last_tick_time);
					if let Err(error) = game.tick(time_since_last_tick) {
						console_writer.on_game_close();
						println!("Error: {error}.");
						break;
					}
					last_tick_time = Some(now);
					time_overflow = match next_tick_time {
						None => Duration::from_nanos(0),
						Some(next_tick_time) => now.saturating_duration_since(next_tick_time),
					};
				}
			}
			yield_now();
		}
		// Switch back to main screen once game has finished
		console_writer.on_game_close();
	}
}