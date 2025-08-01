pub mod game;
pub mod test_demo;

use std::{io::stdout, marker::PhantomData};

use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::{game::Game, test_demo::{new_test_demo, TestDemo}};

#[derive(Copy, Clone, EnumIter)]
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
			Self::TestDemo => Box::new(new_test_demo()),
		}
	}
}

fn main() {
	println!("Games:");
	for game in GameType::iter() {
		let (game_name, game_info, game_short_name) = game.info();
		println!("{}, {game_short_name}: {game_name}, {game_info}", game as usize);
	}

	let writer = stdout();
	//execute!(writer, EnterAlternateScreen, SetTitle("Hi"), SetSize(12, 10), SetCursorStyle::SteadyBlock, Hide, EnableMouseCapture).unwrap();
	//let mut pos = (0, 0);
	//redraw(&mut writer, pos);
	//loop {
	//	if let Ok(event) = read() {
	//		match event {
	//			Event::Mouse(mouse_event) => {
	//				pos = (mouse_event.column, mouse_event.row);
	//				//mouse_event.modifiers
	//				redraw(&mut writer, pos);
	//			}
	//			_ => {}
	//		}
	//	}
	//}
	//term.show_cursor();
	//term.read_char().unwrap();
}

//fn redraw(writer: &mut Stdout, pos: (u16, u16)) {
//	let style = ContentStyle {
//		..Default::default()
//	};
//	execute!(writer, Clear(ClearType::Purge), MoveTo(0, 0)).unwrap();
//	for y in 0..10 {
//		for x in 0..12 {
//			if (x, y) == pos {
//				execute!(writer, PrintStyledContent(StyledContent::new(style, "A"))).unwrap();
//			}
//			else {
//				execute!(writer, PrintStyledContent(StyledContent::new(style, "B"))).unwrap();
//			}
//		}
//		execute!(writer, PrintStyledContent(StyledContent::new(style, "\n"))).unwrap();
//	}
//}