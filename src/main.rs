use std::{io::{stdin, stdout, Read}, iter::repeat};

use crossterm::{cursor::{MoveTo, RestorePosition, SavePosition, SetCursorStyle}, execute, style::{ContentStyle, Print, PrintStyledContent, StyledContent}, terminal::{Clear, ClearType, EnterAlternateScreen, SetSize, SetTitle}};

//use console::Term;

fn main() {
	//let term = Term::stdout();
	//println!("Hi");
	//let mut s = String::new();
	//stdin().read_line(&mut s).unwrap();
	//term.set_title("Hi");
	//term.clear_screen().unwrap();
	let mut writer = stdout();
	execute!(writer, EnterAlternateScreen, SetTitle("Hi"), SetSize(12, 10), SetCursorStyle::SteadyBlock).unwrap();
	let style = ContentStyle {
		..Default::default()
	};
	loop {
		execute!(writer, SavePosition, Clear(ClearType::Purge), MoveTo(0, 0)).unwrap();
		for _ in 0..10 {
			//execute!(writer, PrintStyledContent(StyledContent::new(style, "AAAAAAAAAAAA\n"))).unwrap();
			execute!(writer, PrintStyledContent(StyledContent::new(style, "AAAAAAAAAAAA\n"))).unwrap();
		}
		execute!(writer, RestorePosition).unwrap();
	}
	//term.show_cursor();
	//term.read_char().unwrap();
}
