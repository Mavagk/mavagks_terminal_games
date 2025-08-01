use std::{io::{stdin, stdout, Read}, iter::repeat};

use crossterm::{cursor::{Hide, MoveTo, RestorePosition, SavePosition, SetCursorStyle, Show}, event::EnableMouseCapture, execute, style::{ContentStyle, Print, PrintStyledContent, StyledContent}, terminal::{Clear, ClearType, EnterAlternateScreen, SetSize, SetTitle}};

//use console::Term;

fn main() {
	//let term = Term::stdout();
	//println!("Hi");
	//let mut s = String::new();
	//stdin().read_line(&mut s).unwrap();
	//term.set_title("Hi");
	//term.clear_screen().unwrap();
	let mut writer = stdout();
	execute!(writer, EnterAlternateScreen, SetTitle("Hi"), SetSize(12, 10), SetCursorStyle::SteadyBlock, Hide, EnableMouseCapture).unwrap();
	let style = ContentStyle {
		..Default::default()
	};
	let pos = (0, 0);
	loop {
		execute!(writer, Clear(ClearType::Purge), MoveTo(0, 0)).unwrap();
		for y in 0..10 {
			for x in 0..12 {
				if (x, y) == pos {
					execute!(writer, PrintStyledContent(StyledContent::new(style, "A"))).unwrap();
				}
				else {
					execute!(writer, PrintStyledContent(StyledContent::new(style, "B"))).unwrap();
				}
			}
			execute!(writer, PrintStyledContent(StyledContent::new(style, "\n"))).unwrap();
		}
		//execute!(writer, RestorePosition, Show).unwrap();
	}
	//term.show_cursor();
	//term.read_char().unwrap();
}
