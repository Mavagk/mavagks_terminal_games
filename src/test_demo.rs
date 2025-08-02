use crossterm::{event::{Event, MouseEventKind}, style::ContentStyle};

use crate::{console_writer::ConsoleWriter, game::Game};

pub struct TestDemo {
	mouse_pos: (u16, u16),
	should_redraw: bool,
	should_close: bool,
}

impl Game for TestDemo {
	fn first_draw(&mut self, writer: &mut ConsoleWriter) {
		writer.new_game_screen(12, 10);
		self.draw(writer);
	}

	fn draw(&mut self, writer: &mut ConsoleWriter) {
		let style = ContentStyle {
			..Default::default()
		};
		writer.move_cursor_to((0, 0));
		for y in 0..10 {
			for x in 0..12 {
				if (x, y) == self.mouse_pos {
					writer.write('A', style);
				}
				else {
					writer.write('B', style);
				}
			}
			writer.write('\n', style);
		}
		self.should_redraw = false;
	}

	fn event(&mut self, event: &Event) {
		match event {
			Event::Mouse(mouse_event) => {
				match mouse_event.kind {
					MouseEventKind::Moved => {
						self.mouse_pos = (mouse_event.column, mouse_event.row);
						self.should_redraw = true;
					}
					_ => {}
				}
			}
			//Event::Key(key_event) => {
			//	match key_event.
			//}
			_ => {}
		}
	}

	fn should_redraw(&self) -> bool {
		self.should_redraw
	}

	fn should_close(&self) -> bool {
		self.should_close
	}
}

impl TestDemo {
	pub fn new() -> Self {
		TestDemo {
			mouse_pos: (0, 0),
			should_redraw: false,
			should_close: false,
		}
	}
}