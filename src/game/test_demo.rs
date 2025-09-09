use std::io;

use crossterm::{cursor::MoveToColumn, event::{Event, KeyCode, KeyModifiers}, execute, style::ContentStyle};

use crate::{console::Console, Game};

pub struct TestDemo {
	mouse_pos_in_mouse_zone: Option<(u16, u16)>,
	should_redraw: bool,
	should_close: bool,
}

impl Game for TestDemo {
	fn first_draw(&mut self, writer: &mut Console) -> Result<(), String> {
		writer.new_game_screen((13, 11));
		writer.set_game_mouse_zone((1, 1), (12, 10));
		writer.enable_mouse_capture();
		writer.hide_cursor();
		self.draw(writer)?;
		Ok(())
	}

	fn draw(&mut self, writer: &mut Console) -> Result<(), String> {
		let style = ContentStyle {
			..Default::default()
		};
		writer.move_cursor_to((0, 0));
		print!("               ");
		writer.move_cursor_to((0, 0));
		writer.write(format!("{:?}\n", self.mouse_pos_in_mouse_zone), style);
		execute!(io::stdout(), MoveToColumn(0)).unwrap();
		for y in 0..10 {
			writer.write(' ', style);
			for x in 0..12 {
				if Some((x, y)) == self.mouse_pos_in_mouse_zone {
					writer.write('A', style);
				}
				else {
					writer.write('B', style);
				}
			}
			writer.write('\n', style);
			execute!(io::stdout(), MoveToColumn(0)).unwrap();
		}
		self.should_redraw = false;
		Ok(())
	}
	
	fn keypress(&mut self, key: KeyCode, _modifiers: KeyModifiers, _event: &Event) -> Result<(), String> {
		if key == KeyCode::Esc {
			self.should_close = true;
		}
		Ok(())
	}

	fn mouse_moved_in_game_mouse_zone(&mut self, pos_in_mouse_zone: Option<(u16, u16)>, _event: &Event) -> Result<(), String> {
		self.mouse_pos_in_mouse_zone = pos_in_mouse_zone;
		self.should_redraw = true;
		Ok(())
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
			mouse_pos_in_mouse_zone: None,
			should_redraw: false,
			should_close: false,
		}
	}
}