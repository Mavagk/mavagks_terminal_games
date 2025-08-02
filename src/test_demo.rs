use crossterm::{event::{Event, KeyCode}, style::ContentStyle};

use crate::{console::Console, game::Game};

pub struct TestDemo {
	mouse_pos_in_mouse_zone: Option<(u16, u16)>,
	should_redraw: bool,
	should_close: bool,
}

impl Game for TestDemo {
	fn first_draw(&mut self, writer: &mut Console) {
		writer.new_game_screen((13, 11));
		writer.set_game_mouse_zone((1, 1), (12, 10));
		writer.enable_mouse_capture();
		writer.hide_cursor();
		self.draw(writer);
	}

	fn draw(&mut self, writer: &mut Console) {
		let style = ContentStyle {
			..Default::default()
		};
		writer.move_cursor_to((0, 0));
		print!("               ");
		writer.move_cursor_to((0, 0));
		writer.write(format!("{:?}\n", self.mouse_pos_in_mouse_zone), style);
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
		}
		self.should_redraw = false;
	}

	fn event(&mut self, event: &Event) {
		match event {
			Event::Key(key_event) => {
				if key_event.is_press() && key_event.code == KeyCode::Esc {
					self.should_close = true;
				}
			}
			_ => {}
		}
	}

	fn mouse_moved_in_game_mouse_zone(&mut self, pos_in_mouse_zone: Option<(u16, u16)>, _event: &Event) {
		self.mouse_pos_in_mouse_zone = pos_in_mouse_zone;
		self.should_redraw = true;
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