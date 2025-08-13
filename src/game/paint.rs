use array2d::Array2D;
use crossterm::{event::{Event, KeyCode, MouseButton}, style::{Color, ContentStyle}};

use crate::{console::Console, game::game_trait::Game, get_input_value_or_default};

pub struct Paint {
	should_redraw: bool,
	should_close: bool,
	canvas: Array2D<Cell>
}

impl Paint {
	pub fn new() -> Result<Self, String> {
		// Get width
		print!("Width (blank for 40): ");
		let width: u16 = get_input_value_or_default(50).ok_or("Invalid width.")?;
		// Get height
		print!("Height (blank for half width): ");
		let height: u16 = get_input_value_or_default((width / 2).max(4)).ok_or("Invalid height.")?;
		// Create game
		Ok(Self {
			should_close: false,
			should_redraw: false,
			canvas: Array2D::filled_with(Cell::new(), width as usize, height as usize),
		})
	}

	pub fn canvas_width(&self) -> u16 {
		self.canvas.row_len() as u16
	}

	pub fn canvas_height(&self) -> u16 {
		self.canvas.column_len() as u16
	}

	pub fn canvas_size(&self) -> (u16, u16) {
		(self.canvas_width(), self.canvas_height())
	}

	fn get_cell(&self, pos: (u16, u16)) -> &Cell {
		&self.canvas[(pos.1 as usize, pos.0 as usize)]
	}

	fn get_cell_mut(&mut self, pos: (u16, u16)) -> &mut Cell {
		&mut self.canvas[(pos.1 as usize, pos.0 as usize)]
	}

	fn tool_interact_at(&mut self, pos: (u16, u16), button: u8) {
		let cell = self.get_cell_mut(pos);
		match button {
			0 => cell.color = Color::White,
			1 => cell.color = Color::Black,
			_ => unreachable!()
		}
		self.should_redraw = true;
	}
}

impl Game for Paint {
	fn should_close(&self) -> bool {
		self.should_close
	}

	fn should_redraw(&self) -> bool {
		self.should_redraw
	}

	fn keypress(&mut self, key: KeyCode, _event: &Event) -> Result<(), String> {
		match key {
			KeyCode::Esc => self.should_close = true,
			_ => {}
		}
		Ok(())
	}

	fn mouse_click_in_game_mouse_zone(&mut self, pos_in_mouse_zone: (u16, u16), button: MouseButton, _event: &Event) -> Result<(), String> {
		match button {
			MouseButton::Left => self.tool_interact_at(pos_in_mouse_zone, 0),
			MouseButton::Right => self.tool_interact_at(pos_in_mouse_zone, 1),
			_ => {},
		}
		Ok(())
	}

	fn first_draw(&mut self, writer: &mut Console) -> Result<(), String> {
		let board_size = self.canvas_size();
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
		//let board_size = self.canvas_size();
		// Write info
		writer.move_cursor_to((0, 0));
		// Draw canvas
		writer.move_cursor_to((1, 1));
		for (y, row) in self.canvas.rows_iter().enumerate() {
			writer.move_cursor_to((1, (y + 1) as u16));
			for (_x, cell) in row.enumerate() {
				writer.write(' ', ContentStyle { background_color: Some(cell.color), ..Default::default() });
			}
		}
		self.should_redraw = false;
		Ok(())
	}
}

#[derive(Clone, Copy)]
struct Cell {
	color: Color,
}

impl Cell {
	fn new() -> Self {
		Cell {
			color: Color::Reset,
		}
	}
}