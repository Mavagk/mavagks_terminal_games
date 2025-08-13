use std::collections::HashSet;

use array2d::Array2D;
use crossterm::{event::{Event, KeyCode, MouseButton}, style::{Color, ContentStyle}};

use crate::{console::Console, game::game_trait::Game, get_input_value_or_default};

pub struct Paint {
	should_redraw_entire_canvas: bool,
	should_redraw_info_bar: bool,
	should_close: bool,
	cells_to_redraw: HashSet<(u16, u16)>,
	canvas: Array2D<Cell>,
	button_stored_cell: [Option<Cell>; 3],
	tool: Tool,
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
			should_redraw_entire_canvas: true,
			should_redraw_info_bar: true,
			cells_to_redraw: HashSet::new(),
			canvas: Array2D::filled_with(Cell::new(), height as usize, width as usize),
			button_stored_cell: [Some(Cell { color: Color::White }), Some(Cell { color: Color::Black }), None],
			tool: Tool::SingleCellPaint,
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

	fn tool_interact_at(&mut self, pos: (u16, u16), button: MouseButton) {
		match self.tool {
			Tool::SingleCellPaint => {
				if let Some(format) = self.get_button_stored_cell(button) {
					*self.get_cell_mut(pos) = format.clone();
				}
				self.cells_to_redraw.insert(pos);
			}
			Tool::Picker => {
				*self.get_button_stored_cell_mut(button) = Some(*self.get_cell(pos));
				self.should_redraw_info_bar = true;
			}
		}
	}

	fn get_button_stored_cell(&self, button: MouseButton) -> &Option<Cell> {
		&self.button_stored_cell[button as usize]
	}

	fn get_button_stored_cell_mut(&mut self, button: MouseButton) -> &mut Option<Cell> {
		&mut self.button_stored_cell[button as usize]
	}

	fn set_tool(&mut self, tool: Tool) {
		self.tool = tool;
		self.should_redraw_info_bar = true;
	}
}

impl Game for Paint {
	fn should_close(&self) -> bool {
		self.should_close
	}

	fn should_redraw(&self) -> bool {
		self.should_redraw_entire_canvas || self.should_redraw_info_bar || !self.cells_to_redraw.is_empty()
	}

	fn keypress(&mut self, key: KeyCode, _event: &Event) -> Result<(), String> {
		match key {
			KeyCode::Esc => self.should_close = true,
			KeyCode::Char('p') => self.set_tool(Tool::SingleCellPaint),
			KeyCode::Char('k') => self.set_tool(Tool::Picker),
			_ => {}
		}
		Ok(())
	}

	fn mouse_start_click_in_game_mouse_zone(&mut self, pos_in_mouse_zone: (u16, u16), button: MouseButton, _event: &Event) -> Result<(), String> {
		self.tool_interact_at(pos_in_mouse_zone, button);
		Ok(())
	}

	fn mouse_drag_in_game_mouse_zone(&mut self, pos_in_mouse_zone: (u16, u16), button: MouseButton, _event: &Event) -> Result<(), String> {
		self.tool_interact_at(pos_in_mouse_zone, button);
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
		// Draw info bar
		if self.should_redraw_info_bar {
			writer.move_cursor_to((0, 0));
			writer.write("Colors: ", ContentStyle::default());
			self.should_redraw_info_bar = false;
			for (index, color) in self.button_stored_cell.iter().enumerate() {
				if let Some(color) = color {
					let chr = match index {
						index if index == MouseButton::Left as usize => 'L',
						index if index == MouseButton::Right as usize => 'R',
						index if index == MouseButton::Middle as usize => 'M',
						_ => '?'
					};
					writer.write(chr, ContentStyle::default());
					writer.write(' ', ContentStyle { background_color: Some(color.color), ..Default::default() });
					writer.write(' ', ContentStyle::default());
				}
			}
			writer.write(" Tool: ", ContentStyle::default());
			writer.write(self.tool.name(), ContentStyle::default());
		}
		// Draw canvas if we should redraw the entire thing
		writer.move_cursor_to((1, 1));
		if self.should_redraw_entire_canvas {
			for (y, row) in self.canvas.rows_iter().enumerate() {
				writer.move_cursor_to((1, (y + 1) as u16));
				for (_x, cell) in row.enumerate() {
					writer.write(' ', ContentStyle { background_color: Some(cell.color), ..Default::default() });
				}
			}
			self.should_redraw_entire_canvas = false;
			self.cells_to_redraw.clear();
		}
		// Draw canvas cells to redraw
		for cell_pos in self.cells_to_redraw.iter() {
			writer.move_cursor_to((cell_pos.0 + 1, cell_pos.1 + 1));
			writer.write(' ', ContentStyle { background_color: Some(self.get_cell(*cell_pos).color), ..Default::default() });
		}
		self.cells_to_redraw.clear();
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

#[derive(Clone, Copy)]
enum Tool {
	SingleCellPaint,
	Picker,
}

impl Tool {
	pub fn name(self) -> &'static str {
		match self {
			Self::SingleCellPaint => "Single Cell Painter",
			Self::Picker => "Picker",
		}
	}
}