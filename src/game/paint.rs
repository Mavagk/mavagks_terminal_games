use std::{collections::HashSet, mem::take};

use array2d::Array2D;
use crossterm::{event::{Event, KeyCode, KeyModifiers, MouseButton}, style::{Color, ContentStyle}};

use crate::{console::Console, game::game_trait::Game, get_input_value_or_default};

pub struct Paint {
	should_close: bool,
	canvas: Array2D<Cell>,
	button_stored_cell: [Option<Cell>; 3],
	tool: Tool,
	screen: Screen,
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
			canvas: Array2D::filled_with(Cell::new(), height as usize, width as usize),
			button_stored_cell: [Some(Cell { color: Color::White }), Some(Cell { color: Color::Black }), None],
			tool: Tool::SingleCellPaint,
			screen: Screen::main_screen_default(),
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
		if let Screen::Main { .. } = self.screen {
			match self.tool {
				Tool::SingleCellPaint => {
					if let Some(format) = self.get_button_stored_cell(button) {
						*self.get_cell_mut(pos) = format.clone();
					}
					self.screen.add_tile_to_redraw(pos);
				}
				Tool::Picker => {
					*self.get_button_stored_cell_mut(button) = Some(*self.get_cell(pos));
					self.screen.should_redraw_info_bar();
				}
			}
		}
	}

	fn get_button_stored_cell(&self, button: MouseButton) -> &Option<Cell> {
		&self.button_stored_cell[button as usize]
	}

	fn get_button_stored_cell_mut(&mut self, button: MouseButton) -> &mut Option<Cell> {
		&mut self.button_stored_cell[button as usize]
	}

	fn set_button_stored_cell_color(&mut self, button: MouseButton, color: Color) {
		let stored = self.get_button_stored_cell_mut(button);
		match stored {
			None => *stored = Some(Cell { color }),
			Some(stored) => stored.color = color,
		}
		if let Screen::Main { should_redraw_info_bar, .. } = &mut self.screen {
			*should_redraw_info_bar = true;
		}
	}

	fn set_tool(&mut self, tool: Tool) {
		self.tool = tool;
		self.screen.should_redraw_info_bar();
	}
}

impl Game for Paint {
	fn should_close(&self) -> bool {
		self.should_close
	}

	fn should_redraw(&self) -> bool {
		match &self.screen {
			Screen::Main { should_redraw_entire_canvas, should_redraw_info_bar, cells_to_redraw, should_first_draw }
				=> *should_redraw_entire_canvas || *should_redraw_info_bar || !cells_to_redraw.is_empty() || *should_first_draw,
			Screen::Help { needs_redraw } => *needs_redraw,
		}
	}

	fn keypress(&mut self, key: KeyCode, modifiers: KeyModifiers, _event: &Event) -> Result<(), String> {
		let shift_alt: KeyModifiers = KeyModifiers::SHIFT | KeyModifiers::ALT;
		match (key, modifiers, &self.screen) {
			(KeyCode::Esc, KeyModifiers::NONE, Screen::Main { .. }) => self.should_close = true,
			(KeyCode::Char('h'), KeyModifiers::NONE, Screen::Main { .. }) => self.screen = Screen::help_screen_default(),
			(KeyCode::Char('p'), KeyModifiers::NONE, Screen::Main { .. }) => self.set_tool(Tool::SingleCellPaint),
			(KeyCode::Char('k'), KeyModifiers::NONE, Screen::Main { .. }) => self.set_tool(Tool::Picker),
			(KeyCode::Char('h') | KeyCode::Esc, KeyModifiers::NONE, Screen::Help { .. }) => self.screen = Screen::main_screen_default(),

			(KeyCode::Char('K'), KeyModifiers::SHIFT, Screen::Main { .. }) => self.set_button_stored_cell_color(MouseButton::Left, Color::Black),
			(KeyCode::Char('W'), KeyModifiers::SHIFT, Screen::Main { .. }) => self.set_button_stored_cell_color(MouseButton::Left, Color::White),
			(KeyCode::Char('R'), KeyModifiers::SHIFT, Screen::Main { .. }) => self.set_button_stored_cell_color(MouseButton::Left, Color::Red),
			(KeyCode::Char('G'), KeyModifiers::SHIFT, Screen::Main { .. }) => self.set_button_stored_cell_color(MouseButton::Left, Color::Green),
			(KeyCode::Char('B'), KeyModifiers::SHIFT, Screen::Main { .. }) => self.set_button_stored_cell_color(MouseButton::Left, Color::Blue),
			(KeyCode::Char('C'), KeyModifiers::SHIFT, Screen::Main { .. }) => self.set_button_stored_cell_color(MouseButton::Left, Color::Cyan),
			(KeyCode::Char('Y'), KeyModifiers::SHIFT, Screen::Main { .. }) => self.set_button_stored_cell_color(MouseButton::Left, Color::Yellow),
			(KeyCode::Char('M'), KeyModifiers::SHIFT, Screen::Main { .. }) => self.set_button_stored_cell_color(MouseButton::Left, Color::Magenta),
			(KeyCode::Char('K'), modifiers, Screen::Main { .. }) if modifiers == shift_alt => self.set_button_stored_cell_color(MouseButton::Left, Color::DarkGrey),
			(KeyCode::Char('W'), modifiers, Screen::Main { .. }) if modifiers == shift_alt => self.set_button_stored_cell_color(MouseButton::Left, Color::Grey),
			(KeyCode::Char('R'), modifiers, Screen::Main { .. }) if modifiers == shift_alt => self.set_button_stored_cell_color(MouseButton::Left, Color::DarkRed),
			(KeyCode::Char('G'), modifiers, Screen::Main { .. }) if modifiers == shift_alt => self.set_button_stored_cell_color(MouseButton::Left, Color::DarkGreen),
			(KeyCode::Char('B'), modifiers, Screen::Main { .. }) if modifiers == shift_alt => self.set_button_stored_cell_color(MouseButton::Left, Color::DarkBlue),
			(KeyCode::Char('C'), modifiers, Screen::Main { .. }) if modifiers == shift_alt => self.set_button_stored_cell_color(MouseButton::Left, Color::DarkCyan),
			(KeyCode::Char('Y'), modifiers, Screen::Main { .. }) if modifiers == shift_alt => self.set_button_stored_cell_color(MouseButton::Left, Color::DarkYellow),
			(KeyCode::Char('M'), modifiers, Screen::Main { .. }) if modifiers == shift_alt => self.set_button_stored_cell_color(MouseButton::Left, Color::DarkMagenta),
			(KeyCode::Char('X'), KeyModifiers::SHIFT, Screen::Main { .. }) => self.set_button_stored_cell_color(MouseButton::Left, Color::Reset),
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
		self.draw(writer)?;
		Ok(())
	}

	fn draw(&mut self, writer: &mut Console) -> Result<(), String> {
		let board_size = self.canvas_size();
		// Draw info bar
		match &mut self.screen {
			Screen::Main { should_redraw_entire_canvas, should_redraw_info_bar, cells_to_redraw, should_first_draw } => {
				if *should_first_draw {
					*should_first_draw = false;
					let mut screen_size = board_size;
					screen_size.0 += 1;
					screen_size.1 += 1;
					writer.new_game_screen(screen_size);
					writer.set_game_mouse_zone((1, 1), board_size);
					writer.enable_mouse_capture();
					writer.hide_cursor();
				}
				if *should_redraw_info_bar {
					*should_redraw_info_bar = false;
					writer.move_cursor_to((0, 0));
					writer.write("Colors: ", ContentStyle::default());
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
						}
					}
					writer.write(", Tool: ", ContentStyle::default());
					writer.write(self.tool.name(), ContentStyle::default());
					writer.write(", Keys: [Esc] Exit [H]elp", ContentStyle::default());
				}
				// Draw canvas if we should redraw the entire thing
				writer.move_cursor_to((1, 1));
				if *should_redraw_entire_canvas {
					for (y, row) in self.canvas.rows_iter().enumerate() {
						writer.move_cursor_to((1, (y + 1) as u16));
						for (_x, cell) in row.enumerate() {
							writer.write(' ', ContentStyle { background_color: Some(cell.color), ..Default::default() });
						}
					}
					*should_redraw_entire_canvas = false;
					cells_to_redraw.clear();
				}
				// Draw canvas cells to redraw
				for cell_pos in take(cells_to_redraw).iter() {
					writer.move_cursor_to((cell_pos.0 + 1, cell_pos.1 + 1));
					writer.write(' ', ContentStyle { background_color: Some(self.get_cell(*cell_pos).color), ..Default::default() });
				}
			}
			Screen::Help { needs_redraw } => {
				if *needs_redraw {
					*needs_redraw = false;
					writer.new_game_screen((20, 40));
					writer.hide_cursor();
					writer.move_cursor_to((0, 0));

					writer.write("--- Navigation Keys ---\n", ContentStyle::default());
					writer.write("[Esc]: Go back or exit paint\n", ContentStyle::default());
					writer.write("[H]elp: Go to this menu\n", ContentStyle::default());
					writer.write("--- Tool Keys ---\n", ContentStyle::default());
					writer.write("[P]: Single cell painter tool\n", ContentStyle::default());
					writer.write("[K]: Color picker\n", ContentStyle::default());
					writer.write("--- Colors ---\n", ContentStyle::default());
					writer.write("[Shift+R]ed  [Shift+G]reen  [Shift+B]lue    [Shift+K] Black\n", ContentStyle::default());
					writer.write("[Shift+C]yan [Shift+Y]ellow [Shift+M]agenta [Shift+W]hite\n", ContentStyle::default());
					writer.write("[Alt+Shift+R] Dark Red     [Alt+Shift+G] Dark Green\n", ContentStyle::default());
					writer.write("[Alt+Shift+B] Dark Blue    [Alt+Shift+B] Dark Gray\n", ContentStyle::default());
					writer.write("[Alt+Shift+C] Dark Cyan    [Alt+Shift+Y] Dark Yellow\n", ContentStyle::default());
					writer.write("[Alt+Shift+M] Dark Magenta [Alt+Shift+W] Gray\n", ContentStyle::default());

					writer.write("\nPress [Esc] to return to painter screen.\n", ContentStyle::default());
				}
			}
		}
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

enum Screen {
	Main { should_first_draw: bool, should_redraw_entire_canvas: bool, should_redraw_info_bar: bool, cells_to_redraw: HashSet<(u16, u16)> },
	Help { needs_redraw: bool },
}

impl Screen {
	fn main_screen_default() -> Self {
		Self::Main { should_redraw_entire_canvas: true, should_redraw_info_bar: true, cells_to_redraw: HashSet::new(), should_first_draw: true }
	}

	fn help_screen_default() -> Self {
		Screen::Help { needs_redraw: true }
	}

	fn add_tile_to_redraw(&mut self, pos: (u16, u16)) {
		if let Self::Main { cells_to_redraw, .. } = self {
			cells_to_redraw.insert(pos);
		}
	}

	fn should_redraw_info_bar(&mut self) {
		if let Self::Main { should_redraw_info_bar, .. } = self {
			*should_redraw_info_bar = true;
		}
	}
}