use crate::{game::game_trait::Game, get_input, mavagk_basic::{machine::Machine, token::Token}};

pub struct MavagkBasicTerminal {
	should_exit: bool,
	machine: Machine,
}

impl Game for MavagkBasicTerminal {
	fn before_poll(&mut self) -> Result<(), String> {
		let line = get_input();
		if &*line == "exit" {
			self.should_exit = true;
			return Ok(());
		}
		if line.starts_with("tokens ") {
			let tokens = match Token::parse_line(&line[7..]) {
				Err(error) => {
					println!("Basic error {error}");
					return Ok(());
				},
				Ok(tokens) => tokens,
			};
			println!("{tokens:?}");
			return Ok(());
		}
		if let Err(error) = self.machine.line_of_text_entered(&*line) {
			println!("Basic error {error}");
		}
		Ok(())
	}

	fn should_close(&self) -> bool {
		self.should_exit
	}
}

impl MavagkBasicTerminal {
	pub fn new() -> Self {
		Self {
			should_exit: false,
			machine: Machine::new(),
		}
	}
}