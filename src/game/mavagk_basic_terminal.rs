use crate::{game::game_trait::Game, get_input, mavagk_basic::{abstract_syntax_tree::parse_line, machine::Machine, program::Program, token::Token}};

pub struct MavagkBasicTerminal {
	should_exit: bool,
	machine: Machine,
	program: Program,
}

impl Game for MavagkBasicTerminal {
	fn before_poll(&mut self) -> Result<(), String> {
		let line = get_input();
		if &*line == "exit" {
			self.should_exit = true;
			return Ok(());
		}
		if line.starts_with("tokens ") {
			let tokens = match Token::tokenize_line(&line[7..]) {
				Err(error) => {
					println!("Basic error {error}");
					return Ok(());
				},
				Ok(tokens) => tokens,
			};
			println!("{tokens:?}");
			return Ok(());
		}
		if line.starts_with("ast ") {
			let (_line, tokens) = match Token::tokenize_line(&line[4..]) {
				Err(error) => {
					println!("Basic error {error}");
					return Ok(());
				},
				Ok(tokens) => tokens,
			};
			let trees = match parse_line(&*tokens) {
				Err(error) => {
					println!("Basic error {error}");
					return Ok(());
				},
				Ok(tokens) => tokens,
			};
			for tree in trees {
				tree.print(0);
			}
			return Ok(());
		}
		if let Err(error) = self.machine.line_of_text_entered(line, &mut self.program) {
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
			program: Program::new(),
		}
	}
}