use crate::{game::game_trait::Game, get_input, mavagk_basic::machine::Machine};

pub struct MavagkBasicTerminal {
	should_exit: bool,
	machine: Machine,
}

impl Game for MavagkBasicTerminal {
	fn before_poll(&mut self) -> Result<(), String> {
		let line = get_input();
		if &*line == "exit" {
			self.should_exit = true;
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