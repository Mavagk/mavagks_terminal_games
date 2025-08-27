use crate::mavagk_basic::{abstract_syntax_tree::{parse_line, Statement}, error::Error, program::Program, token::Token};

pub struct Machine {

}

impl Machine {
	pub fn new() -> Self {
		Self {

		}
	}

	pub fn line_of_text_entered(&mut self, line: Box<str>, program: &mut Program) -> Result<(), Error> {
		// Parse line
		let (line_number, tokens) = Token::tokenize_line(line)?;
		let statements = parse_line(&*tokens)?;
		// Enter line number into program and run if it does not have a line number
		match line_number {
			Some(line_number) => {
				if statements.is_empty() {
					program.lines.remove(&line_number);
				}
				else {
					program.lines.insert(line_number, (statements, line));
				}
			}
			None => {

			}
		}
		Ok(())
	}
}