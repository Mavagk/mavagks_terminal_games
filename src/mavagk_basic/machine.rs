use crate::mavagk_basic::error::Error;

pub struct Machine {

}

impl Machine {
	pub fn new() -> Self {
		Self {

		}
	}

	pub fn line_of_text_entered(&mut self, line: &str) -> Result<(), Error> {
		Ok(())
	}
}