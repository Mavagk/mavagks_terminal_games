use std::num::NonZeroUsize;

use num::BigInt;

use crate::mavagk_basic::error::Error;

pub enum Token {

}

impl Token {
	/// Parses a token from a string starting with a token in text form, returns:
	/// * `Ok(Some((token, rest of string with token removed)))` if a token could be found at the start of the string.
	/// * `Ok(None)` if the end of line or a `rem` remark was found.
	/// * `Err(error)` if the text was malformed.
	pub fn parse_token(line_starting_with_token: &str, column_number: NonZeroUsize) -> Result<(Option<(Self, &str)>), Error> {
		todo!()
	}

	/// Takes in a line of basic code in text form. Converts it into a line number and a list of statements, each statement in the form of a list of (column number, token) pairs.
	pub fn parse_line(line: &str) -> Result<(Option<BigInt>, Box<[Box<[(NonZeroUsize, Token)]>]>), Error> {
		todo!()
	}
}