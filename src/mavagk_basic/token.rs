use std::num::NonZeroUsize;

use num::BigInt;

use crate::mavagk_basic::error::Error;

pub struct Token<'a> {
	value: &'a str,
	variant: TokenVariant,
	column_number: NonZeroUsize,
}

impl<'a> Token<'a> {
	/// Parses a token from a string starting with a token in text form, returns:
	/// * `Ok(Some((token, rest of string with token removed)))` if a token could be found at the start of the string.
	/// * `Ok(None)` if the end of line or a `rem` remark was found.
	/// * `Err(error)` if the text was malformed.
	pub fn parse_token(line_starting_with_token: &'a str, column_number: NonZeroUsize) -> Result<Option<(Self, &'a str)>, Error> {
		// Remove prefix whitespaces
		let column_number = column_number.saturating_add(line_starting_with_token.chars().take_while(|chr| chr.is_ascii_whitespace()).count());
		let line_starting_with_token = line_starting_with_token.trim_start_matches(|chr: char| chr.is_ascii_whitespace());
		// Return if we are at the end of the non-comment portion of the line
		if line_starting_with_token.is_empty() || !line_starting_with_token.get(0..=2).is_none_or(|st| !st.eq_ignore_ascii_case("rem")) {
			return Ok(None);
		}
		// Parse token
		let first_char = line_starting_with_token.chars().next().unwrap();
		let (token_length_in_bytes, variant) = match first_char {
			// Operator
			'+' | '-' | '/' | '*' | '↑' | '<' | '=' | '>' => (
				line_starting_with_token.find(|chr| !matches!(chr, '/' | '*' | '↑' | '<' | '=' | '>')).unwrap_or_else(|| line_starting_with_token.len()),
				TokenVariant::Operator
			),
			// Identifier
			chr if chr.is_ascii_alphabetic() => (
				// TODO: Ending in a type specifier char
				line_starting_with_token.find(|chr: char| !(chr.is_alphanumeric() || chr == '_')).unwrap_or_else(|| line_starting_with_token.len()),
				TokenVariant::Identifier
			),
			// Numeric literal
			chr if chr.is_ascii_alphabetic() => (
				// TODO: Starting with a base char
				line_starting_with_token.find(|chr: char| !(chr.is_alphanumeric() || chr == '_' || chr == '.')).unwrap_or_else(|| line_starting_with_token.len()),
				TokenVariant::NumericLiteral
			),
			'(' | ')' | ':' | ',' | ';' => (1, TokenVariant::Separator),
			// TODO
			_ => return Err(Error::NotYetImplemented(None, column_number, "Other tokens".into()))
		};
		// Return
		Ok(Some((Self {
			value: &line_starting_with_token[..token_length_in_bytes],
			column_number,
			variant,
		}, &line_starting_with_token[token_length_in_bytes..])))
	}

	/// Takes in a line of basic code in text form. Converts it into a line number and a list of (column number, token) pairs.
	pub fn parse_line(line: &str) -> Result<(Option<BigInt>, Box<[(NonZeroUsize, Token)]>), Error> {
		todo!()
	}
}

pub enum TokenVariant {
	Operator,
	Separator,
	NumericLiteral,
	StringLiteral,
	Identifier,
}