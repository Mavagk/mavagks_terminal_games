use std::num::NonZeroUsize;

use num::BigInt;

use crate::mavagk_basic::error::Error;

//const fn is_identifier_or_numeric_literal(chr: char) -> bool {
//	matches!(chr, '#' | '$' | '%' | '?' | '_' | '.') || chr.is_ascii_alphanumeric()
//}

#[derive(Debug)]
pub enum Token<'a> {
	Operator(&'a str),
	StringLiteral(&'a str),
	Identifier{ name: &'a str, identifier_type: IdentifierType, is_optional: bool },
	NumericLiteral(&'a str),
	LeftParenthesis,
	RightParenthesis,
	Comma,
	Colon,
	Semicolon,
	SingleQuestionMark,
}

impl<'a> Token<'a> {
	/// Parses a token from a string starting with a token in text form, returns:
	/// * `Ok(Some((token, rest of string with token removed)))` if a token could be found at the start of the string.
	/// * `Ok(None)` if the end of line or a `rem` remark was found.
	/// * `Err(error)` if the text was malformed.
	pub fn parse_token(line_starting_with_token: &'a str, column_number: NonZeroUsize) -> Result<Option<(NonZeroUsize, Self, &'a str)>, Error> {
		// Remove prefix whitespaces
		let column_number = column_number.saturating_add(line_starting_with_token.chars().take_while(|chr| chr.is_ascii_whitespace()).count());
		let line_starting_with_token = line_starting_with_token.trim_start_matches(|chr: char| chr.is_ascii_whitespace());
		// Return if we are at the end of the non-comment portion of the line
		if line_starting_with_token.is_empty() || !line_starting_with_token.get(0..=2).is_none_or(|st| !st.eq_ignore_ascii_case("rem")) {
			return Ok(None);
		}
		// Parse token
		let first_char = line_starting_with_token.chars().next().unwrap();
		let (token, rest_of_string_with_token_removed) = match first_char {
			// Operator
			'+' | '-' | '/' | '*' | '↑' | '<' | '=' | '>' | '&' => {
				let length_of_token_in_bytes = match line_starting_with_token[1..].find(|chr| !matches!(chr, '/' | '*' | '↑' | '<' | '=' | '>')) {
					Some(length_of_token_in_bytes) => length_of_token_in_bytes + 1,
					None => line_starting_with_token.len(),
				};
				let (token_string, rest_of_string_with_token_removed) = line_starting_with_token.split_at(length_of_token_in_bytes);
				(Token::Operator(token_string), rest_of_string_with_token_removed)
			}
			// Separators
			'(' => (Token::LeftParenthesis, &line_starting_with_token[1..]),
			')' => (Token::RightParenthesis, &line_starting_with_token[1..]),
			',' => (Token::Comma, &line_starting_with_token[1..]),
			';' => (Token::Semicolon, &line_starting_with_token[1..]),
			':' => (Token::Colon, &line_starting_with_token[1..]),
			'?' => (Token::SingleQuestionMark, &line_starting_with_token[1..]),
			// Identifier
			'a'..='z' | 'A'..='Z' | '_' => {
				// TODO: Types
				let length_of_token_in_bytes = line_starting_with_token.find(|chr| matches!(chr, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')).unwrap_or_else(|| line_starting_with_token.len());
				let (token_string, rest_of_string_with_token_removed) = line_starting_with_token.split_at(length_of_token_in_bytes);
				(Token::Identifier { name: token_string, identifier_type: IdentifierType::Number, is_optional: false }, rest_of_string_with_token_removed)
			}
			// Numeric literal
			'0'..='9' | '.' => {
				// TODO: E
				// TODO: Second '.' is a new literal
				let length_of_token_in_bytes = line_starting_with_token.find(|chr| matches!(chr, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '.')).unwrap_or_else(|| line_starting_with_token.len());
				let (token_string, rest_of_string_with_token_removed) = line_starting_with_token.split_at(length_of_token_in_bytes);
				(Token::NumericLiteral(token_string), rest_of_string_with_token_removed)
			}
			// TODO: Quoteless string literals in DATA statements
			// TODO
			_ => return Err(Error::NotYetImplemented(None, column_number, "Other tokens".into()))
		};
		// Return
		Ok(Some((column_number, token, rest_of_string_with_token_removed)))
	}

	/// Takes in a line of basic code in text form. Converts it into a line number and a list of (column number, token) pairs.
	pub fn parse_line(mut line: &'a str) -> Result<(Option<BigInt>, Box<[(NonZeroUsize, Self)]>), Error> {
		// Get line number
		let mut column_number: NonZeroUsize = 1.try_into().unwrap();
		let line_number = match line.chars().next() {
			Some('0'..='9' | '-') => {
				let length_of_line_number = line.find(|chr| !matches!(chr, '0'..='9' | '-')).unwrap_or_else(|| line.len());
				let line_number_string;
				(line_number_string, line) = line.split_at(length_of_line_number);
				column_number = column_number.saturating_add(length_of_line_number);
				Some(line_number_string.parse::<BigInt>().map_err(|_| Error::MalformedLineNumber(1.try_into().unwrap(), line_number_string.into()))?)
			}
			_ => None,
		};
		// Parse tokens
		let mut tokens = Vec::new();
		loop {
			let (token_column_number, token, remaining_string) = match Self::parse_token(line, column_number)? {
				None => break,
				Some(result) => result,
			};
			let token_length_in_bytes = line.len() - remaining_string.len();
			column_number = column_number.saturating_add(line[..token_length_in_bytes].chars().count());
			line = remaining_string;
			tokens.push((token_column_number, token));
		}
		// Return
		Ok((line_number, tokens.into()))
	}
}

#[derive(Debug)]
pub enum IdentifierType {
	Number,
	String,
	Integer,
	Boolean,
	ComplexNumber,
}