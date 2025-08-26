use std::num::NonZeroUsize;

use num::BigInt;

use crate::mavagk_basic::error::Error;

#[derive(Debug, PartialEq, Eq)]
pub struct Token<'a> {
	pub variant: TokenVariant<'a>,
	pub start_column: NonZeroUsize,
	pub end_column: NonZeroUsize,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenVariant<'a> {
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
	fn parse_token_from_str(line_starting_with_token: &'a str, column_number: NonZeroUsize) -> Result<Option<(Self, &'a str)>, Error> {
		// Remove prefix whitespaces
		let start_column = column_number.saturating_add(line_starting_with_token.chars().take_while(|chr| chr.is_ascii_whitespace()).count());
		let line_starting_with_token = line_starting_with_token.trim_start_matches(|chr: char| chr.is_ascii_whitespace());
		// Return if we are at the end of the non-comment portion of the line
		if line_starting_with_token.is_empty() || !line_starting_with_token.get(0..=2).is_none_or(|st| !st.eq_ignore_ascii_case("rem")) {
			return Ok(None);
		}
		// Parse token
		let first_char = line_starting_with_token.chars().next().unwrap();
		let (variant, rest_of_string_with_token_removed) = match first_char {
			// Operator
			'+' | '-' | '/' | '*' | '↑' | '<' | '=' | '>' | '&' | '^' => {
				let length_of_token_in_bytes = match line_starting_with_token[1..].find(|chr| !matches!(chr, '/' | '<' | '=' | '>')) {
					Some(length_of_token_in_bytes) => length_of_token_in_bytes + 1,
					None => line_starting_with_token.len(),
				};
				let (token_string, rest_of_string_with_token_removed) = line_starting_with_token.split_at(length_of_token_in_bytes);
				(TokenVariant::Operator(token_string), rest_of_string_with_token_removed)
			}
			// Separators
			'(' => (TokenVariant::LeftParenthesis, &line_starting_with_token[1..]),
			')' => (TokenVariant::RightParenthesis, &line_starting_with_token[1..]),
			',' => (TokenVariant::Comma, &line_starting_with_token[1..]),
			';' => (TokenVariant::Semicolon, &line_starting_with_token[1..]),
			':' => (TokenVariant::Colon, &line_starting_with_token[1..]),
			'?' => (TokenVariant::SingleQuestionMark, &line_starting_with_token[1..]),
			// Identifier
			'a'..='z' | 'A'..='Z' | '_' => {
				// Get name part of identifier
				let length_of_identifier_name_in_bytes = line_starting_with_token.find(|chr| !matches!(chr, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')).unwrap_or_else(|| line_starting_with_token.len());
				let (name, string_with_name_removed) = line_starting_with_token.split_at(length_of_identifier_name_in_bytes);
				// Get type
				match string_with_name_removed {
					_ if string_with_name_removed.starts_with("$?") => (TokenVariant::Identifier { name, identifier_type: IdentifierType::String, is_optional: true }, &string_with_name_removed[2..]),
					_ if string_with_name_removed.starts_with("$") => (TokenVariant::Identifier { name, identifier_type: IdentifierType::String, is_optional: false }, &string_with_name_removed[1..]),
					_ if string_with_name_removed.starts_with("%?") => (TokenVariant::Identifier { name, identifier_type: IdentifierType::Integer, is_optional: true }, &string_with_name_removed[2..]),
					_ if string_with_name_removed.starts_with("%") => (TokenVariant::Identifier { name, identifier_type: IdentifierType::Integer, is_optional: false }, &string_with_name_removed[1..]),
					_ if string_with_name_removed.starts_with("#?") => (TokenVariant::Identifier { name, identifier_type: IdentifierType::ComplexNumber, is_optional: true }, &string_with_name_removed[2..]),
					_ if string_with_name_removed.starts_with("#") => (TokenVariant::Identifier { name, identifier_type: IdentifierType::ComplexNumber, is_optional: false }, &string_with_name_removed[1..]),
					_ if string_with_name_removed.starts_with("?") => (TokenVariant::Identifier { name, identifier_type: IdentifierType::UnmarkedNumber, is_optional: true }, string_with_name_removed),
					_ => (TokenVariant::Identifier { name, identifier_type: IdentifierType::UnmarkedNumber, is_optional: false }, string_with_name_removed),
				}
			}
			// Numeric literal
			'0'..='9' | '.' | '$' | '%' => {
				// TODO: E
				// TODO: Second '.' is a new literal
				let length_of_token_in_bytes = match line_starting_with_token[1..].find(|chr| !matches!(chr, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '.')) {
					Some(length_of_token_in_bytes) => length_of_token_in_bytes + 1,
					None => line_starting_with_token.len(),
				};
				let (token_string, rest_of_string_with_token_removed) = line_starting_with_token.split_at(length_of_token_in_bytes);
				(TokenVariant::NumericLiteral(token_string), rest_of_string_with_token_removed)
			}
			// Strings
			'"' => {
				match line_starting_with_token[1..].find('"') {
					Some(double_quote_index_in_bytes) => (TokenVariant::StringLiteral(&line_starting_with_token[1..double_quote_index_in_bytes + 1]), &line_starting_with_token[double_quote_index_in_bytes + 2..]),
					None => (TokenVariant::StringLiteral(&line_starting_with_token[1..]), ""),
				}
			}
			// TODO: Quoteless string literals in DATA statements
			// TODO
			_ => return Err(Error::NotYetImplemented(None, start_column, "Other tokens".into()))
		};
		// Return
		let token_length_in_bytes = line_starting_with_token.len() - rest_of_string_with_token_removed.len();
		let token_length_in_chars = line_starting_with_token[..token_length_in_bytes].chars().count();
		Ok(Some((Self { variant, start_column, end_column: start_column.saturating_add(token_length_in_chars) }, rest_of_string_with_token_removed)))
	}

	/// Takes in a line of basic code in text form. Converts it into a list of tokens.
	pub fn tokenize_line(mut line: &'a str) -> Result<(Option<BigInt>, Box<[Self]>), Error> {
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
			let (token, remaining_string) = match Self::parse_token_from_str(line, column_number)? {
				None => break,
				Some(result) => result,
			};
			column_number = token.end_column;
			//let token_length_in_bytes = line.len() - remaining_string.len();
			//column_number = column_number.saturating_add(line[..token_length_in_bytes].chars().count());
			line = remaining_string;
			tokens.push(token);
		}
		// Return
		Ok((line_number, tokens.into()))
	}
}

impl<'a> TokenVariant<'a> {
	pub fn is_binary_operator(&self) -> bool {
		match self {
			TokenVariant::Operator(..) => true,
			TokenVariant::Identifier { name, identifier_type: IdentifierType::UnmarkedNumber, is_optional: false } => name.eq_ignore_ascii_case("and") || name.eq_ignore_ascii_case("or"),
			_ => false,
		}
	}

	pub fn is_unary_operator(&self) -> bool {
		match self {
			TokenVariant::Operator("-" | "+") => true,
			TokenVariant::Identifier { name, identifier_type: IdentifierType::UnmarkedNumber, is_optional: false } => name.eq_ignore_ascii_case("not"),
			_ => false,
		}
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum IdentifierType {
	UnmarkedNumber,
	String,
	Integer,
	ComplexNumber,
}