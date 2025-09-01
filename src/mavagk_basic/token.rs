use std::num::NonZeroUsize;

use num::{BigInt, Num};
use strum_macros::EnumIter;
use strum::IntoEnumIterator;

use crate::mavagk_basic::{abstract_syntax_tree::{BinaryOperator, UnaryOperator}, error::{Error, ErrorVariant}};

#[derive(Debug, PartialEq)]
pub struct Token<'a> {
	pub variant: TokenVariant<'a>,
	pub start_column: NonZeroUsize,
	pub end_column: NonZeroUsize,
}

#[derive(Debug, PartialEq)]
pub enum TokenVariant<'a> {
	Operator(Option<BinaryOperator>, Option<UnaryOperator>),
	StringLiteral(&'a str),
	Identifier{ name: &'a str, identifier_type: IdentifierType, is_optional: bool, binary_operator: Option<BinaryOperator>, unary_operator: Option<UnaryOperator>, keyword: Option<Keyword> },
	IntegerLiteral(BigInt),
	FloatLiteral { value: f64, is_imaginary: bool },
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
	pub fn parse_single_token_from_str(line_starting_with_token: &'a str, column_number: NonZeroUsize, line_number: Option<&BigInt>) -> Result<Option<(Self, &'a str)>, Error> {
		// Remove prefix whitespaces
		let start_column = column_number.saturating_add(line_starting_with_token.chars().take_while(|chr| chr.is_ascii_whitespace()).count());
		let line_starting_with_token = line_starting_with_token.trim_start_matches(|chr: char| chr.is_ascii_whitespace());
		// Return if we are at the end of the non-comment portion of the line
		if line_starting_with_token.is_empty() || !line_starting_with_token.get(0..=2).is_none_or(|chars| !chars.eq_ignore_ascii_case("rem")) {
			return Ok(None);
		}
		// Parse token
		let first_char = line_starting_with_token.chars().next().unwrap();
		let (variant, rest_of_string_with_token_removed) = match first_char {
			// Operators
			'+' | '-' | '/' | '*' | '↑' | '<' | '=' | '>' | '&' | '^' | '\\' => {
				// Get the operator chars
				let length_of_token_in_bytes = match line_starting_with_token[1..].find(|chr| !matches!(chr, '/' | '<' | '=' | '>')) {
					Some(length_of_token_in_bytes) => length_of_token_in_bytes + 1,
					None => line_starting_with_token.len(),
				};
				let (token_string, rest_of_string_with_token_removed) = line_starting_with_token.split_at(length_of_token_in_bytes);
				// Decide what operator symbol this is
				let binary_operator = BinaryOperator::from_symbol(token_string);
				let unary_operator = UnaryOperator::from_symbol(token_string);
				if binary_operator.is_none() && unary_operator.is_none() {
					return Err(Error { variant: ErrorVariant::InvalidOperatorSymbol, line_number: line_number.cloned(), column_number: Some(start_column) });
				}
				(TokenVariant::Operator(binary_operator, unary_operator), rest_of_string_with_token_removed)
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
				let (identifier_type, is_optional, rest_of_string_with_token_removed) = match string_with_name_removed {
					_ if string_with_name_removed.starts_with("$?") => (IdentifierType::String,         true,  &string_with_name_removed[2..]),
					_ if string_with_name_removed.starts_with("$")  => (IdentifierType::String,         false, &string_with_name_removed[1..]),
					_ if string_with_name_removed.starts_with("%?") => (IdentifierType::Integer,        true,  &string_with_name_removed[2..]),
					_ if string_with_name_removed.starts_with("%")  => (IdentifierType::Integer,        false, &string_with_name_removed[1..]),
					_ if string_with_name_removed.starts_with("#?") => (IdentifierType::ComplexNumber,  true,  &string_with_name_removed[2..]),
					_ if string_with_name_removed.starts_with("#")  => (IdentifierType::ComplexNumber,  false, &string_with_name_removed[1..]),
					_ if string_with_name_removed.starts_with("?")  => (IdentifierType::UnmarkedNumber, true,  &string_with_name_removed[1..]),
					_                                               => (IdentifierType::UnmarkedNumber, false, string_with_name_removed),
				};
				// Get if the token is an alphabetic operator
				let (binary_operator, unary_operator) = match identifier_type {
					IdentifierType::UnmarkedNumber if !is_optional => (BinaryOperator::from_name(name), UnaryOperator::from_name(name)),
					_ => (None, None),
				};
				// Get if the identifier is a keyword
				let keyword = match is_optional {
					false => Keyword::from_name(name, identifier_type),
					true => None,
				};
				// Assemble into token
				(TokenVariant::Identifier { name, identifier_type, is_optional, binary_operator, unary_operator, keyword }, rest_of_string_with_token_removed)
			}
			// Numeric literal
			'0'..='9' | '.' | '$' | '%' => {
				let mut length_of_token_in_bytes = 0;
				let mut line_after_token_read = line_starting_with_token;
				// Read the number base
				let mut base = NumericBase::Decimal;
				if line_after_token_read.starts_with(|first_char| matches!(first_char, '$' | '%')) {
					length_of_token_in_bytes += 1;
					let base_string;
					(base_string, line_after_token_read) = line_after_token_read.split_at(1);
					base = match base_string {
						"$" => NumericBase::Hexadecimal,
						"%" => NumericBase::Binary,
						_ => unreachable!(),
					}
				}
				// Read integer part
				let length_of_integer_part = match line_after_token_read.find(|chr| !base.is_digit(chr)) {
					Some(length_of_integer_part) => length_of_integer_part,
					None => line_after_token_read.len(),
				};
				length_of_token_in_bytes += length_of_integer_part;
				let integer_part;
				(integer_part, line_after_token_read) = line_after_token_read.split_at(length_of_integer_part);
				// Read decimal point
				if line_after_token_read.starts_with(|first_char| matches!(first_char, '.')) {
					length_of_token_in_bytes += 1;
					line_after_token_read = &line_after_token_read[1..];
				}
				// Read fractional part
				let length_of_fractional_part = match line_after_token_read.find(|chr| !base.is_digit(chr)) {
					Some(length_of_integer_part) => length_of_integer_part,
					None => line_after_token_read.len(),
				};
				length_of_token_in_bytes += length_of_fractional_part;
				let fractional_part;
				(fractional_part, line_after_token_read) = line_after_token_read.split_at(length_of_fractional_part);
				let fractional_part = fractional_part.trim_end_matches('0');
				// Read exrad
				let mut exponent = BigInt::ZERO;
				if base != NumericBase::Hexadecimal && line_after_token_read.starts_with(|first_char| matches!(first_char, 'e' | 'E')) {
					length_of_token_in_bytes += 1;
					line_after_token_read = &line_after_token_read[1..];
					// Read exponent sign
					let mut exponent_is_negative = false;
					if line_after_token_read.starts_with(|first_char| matches!(first_char, '-' | '+')) {
						if line_after_token_read.chars().next().unwrap() == '-' {
							exponent_is_negative = true;
						}
						length_of_token_in_bytes += 1;
						line_after_token_read = &line_after_token_read[1..];
					}
					// Read exponent integer
					let length_of_exponent_integer = match line_after_token_read.find(|chr| !base.is_digit(chr)) {
						Some(length_of_integer_part) => length_of_integer_part,
						None => line_after_token_read.len(),
					};
					length_of_token_in_bytes += length_of_exponent_integer;
					let exponent_after_sign;
					(exponent_after_sign, line_after_token_read) = line_after_token_read.split_at(length_of_exponent_integer);
					// Convert exponent parts to big int
					let exponent_being_constructed: BigInt = match BigInt::from_str_radix(exponent_after_sign, base.radix() as u32) {
						Err(..) => BigInt::ZERO,
						Ok(exrad_being_constructed) => exrad_being_constructed,
					};
					exponent = match exponent_is_negative {
						true => -exponent_being_constructed,
						false => exponent_being_constructed,
					};
				}
				// Read imaginary multiplier
				let mut is_imaginary = false;
				if line_after_token_read.starts_with(|first_char| matches!(first_char, 'i' | 'I')) {
					is_imaginary = true;
					length_of_token_in_bytes += 1;
				}
				// Convert the parts of the numeric literal we read to a token
				match !is_imaginary && exponent == BigInt::ZERO && fractional_part.len() == 0 {
					// If the number should be an integer
					true => {
						let value = match BigInt::from_str_radix(integer_part, base.radix() as u32) {
							Ok(value) => value,
							_ => BigInt::ZERO,
						};
						(TokenVariant::IntegerLiteral(value), &line_starting_with_token[length_of_token_in_bytes..])
					}
					// If the number should be a float
					false => {
						let exponent: i32 = match (&exponent).try_into() {
							Ok(exponent) => exponent,
							Err(_) => match exponent > i32::MAX.into() {
								true => i32::MAX,
								false => i32::MIN,
							}
						};
						let value = match f64::from_str_radix(format!("{integer_part}.{fractional_part}").as_str(), base.radix() as u32) {
							Ok(value) => value,
							_ => 0.,
						} * (base.radix() as f64).powi(exponent);
						(TokenVariant::FloatLiteral { value, is_imaginary }, &line_starting_with_token[length_of_token_in_bytes..])
					}
				}
			}
			// Strings
			'"' => {
				match line_starting_with_token[1..].find('"') {
					Some(double_quote_index_in_bytes) =>
						(TokenVariant::StringLiteral(&line_starting_with_token[1..double_quote_index_in_bytes + 1]), &line_starting_with_token[double_quote_index_in_bytes + 2..]),
					None => (TokenVariant::StringLiteral(&line_starting_with_token[1..]), ""),
				}
			}
			// TODO: Quoteless string literals in DATA statements
			_ => return Err(Error { variant: ErrorVariant::InvalidToken, line_number: line_number.cloned(), column_number: Some(start_column) })
		};
		// Get the end column of the char
		let token_length_in_bytes = line_starting_with_token.len() - rest_of_string_with_token_removed.len();
		let token_length_in_chars = line_starting_with_token[..token_length_in_bytes].chars().count();
		let end_column = start_column.saturating_add(token_length_in_chars);
		// Return
		Ok(Some((Self { variant, start_column, end_column }, rest_of_string_with_token_removed)))
	}

	/// Takes in a line of basic code in text form. Converts it into a (line number, list of tokens) pair.
	pub fn tokenize_line(mut line_text: &'a str) -> Result<(Option<BigInt>, Box<[Self]>), Error> {
		// Get line number
		let mut column_number: NonZeroUsize = 1.try_into().unwrap();
		let line_number = match line_text.chars().next() {
			Some('0'..='9' | '-') => {
				let length_of_line_number = line_text.find(|chr| !matches!(chr, '0'..='9' | '-')).unwrap_or_else(|| line_text.len());
				let line_number_string;
				(line_number_string, line_text) = line_text.split_at(length_of_line_number);
				column_number = column_number.saturating_add(length_of_line_number);
				Some(line_number_string.parse::<BigInt>().map_err(|_| Error { variant: ErrorVariant::MalformedLineNumber(line_number_string.into()), line_number: None, column_number: Some(column_number) })?)
			}
			_ => None,
		};
		// Parse tokens from line until there are none left
		let mut tokens = Vec::new();
		loop {
			let (token, remaining_string) = match Self::parse_single_token_from_str(line_text, column_number, line_number.as_ref())? {
				None => break,
				Some(result) => result,
			};
			column_number = token.end_column;
			line_text = remaining_string;
			tokens.push(token);
		}
		// Return
		Ok((line_number, tokens.into()))
	}
}

impl<'a> TokenVariant<'a> {
	pub fn is_binary_operator(&self) -> bool {
		match self {
			TokenVariant::Operator(Some(..), _) => true,
			TokenVariant::Identifier { binary_operator: Some(_), .. } => true,
			_ => false,
		}
	}

	pub fn is_unary_operator(&self) -> bool {
		match self {
			TokenVariant::Operator(_, Some(..)) => true,
			TokenVariant::Identifier { unary_operator: Some(_), .. } => true,
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum NumericBase {
	Decimal,
	Hexadecimal,
	Binary,
}

impl NumericBase {
	const fn is_digit(self, digit: char) -> bool {
		match self {
			Self::Decimal => digit.is_ascii_digit(),
			Self::Hexadecimal => digit.is_ascii_hexdigit(),
			Self::Binary => matches!(digit, '0' | '1'),
		}
	}

	const fn radix(self) -> u8 {
		match self {
			NumericBase::Decimal => 10,
			NumericBase::Binary => 2,
			NumericBase::Hexadecimal => 16,
		}
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, EnumIter)]
pub enum Keyword {
	Print,
	Goto,
	Run,
	Gosub,
	Fn,
	Let,
}

impl Keyword {
	pub fn get_names(self) -> &'static [(&'static str, IdentifierType)] {
		match self {
			Self::Print => &[("PRINT", IdentifierType::UnmarkedNumber)],
			Self::Goto => &[("GOTO", IdentifierType::UnmarkedNumber)],
			Self::Run => &[("RUN", IdentifierType::UnmarkedNumber)],
			Self::Gosub => &[("GOSUB", IdentifierType::UnmarkedNumber)],
			Self::Fn => &[("FN", IdentifierType::UnmarkedNumber)],
			Self::Let => &[("LET", IdentifierType::UnmarkedNumber)],
		}
	}

	pub fn from_name(name: &str, identifier_type: IdentifierType) -> Option<Self> {
		for keyword in Self::iter() {
			for (keyword_name, keyword_identifier_type) in keyword.get_names() {
				if identifier_type == *keyword_identifier_type && name.eq_ignore_ascii_case(keyword_name) {
					return Some(keyword);
				}
			}
		}
		None
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_parse_single_token_from_str() {
		// Empty string
		assert_eq!(Token::parse_single_token_from_str("", 1.try_into().unwrap(), None).unwrap(), None);
		assert_eq!(Token::parse_single_token_from_str(" ", 1.try_into().unwrap(), None).unwrap(), None);
		assert_eq!(Token::parse_single_token_from_str("rem", 1.try_into().unwrap(), None).unwrap(), None);
		assert_eq!(Token::parse_single_token_from_str("ReM stuff after REM", 1.try_into().unwrap(), None).unwrap(), None);
		assert_eq!(Token::parse_single_token_from_str("	ReM stuff after REM", 1.try_into().unwrap(), None).unwrap(), None);
		// Identifiers
		assert_eq!(
			Token::parse_single_token_from_str("a", 1.try_into().unwrap(), None).unwrap(),
			Some((Token {
				variant: TokenVariant::Identifier { name: "a", identifier_type: IdentifierType::UnmarkedNumber, is_optional: false, binary_operator: None, unary_operator: None, keyword: None },
				start_column: 1.try_into().unwrap(), end_column: 2.try_into().unwrap()
			}, ""))
		);
		assert_eq!(
			Token::parse_single_token_from_str("_num = 8.5", 1.try_into().unwrap(), None).unwrap(),
			Some((Token {
				variant: TokenVariant::Identifier { name: "_num", identifier_type: IdentifierType::UnmarkedNumber, is_optional: false, binary_operator: None, unary_operator: None, keyword: None },
				start_column: 1.try_into().unwrap(), end_column: 5.try_into().unwrap()
			}, " = 8.5"))
		);
		assert_eq!(
			Token::parse_single_token_from_str(" var$", 1.try_into().unwrap(), None).unwrap(),
			Some((Token {
				variant: TokenVariant::Identifier { name: "var", identifier_type: IdentifierType::String, is_optional: false, binary_operator: None, unary_operator: None, keyword: None },
				start_column: 2.try_into().unwrap(), end_column: 6.try_into().unwrap()
			}, ""))
		);
		assert_eq!(
			Token::parse_single_token_from_str("	my_iNt%0", 1.try_into().unwrap(), None).unwrap(),
			Some((Token {
				variant: TokenVariant::Identifier { name: "my_iNt", identifier_type: IdentifierType::Integer, is_optional: false, binary_operator: None, unary_operator: None, keyword: None },
				start_column: 2.try_into().unwrap(), end_column: 9.try_into().unwrap()
			}, "0"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("		 MyComp# = 2i", 1.try_into().unwrap(), None).unwrap(),
			Some((Token {
				variant: TokenVariant::Identifier { name: "MyComp", identifier_type: IdentifierType::ComplexNumber, is_optional: false, binary_operator: None, unary_operator: None, keyword: None },
				start_column: 4.try_into().unwrap(), end_column: 11.try_into().unwrap()
			}, " = 2i"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("a0?=2E5", 1.try_into().unwrap(), None).unwrap(),
			Some((Token {
				variant: TokenVariant::Identifier { name: "a0", identifier_type: IdentifierType::UnmarkedNumber, is_optional: true, binary_operator: None, unary_operator: None, keyword: None },
				start_column: 1.try_into().unwrap(), end_column: 4.try_into().unwrap()
			}, "=2E5"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("a%?(val)", 1.try_into().unwrap(), None).unwrap(),
			Some((Token {
				variant: TokenVariant::Identifier { name: "a", identifier_type: IdentifierType::Integer, is_optional: true, binary_operator: None, unary_operator: None, keyword: None },
				start_column: 1.try_into().unwrap(), end_column: 4.try_into().unwrap()
			}, "(val)"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("a%?(val)", 10.try_into().unwrap(), None).unwrap(),
			Some((Token {
				variant: TokenVariant::Identifier { name: "a", identifier_type: IdentifierType::Integer, is_optional: true, binary_operator: None, unary_operator: None, keyword: None },
				start_column: 10.try_into().unwrap(), end_column: 13.try_into().unwrap()
			}, "(val)"))
		);
		assert_eq!(
			Token::parse_single_token_from_str(" _aA_01Z29_$?(val)", 1.try_into().unwrap(), None).unwrap(),
			Some((Token {
				variant: TokenVariant::Identifier { name: "_aA_01Z29_", identifier_type: IdentifierType::String, is_optional: true, binary_operator: None, unary_operator: None, keyword: None },
				start_column: 2.try_into().unwrap(), end_column: 14.try_into().unwrap()
			}, "(val)"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("VAL#?(val)", 1.try_into().unwrap(), None).unwrap(),
			Some((Token {
				variant: TokenVariant::Identifier { name: "VAL", identifier_type: IdentifierType::ComplexNumber, is_optional: true, binary_operator: None, unary_operator: None, keyword: None },
				start_column: 1.try_into().unwrap(), end_column: 6.try_into().unwrap()
			}, "(val)"))
		);
		// Operators
		assert_eq!(
			Token::parse_single_token_from_str("++", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::Operator(Some(BinaryOperator::AdditionConcatenation), Some(UnaryOperator::UnaryPlus)), start_column: 1.try_into().unwrap(), end_column: 2.try_into().unwrap() }, "+"))
		);
		assert_eq!(
			Token::parse_single_token_from_str(" //+", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::Operator(Some(BinaryOperator::DoubleSlash), None), start_column: 2.try_into().unwrap(), end_column: 4.try_into().unwrap() }, "+"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("<=-", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::Operator(Some(BinaryOperator::LessThanOrEqualTo), None), start_column: 1.try_into().unwrap(), end_column: 3.try_into().unwrap() }, "-"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("&+", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::Operator(Some(BinaryOperator::Concatenation), None), start_column: 1.try_into().unwrap(), end_column: 2.try_into().unwrap() }, "+"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("=<", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::Operator(Some(BinaryOperator::LessThanOrEqualTo), None), start_column: 1.try_into().unwrap(), end_column: 3.try_into().unwrap() }, ""))
		);
		assert_eq!(
			Token::parse_single_token_from_str("/ /", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::Operator(Some(BinaryOperator::Division), None), start_column: 1.try_into().unwrap(), end_column: 2.try_into().unwrap() }, " /"))
		);
		// Separators
		assert_eq!(
			Token::parse_single_token_from_str("()", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::LeftParenthesis, start_column: 1.try_into().unwrap(), end_column: 2.try_into().unwrap() }, ")"))
		);
		assert_eq!(
			Token::parse_single_token_from_str(")", 2.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::RightParenthesis, start_column: 2.try_into().unwrap(), end_column: 3.try_into().unwrap() }, ""))
		);
		assert_eq!(
			Token::parse_single_token_from_str(" : ", 50.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::Colon, start_column: 51.try_into().unwrap(), end_column: 52.try_into().unwrap() }, " "))
		);
		// Integers
		assert_eq!(
			Token::parse_single_token_from_str(" 420 + 420", 50.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(420.into()), start_column: 51.try_into().unwrap(), end_column: 54.try_into().unwrap() }, " + 420"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("6a", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(6.into()), start_column: 1.try_into().unwrap(), end_column: 2.try_into().unwrap() }, "a"))
		);
		assert_eq!(
			Token::parse_single_token_from_str(" ..", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0.into()), start_column: 2.try_into().unwrap(), end_column: 3.try_into().unwrap() }, "."))
		);
		assert_eq!(
			Token::parse_single_token_from_str(" .0.", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0.into()), start_column: 2.try_into().unwrap(), end_column: 4.try_into().unwrap() }, "."))
		);
		assert_eq!(
			Token::parse_single_token_from_str("6.00000k", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(6.into()), start_column: 1.try_into().unwrap(), end_column: 8.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("60.00E+0k", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(60.into()), start_column: 1.try_into().unwrap(), end_column: 9.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("00060.00E+0k", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(60.into()), start_column: 1.try_into().unwrap(), end_column: 12.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("60.00E+000k", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(60.into()), start_column: 1.try_into().unwrap(), end_column: 11.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("60.00E-0k", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(60.into()), start_column: 1.try_into().unwrap(), end_column: 9.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("60.00E+k", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(60.into()), start_column: 1.try_into().unwrap(), end_column: 8.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("60.00E-k", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(60.into()), start_column: 1.try_into().unwrap(), end_column: 8.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("60.00Ek", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(60.into()), start_column: 1.try_into().unwrap(), end_column: 7.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("$7Ek", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0x7E.into()), start_column: 1.try_into().unwrap(), end_column: 4.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("$7ek", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0x7E.into()), start_column: 1.try_into().unwrap(), end_column: 4.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("$k", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0x0.into()), start_column: 1.try_into().unwrap(), end_column: 2.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("$", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0x0.into()), start_column: 1.try_into().unwrap(), end_column: 2.try_into().unwrap() }, ""))
		);
		assert_eq!(
			Token::parse_single_token_from_str("$7F.k", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0x7F.into()), start_column: 1.try_into().unwrap(), end_column: 5.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("$.0k", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0x0.into()), start_column: 1.try_into().unwrap(), end_column: 4.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("$10.0k", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0x10.into()), start_column: 1.try_into().unwrap(), end_column: 6.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("$.", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0x0.into()), start_column: 1.try_into().unwrap(), end_column: 3.try_into().unwrap() }, ""))
		);
		assert_eq!(
			Token::parse_single_token_from_str("%1001k", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0b1001.into()), start_column: 1.try_into().unwrap(), end_column: 6.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("%k", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0b0.into()), start_column: 1.try_into().unwrap(), end_column: 2.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("%", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0b0.into()), start_column: 1.try_into().unwrap(), end_column: 2.try_into().unwrap() }, ""))
		);
		assert_eq!(
			Token::parse_single_token_from_str("%10.k", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0b10.into()), start_column: 1.try_into().unwrap(), end_column: 5.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("%.0k", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0b0.into()), start_column: 1.try_into().unwrap(), end_column: 4.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("%10.0k", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0b10.into()), start_column: 1.try_into().unwrap(), end_column: 6.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("%00010.0k", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0b10.into()), start_column: 1.try_into().unwrap(), end_column: 9.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("%.", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0b0.into()), start_column: 1.try_into().unwrap(), end_column: 3.try_into().unwrap() }, ""))
		);
		assert_eq!(
			Token::parse_single_token_from_str("%.2", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0b0.into()), start_column: 1.try_into().unwrap(), end_column: 3.try_into().unwrap() }, "2"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("%0010E+", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0b10.into()), start_column: 1.try_into().unwrap(), end_column: 8.try_into().unwrap() }, ""))
		);
		assert_eq!(
			Token::parse_single_token_from_str("%0010E+2", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0b10.into()), start_column: 1.try_into().unwrap(), end_column: 8.try_into().unwrap() }, "2"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("%001050", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0b10.into()), start_column: 1.try_into().unwrap(), end_column: 6.try_into().unwrap() }, "50"))
		);
		// Floats
		assert_eq!(
			Token::parse_single_token_from_str("2.5", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::FloatLiteral { value: 2.5, is_imaginary: false }, start_column: 1.try_into().unwrap(), end_column: 4.try_into().unwrap() }, ""))
		);
		assert_eq!(
			Token::parse_single_token_from_str("2.5i", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::FloatLiteral { value: 2.5, is_imaginary: true }, start_column: 1.try_into().unwrap(), end_column: 5.try_into().unwrap() }, ""))
		);
		assert_eq!(
			Token::parse_single_token_from_str("2.5000k", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::FloatLiteral { value: 2.5, is_imaginary: false }, start_column: 1.try_into().unwrap(), end_column: 7.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str(".000760k", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::FloatLiteral { value: 0.00076, is_imaginary: false }, start_column: 1.try_into().unwrap(), end_column: 8.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("00900.000760k", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::FloatLiteral { value: 900.00076, is_imaginary: false }, start_column: 1.try_into().unwrap(), end_column: 13.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("040.0777E3k", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::FloatLiteral { value: 040.0777E3, is_imaginary: false }, start_column: 1.try_into().unwrap(), end_column: 11.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("040.0777e+3.k", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::FloatLiteral { value: 040.0777E3, is_imaginary: false }, start_column: 1.try_into().unwrap(), end_column: 12.try_into().unwrap() }, ".k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("040.0777E+3I.k", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::FloatLiteral { value: 040.0777E3, is_imaginary: true }, start_column: 1.try_into().unwrap(), end_column: 13.try_into().unwrap() }, ".k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("040.0777E-3.k", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::FloatLiteral { value: 040.0777E-3, is_imaginary: false }, start_column: 1.try_into().unwrap(), end_column: 12.try_into().unwrap() }, ".k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("%010.010", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::FloatLiteral { value: 2.25, is_imaginary: false }, start_column: 1.try_into().unwrap(), end_column: 9.try_into().unwrap() }, ""))
		);
		assert_eq!(
			Token::parse_single_token_from_str("%0.10010E10", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::FloatLiteral { value: 2.25, is_imaginary: false }, start_column: 1.try_into().unwrap(), end_column: 12.try_into().unwrap() }, ""))
		);
		// Strings
		assert_eq!(
			Token::parse_single_token_from_str(" \"\"a", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::StringLiteral(""), start_column: 2.try_into().unwrap(), end_column: 4.try_into().unwrap() }, "a"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("\"REM ±Hello\"a", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::StringLiteral("REM ±Hello"), start_column: 1.try_into().unwrap(), end_column: 13.try_into().unwrap() }, "a"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("\"REM ±Hello\"a\"", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::StringLiteral("REM ±Hello"), start_column: 1.try_into().unwrap(), end_column: 13.try_into().unwrap() }, "a\""))
		);
		assert_eq!(
			Token::parse_single_token_from_str("\"REM ±Hello", 1.try_into().unwrap(), None).unwrap(),
			Some((Token { variant: TokenVariant::StringLiteral("REM ±Hello"), start_column: 1.try_into().unwrap(), end_column: 12.try_into().unwrap() }, ""))
		);
	}

	#[test]
	fn test_tokenize_line() {
		let tokens = Token::tokenize_line("10 PRINT 10 + 8").unwrap();
		assert_eq!(tokens.0, Some(10.into()));
		assert_eq!(tokens.1.len(), 4);
		assert_eq!(tokens.1[0], Token {
			variant: TokenVariant::Identifier {
				name: "PRINT", identifier_type: IdentifierType::UnmarkedNumber, is_optional: false, binary_operator: None, unary_operator: None, keyword: Some(Keyword::Print)
			}, start_column: 4.try_into().unwrap(), end_column: 9.try_into().unwrap()
		});
		assert_eq!(tokens.1[1], Token {
			variant: TokenVariant::IntegerLiteral(10.into()), start_column: 10.try_into().unwrap(), end_column: 12.try_into().unwrap()
		});
		assert_eq!(tokens.1[2], Token {
			variant: TokenVariant::Operator(Some(BinaryOperator::AdditionConcatenation), Some(UnaryOperator::UnaryPlus)), start_column: 13.try_into().unwrap(), end_column: 14.try_into().unwrap()
		});
		assert_eq!(tokens.1[3], Token {
			variant: TokenVariant::IntegerLiteral(8.into()), start_column: 15.try_into().unwrap(), end_column: 16.try_into().unwrap()
		});

		let tokens = Token::tokenize_line("PRINT 10 + 8").unwrap();
		assert_eq!(tokens.0, None);
		assert_eq!(tokens.1.len(), 4);
		assert_eq!(tokens.1[0], Token {
			variant: TokenVariant::Identifier {
				name: "PRINT", identifier_type: IdentifierType::UnmarkedNumber, is_optional: false, binary_operator: None, unary_operator: None, keyword: Some(Keyword::Print)
			}, start_column: 1.try_into().unwrap(), end_column: 6.try_into().unwrap()
		});
		assert_eq!(tokens.1[1], Token {
			variant: TokenVariant::IntegerLiteral(10.into()), start_column: 7.try_into().unwrap(), end_column: 9.try_into().unwrap()
		});
		assert_eq!(tokens.1[2], Token {
			variant: TokenVariant::Operator(Some(BinaryOperator::AdditionConcatenation), Some(UnaryOperator::UnaryPlus)), start_column: 10.try_into().unwrap(), end_column: 11.try_into().unwrap()
		});
		assert_eq!(tokens.1[3], Token {
			variant: TokenVariant::IntegerLiteral(8.into()), start_column: 12.try_into().unwrap(), end_column: 13.try_into().unwrap()
		});
	}
}