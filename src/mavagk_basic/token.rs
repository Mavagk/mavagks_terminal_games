use std::num::NonZeroUsize;

use num::{BigInt, BigUint, Num};
use strum_macros::EnumIter;
use strum::IntoEnumIterator;

use crate::mavagk_basic::{abstract_syntax_tree::Datum, error::{Error, ErrorVariant}};

#[derive(Debug, PartialEq)]
/// A token received from parsing a line of text
pub struct Token<'a> {
	/// The token variant, eg. numeric literal, identifier.
	pub variant: TokenVariant<'a>,
	/// The column of the first char that the token was parsed from. Column 1 is the first column.
	pub start_column: NonZeroUsize,
	/// The column of the char after the last char of the line that the token was parsed from. Column 1 is the first column.
	pub end_column: NonZeroUsize,
}

#[derive(Debug, PartialEq)]
/// A token variant, eg. numeric literal, identifier.
pub enum TokenVariant<'a> {
	/// An operator. Eg. +, -, <=.
	Operator(Option<BinaryOperator>, Option<UnaryOperator>),
	/// A string literal. Eg. "Hello world".
	StringLiteral(&'a str),
	/// An identifier. Eg. text$, ABS#, var, GOTO.
	Identifier {
		/// The identifier text without the trailing type chars.
		name: &'a str,
		/// The identifier type parsed from the trailing type chars.
		identifier_type: IdentifierType,
		/// Does the identifier end with a '?' char?
		is_optional: bool,
		/// The binary infix operator that this identifier could be used as. Eg. AND, OR.
		binary_operator: Option<BinaryOperator>,
		/// The unary prefix operator that this identifier could be used as. Eg. NOT.
		unary_operator: Option<UnaryOperator>,
		/// The keyword that this identifier could be used as. Eg. PRINT, FOR, GOTO.
		keyword: Option<Keyword>,
		/// Is this a reserved keyword? Reserved keywords are PRINT, ELSE, NOT, REM. Reserved keywords cannot be used to name a variable, function, array or anything user-defined.
		is_reserved_keyword: bool,
		/// The supplied (built-in) function that that identifier could be used as. Eg. ABS, INT, RND, SQR, TRUE, PI.
		/// The trailing type chars are ignored when parsing this value, so ABS and ABS% parse to the same value.
		supplied_function: Option<SuppliedFunction>
	},
	/// An integer literal. Eg. 123, 420, 69. A leading minus sign will be parsed as it's own separate operator.
	IntegerLiteral(BigUint),
	/// A float literal. Eg. 2.5, 0.002, 5i, 3.6i, 2E-7, 3E9i.
	FloatLiteral {
		// The parsed value, is non-negative.
		value: f64,
		// Did this token have a trailing i?
		is_imaginary: bool
	},
	/// The separator (.
	LeftParenthesis,
	/// The separator ).
	RightParenthesis,
	/// The separator ,.
	Comma,
	/// The separator :.
	Colon,
	/// The separator ;.
	Semicolon,
	/// The separator ?.
	SingleQuestionMark,
	Datum(Datum)
}

impl<'a> Token<'a> {
	/// Parses a token from a string starting with a token in text form, returns:
	/// * `Ok(Some((token, rest of string with token removed)))` if a token could be found at the start of the string.
	/// * `Ok(None)` if the end of line or a `rem` remark was found.
	/// * `Err(error)` if the text was malformed.
	pub fn parse_single_token_from_str(line_starting_with_token: &'a str, column_number: NonZeroUsize, _is_datum: bool) -> Result<Option<(Self, &'a str)>, Error> {
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
					return Err(ErrorVariant::InvalidOperatorSymbol.at_column(column_number));
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
					_ if string_with_name_removed.starts_with("?")  => (IdentifierType::UnmarkedOrFloat, true,  &string_with_name_removed[1..]),
					_                                               => (IdentifierType::UnmarkedOrFloat, false, string_with_name_removed),
				};
				// Get if the token is an alphabetic operator
				let (binary_operator, unary_operator) = match identifier_type {
					IdentifierType::UnmarkedOrFloat if !is_optional => (BinaryOperator::from_name(name), UnaryOperator::from_name(name)),
					_ => (None, None),
				};
				// Get if the identifier is a keyword
				let keyword = match is_optional {
					false => Keyword::from_name(name, identifier_type),
					true => None,
				};
				// Get if the identifier is a supplied function
				let supplied_function = match is_optional {
					false => SuppliedFunction::from_name(name),
					true => None,
				};
				// Get if this a reserved keyword
				let is_reserved_keyword = (matches!(keyword, Some(Keyword::Else | Keyword::Print)) | name.eq_ignore_ascii_case("NOT") | name.eq_ignore_ascii_case("REM")) &&
					identifier_type == IdentifierType::UnmarkedOrFloat;
				// Assemble into token
				(TokenVariant::Identifier {
					name, identifier_type, is_optional, binary_operator, unary_operator, keyword, is_reserved_keyword, supplied_function
				}, rest_of_string_with_token_removed)
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
						let value = match BigUint::from_str_radix(integer_part, base.radix() as u32) {
							Ok(value) => value,
							_ => BigUint::ZERO,
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
			_ => return Err(ErrorVariant::InvalidToken.at_column(column_number))
		};
		// Get the end column of the char
		let token_length_in_bytes = line_starting_with_token.len() - rest_of_string_with_token_removed.len();
		let token_length_in_chars = line_starting_with_token[..token_length_in_bytes].chars().count();
		let end_column = start_column.saturating_add(token_length_in_chars);
		// Return
		Ok(Some((Self { variant, start_column, end_column }, rest_of_string_with_token_removed)))
	}

	/// Takes in a line of basic code in text form. Converts it into a (line number, list of tokens) pair.
	pub fn tokenize_line(mut line_text: &'a str) -> (Option<BigInt>, Result<Box<[Self]>, Error>) {
		// Get line number
		let mut column_number: NonZeroUsize = 1.try_into().unwrap();
		let line_number = match line_text.chars().next() {
			Some('0'..='9' | '-') => {
				let length_of_line_number = line_text.find(|chr| !matches!(chr, '0'..='9' | '-')).unwrap_or_else(|| line_text.len());
				let line_number_string;
				(line_number_string, line_text) = line_text.split_at(length_of_line_number);
				column_number = column_number.saturating_add(length_of_line_number);
				match line_number_string.parse::<BigInt>() {
					Ok(line_number) => Some(line_number),
					Err(_) => return (None, Err(ErrorVariant::MalformedLineNumber(line_number_string.into()).at_column(column_number))),
				}
			}
			_ => None,
		};
		// Parse tokens from line until there are none left
		let mut parenthesis_depth = 0usize;
		let mut is_datum = false;
		let mut tokens = Vec::new();
		loop {
			let (token, remaining_string) = match Self::parse_single_token_from_str(line_text, column_number, is_datum) {
				Err(error) => return (line_number, Err(error)),
				Ok(None) => break,
				Ok(Some(result)) => result,
			};
			match token.variant {
				TokenVariant::LeftParenthesis => parenthesis_depth += 1,
				TokenVariant::RightParenthesis => parenthesis_depth = parenthesis_depth.saturating_sub(1),
				TokenVariant::Colon => is_datum = false,
				TokenVariant::Identifier { keyword: Some(Keyword::Data), .. } if matches!(tokens.last(), None | Some(Token { variant: TokenVariant::Colon, .. })) && parenthesis_depth == 0 => is_datum = true,
				_ => {}
			}
			column_number = token.end_column;
			line_text = remaining_string;
			tokens.push(token);
		}
		// Return
		(line_number, Ok(tokens.into()))
	}
}

impl<'a> TokenVariant<'a> {
	/// Is can this token be used as a binary operator. Eg. +, <=, AND.
	pub fn is_binary_operator(&self) -> bool {
		match self {
			TokenVariant::Operator(Some(..), _) => true,
			TokenVariant::Identifier { binary_operator: Some(_), .. } => true,
			_ => false,
		}
	}

	/// Is can this token be used as a unary operator. Eg. +, -, NOT.
	pub fn is_unary_operator(&self) -> bool {
		match self {
			TokenVariant::Operator(_, Some(..)) => true,
			TokenVariant::Identifier { unary_operator: Some(_), .. } => true,
			_ => false,
		}
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
/// The type of an identifier. Eg. Integer, complex.
pub enum IdentifierType {
	/// A float identifier with no trailing %/#/$.
	UnmarkedOrFloat,
	/// A string identifier with a trailing $.
	String,
	/// An integer identifier with a trailing %.
	Integer,
	/// A complex identifier with a trailing #.
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
/// A keyword that identifier could be used as. Eg. PRINT, GOTO.
pub enum Keyword {
	Print,
	Goto,
	Run,
	Gosub,
	Let,
	List,
	Close,
	Clr,
	Cmd,
	Cont,
	Data,
	Def,
	Dim,
	End,
	For,
	Get,
	GetHash,
	Go,
	Sub,
	If,
	Input,
	InputHash,
	Load,
	New,
	Next,
	On,
	Open,
	Poke,
	PrintHash,
	Read,
	Restore,
	Return,
	Save,
	Step,
	Stop,
	Sys,
	Then,
	To,
	Verify,
	Wait,
	Else,
	Option,
	Angle,
	Radians,
	Degrees,
	Gradians,
	Revolutions,
	Arithmetic,
	Decimal,
	Native,
	Math,
	Ansi,
	Ieee,
	Prompt,
	Timeout,
	Elapsed,
	Machine,
	Default,
	C64,
	Access,
	Ask,
	Begin,
	Case,
	Chain,
	Break,
	Cause,
	Datum,
	Call,
	Collate,
	Debug,
	Display,
	External,
	Margin,
	Missing,
	Name,
	Off,
	Pointer,
	Randomize,
	RecType,
	Retry,
	Same,
	Setter,
	Size,
	Until,
	Using,
	Declare,
	Do,
	Erasable,
	Exception,
	Function,
	Handler,
	Image,
	Internal,
	Length,
	Line,
	Mat,
	Numeric,
	OutIn,
	Rest,
	Sequential,
	Skip,
	Stream,
	There,
	ElseIf,
	Erase,
	Exit,
	FileType,
	In,
	Is,
	Loop,
	Organization,
	Output,
	Program,
	RecSize,
	Select,
	Set,
	Standard,
	String,
	Trace,
	Use,
	Variable,
	When,
	While,
	Write,
	ZoneWidth,
	Width,
}

impl Keyword {
	/// Get the full caps name of the keyword and its trailing type char (nothing or #). Eg. GET and GET# are different.
	pub fn get_names(self) -> &'static [(&'static str, IdentifierType)] {
		match self {
			Self::Print =>        &[("PRINT",        IdentifierType::UnmarkedOrFloat)],
			Self::Goto =>         &[("GOTO",         IdentifierType::UnmarkedOrFloat)],
			Self::Run =>          &[("RUN",          IdentifierType::UnmarkedOrFloat)],
			Self::Gosub =>        &[("GOSUB",        IdentifierType::UnmarkedOrFloat)],
			Self::Let =>          &[("LET",          IdentifierType::UnmarkedOrFloat)],
			Self::List =>         &[("LIST",         IdentifierType::UnmarkedOrFloat)],
			Self::Close =>        &[("CLOSE",        IdentifierType::UnmarkedOrFloat)],
			Self::Clr =>          &[("CLR",          IdentifierType::UnmarkedOrFloat)],
			Self::Cmd =>          &[("CMD",          IdentifierType::UnmarkedOrFloat)],
			Self::Cont =>         &[("CONT",         IdentifierType::UnmarkedOrFloat), ("CONTINUE", IdentifierType::UnmarkedOrFloat)],
			Self::Data =>         &[("DATA",         IdentifierType::UnmarkedOrFloat)],
			Self::Def =>          &[("DEF",          IdentifierType::UnmarkedOrFloat)],
			Self::Dim =>          &[("DIM",          IdentifierType::UnmarkedOrFloat)],
			Self::End =>          &[("END",          IdentifierType::UnmarkedOrFloat)],
			Self::For =>          &[("FOR",          IdentifierType::UnmarkedOrFloat)],
			Self::Get =>          &[("GET",          IdentifierType::UnmarkedOrFloat)],
			Self::GetHash =>      &[("GET",          IdentifierType::ComplexNumber )],
			Self::If =>           &[("IF",           IdentifierType::UnmarkedOrFloat)],
			Self::Input =>        &[("INPUT",        IdentifierType::UnmarkedOrFloat)],
			Self::InputHash =>    &[("INPUT",        IdentifierType::ComplexNumber )],
			Self::Load =>         &[("LOAD",         IdentifierType::UnmarkedOrFloat)],
			Self::New =>          &[("NEW",          IdentifierType::UnmarkedOrFloat)],
			Self::Next =>         &[("NEXT",         IdentifierType::UnmarkedOrFloat)],
			Self::On =>           &[("ON",           IdentifierType::UnmarkedOrFloat)],
			Self::Open =>         &[("OPEN",         IdentifierType::UnmarkedOrFloat)],
			Self::Poke =>         &[("POKE",         IdentifierType::UnmarkedOrFloat)],
			Self::PrintHash =>    &[("PRINT",        IdentifierType::ComplexNumber )],
			Self::Read =>         &[("READ",         IdentifierType::UnmarkedOrFloat)],
			Self::Restore =>      &[("RESTORE",      IdentifierType::UnmarkedOrFloat)],
			Self::Return =>       &[("RETURN",       IdentifierType::UnmarkedOrFloat)],
			Self::Save =>         &[("SAVE",         IdentifierType::UnmarkedOrFloat)],
			Self::Step =>         &[("STEP",         IdentifierType::UnmarkedOrFloat)],
			Self::Stop =>         &[("STOP",         IdentifierType::UnmarkedOrFloat)],
			Self::Sys =>          &[("SYS",          IdentifierType::UnmarkedOrFloat)],
			Self::Then =>         &[("THEN",         IdentifierType::UnmarkedOrFloat)],
			Self::To =>           &[("TO",           IdentifierType::UnmarkedOrFloat)],
			Self::Verify =>       &[("VERIFY",       IdentifierType::UnmarkedOrFloat)],
			Self::Wait =>         &[("WAIT",         IdentifierType::UnmarkedOrFloat)],
			Self::Go =>           &[("GO",           IdentifierType::UnmarkedOrFloat)],
			Self::Sub =>          &[("SUB",          IdentifierType::UnmarkedOrFloat)],
			Self::Else =>         &[("ELSE",         IdentifierType::UnmarkedOrFloat)],
			Self::Option =>       &[("OPTION",       IdentifierType::UnmarkedOrFloat)],
			Self::Angle =>        &[("ANGLE",        IdentifierType::UnmarkedOrFloat)],
			Self::Arithmetic =>   &[("ARITHMETIC",   IdentifierType::UnmarkedOrFloat)],
			Self::Decimal =>      &[("DECIMAL",      IdentifierType::UnmarkedOrFloat)],
			Self::Degrees =>      &[("DEGREES",      IdentifierType::UnmarkedOrFloat)],
			Self::Gradians =>     &[("GRADIANS",     IdentifierType::UnmarkedOrFloat)],
			Self::Native =>       &[("NATIVE",       IdentifierType::UnmarkedOrFloat)],
			Self::Radians =>      &[("RADIANS",      IdentifierType::UnmarkedOrFloat)],
			Self::Revolutions =>  &[("REVOLUTIONS",  IdentifierType::UnmarkedOrFloat)],
			Self::Math =>         &[("MATH",         IdentifierType::UnmarkedOrFloat)],
			Self::Ansi =>         &[("ANSI",         IdentifierType::UnmarkedOrFloat)],
			Self::Ieee =>         &[("IEEE",         IdentifierType::UnmarkedOrFloat)],
			Self::Prompt =>       &[("PROMPT",       IdentifierType::UnmarkedOrFloat)],
			Self::Timeout =>      &[("TIMEOUT",      IdentifierType::UnmarkedOrFloat)],
			Self::Elapsed =>      &[("ELAPSED",      IdentifierType::UnmarkedOrFloat)],
			Self::Machine =>      &[("MACHINE",      IdentifierType::UnmarkedOrFloat)],
			Self::C64 =>          &[("C64",          IdentifierType::UnmarkedOrFloat)],
			Self::Default =>      &[("DEFAULT",      IdentifierType::UnmarkedOrFloat)],
			Self::Access =>       &[("ACCESS",       IdentifierType::UnmarkedOrFloat)],
			Self::Ask =>          &[("ASK",          IdentifierType::UnmarkedOrFloat)],
			Self::Begin =>        &[("BEGIN",        IdentifierType::UnmarkedOrFloat)],
			Self::Break =>        &[("BREAK",        IdentifierType::UnmarkedOrFloat)],
			Self::Call =>         &[("CALL",         IdentifierType::UnmarkedOrFloat)],
			Self::Case =>         &[("CASE",         IdentifierType::UnmarkedOrFloat)],
			Self::Cause =>        &[("CAUSE",        IdentifierType::UnmarkedOrFloat)],
			Self::Chain =>        &[("CHAIN",        IdentifierType::UnmarkedOrFloat)],
			Self::Collate =>      &[("COLLATE",      IdentifierType::UnmarkedOrFloat)],
			Self::Datum =>        &[("DATUM",        IdentifierType::UnmarkedOrFloat)],
			Self::Debug =>        &[("DEBUG",        IdentifierType::UnmarkedOrFloat)],
			Self::Declare =>      &[("DECLARE",      IdentifierType::UnmarkedOrFloat)],
			Self::Display =>      &[("DISPLAY",      IdentifierType::UnmarkedOrFloat)],
			Self::Do =>           &[("DO",           IdentifierType::UnmarkedOrFloat)],
			Self::ElseIf =>       &[("ELSEIF",       IdentifierType::UnmarkedOrFloat)],
			Self::Erasable =>     &[("ERASABLE",     IdentifierType::UnmarkedOrFloat)],
			Self::Erase =>        &[("ERASE",        IdentifierType::UnmarkedOrFloat)],
			Self::Exception =>    &[("EXCEPTION",    IdentifierType::UnmarkedOrFloat)],
			Self::Exit =>         &[("EXIT",         IdentifierType::UnmarkedOrFloat)],
			Self::External =>     &[("EXTERNAL",     IdentifierType::UnmarkedOrFloat)],
			Self::FileType =>     &[("FILETYPE",     IdentifierType::UnmarkedOrFloat)],
			Self::Function =>     &[("FUNCTION",     IdentifierType::UnmarkedOrFloat)],
			Self::Handler =>      &[("HANDLER",      IdentifierType::UnmarkedOrFloat)],
			Self::Image =>        &[("IMAGE",        IdentifierType::UnmarkedOrFloat)],
			Self::Internal =>     &[("INTERNAL",     IdentifierType::UnmarkedOrFloat)],
			Self::Is =>           &[("IS",           IdentifierType::UnmarkedOrFloat)],
			Self::Length =>       &[("LENGTH",       IdentifierType::UnmarkedOrFloat)],
			Self::Line =>         &[("LINE",         IdentifierType::UnmarkedOrFloat)],
			Self::Loop =>         &[("LOOP",         IdentifierType::UnmarkedOrFloat)],
			Self::Margin =>       &[("MARGIN",       IdentifierType::UnmarkedOrFloat)],
			Self::Missing =>      &[("MISSING",      IdentifierType::UnmarkedOrFloat)],
			Self::Name =>         &[("NAME",         IdentifierType::UnmarkedOrFloat)],
			Self::Numeric =>      &[("NUMERIC",      IdentifierType::UnmarkedOrFloat)],
			Self::Off =>          &[("OFF",          IdentifierType::UnmarkedOrFloat)],
			Self::Organization => &[("ORGANIZATION", IdentifierType::UnmarkedOrFloat)],
			Self::OutIn =>        &[("OUTIN",        IdentifierType::UnmarkedOrFloat)],
			Self::Output =>       &[("OUTPUT",       IdentifierType::UnmarkedOrFloat)],
			Self::Pointer =>      &[("POINTER",      IdentifierType::UnmarkedOrFloat)],
			Self::Program =>      &[("PROGRAM",      IdentifierType::UnmarkedOrFloat)],
			Self::Randomize =>    &[("RANDOMIZE",    IdentifierType::UnmarkedOrFloat)],
			Self::RecSize =>      &[("RECSIZE",      IdentifierType::UnmarkedOrFloat)],
			Self::RecType =>      &[("RECTYPE",      IdentifierType::UnmarkedOrFloat)],
			Self::Retry =>        &[("RETRY",        IdentifierType::UnmarkedOrFloat)],
			Self::Same =>         &[("SAME",         IdentifierType::UnmarkedOrFloat)],
			Self::Select =>       &[("SELECT",       IdentifierType::UnmarkedOrFloat)],
			Self::Sequential =>   &[("SEQUENTIAL",   IdentifierType::UnmarkedOrFloat)],
			Self::Set =>          &[("SET",          IdentifierType::UnmarkedOrFloat)],
			Self::Setter =>       &[("SETTER",       IdentifierType::UnmarkedOrFloat)],
			Self::Skip =>         &[("SKIP",         IdentifierType::UnmarkedOrFloat)],
			Self::Standard =>     &[("STANDARD",     IdentifierType::UnmarkedOrFloat)],
			Self::Stream =>       &[("STREAM",       IdentifierType::UnmarkedOrFloat)],
			Self::String =>       &[("STRING",       IdentifierType::UnmarkedOrFloat)],
			Self::There =>        &[("THERE",        IdentifierType::UnmarkedOrFloat)],
			Self::Trace =>        &[("TRACE",        IdentifierType::UnmarkedOrFloat)],
			Self::Until =>        &[("UNTIL",        IdentifierType::UnmarkedOrFloat)],
			Self::Use =>          &[("USE",          IdentifierType::UnmarkedOrFloat)],
			Self::Using =>        &[("USING",        IdentifierType::UnmarkedOrFloat)],
			Self::Variable =>     &[("VARIABLE",     IdentifierType::UnmarkedOrFloat)],
			Self::When =>         &[("WHEN",         IdentifierType::UnmarkedOrFloat)],
			Self::While =>        &[("WHILE",        IdentifierType::UnmarkedOrFloat)],
			Self::Width =>        &[("WIDTH",        IdentifierType::UnmarkedOrFloat)],
			Self::Write =>        &[("WRITE",        IdentifierType::UnmarkedOrFloat)],
			Self::ZoneWidth =>    &[("ZONEWIDTH",    IdentifierType::UnmarkedOrFloat)],
			Self::Size =>         &[("SIZE",         IdentifierType::UnmarkedOrFloat)],
			Self::Mat =>          &[("MAT",          IdentifierType::UnmarkedOrFloat)],
			Self::Rest =>         &[("REST",         IdentifierType::UnmarkedOrFloat)],
			Self::In =>           &[("IN",           IdentifierType::UnmarkedOrFloat)],
		}
	}

	/// Returns a list of (second keyword, combined keyword) tuples. Eg. GO returns (TO, GOTO) since GO TO is the same as GOTO.
	pub fn get_double_word_tokens(self) -> &'static [(Self, Self)] {
		match self {
			Self::Go => &[(Self::To, Self::Goto), (Self::Sub, Self::Gosub)],
			_ =>        &[],
		}
	}

	/// Takes a identifier name and its type and returns the keyword it could be used as if it could be used as a keyword.
	/// Eg. "PRINT", "for", "InPut", ("PRINT", #) and "myVar" return Some(PRINT), Some(FOR), Some(INPUT), Some(PrintHash) and None.
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

#[derive(Debug, PartialEq, Eq, Clone, Copy, EnumIter)]
/// A supplied (built-in) function that an identifier could be used as. Eg. ABS, SGN, PI, RND.
pub enum SuppliedFunction {
	Abs,
	Sqr,
	True,
	False,
	Int,
	Len,
	Sgn,
	Tab,
	Rnd,
	Pi,
	E,
	MaxNum,
	NaN,
	Inf,
	NInf,
	Tau,
	Phi,
	EGamma,
	I,
	Real,
	Imag,
	Sin,
	Cos,
	Tan,
	Cot,
	Sec,
	Csc,
	Sinh,
	Cosh,
	Tanh,
	Coth,
	Sech,
	Csch,
	Asin,
	Acos,
	Atan,
	Acot,
	Asec,
	Acsc,
	Asinh,
	Acosh,
	Atanh,
	Acoth,
	Asech,
	Acsch,
	Log,
	Exp,
}

impl SuppliedFunction {
	/// Returns the full caps name of this supplied function.
	pub fn get_names(self) -> &'static [&'static str] {
		match self {
			Self::Abs => &["ABS"],
			Self::Sqr => &["SQR"],
			Self::True => &["TRUE"],
			Self::False => &["FALSE"],
			Self::Int => &["INT"],
			Self::Len => &["LEN"],
			Self::Sgn => &["SGN"],
			Self::Tab => &["TAB"],
			Self::Rnd => &["RND"],
			Self::Sin => &["SIN"],
			Self::Cos => &["COS"],
			Self::Tan => &["TAN"],
			Self::Cot => &["COT"],
			Self::Sec => &["SEC"],
			Self::Csc => &["CSC"],
			Self::Sinh => &["SINH"],
			Self::Cosh => &["COSH"],
			Self::Tanh => &["TANH"],
			Self::Coth => &["COTH"],
			Self::Sech => &["SECH"],
			Self::Csch => &["CSCH"],
			Self::Asin => &["ASIN"],
			Self::Acos => &["ACOS"],
			Self::Atan => &["ATAN", "ATN"],
			Self::Acot => &["ACOT"],
			Self::Asec => &["ASEC"],
			Self::Acsc => &["ACSC"],
			Self::Asinh => &["ASINH"],
			Self::Acosh => &["ACOSH"],
			Self::Atanh => &["ATANH", "ATNH"],
			Self::Acoth => &["ACOTH"],
			Self::Asech => &["ASECH"],
			Self::Acsch => &["ACSCH"],
			Self::Pi => &["PI", "π"],
			Self::E => &["E"],
			Self::MaxNum => &["MAXNUM"],
			Self::NaN => &["NAN"],
			Self::Inf => &["INF"],
			Self::NInf => &["NINF"],
			Self::Tau => &["TAU"],
			Self::Phi => &["PHI"],
			Self::EGamma => &["EGAMMA"],
			Self::I => &["I"],
			Self::Real => &["REAL"],
			Self::Imag => &["IMAG"],
			Self::Log => &["LOG", "LN"],
			Self::Exp => &["EXP"],
		}
	}

	/// Takes a identifier name and returns the supplied function it could be used as if it could be used as a function.
	pub fn from_name(name: &str) -> Option<Self> {
		for keyword in Self::iter() {
			for function_name in keyword.get_names() {
				if name.eq_ignore_ascii_case(function_name) {
					return Some(keyword);
				}
			}
		}
		None
	}
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
/// A binary infix operator. Eg. +, <=, AND, OR.
pub enum BinaryOperator {
	Exponentiation,
	Multiplication,
	Division,
	DoubleSlash,
	BackSlash,
	AdditionConcatenation,
	Subtraction,
	LessThan,
	GreaterThan,
	Equal,
	NotEqualTo,
	LessThanOrEqualTo,
	GreaterThanOrEqualTo,
	And,
	Or,
	Concatenation,
}

impl BinaryOperator {
	/// Get the operator precedence of this operator, smaller numbers are parsed first.
	pub fn get_operator_precedence(self) -> u8 {
		match self {
			Self::Exponentiation => 0,
			Self::Multiplication | Self::Division | Self::DoubleSlash => 2,
			Self::BackSlash => 3,
			Self::AdditionConcatenation | Self::Subtraction | Self::Concatenation => 4,
			Self::LessThan | Self::GreaterThan | Self::LessThanOrEqualTo | Self::GreaterThanOrEqualTo | Self::Equal | Self::NotEqualTo => 5,
			Self::And => 7,
			Self::Or => 8,
		}
	}

	/// Get a list of non-alphabetic symbols that could be used for this operator. Eg. "-" for subtraction, "<" for less than.
	pub fn from_symbol(symbol: &str) -> Option<Self> {
		match symbol {
			"^" | "↑" => Some(Self::Exponentiation),
			"*" => Some(Self::Multiplication),
			"/" => Some(Self::Division),
			"//" => Some(Self::DoubleSlash),
			"\\" => Some(Self::BackSlash),
			"+" => Some(Self::AdditionConcatenation),
			"-" => Some(Self::Subtraction),
			"<" => Some(Self::LessThan),
			">" => Some(Self::GreaterThan),
			"=" => Some(Self::Equal),
			"<>" | "><" => Some(Self::NotEqualTo),
			"<=" | "=<" => Some(Self::LessThanOrEqualTo),
			">=" | "=>" => Some(Self::GreaterThanOrEqualTo),
			"&" => Some(Self::Concatenation),
			_ => None,
		}
	}

	/// Get a list of alphabetic symbols that could be used for this operator. Eg. "AND" for and, "OR" for or.
	pub fn from_name(name: &str) -> Option<Self> {
		match name {
			_ if name.eq_ignore_ascii_case("AND") => Some(Self::And),
			_ if name.eq_ignore_ascii_case("OR") => Some(Self::Or),
			_ => None,
		}
	}

	/// Find this operator in a list of tokens, ignores tokens inside parentheses.
	pub fn find_in(&self, find_in: &[Token]) -> Option<usize> {
		let mut bracket_depth = 0usize;
		for (index, token) in find_in.iter().enumerate() {
			match token.variant {
				TokenVariant::Operator(binary_operator, _) | TokenVariant::Identifier { binary_operator, .. }
					if bracket_depth == 0 && Some(self) == binary_operator.as_ref() =>
				{
					return Some(index);
				},
				TokenVariant::LeftParenthesis => bracket_depth += 1,
				TokenVariant::RightParenthesis => bracket_depth = match bracket_depth.checked_sub(1) {
					Some(bracket_depth) => bracket_depth,
					None => return None,
				},
				_ => {},
			}
		}
		None
	}
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
/// A unary prefix operator. Eg. +, -, NOT.
pub enum UnaryOperator {
	Negation,
	UnaryPlus,
	Not,
}

impl UnaryOperator {
	/// Get the operator precedence of this operator, smaller numbers are parsed first.
	pub fn get_operator_precedence(self) -> u8 {
		match self {
			Self::UnaryPlus | Self::Negation => 1,
			Self::Not => 6,
		}
	}

	/// Get a list of non-alphabetic symbols that could be used for this operator. Eg. "-" for negation, "+" for unary plus.
	pub fn from_symbol(symbol: &str) -> Option<Self> {
		match symbol {
			"-" => Some(Self::Negation),
			"+" => Some(Self::UnaryPlus),
			_ => None,
		}
	}

	/// Get a list of alphabetic symbols that could be used for this operator. Eg. "NOT" for not.
	pub fn from_name(name: &str) -> Option<Self> {
		match name {
			_ if name.eq_ignore_ascii_case("NOT") => Some(Self::Not),
			_ => None,
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_parse_single_token_from_str() {
		// Empty string
		assert_eq!(Token::parse_single_token_from_str("", 1.try_into().unwrap(), false).unwrap(), None);
		assert_eq!(Token::parse_single_token_from_str(" ", 1.try_into().unwrap(), false).unwrap(), None);
		assert_eq!(Token::parse_single_token_from_str("rem", 1.try_into().unwrap(), false).unwrap(), None);
		assert_eq!(Token::parse_single_token_from_str("ReM stuff after REM", 1.try_into().unwrap(), false).unwrap(), None);
		assert_eq!(Token::parse_single_token_from_str("	ReM stuff after REM", 1.try_into().unwrap(), false).unwrap(), None);
		// Identifiers
		assert_eq!(
			Token::parse_single_token_from_str("a", 1.try_into().unwrap(), false).unwrap(),
			Some((Token {
				variant: TokenVariant::Identifier { name: "a", identifier_type: IdentifierType::UnmarkedOrFloat, is_optional: false, binary_operator: None, unary_operator: None, keyword: None, is_reserved_keyword: false, supplied_function: None },
				start_column: 1.try_into().unwrap(), end_column: 2.try_into().unwrap()
			}, ""))
		);
		assert_eq!(
			Token::parse_single_token_from_str("_num = 8.5", 1.try_into().unwrap(), false).unwrap(),
			Some((Token {
				variant: TokenVariant::Identifier { name: "_num", identifier_type: IdentifierType::UnmarkedOrFloat, is_optional: false, binary_operator: None, unary_operator: None, keyword: None, is_reserved_keyword: false, supplied_function: None },
				start_column: 1.try_into().unwrap(), end_column: 5.try_into().unwrap()
			}, " = 8.5"))
		);
		assert_eq!(
			Token::parse_single_token_from_str(" var$", 1.try_into().unwrap(), false).unwrap(),
			Some((Token {
				variant: TokenVariant::Identifier { name: "var", identifier_type: IdentifierType::String, is_optional: false, binary_operator: None, unary_operator: None, keyword: None, is_reserved_keyword: false, supplied_function: None },
				start_column: 2.try_into().unwrap(), end_column: 6.try_into().unwrap()
			}, ""))
		);
		assert_eq!(
			Token::parse_single_token_from_str("	my_iNt%0", 1.try_into().unwrap(), false).unwrap(),
			Some((Token {
				variant: TokenVariant::Identifier { name: "my_iNt", identifier_type: IdentifierType::Integer, is_optional: false, binary_operator: None, unary_operator: None, keyword: None, is_reserved_keyword: false, supplied_function: None },
				start_column: 2.try_into().unwrap(), end_column: 9.try_into().unwrap()
			}, "0"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("		 MyComp# = 2i", 1.try_into().unwrap(), false).unwrap(),
			Some((Token {
				variant: TokenVariant::Identifier { name: "MyComp", identifier_type: IdentifierType::ComplexNumber, is_optional: false, binary_operator: None, unary_operator: None, keyword: None, is_reserved_keyword: false, supplied_function: None },
				start_column: 4.try_into().unwrap(), end_column: 11.try_into().unwrap()
			}, " = 2i"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("a0?=2E5", 1.try_into().unwrap(), false).unwrap(),
			Some((Token {
				variant: TokenVariant::Identifier { name: "a0", identifier_type: IdentifierType::UnmarkedOrFloat, is_optional: true, binary_operator: None, unary_operator: None, keyword: None, is_reserved_keyword: false, supplied_function: None },
				start_column: 1.try_into().unwrap(), end_column: 4.try_into().unwrap()
			}, "=2E5"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("a%?(val)", 1.try_into().unwrap(), false).unwrap(),
			Some((Token {
				variant: TokenVariant::Identifier { name: "a", identifier_type: IdentifierType::Integer, is_optional: true, binary_operator: None, unary_operator: None, keyword: None, is_reserved_keyword: false, supplied_function: None },
				start_column: 1.try_into().unwrap(), end_column: 4.try_into().unwrap()
			}, "(val)"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("a%?(val)", 10.try_into().unwrap(), false).unwrap(),
			Some((Token {
				variant: TokenVariant::Identifier { name: "a", identifier_type: IdentifierType::Integer, is_optional: true, binary_operator: None, unary_operator: None, keyword: None, is_reserved_keyword: false, supplied_function: None },
				start_column: 10.try_into().unwrap(), end_column: 13.try_into().unwrap()
			}, "(val)"))
		);
		assert_eq!(
			Token::parse_single_token_from_str(" _aA_01Z29_$?(val)", 1.try_into().unwrap(), false).unwrap(),
			Some((Token {
				variant: TokenVariant::Identifier { name: "_aA_01Z29_", identifier_type: IdentifierType::String, is_optional: true, binary_operator: None, unary_operator: None, keyword: None, is_reserved_keyword: false, supplied_function: None },
				start_column: 2.try_into().unwrap(), end_column: 14.try_into().unwrap()
			}, "(val)"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("VAL#?(val)", 1.try_into().unwrap(), false).unwrap(),
			Some((Token {
				variant: TokenVariant::Identifier { name: "VAL", identifier_type: IdentifierType::ComplexNumber, is_optional: true, binary_operator: None, unary_operator: None, keyword: None, is_reserved_keyword: false, supplied_function: None },
				start_column: 1.try_into().unwrap(), end_column: 6.try_into().unwrap()
			}, "(val)"))
		);
		// Operators
		assert_eq!(
			Token::parse_single_token_from_str("++", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::Operator(Some(BinaryOperator::AdditionConcatenation), Some(UnaryOperator::UnaryPlus)), start_column: 1.try_into().unwrap(), end_column: 2.try_into().unwrap() }, "+"))
		);
		assert_eq!(
			Token::parse_single_token_from_str(" //+", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::Operator(Some(BinaryOperator::DoubleSlash), None), start_column: 2.try_into().unwrap(), end_column: 4.try_into().unwrap() }, "+"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("<=-", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::Operator(Some(BinaryOperator::LessThanOrEqualTo), None), start_column: 1.try_into().unwrap(), end_column: 3.try_into().unwrap() }, "-"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("&+", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::Operator(Some(BinaryOperator::Concatenation), None), start_column: 1.try_into().unwrap(), end_column: 2.try_into().unwrap() }, "+"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("=<", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::Operator(Some(BinaryOperator::LessThanOrEqualTo), None), start_column: 1.try_into().unwrap(), end_column: 3.try_into().unwrap() }, ""))
		);
		assert_eq!(
			Token::parse_single_token_from_str("/ /", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::Operator(Some(BinaryOperator::Division), None), start_column: 1.try_into().unwrap(), end_column: 2.try_into().unwrap() }, " /"))
		);
		// Separators
		assert_eq!(
			Token::parse_single_token_from_str("()", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::LeftParenthesis, start_column: 1.try_into().unwrap(), end_column: 2.try_into().unwrap() }, ")"))
		);
		assert_eq!(
			Token::parse_single_token_from_str(")", 2.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::RightParenthesis, start_column: 2.try_into().unwrap(), end_column: 3.try_into().unwrap() }, ""))
		);
		assert_eq!(
			Token::parse_single_token_from_str(" : ", 50.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::Colon, start_column: 51.try_into().unwrap(), end_column: 52.try_into().unwrap() }, " "))
		);
		// Integers
		assert_eq!(
			Token::parse_single_token_from_str(" 420 + 420", 50.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(420u64.into()), start_column: 51.try_into().unwrap(), end_column: 54.try_into().unwrap() }, " + 420"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("6a", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(6u64.into()), start_column: 1.try_into().unwrap(), end_column: 2.try_into().unwrap() }, "a"))
		);
		assert_eq!(
			Token::parse_single_token_from_str(" ..", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0u64.into()), start_column: 2.try_into().unwrap(), end_column: 3.try_into().unwrap() }, "."))
		);
		assert_eq!(
			Token::parse_single_token_from_str(" .0.", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0u64.into()), start_column: 2.try_into().unwrap(), end_column: 4.try_into().unwrap() }, "."))
		);
		assert_eq!(
			Token::parse_single_token_from_str("6.00000k", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(6u64.into()), start_column: 1.try_into().unwrap(), end_column: 8.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("60.00E+0k", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(60u64.into()), start_column: 1.try_into().unwrap(), end_column: 9.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("00060.00E+0k", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(60u64.into()), start_column: 1.try_into().unwrap(), end_column: 12.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("60.00E+000k", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(60u64.into()), start_column: 1.try_into().unwrap(), end_column: 11.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("60.00E-0k", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(60u64.into()), start_column: 1.try_into().unwrap(), end_column: 9.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("60.00E+k", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(60u64.into()), start_column: 1.try_into().unwrap(), end_column: 8.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("60.00E-k", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(60u64.into()), start_column: 1.try_into().unwrap(), end_column: 8.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("60.00Ek", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(60u64.into()), start_column: 1.try_into().unwrap(), end_column: 7.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("$7Ek", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0x7Eu64.into()), start_column: 1.try_into().unwrap(), end_column: 4.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("$7ek", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0x7Eu64.into()), start_column: 1.try_into().unwrap(), end_column: 4.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("$k", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0x0u64.into()), start_column: 1.try_into().unwrap(), end_column: 2.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("$", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0x0u64.into()), start_column: 1.try_into().unwrap(), end_column: 2.try_into().unwrap() }, ""))
		);
		assert_eq!(
			Token::parse_single_token_from_str("$7F.k", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0x7Fu64.into()), start_column: 1.try_into().unwrap(), end_column: 5.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("$.0k", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0x0u64.into()), start_column: 1.try_into().unwrap(), end_column: 4.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("$10.0k", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0x10u64.into()), start_column: 1.try_into().unwrap(), end_column: 6.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("$.", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0x0u64.into()), start_column: 1.try_into().unwrap(), end_column: 3.try_into().unwrap() }, ""))
		);
		assert_eq!(
			Token::parse_single_token_from_str("%1001k", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0b1001u64.into()), start_column: 1.try_into().unwrap(), end_column: 6.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("%k", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0b0u64.into()), start_column: 1.try_into().unwrap(), end_column: 2.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("%", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0b0u64.into()), start_column: 1.try_into().unwrap(), end_column: 2.try_into().unwrap() }, ""))
		);
		assert_eq!(
			Token::parse_single_token_from_str("%10.k", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0b10u64.into()), start_column: 1.try_into().unwrap(), end_column: 5.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("%.0k", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0b0u64.into()), start_column: 1.try_into().unwrap(), end_column: 4.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("%10.0k", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0b10u64.into()), start_column: 1.try_into().unwrap(), end_column: 6.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("%00010.0k", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0b10u64.into()), start_column: 1.try_into().unwrap(), end_column: 9.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("%.", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0b0u64.into()), start_column: 1.try_into().unwrap(), end_column: 3.try_into().unwrap() }, ""))
		);
		assert_eq!(
			Token::parse_single_token_from_str("%.2", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0b0u64.into()), start_column: 1.try_into().unwrap(), end_column: 3.try_into().unwrap() }, "2"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("%0010E+", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0b10u64.into()), start_column: 1.try_into().unwrap(), end_column: 8.try_into().unwrap() }, ""))
		);
		assert_eq!(
			Token::parse_single_token_from_str("%0010E+2", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0b10u64.into()), start_column: 1.try_into().unwrap(), end_column: 8.try_into().unwrap() }, "2"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("%001050", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::IntegerLiteral(0b10u64.into()), start_column: 1.try_into().unwrap(), end_column: 6.try_into().unwrap() }, "50"))
		);
		// Floats
		assert_eq!(
			Token::parse_single_token_from_str("2.5", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::FloatLiteral { value: 2.5, is_imaginary: false }, start_column: 1.try_into().unwrap(), end_column: 4.try_into().unwrap() }, ""))
		);
		assert_eq!(
			Token::parse_single_token_from_str("2.5i", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::FloatLiteral { value: 2.5, is_imaginary: true }, start_column: 1.try_into().unwrap(), end_column: 5.try_into().unwrap() }, ""))
		);
		assert_eq!(
			Token::parse_single_token_from_str("2.5000k", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::FloatLiteral { value: 2.5, is_imaginary: false }, start_column: 1.try_into().unwrap(), end_column: 7.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str(".000760k", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::FloatLiteral { value: 0.00076, is_imaginary: false }, start_column: 1.try_into().unwrap(), end_column: 8.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("00900.000760k", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::FloatLiteral { value: 900.00076, is_imaginary: false }, start_column: 1.try_into().unwrap(), end_column: 13.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("040.0777E3k", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::FloatLiteral { value: 040.0777E3, is_imaginary: false }, start_column: 1.try_into().unwrap(), end_column: 11.try_into().unwrap() }, "k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("040.0777e+3.k", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::FloatLiteral { value: 040.0777E3, is_imaginary: false }, start_column: 1.try_into().unwrap(), end_column: 12.try_into().unwrap() }, ".k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("040.0777E+3I.k", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::FloatLiteral { value: 040.0777E3, is_imaginary: true }, start_column: 1.try_into().unwrap(), end_column: 13.try_into().unwrap() }, ".k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("040.0777E-3.k", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::FloatLiteral { value: 040.0777E-3, is_imaginary: false }, start_column: 1.try_into().unwrap(), end_column: 12.try_into().unwrap() }, ".k"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("%010.010", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::FloatLiteral { value: 2.25, is_imaginary: false }, start_column: 1.try_into().unwrap(), end_column: 9.try_into().unwrap() }, ""))
		);
		assert_eq!(
			Token::parse_single_token_from_str("%0.10010E10", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::FloatLiteral { value: 2.25, is_imaginary: false }, start_column: 1.try_into().unwrap(), end_column: 12.try_into().unwrap() }, ""))
		);
		// Strings
		assert_eq!(
			Token::parse_single_token_from_str(" \"\"a", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::StringLiteral(""), start_column: 2.try_into().unwrap(), end_column: 4.try_into().unwrap() }, "a"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("\"REM ±Hello\"a", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::StringLiteral("REM ±Hello"), start_column: 1.try_into().unwrap(), end_column: 13.try_into().unwrap() }, "a"))
		);
		assert_eq!(
			Token::parse_single_token_from_str("\"REM ±Hello\"a\"", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::StringLiteral("REM ±Hello"), start_column: 1.try_into().unwrap(), end_column: 13.try_into().unwrap() }, "a\""))
		);
		assert_eq!(
			Token::parse_single_token_from_str("\"REM ±Hello", 1.try_into().unwrap(), false).unwrap(),
			Some((Token { variant: TokenVariant::StringLiteral("REM ±Hello"), start_column: 1.try_into().unwrap(), end_column: 12.try_into().unwrap() }, ""))
		);
	}

	#[test]
	fn test_tokenize_line() {
		let tokens = Token::tokenize_line("10 PRINT 10 + 8");
		assert_eq!(tokens.0, Some(10.into()));
		assert_eq!(tokens.1.as_ref().unwrap().len(), 4);
		assert_eq!(tokens.1.as_ref().unwrap()[0], Token {
			variant: TokenVariant::Identifier {
				name: "PRINT", identifier_type: IdentifierType::UnmarkedOrFloat, is_optional: false, binary_operator: None, unary_operator: None, keyword: Some(Keyword::Print), is_reserved_keyword: true, supplied_function: None
			}, start_column: 4.try_into().unwrap(), end_column: 9.try_into().unwrap()
		});
		assert_eq!(tokens.1.as_ref().unwrap()[1], Token {
			variant: TokenVariant::IntegerLiteral(10u64.into()), start_column: 10.try_into().unwrap(), end_column: 12.try_into().unwrap()
		});
		assert_eq!(tokens.1.as_ref().unwrap()[2], Token {
			variant: TokenVariant::Operator(Some(BinaryOperator::AdditionConcatenation), Some(UnaryOperator::UnaryPlus)), start_column: 13.try_into().unwrap(), end_column: 14.try_into().unwrap()
		});
		assert_eq!(tokens.1.as_ref().unwrap()[3], Token {
			variant: TokenVariant::IntegerLiteral(8u64.into()), start_column: 15.try_into().unwrap(), end_column: 16.try_into().unwrap()
		});

		let tokens = Token::tokenize_line("PRINT 10 + 8");
		assert_eq!(tokens.0, None);
		assert_eq!(tokens.1.as_ref().unwrap().len(), 4);
		assert_eq!(tokens.1.as_ref().unwrap()[0], Token {
			variant: TokenVariant::Identifier {
				name: "PRINT", identifier_type: IdentifierType::UnmarkedOrFloat, is_optional: false, binary_operator: None, unary_operator: None, keyword: Some(Keyword::Print), is_reserved_keyword: true, supplied_function: None
			}, start_column: 1.try_into().unwrap(), end_column: 6.try_into().unwrap()
		});
		assert_eq!(tokens.1.as_ref().unwrap()[1], Token {
			variant: TokenVariant::IntegerLiteral(10u64.into()), start_column: 7.try_into().unwrap(), end_column: 9.try_into().unwrap()
		});
		assert_eq!(tokens.1.as_ref().unwrap()[2], Token {
			variant: TokenVariant::Operator(Some(BinaryOperator::AdditionConcatenation), Some(UnaryOperator::UnaryPlus)), start_column: 10.try_into().unwrap(), end_column: 11.try_into().unwrap()
		});
		assert_eq!(tokens.1.as_ref().unwrap()[3], Token {
			variant: TokenVariant::IntegerLiteral(8u64.into()), start_column: 12.try_into().unwrap(), end_column: 13.try_into().unwrap()
		});
	}
}