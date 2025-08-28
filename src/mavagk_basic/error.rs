use std::{fmt::{self, Display, Formatter}, num::NonZeroUsize};

use num::BigInt;

use crate::mavagk_basic::machine::Value;

pub struct Error {
	pub variant: ErrorVariant,
	pub line_number: Option<BigInt>,
	pub column_number: Option<NonZeroUsize>,
}

impl Display for Error {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match (&self.line_number, self.column_number) {
			(None, None) => write!(f, ": {}", self.variant),
			(None, Some(column_number)) => write!(f, " in column {column_number}: {}", self.variant),
			(Some(line_number), None) => write!(f, " on line {line_number}: {}", self.variant),
			(Some(line_number), Some(column_number)) => write!(f, " at row:column {line_number}:{column_number}: {}", self.variant),
		}
	}
}

pub enum ErrorVariant {
	InvalidTokenFirstChar(char),
	NotYetImplemented(String),
	MalformedLineNumber(String),
	ExpectedExpression,
	MoreLeftParenthesesThanRightParentheses,
	MoreRightParenthesesThanLeftParentheses,
	FnWithoutIdentifier,
	NothingInParentheses,
	ParenthesesDoNotContainOneExpression,
	FunctionArgumentsNotCommaSeparated,
	InvalidOperator,
	InvalidLineNumber,
	NonNumberValueCastToInt(Value),
	NonRealComplexValueCastToReal(Value),
	StringCastToNumber,
}

impl Display for ErrorVariant {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			Self::InvalidTokenFirstChar(chr) => write!(f, "Invalid token first char '{chr}'."),
			Self::NotYetImplemented(feature) => write!(f, "{feature} not yet implemented."),
			Self::MalformedLineNumber(number) => write!(f, "Malformed line number \"{number}\"."),
			Self::ExpectedExpression => write!(f, "Expected an expression."),
			Self::MoreLeftParenthesesThanRightParentheses => write!(f, "More left parentheses that right parentheses."),
			Self::MoreRightParenthesesThanLeftParentheses => write!(f, "More right parentheses that left parentheses."),
			Self::FnWithoutIdentifier => write!(f, "FN keyword that is not followed by an identifier."),
			Self::NothingInParentheses => write!(f, "Nothing in parentheses."),
			Self::ParenthesesDoNotContainOneExpression => write!(f, "Parentheses do not contain one expression that takes up the entire parenthesised area."),
			Self::FunctionArgumentsNotCommaSeparated => write!(f, "Function parentheses do not contain a comma separated list of arguments."),
			Self::InvalidOperator => write!(f, "Invalid operator."),
			Self::InvalidLineNumber => write!(f, "Line not found."),
			Self::NonNumberValueCastToInt(value) => write!(f, "Non-number {} value {value} cast to int.", value.get_type_name()),
			Self::NonRealComplexValueCastToReal(value) => write!(f, "Non-real complex value {value} cast to real number."),
			Self::StringCastToNumber => write!(f, "String cast to number."),
		}
	}
}