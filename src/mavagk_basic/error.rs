use std::{fmt::{self, Display, Formatter}, num::NonZeroUsize};

use num::{complex::Complex64, BigInt};

use crate::mavagk_basic::value::ComplexValue;

//use crate::mavagk_basic::machine::Value;

#[derive(Debug)]
pub struct Error {
	pub variant: ErrorVariant,
	pub line_number: Option<BigInt>,
	pub column_number: Option<NonZeroUsize>,
}

impl Error {
	pub fn set_line_number(mut self, line_number: Option<&BigInt>) -> Self {
		self.line_number = line_number.cloned();
		self
	}

	pub fn set_column_number(mut self, column_number: NonZeroUsize) -> Self {
		self.column_number = Some(column_number);
		self
	}
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

#[derive(Debug)]
pub enum ErrorVariant {
	InvalidTokenFirstChar(char),
	InvalidToken,
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
	InvalidOperatorSymbol,
	InvalidLineNumber,
	NonNumberValueCastToInt(f64),
	NonRealComplexValueCastToReal(ComplexValue),
	StringCastToNumber,
	NumberCastToString,
	StatementShouldEnd,
	CannotUseThisOperatorOnAString,
	CannotConcatenateNumbers,
	FlooredDivisionByZero,
	ExpectedEqualSign,
	InvalidLValue,
	VariableNotFound,
	ExpectedStatementKeyword,
	ExpectedRightParenthesis,
	UnexpectedOperator,
	ExpectedFunctionNameAfterFn,
	LeadingCommaInFunctionArguments,
	TrailingCommaInFunctionArguments,
	TwoSequentialCommasTogetherInFunctionArguments,
	NoCommaBetweenFunctionArguments,
	InvalidSeparatorInFunctionArguments,
	UnaryOperatorsAtEndOfExpression,
	ExpectedExpressionPrimary,
	NonRealComparison(Complex64, Complex64),
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
			Self::InvalidOperatorSymbol => write!(f, "Invalid operator symbol."),
			Self::InvalidLineNumber => write!(f, "Line not found."),
			Self::NonNumberValueCastToInt(value) => write!(f, "Non-number value {value} cast to int."),
			Self::NonRealComplexValueCastToReal(value) => write!(f, "Non-real complex value {value} cast to real number."),
			Self::StringCastToNumber => write!(f, "String cast to number."),
			Self::NumberCastToString => write!(f, "Number cast to string."),
			Self::StatementShouldEnd => write!(f, "Statement should end."),
			Self::CannotConcatenateNumbers => write!(f, "Cannot concatenate numbers."),
			Self::CannotUseThisOperatorOnAString => write!(f, "Cannot use this operator on a string."),
			Self::FlooredDivisionByZero => write!(f, "Floored division by zero."),
			Self::ExpectedEqualSign => write!(f, "Expected equal sign."),
			Self::InvalidLValue => write!(f, "Invalid l-value."),
			Self::VariableNotFound => write!(f, "Variable not found."),
			Self::InvalidToken => write!(f, "Invalid token."),
			Self::ExpectedStatementKeyword => write!(f, "Expected statement keyword."),
			Self::ExpectedRightParenthesis => write!(f, "Expected right parenthesis."),
			Self::UnexpectedOperator => write!(f, "Unexpected operator."),
			Self::ExpectedFunctionNameAfterFn => write!(f, "Expected function name after FN keyword."),
			Self::LeadingCommaInFunctionArguments => write!(f, "Leading comma in function arguments."),
			Self::TrailingCommaInFunctionArguments => write!(f, "Trailing comma in function arguments."),
			Self::TwoSequentialCommasTogetherInFunctionArguments => write!(f, "Two sequential commas in function arguments."),
			Self::NoCommaBetweenFunctionArguments => write!(f, "No comma between function arguments."),
			Self::InvalidSeparatorInFunctionArguments => write!(f, "Invalid separator in function arguments."),
			Self::UnaryOperatorsAtEndOfExpression => write!(f, "Unary operators at end of expression."),
			Self::ExpectedExpressionPrimary => write!(f, "Expected expression primary."),
			Self::NonRealComparison(lhs, rhs) => write!(f, "Comparison between non-real numbers {lhs} and {rhs}."),
		}
	}
}

/// Takes in a `Result` value that may have an error, prints the error if it exists.
pub fn handle_error<T>(maybe_error: Result<T, Error>) -> Option<T> {
	match maybe_error {
		Ok(not_error) => Some(not_error),
		Err(error) => {
			println!("\nBasic error{error}");
			None
		}
	}
}