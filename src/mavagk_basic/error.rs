use std::{fmt::{self, Display, Formatter}, num::NonZeroUsize};

use num::BigInt;

pub enum Error {
	InvalidTokenFirstChar(NonZeroUsize, char),
	NotYetImplemented(Option<BigInt>, NonZeroUsize, String),
	MalformedLineNumber(NonZeroUsize, String),
	ExpectedExpression(NonZeroUsize),
	MoreLeftParenthesesThanRightParentheses(NonZeroUsize),
	MoreRightParenthesesThanLeftParentheses(NonZeroUsize),
	FnWithoutIdentifier(NonZeroUsize),
	NothingInParentheses(NonZeroUsize),
	ParenthesesDoNotContainOneExpression(NonZeroUsize),
	FunctionArgumentsNotCommaSeparated(NonZeroUsize),
	InvalidOperator(NonZeroUsize),
	InvalidLineNumber(BigInt),
}

impl Display for Error {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			Self::InvalidTokenFirstChar(column, chr) => write!(f, "in column {column}: Invalid token first char '{chr}'."),
			Error::NotYetImplemented(line, column, feature) => match line {
				Some(line) => write!(f, "at {line}:{column}: {feature} not yet implemented."),
				None => write!(f, "in column {column}: {feature} not yet implemented."),
			},
			Self::MalformedLineNumber(column, line_number_chars) => write!(f, "in column {column}: Malformed line number \"{line_number_chars}\"."),
			Self::ExpectedExpression(column) => write!(f, "in column {column}: Expected an expression."),
			Self::MoreLeftParenthesesThanRightParentheses(column) => write!(f, "in column {column}: More left parentheses that right parentheses."),
			Self::MoreRightParenthesesThanLeftParentheses(column) => write!(f, "in column {column}: More right parentheses that left parentheses."),
			Self::FnWithoutIdentifier(column) => write!(f, "in column {column}: FN keyword that is not followed by an identifier."),
			Self::NothingInParentheses(column) => write!(f, "in column {column}: Nothing in parentheses."),
			Self::ParenthesesDoNotContainOneExpression(column) => write!(f, "in column {column}: Parentheses do not contain one expression that takes up the entire parenthesised area."),
			Self::FunctionArgumentsNotCommaSeparated(column) => write!(f, "in column {column}: Function parentheses do not contain a comma separated list of arguments."),
			Self::InvalidOperator(column) => write!(f, "in column {column}: Invalid operator."),
			Self::InvalidLineNumber(line_number) => write!(f, "trying to execute line {line_number}: Line not found."),
		}
	}
}