use std::{fmt::{self, Display, Formatter}, io::{stdout, Write}, num::NonZeroUsize};

use crossterm::{cursor::position, execute, style::{Color, ContentStyle, PrintStyledContent, StyledContent}};
use num::{complex::Complex64, BigInt};

/// An error that may have a column number attached.
#[derive(Debug, Clone)]
pub struct Error {
	pub variant: ErrorVariant,
	pub column_number: Option<NonZeroUsize>,
}

impl Error {
	pub fn to_full_error(self, line_number: Option<BigInt>, line_text: Option<String>) -> FullError {
		FullError {
			variant: self.variant,
			column_number: self.column_number,
			line_number,
			line_text,
		}
	}
}

/// An error with that may have a line and column as well as the source code of the line that the error occurred on.
#[derive(Debug, Clone)]
pub struct FullError {
	pub variant: ErrorVariant,
	pub line_number: Option<BigInt>,
	pub column_number: Option<NonZeroUsize>,
	pub line_text: Option<String>,
}

impl Display for FullError {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match (&self.line_number, self.column_number) {
			(None, None) => write!(f, ": {}", self.variant),
			(None, Some(column_number)) => write!(f, " in column {column_number}: {}", self.variant),
			(Some(line_number), None) => write!(f, " on line {line_number}: {}", self.variant),
			(Some(line_number), Some(column_number)) => write!(f, " at row:column {line_number}:{column_number}: {}", self.variant),
		}
	}
}

#[derive(Debug, Clone)]
#[repr(u16)]
pub enum ErrorVariant {
	InvalidTokenFirstChar(char),
	InvalidToken,
	NotYetImplemented(String), // For features that should be implemented in the future.
	Unimplemented(String), // For features that may or may not be implemented in the future.
	MalformedLineNumber(String),
	ExpectedExpression,
	ExpectedListHyphen,
	MoreLeftParenthesesThanRightParentheses,
	MoreRightParenthesesThanLeftParentheses,
	FnWithoutIdentifier,
	NothingInParentheses,
	ParenthesesDoNotContainOneExpression,
	FunctionArgumentsNotCommaSeparated,
	InvalidOperator,
	InvalidOperatorSymbol,
	InvalidLineNumber(BigInt),
	NonNumberValueCastToInt(f64),
	NonRealComplexValueCastToReal(Complex64),
	StringCastToNumber,
	NumberCastToString,
	StatementShouldEnd,
	CannotUseThisOperatorOnAString,
	CannotConcatenateNumbers,
	FlooredDivisionByZero,
	ExpectedEqualSign,
	ExpectedToKeyword,
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
	UnexpectedSecondListHyphen,
	SingleGoKeyword,
	ExpectedThenKeyword,
	ExpectedElseOrStatementEnd,
	StatementCannotBeNested,
	IntSquareRootOfNegativeNumber,
	ExpectedOptionArguments,
	InvalidOptionVariableOrValue,
	ExpectedColonAfterInputPrompt,
	MultiplePromptsForInput,
	MultipleTimeoutsForInput,
	MultipleElapsedsForInput,
	ExpectedInputPrompt,
	LoopVariableNotSimpleVar,
	NoLoops,
	ForLoopVariableNotFound,
	// TODO Different overflows
	ValueOverflow,
	DivisionByZero = 3001,
	NegativeNumberRaisedToNonIntegerPower = 3002,
	ZeroRaisedToNegativePower = 3003,
	LogOfNonPositive = 3004,
	SquareRootOfNegative = 3005,
	ModOrRemainderByZero = 3006,
	ACosOrASinOutOfRange = 3007,
	AngleOfZeroZero = 3008,
}

impl ErrorVariant {
	pub fn at_column(self, column_number: NonZeroUsize) -> Error {
		Error {
			variant: self,
			column_number: Some(column_number),
		}
	}

	pub fn error(self) -> Error {
		Error {
			variant: self,
			column_number: None,
		}
	}
}

impl Display for ErrorVariant {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			Self::InvalidTokenFirstChar(chr) => write!(f, "Invalid token first char '{chr}'."),
			Self::NotYetImplemented(feature) => write!(f, "{feature} not yet implemented."),
			Self::Unimplemented(feature) => write!(f, "{feature} unimplemented."),
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
			Self::InvalidLineNumber(line_number) => write!(f, "Attempted to jump to line {line_number}, line not found."),
			Self::NonNumberValueCastToInt(value) => write!(f, "Non-number value {value} cast to int."),
			Self::NonRealComplexValueCastToReal(value) => write!(f, "Non-real complex value {value} cast to real number."),
			Self::StringCastToNumber => write!(f, "String cast to number."),
			Self::NumberCastToString => write!(f, "Number cast to string."),
			Self::StatementShouldEnd => write!(f, "Statement must end."),
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
			Self::ExpectedListHyphen => write!(f, "Expected LIST hyphen."),
			Self::UnexpectedSecondListHyphen => write!(f, "Unexpected second LIST hyphen."),
			Self::SingleGoKeyword => write!(f, "GO keyword found that was not part of a GO TO or GO SUB word pair."),
			Self::ExpectedThenKeyword => write!(f, "Expected THEN keyword."),
			Self::ExpectedElseOrStatementEnd => write!(f, "Expected either an ELSE keyword or for the statement to end."),
			Self::StatementCannotBeNested => write!(f, "Statement cannot be nested."),
			Self::IntSquareRootOfNegativeNumber => write!(f, "Attempted to take the integer square root of a negative number."),
			Self::ExpectedOptionArguments => write!(f, "Expected two arguments after a OPTION keyword."),
			Self::InvalidOptionVariableOrValue => write!(f, "Invalid OPTION variable value pair."),
			Self::ValueOverflow => write!(f, "Value overflow."),
			Self::DivisionByZero => write!(f, "Division by zero."),
			Self::NegativeNumberRaisedToNonIntegerPower => write!(f, "Negative number raised to non-integer power."),
			Self::ZeroRaisedToNegativePower => write!(f, "Zero raised to negative power."),
			Self::LogOfNonPositive => write!(f, "Logarithm of non-positive number."),
			Self::SquareRootOfNegative => write!(f, "Square root of negative number."),
			Self::ModOrRemainderByZero => write!(f, "MOD or REMAINDER by zero."),
			Self::ACosOrASinOutOfRange => write!(f, "ACOS or ASIN argument out of range."),
			Self::AngleOfZeroZero => write!(f, "ANGLE of 0, 0."),
			Self::ExpectedColonAfterInputPrompt => write!(f, "Expected colon or semicolon after input prompt."),
			Self::MultiplePromptsForInput => write!(f, "Multiple PROMPT arguments for an INPUT statement."),
			Self::MultipleTimeoutsForInput => write!(f, "Multiple TIMEOUT arguments for an INPUT statement."),
			Self::MultipleElapsedsForInput => write!(f, "Multiple ELAPSED arguments for an INPUT statement."),
			Self::ExpectedInputPrompt => write!(f, "Expected a PROMPT, TIMEOUT or ELAPSED keyword."),
			Self::ExpectedToKeyword => write!(f, "Expected TO keyword."),
			Self::LoopVariableNotSimpleVar => write!(f, "A loop variable must not contain parentheses."),
			Self::NoLoops => write!(f, "There are not any active loops for a no argument NEXT statement to loop."),
			Self::ForLoopVariableNotFound => write!(f, "FOR loop variable not found."),
		}
	}
}

/// Takes in a `Result` value that may have an error, prints the error if it exists.
pub fn handle_error<T>(maybe_error: Result<T, FullError>) -> Option<T> {
	match maybe_error {
		Ok(not_error) => Some(not_error),
		Err(error) => {
			stdout().flush().unwrap();
			if position().unwrap().0 != 0 {
				println!();
			}
			execute!(stdout(), PrintStyledContent(StyledContent::new(ContentStyle { foreground_color: Some(Color::Red), ..Default::default() }, format!("Basic error{error}\n")))).unwrap();
			if let Some(line_number) = error.column_number {
				if let Some(line_text) = error.line_text {
					println!("{line_text}");
					for _ in 0..line_number.get() - 1 {
						print!(" ");
					}
					execute!(stdout(), PrintStyledContent(StyledContent::new(ContentStyle { foreground_color: Some(Color::Red), ..Default::default() }, "^\n"))).unwrap();
				}
			}
			None
		}
	}
}