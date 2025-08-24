use std::num::NonZeroUsize;

use crate::mavagk_basic::{error::Error, token::{IdentifierType, Token, TokenVariant}};

#[derive(Debug)]
pub struct Statement<'a> {
	variant: StatementVariant<'a>,
	column: NonZeroUsize,
}

impl<'a> Statement<'a> {
	pub fn parse<'b>(tokens: &'b [Token<'a>]) -> Result<Option<(Self, &'b [Token<'a>])>, Error> {
		if let Some(Token { variant, start_column, end_column }) = tokens.first() {
			if let TokenVariant::Identifier { name: "print", identifier_type: IdentifierType::Number, is_optional: false } = variant {
				let (expression, remaining_tokens) = match Expression::parse(&tokens[1..], *end_column)? {
					None => return Err(Error::NotYetImplemented(None, *end_column, "Empty print statement".into())),
					Some(result) => result,
				};
				if !remaining_tokens.is_empty() {
					return Err(Error::NotYetImplemented(None, *start_column, "More that one print sub-expression".into()));
				}
				return Ok(Some((Self { column: *start_column, variant: StatementVariant::Print(expression) }, remaining_tokens)));
			}
			return Err(Error::NotYetImplemented(None, *start_column, "Statements that are not print statements".into()));
		}
		Ok(None)
	}
}

#[derive(Debug)]
pub enum StatementVariant<'a> {
	Print(Expression<'a>),
}

#[derive(Debug)]
pub struct Expression<'a> {
	pub variant: ExpressionVariant<'a>,
	pub column: NonZeroUsize,
}

impl<'a> Expression<'a> {
	pub fn parse<'b>(tokens: &'b [Token<'a>], start_column: NonZeroUsize) -> Result<Option<(Self, &'b [Token<'a>])>, Error> {
		if let Some(Token { variant, start_column, end_column: _ }) = tokens.first() {
			if let TokenVariant::StringLiteral(value) = variant {
				return Ok(Some((Expression { column: *start_column, variant: ExpressionVariant::StringLiteral(*value) }, &tokens[1..])));
			}
			return Err(Error::NotYetImplemented(None, *start_column, "Expressions that are not string literals".into()));
		}
		Err(Error::ExpectedExpression(start_column))
	}

	fn get_expression_length(tokens: &[Token<'a>]) -> usize {
		todo!()
	}
}

#[derive(Debug)]
pub enum ExpressionVariant<'a> {
	StringLiteral(&'a str),
}

pub fn parse_line<'a>(mut tokens: &[Token<'a>]) -> Result<Box<[Statement<'a>]>, Error> {
	let mut out = Vec::new();
	loop {
		match Statement::parse(tokens)? {
			Some((ast, rest_of_tokens)) => {
				tokens = rest_of_tokens;
				out.push(ast);
			}
			None => break,
		}
	}
	Ok(out.into())
}