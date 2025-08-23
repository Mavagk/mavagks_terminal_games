use std::num::NonZeroUsize;

use crate::mavagk_basic::{error::Error, token::{IdentifierType, Token}};

#[derive(Debug)]
pub struct Statement<'a> {
	variant: StatementVariant<'a>,
	column: NonZeroUsize,
}

impl<'a> Statement<'a> {
	pub fn parse<'b>(tokens: &'b [(NonZeroUsize, Token<'a>)]) -> Result<Option<(Self, &'b [(NonZeroUsize, Token<'a>)])>, Error> {
		if let Some((column_number, token)) = tokens.first() {
			if let Token::Identifier { name: "print", identifier_type: IdentifierType::Number, is_optional: false } = token {
				let (expression, remaining_tokens) = Expression::parse(&tokens[1..])?;
				if !remaining_tokens.is_empty() {
					return Err(Error::NotYetImplemented(None, *column_number, "More that one print sub-expression".into()));
				}
				return Ok(Some((Self { column: *column_number, variant: StatementVariant::Print(expression) }, remaining_tokens)));
			}
			return Err(Error::NotYetImplemented(None, *column_number, "Statements that are not print statements".into()));
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
	variant: ExpressionVariant<'a>,
	column: NonZeroUsize,
}

impl<'a> Expression<'a> {
	pub fn parse<'b>(tokens: &'b [(NonZeroUsize, Token<'a>)]) -> Result<(Self, &'b [(NonZeroUsize, Token<'a>)]), Error> {
		// TODO: Refactor
		if let Some((column_number, token)) = tokens.first() {
			if let Token::StringLiteral(value) = token {
				return Ok((Expression { column: *column_number, variant: ExpressionVariant::StringLiteral(*value) }, &tokens[1..]));
			}
			return Err(Error::NotYetImplemented(None, *column_number, "Expressions that are not string literals".into()));
		}
		Err(Error::ExpectedExpression(1.try_into().unwrap()))
	}
}

#[derive(Debug)]
pub enum ExpressionVariant<'a> {
	StringLiteral(&'a str),
}

pub fn parse_line<'a>(mut tokens: &[(NonZeroUsize, Token<'a>)]) -> Result<Box<[Statement<'a>]>, Error> {
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