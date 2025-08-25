use std::num::NonZeroUsize;

use crate::mavagk_basic::{error::Error, token::{IdentifierType, Token, TokenVariant}};

#[derive(Debug)]
pub struct Statement<'a> {
	pub variant: StatementVariant<'a>,
	pub column: NonZeroUsize,
}

impl<'a> Statement<'a> {
	pub fn parse<'b>(mut tokens: &'b [Token<'a>]) -> Result<Option<(Self, &'b [Token<'a>])>, Error> {
		// Strip leading colons
		while matches!(tokens.first(), Some(Token { variant: TokenVariant::Colon, .. })) {
			tokens = &tokens[1..];
		}
		// Get the length of this expression
		let statement_length = Self::get_statement_length(tokens);
		let (tokens, rest_of_tokens) = tokens.split_at(statement_length);
		// Get first token or return if we are at the end of the tokens
		let identifier_token = match tokens.first() {
			Some(token) => token,
			None => return Ok(None),
		};
		// Parse depending on keyword
		match identifier_token.variant {
			// PRINT
			TokenVariant::Identifier { name: "print", identifier_type: IdentifierType::UnmarkedNumber, is_optional: false } => {
				let mut remaining_tokens = &tokens[1..];
				let mut expressions = Vec::new();
				while !remaining_tokens.is_empty() {
					match &remaining_tokens[0] {
						Token { variant: TokenVariant::Comma, start_column, end_column: _ } => {
							expressions.push(Expression { variant: ExpressionVariant::PrintComma, column: *start_column });
							remaining_tokens = &remaining_tokens[1..];
							continue;
						}
						Token { variant: TokenVariant::Semicolon, start_column, end_column: _ } => {
							expressions.push(Expression { variant: ExpressionVariant::PrintSemicolon, column: *start_column });
							remaining_tokens = &remaining_tokens[1..];
							continue;
						}
						_ => {}
					}
					let expression;
					(expression, remaining_tokens) = match Expression::parse(remaining_tokens, identifier_token.end_column)? {
						None => break,
						Some(result) => result,
					};
					expressions.push(expression);
				}
				debug_assert!(remaining_tokens.is_empty());
				Ok(Some((Self { column: identifier_token.start_column, variant: StatementVariant::Print(expressions.into()) }, rest_of_tokens)))
			}
			_ => Err(Error::NotYetImplemented(None, identifier_token.start_column, "Statements that are not print statements".into())),
		}
	}

	fn get_statement_length(tokens: &[Token<'a>]) -> usize {
		let mut parenthesis_depth = 0usize;
		for (index, token) in tokens.iter().enumerate() {
			match token.variant {
				TokenVariant::LeftParenthesis => parenthesis_depth += 1,
				TokenVariant::RightParenthesis => parenthesis_depth = parenthesis_depth.saturating_sub(1),
				TokenVariant::Colon if parenthesis_depth == 0 => return index,
				_ => {}
			}
		}
		tokens.len()
	}
}

#[derive(Debug)]
pub enum StatementVariant<'a> {
	Print(Box<[Expression<'a>]>),
}

#[derive(Debug)]
pub struct Expression<'a> {
	pub variant: ExpressionVariant<'a>,
	pub column: NonZeroUsize,
}

impl<'a> Expression<'a> {
	pub fn parse<'b>(tokens: &'b [Token<'a>], start_column: NonZeroUsize) -> Result<Option<(Self, &'b [Token<'a>])>, Error> {
		// Get the tokens for this expression or return if no tokens where passed in
		let expression_length = Self::get_expression_length(tokens);
		if expression_length == 0 {
			return Ok(None);
		}
		let (expression_tokens, tokens_after_expression_tokens) = tokens.split_at(expression_length);
		// Parse each token
		let mut maybe_parsed_tokens = Vec::new();
		for token in expression_tokens {
			match token.variant {
				TokenVariant::StringLiteral(value) => maybe_parsed_tokens.push(MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::StringLiteral(value), column: token.start_column })),
				TokenVariant::NumericLiteral(value) => maybe_parsed_tokens.push(MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::NumericLiteral(value), column: token.start_column })),
				_ => return Err(Error::NotYetImplemented(None, start_column, "other expressions".into())),
			}
		}
		// Return
		debug_assert!(maybe_parsed_tokens.len() == 1);
		let expression = match maybe_parsed_tokens.into_iter().next() {
			Some(MaybeParsedToken::Expression(expression)) => expression,
			_ => panic!()
		};
		Ok(Some((expression, tokens_after_expression_tokens)))
	}

	/// Takes in a list of tokens and returns how many form one expression given the following productions:
	///
	/// `expression = operand (binary-operator operand)*`
	///
	/// `operand = unary-operator operand / (fn-keyword? identifier)? left-parenthesis parentheses-content right-parenthesis`
	fn get_expression_length(tokens: &[Token<'a>]) -> usize {
		let mut last_token: Option<&TokenVariant<'_>> = None;
		let mut parenthesis_depth = 0usize;
		for (index, token) in tokens.iter().enumerate() {
			if matches!(token.variant, TokenVariant::LeftParenthesis) {
				parenthesis_depth += 1;
			}
			if matches!(token.variant, TokenVariant::RightParenthesis) {
				parenthesis_depth = parenthesis_depth.saturating_sub(1);
			}
			if parenthesis_depth > 0 {
				continue;
			}
			if matches!(last_token, Some(TokenVariant::Identifier { .. } | TokenVariant::NumericLiteral(..) | TokenVariant::StringLiteral(..) | TokenVariant::RightParenthesis)) &&
				matches!(token.variant, TokenVariant::Identifier { .. } | TokenVariant::NumericLiteral(..) | TokenVariant::StringLiteral(..)) &&
				!last_token.unwrap().is_unary_operator() && !token.variant.is_binary_operator() && !last_token.unwrap().is_binary_operator() &&
				!matches!(last_token, Some(TokenVariant::Identifier { name, identifier_type: IdentifierType::UnmarkedNumber, is_optional: false }) if name.eq_ignore_ascii_case("fn"))
			{
				return index;
			}
			if matches!(token.variant, TokenVariant::Colon | TokenVariant::Comma | TokenVariant::RightParenthesis | TokenVariant::Semicolon) {
				return index
			}
			last_token = Some(&token.variant);
		}
		tokens.len()
	}
}

#[derive(Debug)]
pub enum ExpressionVariant<'a> {
	StringLiteral(&'a str),
	NumericLiteral(&'a str),
	PrintComma,
	PrintSemicolon,
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

enum MaybeParsedToken<'a> {
	Token(Token<'a>),
	Expression(Expression<'a>),
}