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
		// First parse run
		let mut last_token: Option<&Token<'_>> = None;
		let mut parentheses_depth = 0usize;
		let mut maybe_parsed_tokens = Vec::new();
		let mut start_parenthesis_index = 0;
		for (index, token) in expression_tokens.iter().enumerate() {
			let next_token = expression_tokens.get(index + 1);
			match &token.variant {
				// Left parentheses
				TokenVariant::LeftParenthesis => {
					if parentheses_depth == 0 {
						start_parenthesis_index = index;
					}
					parentheses_depth += 1;
				}
				// Right parentheses
				TokenVariant::RightParenthesis => {
					parentheses_depth = parentheses_depth.checked_sub(1).ok_or_else(|| Error::MoreRightParenthesesThanLeftParentheses(token.start_column))?;
					// Parse the parenthesised area/function that this is closing
					if parentheses_depth == 0 {
						let parentheses_content_tokens = &tokens[start_parenthesis_index + 1..index];
						let mut function_start_column;
						let function_identifier;
						(function_identifier, function_start_column) = match start_parenthesis_index.checked_sub(1) {
							Some(function_identifier_index) => match &tokens[function_identifier_index] {
								Token { variant: TokenVariant::Identifier { .. }, start_column, .. } => (Some(token), *start_column),
								_ => (None, 1.try_into().unwrap()),
							},
							None => (None, 1.try_into().unwrap()),
						};
						let is_fn_function = match start_parenthesis_index.checked_sub(2) {
							None => false,
							Some(token_index) => match &tokens[token_index] {
								Token { variant: TokenVariant::Identifier { name, identifier_type: IdentifierType::UnmarkedNumber, is_optional: false }, start_column, .. } if name.eq_ignore_ascii_case("fn") => {
									function_start_column = *start_column;
									true
								}
								_ => false,
							},
						};
						match function_identifier {
							// If we are parsing a function
							Some(function_identifier) => {
								let mut parentheses_content_tokens_left = parentheses_content_tokens;
								let mut function_arguments = Vec::new();
								if Self::get_expression_length(parentheses_content_tokens_left) == 0 && parentheses_content_tokens_left.len() != 0 {
									return Err(Error::FunctionArgumentsNotCommaSeparated(tokens[start_parenthesis_index].end_column));
								}
								while !parentheses_content_tokens_left.is_empty() {
									let parsed_argument_expression;
									(parsed_argument_expression, parentheses_content_tokens_left) = Self::parse(parentheses_content_tokens_left, parentheses_content_tokens_left[0].start_column)?.unwrap();
									function_arguments.push(parsed_argument_expression);
									match parentheses_content_tokens_left.get(0) {
										Some(Token { variant: TokenVariant::Comma, .. }) => parentheses_content_tokens_left = &parentheses_content_tokens_left[1..],
										None => {}
										Some(Token { variant: _, start_column, .. }) => return Err(Error::FunctionArgumentsNotCommaSeparated(*start_column))
									}
								}
								let (name, identifier_type, is_optional) = match function_identifier {
									Token { variant: TokenVariant::Identifier { name, identifier_type, is_optional }, .. } => (*name, *identifier_type, *is_optional),
									_ => unreachable!(),
								};
								maybe_parsed_tokens.push(MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::IdentifierOrFunction { name, identifier_type, is_optional, arguments: function_arguments.into(), uses_fn_keyword: is_fn_function, has_parentheses: true }, column: function_start_column }));
							}
							// If we are just parsing some brackets that are not part of a function
							None => {
								if parentheses_content_tokens.len() == 0 {
									return Err(Error::NothingInParentheses(tokens[start_parenthesis_index].end_column));
								}
								if Self::get_expression_length(parentheses_content_tokens) != parentheses_content_tokens.len() {
									return Err(Error::ParenthesesDoNotContainOneExpression(tokens[start_parenthesis_index].end_column));
								}
								let content_parsed = Self::parse(parentheses_content_tokens, tokens[start_parenthesis_index].end_column)?.unwrap().0;
								maybe_parsed_tokens.push(MaybeParsedToken::Expression(content_parsed));
							}
						}
					}
				}
				// Operators
				_ if token.variant.is_binary_operator() || token.variant.is_unary_operator() => {
					maybe_parsed_tokens.push(MaybeParsedToken::Token(token));
				}
				// Literals should be copied across
				TokenVariant::StringLiteral(value) => maybe_parsed_tokens.push(MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::StringLiteral(value), column: token.start_column })),
				TokenVariant::NumericLiteral(value) if parentheses_depth == 0 => maybe_parsed_tokens.push(MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::NumericLiteral(value), column: token.start_column })),
				// Identifiers
				TokenVariant::Identifier { name, identifier_type, is_optional } if parentheses_depth == 0 => {
					match () {
						// Ignore fn keywords or throw an error if they are not followed by an identifier.
						_ if matches!(token, Token { variant: TokenVariant::Identifier { name, identifier_type: IdentifierType::UnmarkedNumber, is_optional: false }, .. } if name.eq_ignore_ascii_case("fn")) => {
							match next_token {
								Some(Token { variant: TokenVariant::Identifier { name: next_name, .. }, .. }) if !next_name.eq_ignore_ascii_case("fn") => {}
								_ => return Err(Error::FnWithoutIdentifier(start_column)),
							}
						}
						// If the identifier has a open parenthesis to the right, do nothing, we will parse it once we get to the matching closing parenthesis
						_ if !matches!(next_token, Some(Token { variant: TokenVariant::LeftParenthesis, .. })) => {}
						_ => match last_token {
							// If the last token was a "fn" keyword, this is a fn identifier without arguments
							Some(Token { variant: TokenVariant::Identifier { name, identifier_type: IdentifierType::UnmarkedNumber, is_optional: false }, .. }) if name.eq_ignore_ascii_case("fn") =>
								maybe_parsed_tokens.push(MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::IdentifierOrFunction { name, identifier_type: *identifier_type, is_optional: *is_optional, arguments: Box::default(), uses_fn_keyword: true, has_parentheses: false }, column: last_token.unwrap().start_column })),
							// Else it is a non-fn identifier
							_ => maybe_parsed_tokens.push(MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::IdentifierOrFunction { name, identifier_type: *identifier_type, is_optional: *is_optional, arguments: Box::default(), uses_fn_keyword: false, has_parentheses: false }, column: start_column })),
						}
					}
				}
				_ => {}//return Err(Error::NotYetImplemented(None, start_column, "other expressions".into())),
			}
			last_token = Some(token);
		}
		if parentheses_depth > 0 {
			return Err(Error::MoreLeftParenthesesThanRightParentheses(last_token.unwrap().end_column));
		}
		// Return
		println!("{maybe_parsed_tokens:?}");
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
			if parenthesis_depth == 0 {
				if matches!(last_token, Some(TokenVariant::Identifier { .. } | TokenVariant::NumericLiteral(..) | TokenVariant::StringLiteral(..) | TokenVariant::RightParenthesis)) &&
					matches!(token.variant, TokenVariant::Identifier { .. } | TokenVariant::NumericLiteral(..) | TokenVariant::StringLiteral(..)) &&
					!last_token.unwrap().is_unary_operator() && !token.variant.is_binary_operator() && !last_token.unwrap().is_binary_operator() &&
					!matches!(last_token, Some(TokenVariant::Identifier { name, identifier_type: IdentifierType::UnmarkedNumber, is_optional: false }) if name.eq_ignore_ascii_case("fn"))
				{
					return index;
				}
				if matches!(token.variant, TokenVariant::Colon | TokenVariant::Comma | TokenVariant::Semicolon) {
					return index
				}
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
	IdentifierOrFunction { name: &'a str, identifier_type: IdentifierType, is_optional: bool, arguments: Box<[Expression<'a>]>, uses_fn_keyword: bool, has_parentheses: bool }
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

#[derive(Debug)]
enum MaybeParsedToken<'a, 'b> {
	Token(&'b Token<'a>),
	Expression(Expression<'a>),
}