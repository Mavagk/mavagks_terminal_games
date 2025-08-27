use std::num::NonZeroUsize;

use num::BigInt;

use crate::mavagk_basic::{error::Error, token::{IdentifierType, Token, TokenVariant}};

#[derive(Debug)]
pub struct Statement {
	pub variant: StatementVariant,
	pub column: NonZeroUsize,
}

impl Statement {
	pub fn parse<'a, 'b>(mut tokens: &'b [Token<'a>]) -> Result<Option<(Self, &'b [Token<'a>])>, Error> {
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
			TokenVariant::Identifier { name, identifier_type: IdentifierType::UnmarkedNumber, is_optional: false } if name.eq_ignore_ascii_case("print") => {
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

	fn get_statement_length<'a>(tokens: &[Token<'a>]) -> usize {
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

	pub fn print(&self, depth: usize) {
		for _ in 0..depth {
			print!("-");
		}
		print!(" {:03}: ", self.column);
		match &self.variant {
			StatementVariant::Print(arguments) => {
				print!("Print");
				println!();
				for argument in arguments {
					argument.print(depth + 1);
				}
			}
		}
	}
}

#[derive(Debug)]
pub enum StatementVariant {
	Print(Box<[Expression]>),
}

#[derive(Debug)]
pub struct Expression {
	pub variant: ExpressionVariant,
	pub column: NonZeroUsize,
}

impl Expression {
	pub fn parse<'a, 'b>(tokens: &'b [Token<'a>], start_column: NonZeroUsize) -> Result<Option<(Self, &'b [Token<'a>])>, Error> {
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
								Token { variant: TokenVariant::Identifier { .. }, start_column, .. } => (Some(&tokens[function_identifier_index]), *start_column),
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
								while !parentheses_content_tokens_left.is_empty() {
									if Self::get_expression_length(parentheses_content_tokens_left) == 0 && parentheses_content_tokens_left.len() != 0 {
										return Err(Error::FunctionArgumentsNotCommaSeparated(tokens[start_parenthesis_index].end_column));
									}
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
								maybe_parsed_tokens.push(MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::IdentifierOrFunction { name: name.into(), identifier_type, is_optional, arguments: function_arguments.into(), uses_fn_keyword: is_fn_function, has_parentheses: true }, column: function_start_column }));
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
				_ if (token.variant.is_binary_operator() || token.variant.is_unary_operator()) && parentheses_depth == 0 => {
					maybe_parsed_tokens.push(MaybeParsedToken::Token(token));
				}
				// Literals should be copied across
				TokenVariant::StringLiteral(value) => maybe_parsed_tokens.push(MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::StringLiteral((*value).into()), column: token.start_column })),
				TokenVariant::IntegerLiteral(value) => maybe_parsed_tokens.push(MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::IntegerLiteral(value.clone()), column: token.start_column})),
				TokenVariant::FloatLiteral { value, is_imaginary } => maybe_parsed_tokens.push(MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::FloatLiteral { value: *value, is_imaginary: *is_imaginary }, column: token.start_column})),
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
						_ if matches!(next_token, Some(Token { variant: TokenVariant::LeftParenthesis, .. })) => {}
						_ => match last_token {
							// If the last token was a "fn" keyword, this is a fn identifier without arguments
							Some(Token { variant: TokenVariant::Identifier { name: last_token_name, identifier_type: IdentifierType::UnmarkedNumber, is_optional: false }, .. }) if last_token_name.eq_ignore_ascii_case("fn") =>
								maybe_parsed_tokens.push(MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::IdentifierOrFunction { name: (*name).into(), identifier_type: *identifier_type, is_optional: *is_optional, arguments: Box::default(), uses_fn_keyword: true, has_parentheses: false }, column: last_token.unwrap().start_column })),
							// Else it is a non-fn identifier
							_ => maybe_parsed_tokens.push(MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::IdentifierOrFunction { name: (*name).into(), identifier_type: *identifier_type, is_optional: *is_optional, arguments: Box::default(), uses_fn_keyword: false, has_parentheses: false }, column: token.start_column })),
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
		// Parse operators
		'a: loop {
			// Exponentiation
			'b: for index in 0..maybe_parsed_tokens.len() {
				if index == 0 || index == maybe_parsed_tokens.len() - 1 {
					continue 'b;
				}
				let column = match maybe_parsed_tokens[index] {
					MaybeParsedToken::Token(Token { variant: TokenVariant::Operator("↑" | "^"), start_column, .. }) => *start_column,
					_ => continue 'b,
				};
				if !(matches!(&maybe_parsed_tokens[index - 1], MaybeParsedToken::Expression(_)) && matches!(&maybe_parsed_tokens[index + 1], MaybeParsedToken::Expression(_))) {
					continue 'b;
				}
				let left_expression = match maybe_parsed_tokens.remove(index - 1) {
					MaybeParsedToken::Expression(expression) => expression,
					_ => unreachable!(),
				};
				let right_expression = match maybe_parsed_tokens.remove(index) {
					MaybeParsedToken::Expression(expression) => expression,
					_ => unreachable!(),
				};
				maybe_parsed_tokens[index - 1] = MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::Exponentiation(Box::new(left_expression), Box::new(right_expression)), column });
				continue 'a;
			}
			// Unary negation and plus
			'b: for index in (0..maybe_parsed_tokens.len()).rev() {
				if index == maybe_parsed_tokens.len() - 1 {
					continue 'b;
				}
				let (column, is_plus) = match maybe_parsed_tokens[index] {
					MaybeParsedToken::Token(Token { variant: TokenVariant::Operator("-"), start_column, .. }) => (*start_column, false),
					MaybeParsedToken::Token(Token { variant: TokenVariant::Operator("+"), start_column, .. }) => (*start_column, true),
					_ => continue 'b,
				};
				match index.checked_sub(1) {
					Some(left_index) => match &maybe_parsed_tokens[left_index] {
						MaybeParsedToken::Expression(..) => continue 'b,
						MaybeParsedToken::Token(..) => {}
					}
					None => {}
				}
				if !matches!(&maybe_parsed_tokens[index + 1], MaybeParsedToken::Expression(_)) {
					continue 'b;
				}
				let right_expression = match maybe_parsed_tokens.remove(index + 1) {
					MaybeParsedToken::Expression(expression) => expression,
					_ => unreachable!(),
				};
				maybe_parsed_tokens[index] = match is_plus {
					false => MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::Negation(Box::new(right_expression)), column }),
					true => MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::UnaryPlus(Box::new(right_expression)), column }),
				};
				continue 'a;
			}
			// Multiplication and division
			'b: for index in 0..maybe_parsed_tokens.len() {
				if index == 0 || index == maybe_parsed_tokens.len() - 1 {
					continue 'b;
				}
				let (column, is_division) = match maybe_parsed_tokens[index] {
					MaybeParsedToken::Token(Token { variant: TokenVariant::Operator("*"), start_column, .. }) => (*start_column, false),
					MaybeParsedToken::Token(Token { variant: TokenVariant::Operator("/"), start_column, .. }) => (*start_column, true),
					_ => continue 'b,
				};
				if !(matches!(&maybe_parsed_tokens[index - 1], MaybeParsedToken::Expression(_)) && matches!(&maybe_parsed_tokens[index + 1], MaybeParsedToken::Expression(_))) {
					continue 'b;
				}
				let left_expression = match maybe_parsed_tokens.remove(index - 1) {
					MaybeParsedToken::Expression(expression) => expression,
					_ => unreachable!(),
				};
				let right_expression = match maybe_parsed_tokens.remove(index) {
					MaybeParsedToken::Expression(expression) => expression,
					_ => unreachable!(),
				};
				maybe_parsed_tokens[index - 1] = match is_division {
					false => MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::Multiplication(Box::new(left_expression), Box::new(right_expression)), column }),
					true => MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::Division(Box::new(left_expression), Box::new(right_expression)), column }),
				};
				continue 'a;
			}
			// Addition and subtraction
			'b: for index in 0..maybe_parsed_tokens.len() {
				if index == 0 || index == maybe_parsed_tokens.len() - 1 {
					continue 'b;
				}
				let (column, is_subtraction) = match maybe_parsed_tokens[index] {
					MaybeParsedToken::Token(Token { variant: TokenVariant::Operator("+"), start_column, .. }) => (*start_column, false),
					MaybeParsedToken::Token(Token { variant: TokenVariant::Operator("-"), start_column, .. }) => (*start_column, true),
					_ => continue 'b,
				};
				if !(matches!(&maybe_parsed_tokens[index - 1], MaybeParsedToken::Expression(_)) && matches!(&maybe_parsed_tokens[index + 1], MaybeParsedToken::Expression(_))) {
					continue 'b;
				}
				let left_expression = match maybe_parsed_tokens.remove(index - 1) {
					MaybeParsedToken::Expression(expression) => expression,
					_ => unreachable!(),
				};
				let right_expression = match maybe_parsed_tokens.remove(index) {
					MaybeParsedToken::Expression(expression) => expression,
					_ => unreachable!(),
				};
				maybe_parsed_tokens[index - 1] = match is_subtraction {
					false => MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::AdditionConcatenation(Box::new(left_expression), Box::new(right_expression)), column }),
					true => MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::Subtraction(Box::new(left_expression), Box::new(right_expression)), column }),
				};
				continue 'a;
			}
			// Comparison
			'b: for index in 0..maybe_parsed_tokens.len() {
				if index == 0 || index == maybe_parsed_tokens.len() - 1 {
					continue 'b;
				}
				let (column, operator) = match maybe_parsed_tokens[index] {
					MaybeParsedToken::Token(Token { variant: TokenVariant::Operator(operator), start_column, .. }) if matches!(*operator, "<" | "<=" | ">" | ">=" | "=" | "<>" | "=<" | "=>" | "><") => (*start_column, *operator),
					_ => continue 'b,
				};
				if !(matches!(&maybe_parsed_tokens[index - 1], MaybeParsedToken::Expression(_)) && matches!(&maybe_parsed_tokens[index + 1], MaybeParsedToken::Expression(_))) {
					continue 'b;
				}
				let left_expression = match maybe_parsed_tokens.remove(index - 1) {
					MaybeParsedToken::Expression(expression) => expression,
					_ => unreachable!(),
				};
				let right_expression = match maybe_parsed_tokens.remove(index) {
					MaybeParsedToken::Expression(expression) => expression,
					_ => unreachable!(),
				};
				maybe_parsed_tokens[index - 1] = match operator {
					"<" => MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::LessThan(Box::new(left_expression), Box::new(right_expression)), column }),
					"<=" | "=<" => MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::LessThanOrEqualTo(Box::new(left_expression), Box::new(right_expression)), column }),
					">" => MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::GreaterThan(Box::new(left_expression), Box::new(right_expression)), column }),
					">=" | "=>" => MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::GreaterThanOrEqualTo(Box::new(left_expression), Box::new(right_expression)), column }),
					"=" => MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::EqualTo(Box::new(left_expression), Box::new(right_expression)), column }),
					"<>" | "><" => MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::NotEqualTo(Box::new(left_expression), Box::new(right_expression)), column }),
					_ => unreachable!(),
				};
				continue 'a;
			}
			// Not
			'b: for index in (0..maybe_parsed_tokens.len()).rev() {
				if index == maybe_parsed_tokens.len() - 1 {
					continue 'b;
				}
				let column = match maybe_parsed_tokens[index] {
					MaybeParsedToken::Token(Token { variant: TokenVariant::Identifier { name, identifier_type: IdentifierType::UnmarkedNumber, is_optional: false }, start_column, .. })
						if name.eq_ignore_ascii_case("not") => *start_column,
					_ => continue 'b,
				};
				match index.checked_sub(1) {
					Some(left_index) => match &maybe_parsed_tokens[left_index] {
						MaybeParsedToken::Expression(..) => continue 'b,
						MaybeParsedToken::Token(..) => {}
					}
					None => {}
				}
				if !matches!(&maybe_parsed_tokens[index + 1], MaybeParsedToken::Expression(_)) {
					continue 'b;
				}
				let right_expression = match maybe_parsed_tokens.remove(index + 1) {
					MaybeParsedToken::Expression(expression) => expression,
					_ => unreachable!(),
				};
				maybe_parsed_tokens[index] = MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::Not(Box::new(right_expression)), column });
				continue 'a;
			}
			// And
			'b: for index in 0..maybe_parsed_tokens.len() {
				if index == 0 || index == maybe_parsed_tokens.len() - 1 {
					continue 'b;
				}
				let column = match maybe_parsed_tokens[index] {
					MaybeParsedToken::Token(Token { variant: TokenVariant::Identifier { name, identifier_type: IdentifierType::UnmarkedNumber, is_optional: false }, start_column, .. })
						if name.eq_ignore_ascii_case("and") => *start_column,
					_ => continue 'b,
				};
				if !(matches!(&maybe_parsed_tokens[index - 1], MaybeParsedToken::Expression(_)) && matches!(&maybe_parsed_tokens[index + 1], MaybeParsedToken::Expression(_))) {
					continue 'b;
				}
				let left_expression = match maybe_parsed_tokens.remove(index - 1) {
					MaybeParsedToken::Expression(expression) => expression,
					_ => unreachable!(),
				};
				let right_expression = match maybe_parsed_tokens.remove(index) {
					MaybeParsedToken::Expression(expression) => expression,
					_ => unreachable!(),
				};
				maybe_parsed_tokens[index - 1] = MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::And(Box::new(left_expression), Box::new(right_expression)), column });
				continue 'a;
			}
			// Or
			'b: for index in 0..maybe_parsed_tokens.len() {
				if index == 0 || index == maybe_parsed_tokens.len() - 1 {
					continue 'b;
				}
				let column = match maybe_parsed_tokens[index] {
					MaybeParsedToken::Token(Token { variant: TokenVariant::Identifier { name, identifier_type: IdentifierType::UnmarkedNumber, is_optional: false }, start_column, .. })
						if name.eq_ignore_ascii_case("or") => *start_column,
					_ => continue 'b,
				};
				if !(matches!(&maybe_parsed_tokens[index - 1], MaybeParsedToken::Expression(_)) && matches!(&maybe_parsed_tokens[index + 1], MaybeParsedToken::Expression(_))) {
					continue 'b;
				}
				let left_expression = match maybe_parsed_tokens.remove(index - 1) {
					MaybeParsedToken::Expression(expression) => expression,
					_ => unreachable!(),
				};
				let right_expression = match maybe_parsed_tokens.remove(index) {
					MaybeParsedToken::Expression(expression) => expression,
					_ => unreachable!(),
				};
				maybe_parsed_tokens[index - 1] = MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::Or(Box::new(left_expression), Box::new(right_expression)), column });
				continue 'a;
			}
			// Concatenation
			'b: for index in 0..maybe_parsed_tokens.len() {
				if index == 0 || index == maybe_parsed_tokens.len() - 1 {
					continue 'b;
				}
				let column = match maybe_parsed_tokens[index] {
					MaybeParsedToken::Token(Token { variant: TokenVariant::Operator("&"), start_column, .. }) => *start_column,
					_ => continue 'b,
				};
				if !(matches!(&maybe_parsed_tokens[index - 1], MaybeParsedToken::Expression(_)) && matches!(&maybe_parsed_tokens[index + 1], MaybeParsedToken::Expression(_))) {
					continue 'b;
				}
				let left_expression = match maybe_parsed_tokens.remove(index - 1) {
					MaybeParsedToken::Expression(expression) => expression,
					_ => unreachable!(),
				};
				let right_expression = match maybe_parsed_tokens.remove(index) {
					MaybeParsedToken::Expression(expression) => expression,
					_ => unreachable!(),
				};
				maybe_parsed_tokens[index - 1] = MaybeParsedToken::Expression(Expression { variant: ExpressionVariant::Concatenation(Box::new(left_expression), Box::new(right_expression)), column });
				continue 'a;
			}
			break 'a;
		}
		for maybe_parsed_token in maybe_parsed_tokens.iter() {
			match maybe_parsed_token {
				MaybeParsedToken::Token(Token { start_column, .. }) => return Err(Error::InvalidOperator(*start_column)),
				_ => {}
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
	fn get_expression_length<'a>(tokens: &[Token<'a>]) -> usize {
		let mut last_token: Option<&TokenVariant<'_>> = None;
		let mut parenthesis_depth = 0usize;
		for (index, token) in tokens.iter().enumerate() {
			if matches!(token.variant, TokenVariant::LeftParenthesis) {
				parenthesis_depth += 1;
			}
			if matches!(token.variant, TokenVariant::RightParenthesis) {
				parenthesis_depth = parenthesis_depth.saturating_sub(1);
			}
			if matches!(token.variant, TokenVariant::LeftParenthesis) && matches!(last_token, Some(TokenVariant::IntegerLiteral(..) | TokenVariant::FloatLiteral { .. } | TokenVariant::StringLiteral(_))) && parenthesis_depth == 1 {
				return index;
			}
			if parenthesis_depth == 0 {
				if matches!(last_token, Some(TokenVariant::Identifier { .. } | TokenVariant::IntegerLiteral(..) | TokenVariant::FloatLiteral { .. } | TokenVariant::StringLiteral(..) | TokenVariant::RightParenthesis)) &&
					matches!(token.variant, TokenVariant::Identifier { .. } | TokenVariant::IntegerLiteral(..) | TokenVariant::FloatLiteral { .. } | TokenVariant::StringLiteral(..)) &&
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

	pub fn print(&self, depth: usize) {
		for _ in 0..depth {
			print!("-");
		}
		print!(" {:03}: ", self.column);
		match &self.variant {
			ExpressionVariant::FloatLiteral { value, is_imaginary } => {
				print!("Float Literal {value}");
				if *is_imaginary {
					print!(", Imaginary/i");
				}
			}
			ExpressionVariant::IntegerLiteral(value) => print!("Integer Literal {value}"),
			ExpressionVariant::StringLiteral(value) => print!("String Literal \"{value}\""),
			ExpressionVariant::PrintComma => print!("Comma"),
			ExpressionVariant::PrintSemicolon => print!("Semicolon"),
			ExpressionVariant::IdentifierOrFunction { name, identifier_type, is_optional, arguments, uses_fn_keyword, has_parentheses } => {
				print!("Identifier/Function \"{name}\", ");
				match identifier_type {
					IdentifierType::UnmarkedNumber => print!("Number/Unmarked"),
					IdentifierType::Integer => print!("Integer/%"),
					IdentifierType::String => print!("String/$"),
					IdentifierType::ComplexNumber => print!("Complex Number/#"),
				}
				if *is_optional {
					print!(", Optional/?");
				}
				if *uses_fn_keyword {
					print!(", Fn");
				}
				if *has_parentheses {
					print!(", Parenthesised/()");
				}
				println!();
				for argument in arguments {
					argument.print(depth + 1);
				}
			}
			ExpressionVariant::Exponentiation(left_operand, right_operand) => {
				print!("Exponentiation/^");
				println!();
				left_operand.print(depth + 1);
				right_operand.print(depth + 1);
			}
			ExpressionVariant::Negation(operand) => {
				print!("Negation/-");
				println!();
				operand.print(depth + 1);
			}
			ExpressionVariant::UnaryPlus(operand) => {
				print!("Unary Plus/+");
				println!();
				operand.print(depth + 1);
			}
			ExpressionVariant::Multiplication(left_operand, right_operand) => {
				print!("Multiplication/*");
				println!();
				left_operand.print(depth + 1);
				right_operand.print(depth + 1);
			}
			ExpressionVariant::Division(left_operand, right_operand) => {
				print!("Division//");
				println!();
				left_operand.print(depth + 1);
				right_operand.print(depth + 1);
			}
			ExpressionVariant::AdditionConcatenation(left_operand, right_operand) => {
				print!("Addition/Concatenation/+");
				println!();
				left_operand.print(depth + 1);
				right_operand.print(depth + 1);
			}
			ExpressionVariant::Subtraction(left_operand, right_operand) => {
				print!("Subtraction/-");
				println!();
				left_operand.print(depth + 1);
				right_operand.print(depth + 1);
			}
			ExpressionVariant::GreaterThan(left_operand, right_operand) => {
				print!("Greater Than/>");
				println!();
				left_operand.print(depth + 1);
				right_operand.print(depth + 1);
			}
			ExpressionVariant::LessThan(left_operand, right_operand) => {
				print!("Less Than/<");
				println!();
				left_operand.print(depth + 1);
				right_operand.print(depth + 1);
			}
			ExpressionVariant::GreaterThanOrEqualTo(left_operand, right_operand) => {
				print!("Greater Than or Equal to/>=");
				println!();
				left_operand.print(depth + 1);
				right_operand.print(depth + 1);
			}
			ExpressionVariant::LessThanOrEqualTo(left_operand, right_operand) => {
				print!("Less Than Or Equal to/<=");
				println!();
				left_operand.print(depth + 1);
				right_operand.print(depth + 1);
			}
			ExpressionVariant::EqualTo(left_operand, right_operand) => {
				print!("Equal to/=");
				println!();
				left_operand.print(depth + 1);
				right_operand.print(depth + 1);
			}
			ExpressionVariant::NotEqualTo(left_operand, right_operand) => {
				print!("Not Equal to/<>");
				println!();
				left_operand.print(depth + 1);
				right_operand.print(depth + 1);
			}
			ExpressionVariant::Not(operand) => {
				print!("Not");
				println!();
				operand.print(depth + 1);
			}
			ExpressionVariant::And(left_operand, right_operand) => {
				print!("And");
				println!();
				left_operand.print(depth + 1);
				right_operand.print(depth + 1);
			}
			ExpressionVariant::Or(left_operand, right_operand) => {
				print!("Or");
				println!();
				left_operand.print(depth + 1);
				right_operand.print(depth + 1);
			}
			ExpressionVariant::Concatenation(left_operand, right_operand) => {
				print!("Concatenation/&");
				println!();
				left_operand.print(depth + 1);
				right_operand.print(depth + 1);
			}
		}
		if matches!(self.variant, ExpressionVariant::IntegerLiteral { .. } | ExpressionVariant::FloatLiteral { .. } | ExpressionVariant::PrintComma | ExpressionVariant::PrintSemicolon | ExpressionVariant::StringLiteral(..)) {
			println!();
		}
	}
}

#[derive(Debug)]
pub enum ExpressionVariant {
	StringLiteral(Box<str>),
	IntegerLiteral(BigInt),
	FloatLiteral { value: f64, is_imaginary: bool },
	PrintComma,
	PrintSemicolon,
	IdentifierOrFunction { name: Box<str>, identifier_type: IdentifierType, is_optional: bool, arguments: Box<[Expression]>, uses_fn_keyword: bool, has_parentheses: bool },
	Exponentiation(Box<Expression>, Box<Expression>),
	Negation(Box<Expression>),
	UnaryPlus(Box<Expression>),
	Multiplication(Box<Expression>, Box<Expression>),
	Division(Box<Expression>, Box<Expression>),
	AdditionConcatenation(Box<Expression>, Box<Expression>),
	Subtraction(Box<Expression>, Box<Expression>),
	LessThan(Box<Expression>, Box<Expression>),
	GreaterThan(Box<Expression>, Box<Expression>),
	EqualTo(Box<Expression>, Box<Expression>),
	NotEqualTo(Box<Expression>, Box<Expression>),
	LessThanOrEqualTo(Box<Expression>, Box<Expression>),
	GreaterThanOrEqualTo(Box<Expression>, Box<Expression>),
	Not(Box<Expression>),
	And(Box<Expression>, Box<Expression>),
	Or(Box<Expression>, Box<Expression>),
	Concatenation(Box<Expression>, Box<Expression>),
}

pub fn parse_line<'a>(mut tokens: &[Token<'a>]) -> Result<Box<[Statement]>, Error> {
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
	Expression(Expression),
}