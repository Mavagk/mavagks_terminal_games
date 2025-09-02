use std::{mem::replace, num::NonZeroUsize, rc::Rc};

use num::BigInt;

use crate::mavagk_basic::{error::{Error, ErrorVariant}, token::{IdentifierType, Keyword, Token, TokenVariant}};

#[derive(Debug)]
pub struct Statement {
	pub variant: StatementVariant,
	pub column: NonZeroUsize,
}

impl Statement {
	pub fn parse<'a, 'b>(mut tokens: &'b [Token<'a>], line_number: Option<&BigInt>) -> Result<Option<(Self, &'b [Token<'a>])>, Error> {
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
		// Parse assignments without LET
		'a: {
			// Check if this a non-LET assignment
			let l_value_length = Expression::get_l_value_length(tokens);
			if l_value_length == 0 {
				break 'a;
			}
			let equal_sign_end_column = match tokens.get(l_value_length) {
				None => break 'a,
				Some(Token { variant: TokenVariant::Operator(Some(BinaryOperator::Equal), _), end_column, .. }) => *end_column,
				_ => break 'a,
			};
			// Get l-value
			let l_value_expression;
			(l_value_expression, _, _) = Expression::parse_expression_primary(&tokens[..l_value_length], line_number, identifier_token.end_column)?.unwrap();
			// Get r-value expression
				let r_value_expression;
				let remaining_tokens;
				(r_value_expression, remaining_tokens, _) = match Expression::parse_expression(&tokens[l_value_length + 1..], line_number, equal_sign_end_column)? {
					None => return Err(Error { variant: ErrorVariant::ExpectedExpression, line_number: line_number.cloned(), column_number: Some(equal_sign_end_column) }),
					Some((l_value_expression, remaining_tokens, expression_end_column)) => (l_value_expression, remaining_tokens, expression_end_column),
				};
				if remaining_tokens.len() != 0 {
					return Err(Error { variant: ErrorVariant::StatementShouldEnd, line_number: line_number.cloned(), column_number: Some(remaining_tokens[0].start_column) });
				}

				return Ok(Some((Self { column: identifier_token.start_column, variant: StatementVariant::Assign(l_value_expression, r_value_expression) }, rest_of_tokens)));
		}
		// Parse depending on keyword
		let keyword = match identifier_token.variant {
			TokenVariant::Identifier { keyword: Some(keyword), .. } => keyword,
			TokenVariant::Identifier { keyword: None, .. } =>
				return Err(Error { variant: ErrorVariant::NotYetImplemented("Many statements".into()), line_number: line_number.cloned(), column_number: Some(identifier_token.start_column) }),
			_ => return Err(Error { variant: ErrorVariant::ExpectedStatementKeyword, line_number: line_number.cloned(), column_number: Some(identifier_token.start_column) }),
		};
		match keyword {
			// LET
			Keyword::Let => {
				let mut remaining_tokens = &tokens[1..];
				// Get l-value expression
				let l_value_length = Expression::get_l_value_length(remaining_tokens);
				let l_value_expression;
				(l_value_expression, _, _) = match Expression::parse_expression_primary(&remaining_tokens[..l_value_length], line_number, identifier_token.end_column)? {
					None => return Err(Error { variant: ErrorVariant::ExpectedExpression, line_number: line_number.cloned(), column_number: Some(tokens[0].end_column) }),
					Some((l_value_expression, remaining_tokens, expression_end_column)) => (l_value_expression, remaining_tokens, expression_end_column),
				};
				remaining_tokens = &remaining_tokens[l_value_length..];
				// Expect equal sign
				let equal_sign_end_column = match remaining_tokens.get(0) {
					Some(Token { variant: TokenVariant::Operator(Some(BinaryOperator::Equal), _), end_column, .. }) => *end_column,
					Some(Token { start_column, .. }) => return Err(Error { variant: ErrorVariant::ExpectedEqualSign, line_number: line_number.cloned(), column_number: Some(*start_column) }),
					None => return Err(Error { variant: ErrorVariant::ExpectedEqualSign, line_number: line_number.cloned(), column_number: Some(tokens[l_value_length].end_column) }),
				};
				// Get r-value expression
				let r_value_expression;
				(r_value_expression, remaining_tokens, _) = match Expression::parse_expression(&remaining_tokens[1..], line_number, equal_sign_end_column)? {
					None => return Err(Error { variant: ErrorVariant::ExpectedExpression, line_number: line_number.cloned(), column_number: Some(equal_sign_end_column) }),
					Some((l_value_expression, remaining_tokens, expression_end_column)) => (l_value_expression, remaining_tokens, expression_end_column),
				};
				if remaining_tokens.len() != 0 {
					return Err(Error { variant: ErrorVariant::StatementShouldEnd, line_number: line_number.cloned(), column_number: Some(remaining_tokens[0].start_column) });
				}

				Ok(Some((Self { column: identifier_token.start_column, variant: StatementVariant::Assign(l_value_expression, r_value_expression) }, rest_of_tokens)))
			}
			// PRINT
			Keyword::Print => {
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
					(expression, remaining_tokens, _) = match Expression::parse_expression(remaining_tokens, line_number, identifier_token.end_column)? {
						None => break,
						Some(result) => result,
					};
					expressions.push(expression);
				}
				debug_assert!(remaining_tokens.is_empty());
				Ok(Some((Self { column: identifier_token.start_column, variant: StatementVariant::Print(expressions.into()) }, rest_of_tokens)))
			}
			// RUN / GOTO / GOSUB
			Keyword::Goto | Keyword::Run | Keyword::Gosub => {
				let mut remaining_tokens = &tokens[1..];
				let mut expression = None;
				if !remaining_tokens.is_empty() {
					(expression, remaining_tokens, _) = match Expression::parse_expression(remaining_tokens, line_number, identifier_token.end_column)? {
						None => return Err(Error { variant: ErrorVariant::ExpectedExpression, line_number: line_number.cloned(), column_number: Some(remaining_tokens[0].start_column) }),
						Some((result, remaining_tokens, expression_end_column)) => (Some(result), remaining_tokens, expression_end_column),
					};
					if !remaining_tokens.is_empty() {
						return Err(Error { variant: ErrorVariant::StatementShouldEnd, line_number: line_number.cloned(), column_number: Some(remaining_tokens[0].start_column) });
					}
				}
				let variant = match keyword {
					Keyword::Run => StatementVariant::Run(expression),
					Keyword::Goto => StatementVariant::Goto(expression),
					Keyword::Gosub => StatementVariant::Gosub(expression),
					_ => unreachable!(),
				};
				Ok(Some((Self { column: identifier_token.start_column, variant }, rest_of_tokens)))
			}
			Keyword::Fn => Err(Error { variant: ErrorVariant::ExpectedStatementKeyword, line_number: line_number.cloned(), column_number: Some(identifier_token.start_column) }),
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
				print!("PRINT");
				println!();
				for argument in arguments {
					argument.print(depth + 1);
				}
			}
			StatementVariant::Run(argument) => {
				print!("RUN");
				println!();
				if let Some(argument) = argument {
					argument.print(depth + 1);
				}
			}
			StatementVariant::Goto(argument) => {
				print!("GOTO");
				println!();
				if let Some(argument) = argument {
					argument.print(depth + 1);
				}
			}
			StatementVariant::Gosub(argument) => {
				print!("GOSUB");
				println!();
				if let Some(argument) = argument {
					argument.print(depth + 1);
				}
			}
			StatementVariant::Assign(l_value, r_value) => {
				print!("LET");
				println!();
				l_value.print(depth + 1);
				r_value.print(depth + 1);
			}
		}
	}
}

#[derive(Debug)]
pub enum StatementVariant {
	Print(Box<[Expression]>),
	Run(Option<Expression>),
	Goto(Option<Expression>),
	Gosub(Option<Expression>),
	Assign(Expression, Expression),
}

#[derive(Debug, PartialEq)]
pub struct Expression {
	pub variant: ExpressionVariant,
	pub column: NonZeroUsize,
}

impl Expression {
	pub fn parse_expression<'a, 'b>(tokens: &'b [Token<'a>], line_number: Option<&BigInt>, start_column: NonZeroUsize)-> Result<Option<(Self, &'b [Token<'a>], NonZeroUsize)>, Error> {
		let mut remaining_tokens = tokens;
		let mut end_column_of_last_token = start_column;
		let mut expression_primaries_and_their_unary_operators = Vec::new();
		let mut operators = Vec::new();
		'a: loop {
			// Get any unary operators before the expression primary
			let mut unary_operators_before_expression_primary = Vec::new();
			'b: loop {
				match remaining_tokens.get(0) {
					Some(Token { variant: TokenVariant::Operator(_, Some(unary_operator)), start_column, end_column }) |
					Some(Token { variant: TokenVariant::Identifier { unary_operator: Some(unary_operator), .. }, start_column, end_column, .. }) => {
						unary_operators_before_expression_primary.push((*unary_operator, *start_column));
						end_column_of_last_token = *end_column;
						remaining_tokens = &remaining_tokens[1..];
					}
					_ => break 'b,
				}
			}
			// Get expression primary
			let (expression_primary, tokens_after_expression_primary, end_column_of_expression_primary) = match Self::parse_expression_primary(remaining_tokens, line_number, start_column)? {
				Some((expression_primary, tokens_after_expression_primary, end_column_of_expression_primary)) =>
					(expression_primary, tokens_after_expression_primary, end_column_of_expression_primary),
				None => {
					if !expression_primaries_and_their_unary_operators.is_empty() {
						return Err(Error { variant: ErrorVariant::ExpectedExpressionPrimary, column_number: Some(end_column_of_last_token), line_number: line_number.cloned() });
					}
					if !unary_operators_before_expression_primary.is_empty() {
						return Err(Error { variant: ErrorVariant::UnaryOperatorsAtEndOfExpression, column_number: Some(end_column_of_last_token), line_number: line_number.cloned() });
					}
					break 'a;
				}
			};
			remaining_tokens = tokens_after_expression_primary;
			end_column_of_last_token = end_column_of_expression_primary;
			// Get binary operator or break
			let (binary_operator, binary_operator_start_column) = match remaining_tokens.get(0) {
				Some(Token { variant: TokenVariant::Operator(Some(binary_operator), _), start_column, end_column }) |
				Some(Token { variant: TokenVariant::Identifier { binary_operator: Some(binary_operator), .. }, start_column, end_column, .. }) => {
					end_column_of_last_token = *end_column;
					remaining_tokens = &remaining_tokens[1..];
					(*binary_operator, *start_column)
				}
				_ => {
					expression_primaries_and_their_unary_operators.push((expression_primary, unary_operators_before_expression_primary));
					break 'a;
				}
			};
			// Solve and push
			expression_primaries_and_their_unary_operators.push((expression_primary, unary_operators_before_expression_primary));
			Self::solve_operators_by_precedence(&mut expression_primaries_and_their_unary_operators, &mut operators, Some(binary_operator.get_operator_precedence()));
			operators.push((binary_operator, binary_operator_start_column));
		}
		// Solve
		Self::solve_operators_by_precedence(&mut expression_primaries_and_their_unary_operators, &mut operators, None);
		// Return
		debug_assert_eq!(expression_primaries_and_their_unary_operators.len(), 1);
		debug_assert_eq!(operators.len(), 0);
		debug_assert_eq!(expression_primaries_and_their_unary_operators[0].1.len(), 0);
		Ok(Some((expression_primaries_and_their_unary_operators.pop().unwrap().0, remaining_tokens, end_column_of_last_token)))
	}

	pub fn solve_operators_by_precedence(expression_stack: &mut Vec<(Expression, Vec<(UnaryOperator, NonZeroUsize)>)>, operator_stack: &mut Vec<(BinaryOperator, NonZeroUsize)>, precedence: Option<u8>) {
		loop {
			// Return if the operator precedence of the operator at the top of the stack is not greater than or equal to the input precedence
			let (binary_operator, binary_operator_start_column) = match operator_stack.last() {
				Some((operator, operator_start_column)) => {
					if let Some(precedence) = precedence && precedence < operator.get_operator_precedence()  {
						return;
					}
					(*operator, *operator_start_column)
				}
				None => break,
			};
			// Pop the rhs operator operand and parse wrap it in its unary operators
			let (mut rhs_expression, mut rhs_unary_operators) = expression_stack.pop().unwrap();
			while !rhs_unary_operators.is_empty() {
				let unary_operator = rhs_unary_operators.pop().unwrap();
				rhs_expression = unary_operator.0.to_expression(unary_operator.1, rhs_expression);
			}
			// Pop the lhs operator operand and parse wrap it in its unary operators that have a lower than or equal precedence to the binary operator
			let (mut lhs_expression, mut lhs_unary_operators) = expression_stack.pop().unwrap();
			while !lhs_unary_operators.is_empty() {
				if lhs_unary_operators.last().unwrap().0.get_operator_precedence() > binary_operator.get_operator_precedence() {
					break;
				}
				let unary_operator = lhs_unary_operators.pop().unwrap();
				lhs_expression = unary_operator.0.to_expression(unary_operator.1, lhs_expression);
			}
			// Push parsed expressions and unparsed unary operators
			expression_stack.push((binary_operator.to_expression(binary_operator_start_column, lhs_expression, rhs_expression), lhs_unary_operators));
			operator_stack.pop();
		}

		if precedence != None {
			return;
		}
		let (expression, unary_operators) = match expression_stack.get_mut(0) {
			Some((expression, unary_operators)) => (expression, unary_operators),
			None => return,
		};
		while !unary_operators.is_empty() {
			let unary_operator = unary_operators.pop().unwrap();
			*expression = unary_operator.0.to_expression(unary_operator.1, replace(expression, Expression { variant: ExpressionVariant::PrintComma, column: 1.try_into().unwrap() }));
		}
	}

	pub fn parse_expression_primary<'a, 'b>(tokens: &'b [Token<'a>], line_number: Option<&BigInt>, _start_column: NonZeroUsize)-> Result<Option<(Self, &'b [Token<'a>], NonZeroUsize)>, Error> {
		// Get the first token or return if there are no more tokens to parse
		let Token { variant: first_token_variant, start_column: first_token_start_column, end_column: first_token_end_column } = match tokens.first() {
			Some(first_token) => first_token,
			None => return Ok(None),
		};
		// Parse the expression primary
		Ok(Some(match first_token_variant {
			// Literals
			TokenVariant::IntegerLiteral(value) =>
				(Expression { variant: ExpressionVariant::IntegerLiteral(Rc::new(value.clone())), column: *first_token_start_column }, &tokens[1..], *first_token_end_column),
			TokenVariant::FloatLiteral { value, is_imaginary } =>
				(Expression { variant: ExpressionVariant::FloatLiteral { value: *value, is_imaginary: *is_imaginary }, column: *first_token_start_column }, &tokens[1..], *first_token_end_column),
			TokenVariant::StringLiteral(value) =>
				(Expression { variant: ExpressionVariant::StringLiteral(Rc::new((*value).into())), column: *first_token_start_column }, &tokens[1..], *first_token_end_column),
			// An expression in parentheses
			TokenVariant::LeftParenthesis => {
				// Get the sub-expression
				let (sub_expression, tokens_after_sub_expression, sub_expression_end_column)
					= match Self::parse_expression(&tokens[1..], line_number, *first_token_end_column)?
				{
					None => return Err(Error { variant: ErrorVariant::ExpectedExpression, column_number: Some(*first_token_start_column), line_number: line_number.cloned() }),
					Some((expression, tokens_after_sub_expression, sub_expression_end_column)) => (expression, tokens_after_sub_expression, sub_expression_end_column),
				};
				// Make sure that there is a closing parenthesis after the sub-expression
				let (tokens_after_expression_primary, expression_primary_end_column) = match tokens_after_sub_expression.first() {
					Some(Token { variant: TokenVariant::RightParenthesis, end_column, .. }) => (&tokens_after_sub_expression[1..], end_column),
					Some(Token { variant: _, start_column, .. }) =>
						return Err(Error { variant: ErrorVariant::ExpectedRightParenthesis, column_number: Some(*start_column), line_number: line_number.cloned() }),
					None => return Err(Error { variant: ErrorVariant::ExpectedRightParenthesis, column_number: Some(sub_expression_end_column), line_number: line_number.cloned() }),
				};
				// Return
				(sub_expression, tokens_after_expression_primary, *expression_primary_end_column)
			}
			// There should not be operators
			TokenVariant::Operator(..) => return Err(Error { variant: ErrorVariant::UnexpectedOperator, column_number: Some(*first_token_start_column), line_number: line_number.cloned() }),
			// Identifiers
			TokenVariant::Identifier { keyword, .. } => 'a: {
				// Get if this is a FN function
				let uses_fn_keyword = *keyword == Some(Keyword::Fn);
				let tokens_after_fn = match uses_fn_keyword {
					false => tokens,
					true => &tokens[1..],
				};
				let Token { variant: token_after_fn_variant, start_column: token_after_fn_start_column, end_column: token_after_fn_end_column }
					= match tokens_after_fn.get(0)
				{
					Some(token_after_fn) => token_after_fn,
					None => return Err(Error { variant: ErrorVariant::ExpectedFunctionNameAfterFn, column_number: Some(*first_token_end_column), line_number: line_number.cloned() }),
				};
				// Get identifier name
				let (identifier_name, identifier_type, identifier_is_optional) = match token_after_fn_variant {
					TokenVariant::Identifier { name, identifier_type, is_optional, .. } => (name, identifier_type, is_optional),
					_ => return Err(Error { variant: ErrorVariant::ExpectedFunctionNameAfterFn, column_number: Some(*token_after_fn_start_column), line_number: line_number.cloned() }),
				};
				// Return if there is not a left parenthesis after the identifier
				let tokens_after_identifier = &tokens_after_fn[1..];
				let parenthesis_end_column = match tokens_after_identifier.get(0) {
					Some(Token { variant: TokenVariant::LeftParenthesis, end_column, .. }) => *end_column,
					_ => break 'a (Expression { variant: ExpressionVariant::IdentifierOrFunction {
						name: (*identifier_name).into(), identifier_type: *identifier_type, is_optional: *identifier_is_optional, arguments: Box::default(), uses_fn_keyword, has_parentheses: false
					}, column: *first_token_start_column }, tokens_after_identifier, *token_after_fn_end_column),
				};
				// Get arguments
				let mut argument_tokens = &tokens_after_identifier[1..];
				let mut end_column_of_token_before_argument = parenthesis_end_column;
				let mut arguments = Vec::new();
				// Make sure there is not a leading comma
				match argument_tokens.get(0) {
					Some(Token { variant: TokenVariant::Comma, start_column, .. }) =>
						return Err(Error { variant: ErrorVariant::LeadingCommaInFunctionArguments, column_number: Some(*start_column), line_number: line_number.cloned() }),
					_ => {},
				}
				// Parse each argument
				'b: loop {
					// Parse argument
					let argument_parse_result = Expression::parse_expression(argument_tokens, line_number, end_column_of_token_before_argument)?;
					let (argument_expression, tokens_after_argument, end_column_of_argument) = match argument_parse_result {
						// If we did not an argument, why?
						None => {
							match argument_tokens.get(0) {
								// If it was because there was a comma
								Some(Token { variant: TokenVariant::Comma, start_column, ..}) =>
									return Err(Error { variant: ErrorVariant::TwoSequentialCommasTogetherInFunctionArguments, column_number: Some(*start_column), line_number: line_number.cloned() }),
								// If it was because we reached a right parenthesis
								Some(Token { variant: TokenVariant::RightParenthesis, end_column, .. }) => {
									argument_tokens = &argument_tokens[1..];
									end_column_of_token_before_argument = *end_column;
									break 'b;
								}
								// If it was because of an invalid separator
								Some(Token { variant: TokenVariant::Colon | TokenVariant::Semicolon, start_column, .. }) =>
									return Err(Error { variant: ErrorVariant::InvalidSeparatorInFunctionArguments, column_number: Some(*start_column), line_number: line_number.cloned() }),
								_ => unreachable!(),
							};
						}
						// If we did get an argument
						Some((argument_expression, tokens_after_argument, end_column_of_argument)) =>
							(argument_expression, tokens_after_argument, end_column_of_argument),
					};
					arguments.push(argument_expression);
					argument_tokens = tokens_after_argument;
					end_column_of_token_before_argument = end_column_of_argument;
					// Parse comma or right parentheses
					match argument_tokens.get(0) {
						Some(Token { variant: TokenVariant::Comma, end_column , ..}) => {
							argument_tokens = &argument_tokens[1..];
							end_column_of_token_before_argument = *end_column;
						}
						Some(Token { variant: TokenVariant::RightParenthesis, end_column, .. }) => {
							argument_tokens = &argument_tokens[1..];
							end_column_of_token_before_argument = *end_column;
							break 'b;
						}
						_ => {}
					};
				}
				// Return
				(Expression { variant: ExpressionVariant::IdentifierOrFunction {
					name: (*identifier_name).into(), identifier_type: *identifier_type, is_optional: *identifier_is_optional, arguments: arguments.into(), uses_fn_keyword, has_parentheses: true
				}, column: *first_token_start_column }, argument_tokens, end_column_of_token_before_argument)
			}
			// End of expression
			TokenVariant::Colon | TokenVariant::Comma | TokenVariant::RightParenthesis | TokenVariant::Semicolon => return Ok(None),
			TokenVariant::SingleQuestionMark =>
				return Err(Error { variant: ErrorVariant::NotYetImplemented("Question mark not as type".into()), column_number: Some(*first_token_start_column), line_number: line_number.cloned() }),
		}))
	}

	/// Takes in a list of tokens and returns how many form one expression given the following productions:
	///
	/// `expression = operand (binary-operator operand)*`
	///
	/// `operand = unary-operator operand / (fn-keyword? identifier)? left-parenthesis parentheses-content right-parenthesis`
	pub fn get_expression_length_old<'a>(tokens: &[Token<'a>]) -> usize {
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
					!matches!(last_token, Some(TokenVariant::Identifier { name, identifier_type: IdentifierType::UnmarkedNumber, is_optional: false, .. }) if name.eq_ignore_ascii_case("fn"))
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

	pub fn get_l_value_length<'a>(tokens: &[Token<'a>]) -> usize {
		let mut parenthesis_depth = 0usize;
		for (index, token) in tokens.iter().enumerate() {
			if matches!(token.variant, TokenVariant::LeftParenthesis) {
				parenthesis_depth += 1;
			}
			if matches!(token.variant, TokenVariant::RightParenthesis) {
				parenthesis_depth = parenthesis_depth.saturating_sub(1);
				if parenthesis_depth == 0 {
					return index + 1;
				}
			}
			if index == 1 && !matches!(token, Token { variant: TokenVariant::LeftParenthesis, .. }) {
					return index;
				}
			if parenthesis_depth == 0 {
				if !matches!(token, Token { variant: TokenVariant::Identifier { .. }, .. }) {
					return index;
				}
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
			ExpressionVariant::FlooredDivision(left_operand, right_operand) => {
				print!("Floored Division/\\");
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

#[derive(Debug, PartialEq)]
pub enum ExpressionVariant {
	StringLiteral(Rc<String>),
	IntegerLiteral(Rc<BigInt>),
	FloatLiteral { value: f64, is_imaginary: bool },
	PrintComma,
	PrintSemicolon,
	IdentifierOrFunction { name: Box<str>, identifier_type: IdentifierType, is_optional: bool, arguments: Box<[Expression]>, uses_fn_keyword: bool, has_parentheses: bool },
	Exponentiation(Box<Expression>, Box<Expression>),
	Negation(Box<Expression>),
	UnaryPlus(Box<Expression>),
	Multiplication(Box<Expression>, Box<Expression>),
	Division(Box<Expression>, Box<Expression>),
	FlooredDivision(Box<Expression>, Box<Expression>),
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

pub fn parse_line<'a>(mut tokens: &[Token<'a>], line_number: Option<&BigInt>) -> Result<Box<[Statement]>, Error> {
	let mut out = Vec::new();
	loop {
		match Statement::parse(tokens, line_number)? {
			Some((ast, rest_of_tokens)) => {
				tokens = rest_of_tokens;
				out.push(ast);
			}
			None => break,
		}
	}
	Ok(out.into())
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

	pub fn from_name(name: &str) -> Option<Self> {
		match name {
			_ if name.eq_ignore_ascii_case("AND") => Some(Self::And),
			_ if name.eq_ignore_ascii_case("OR") => Some(Self::Or),
			_ => None,
		}
	}

	pub fn to_expression(self, start_column: NonZeroUsize, lhs: Expression, rhs: Expression) -> Expression {
		match self {
			Self::AdditionConcatenation => Expression { variant: ExpressionVariant::AdditionConcatenation(Box::new(lhs), Box::new(rhs)), column: start_column },
			Self::Subtraction => Expression { variant: ExpressionVariant::Subtraction(Box::new(lhs), Box::new(rhs)), column: start_column },
			Self::Multiplication => Expression { variant: ExpressionVariant::Multiplication(Box::new(lhs), Box::new(rhs)), column: start_column },
			Self::Division => Expression { variant: ExpressionVariant::Division(Box::new(lhs), Box::new(rhs)), column: start_column },
			Self::Exponentiation => Expression { variant: ExpressionVariant::Exponentiation(Box::new(lhs), Box::new(rhs)), column: start_column },
			Self::Concatenation => Expression { variant: ExpressionVariant::Concatenation(Box::new(lhs), Box::new(rhs)), column: start_column },
			Self::DoubleSlash => Expression { variant: ExpressionVariant::FlooredDivision(Box::new(lhs), Box::new(rhs)), column: start_column },
			Self::BackSlash => Expression { variant: ExpressionVariant::FlooredDivision(Box::new(lhs), Box::new(rhs)), column: start_column },
			Self::GreaterThan => Expression { variant: ExpressionVariant::GreaterThan(Box::new(lhs), Box::new(rhs)), column: start_column },
			Self::GreaterThanOrEqualTo => Expression { variant: ExpressionVariant::GreaterThanOrEqualTo(Box::new(lhs), Box::new(rhs)), column: start_column },
			Self::LessThan => Expression { variant: ExpressionVariant::LessThan(Box::new(lhs), Box::new(rhs)), column: start_column },
			Self::LessThanOrEqualTo => Expression { variant: ExpressionVariant::LessThanOrEqualTo(Box::new(lhs), Box::new(rhs)), column: start_column },
			Self::NotEqualTo => Expression { variant: ExpressionVariant::NotEqualTo(Box::new(lhs), Box::new(rhs)), column: start_column },
			Self::Equal => Expression { variant: ExpressionVariant::EqualTo(Box::new(lhs), Box::new(rhs)), column: start_column },
			Self::And => Expression { variant: ExpressionVariant::And(Box::new(lhs), Box::new(rhs)), column: start_column },
			Self::Or => Expression { variant: ExpressionVariant::Or(Box::new(lhs), Box::new(rhs)), column: start_column },
		}
	}
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOperator {
	Negation,
	UnaryPlus,
	Not,
}

impl UnaryOperator {
	pub fn get_operator_precedence(self) -> u8 {
		match self {
			Self::UnaryPlus | Self::Negation => 1,
			Self::Not => 6,
		}
	}

	pub fn from_symbol(symbol: &str) -> Option<Self> {
		match symbol {
			"-" => Some(Self::Negation),
			"+" => Some(Self::UnaryPlus),
			_ => None,
		}
	}

	pub fn from_name(name: &str) -> Option<Self> {
		match name {
			_ if name.eq_ignore_ascii_case("NOT") => Some(Self::Not),
			_ => None,
		}
	}

	pub fn to_expression(self, start_column: NonZeroUsize, operand: Expression) -> Expression {
		match self {
			UnaryOperator::Negation => Expression { variant: ExpressionVariant::Negation(Box::new(operand)), column: start_column },
			UnaryOperator::UnaryPlus => Expression { variant: ExpressionVariant::UnaryPlus(Box::new(operand)), column: start_column },
			UnaryOperator::Not => Expression { variant: ExpressionVariant::Not(Box::new(operand)), column: start_column },
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_parse_expression() {
		assert_eq!(
			Expression::parse_expression(&*(Token::tokenize_line("1 1").unwrap().1), None, 1.try_into().unwrap()).unwrap().unwrap(),
			(
				Expression { variant: ExpressionVariant::IntegerLiteral(Rc::new(1.try_into().unwrap())), column: 3.try_into().unwrap() },
				[].as_slice(),
				4.try_into().unwrap(),
			)
		);

		assert_eq!(
			Expression::parse_expression(&*(Token::tokenize_line("1 2 3").unwrap().1), None, 2.try_into().unwrap()).unwrap().unwrap(),
			(
				Expression { variant: ExpressionVariant::IntegerLiteral(Rc::new(2.try_into().unwrap())), column: 3.try_into().unwrap() },
				[Token { variant: TokenVariant::IntegerLiteral(3.try_into().unwrap()), start_column: 5.try_into().unwrap(), end_column: 6.try_into().unwrap() }].as_slice(),
				4.try_into().unwrap(),
			)
		);

		assert_eq!(
			Expression::parse_expression(&*(Token::tokenize_line("1 2 + 3").unwrap().1), None, 2.try_into().unwrap()).unwrap().unwrap(),
			(
				Expression { variant: ExpressionVariant::AdditionConcatenation(
					Box::new(Expression { variant: ExpressionVariant::IntegerLiteral(Rc::new(2.try_into().unwrap())), column: 3.try_into().unwrap() }),
					Box::new(Expression { variant: ExpressionVariant::IntegerLiteral(Rc::new(3.try_into().unwrap())), column: 7.try_into().unwrap() }),
				), column: 5.try_into().unwrap() },
				[].as_slice(),
				8.try_into().unwrap(),
			)
		);
	}
}