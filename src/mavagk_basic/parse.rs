use std::{mem::replace, num::NonZeroUsize, rc::Rc};

use num::{complex::Complex64, BigInt};

use crate::mavagk_basic::{abstract_syntax_tree::{AnyTypeExpression, BoolExpression, BoolExpressionVariant, ComplexExpression, ComplexExpressionVariant, IntExpression, IntExpressionVariant, RealExpression, RealExpressionVariant, Statement, StatementVariant, StringExpression, StringExpressionVariant}, error::{Error, ErrorVariant}, token::{BinaryOperator, IdentifierType, Keyword, Token, TokenVariant, UnaryOperator}, value::{ComplexValue, IntValue, RealValue, StringValue}};

pub fn parse_line<'a>(mut tokens: &[Token<'a>], line_number: Option<&BigInt>) -> Result<Box<[Statement]>, Error> {
	let mut out = Vec::new();
	loop {
		match parse(tokens, line_number)? {
			Some((ast, rest_of_tokens)) => {
				tokens = rest_of_tokens;
				out.push(ast);
			}
			None => break,
		}
	}
	Ok(out.into())
}

pub fn parse<'a, 'b>(mut tokens: &'b [Token<'a>], line_number: Option<&BigInt>) -> Result<Option<(Statement, &'b [Token<'a>])>, Error> {
	// Strip leading colons
	while matches!(tokens.first(), Some(Token { variant: TokenVariant::Colon, .. })) {
		tokens = &tokens[1..];
	}
	// Get the length of this expression
	let statement_length = get_statement_length(tokens);
	let (tokens, rest_of_tokens) = tokens.split_at(statement_length);
	// Get first token or return if we are at the end of the tokens
	let identifier_token = match tokens.first() {
		Some(token) => token,
		None => return Ok(None),
	};
	// Parse assignments without LET
	'a: {
		// Check if this a non-LET assignment
		let l_value_length = get_l_value_length_old(tokens);
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
		(l_value_expression, _, _) = parse_expression_primary(&tokens[..l_value_length], line_number, identifier_token.end_column)?.unwrap();
		// Get r-value expression
			let r_value_expression;
			let remaining_tokens;
			(r_value_expression, remaining_tokens, _) = match parse_expression(&tokens[l_value_length + 1..], line_number, equal_sign_end_column)? {
				None => return Err(Error { variant: ErrorVariant::ExpectedExpression, line_number: line_number.cloned(), column_number: Some(equal_sign_end_column) }),
				Some((l_value_expression, remaining_tokens, expression_end_column)) => (l_value_expression, remaining_tokens, expression_end_column),
			};
			if remaining_tokens.len() != 0 {
				return Err(Error { variant: ErrorVariant::StatementShouldEnd, line_number: line_number.cloned(), column_number: Some(remaining_tokens[0].start_column) });
			}
			// Assemble into statement
			let statement = match l_value_expression {
				AnyTypeExpression::Int(..) => Statement {
					column: identifier_token.start_column,
					variant: StatementVariant::AssignInt(
						l_value_expression.to_int_expression(line_number)?,
						r_value_expression.to_int_expression(line_number)?
					),
				},
				AnyTypeExpression::Real(..) => Statement {
					column: identifier_token.start_column,
					variant: StatementVariant::AssignReal(
						l_value_expression.to_real_expression(line_number)?,
						r_value_expression.to_real_expression(line_number)?
					),
				},
				AnyTypeExpression::Complex(..) => Statement {
					column: identifier_token.start_column,
					variant: StatementVariant::AssignComplex(
						l_value_expression.to_complex_expression(line_number)?,
						r_value_expression.to_complex_expression(line_number)?
					),
				},
				AnyTypeExpression::String(..) => Statement {
					column: identifier_token.start_column,
					variant: StatementVariant::AssignString(
						l_value_expression.to_string_expression(line_number)?,
						r_value_expression.to_string_expression(line_number)?
					),
				},
				_ => unreachable!(),
			};
			return Ok(Some((statement, rest_of_tokens)));
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
			let l_value_length = get_l_value_length_old(remaining_tokens);
			let l_value_expression;
			(l_value_expression, _, _) = match parse_expression_primary(&remaining_tokens[..l_value_length], line_number, identifier_token.end_column)? {
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
			(r_value_expression, remaining_tokens, _) = match parse_expression(&remaining_tokens[1..], line_number, equal_sign_end_column)? {
				None => return Err(Error { variant: ErrorVariant::ExpectedExpression, line_number: line_number.cloned(), column_number: Some(equal_sign_end_column) }),
				Some((l_value_expression, remaining_tokens, expression_end_column)) => (l_value_expression, remaining_tokens, expression_end_column),
			};
			if remaining_tokens.len() != 0 {
				return Err(Error { variant: ErrorVariant::StatementShouldEnd, line_number: line_number.cloned(), column_number: Some(remaining_tokens[0].start_column) });
			}
			// Assemble into statement
			let statement = match l_value_expression {
				AnyTypeExpression::Int(..) => Statement {
					column: identifier_token.start_column,
					variant: StatementVariant::AssignInt(
						l_value_expression.to_int_expression(line_number)?,
						r_value_expression.to_int_expression(line_number)?
					),
				},
				AnyTypeExpression::Real(..) => Statement {
					column: identifier_token.start_column,
					variant: StatementVariant::AssignReal(
						l_value_expression.to_real_expression(line_number)?,
						r_value_expression.to_real_expression(line_number)?
					),
				},
				AnyTypeExpression::Complex(..) => Statement {
					column: identifier_token.start_column,
					variant: StatementVariant::AssignComplex(
						l_value_expression.to_complex_expression(line_number)?,
						r_value_expression.to_complex_expression(line_number)?
					),
				},
				AnyTypeExpression::String(..) => Statement {
					column: identifier_token.start_column,
					variant: StatementVariant::AssignString(
						l_value_expression.to_string_expression(line_number)?,
						r_value_expression.to_string_expression(line_number)?
					),
				},
				_ => unreachable!(),
			};
			Ok(Some((statement, rest_of_tokens)))
		}
		// PRINT
		Keyword::Print => {
			let mut remaining_tokens = &tokens[1..];
			let mut expressions = Vec::new();
			while !remaining_tokens.is_empty() {
				match &remaining_tokens[0] {
					Token { variant: TokenVariant::Comma, start_column, end_column: _ } => {
						expressions.push(AnyTypeExpression::PrintComma(*start_column));
						remaining_tokens = &remaining_tokens[1..];
						continue;
					}
					Token { variant: TokenVariant::Semicolon, start_column, end_column: _ } => {
						expressions.push(AnyTypeExpression::PrintSemicolon(*start_column));
						remaining_tokens = &remaining_tokens[1..];
						continue;
					}
					_ => {}
				}
				let expression;
				(expression, remaining_tokens, _) = match parse_expression(remaining_tokens, line_number, identifier_token.end_column)? {
					None => break,
					Some(result) => result,
				};
				expressions.push(expression);
			}
			debug_assert!(remaining_tokens.is_empty());
			Ok(Some((Statement { column: identifier_token.start_column, variant: StatementVariant::Print(expressions.into()) }, rest_of_tokens)))
		}
		// RUN / GOTO / GOSUB
		Keyword::Goto | Keyword::Run | Keyword::Gosub => {
			let mut remaining_tokens = &tokens[1..];
			let mut expression = None;
			if !remaining_tokens.is_empty() {
				(expression, remaining_tokens, _) = match parse_expression(remaining_tokens, line_number, identifier_token.end_column)? {
					None => return Err(Error { variant: ErrorVariant::ExpectedExpression, line_number: line_number.cloned(), column_number: Some(remaining_tokens[0].start_column) }),
					Some((result, remaining_tokens, expression_end_column)) =>
						(Some(result.to_int_expression(line_number)?), remaining_tokens, expression_end_column),
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
			Ok(Some((Statement { column: identifier_token.start_column, variant }, rest_of_tokens)))
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

pub fn parse_expression<'a, 'b>(tokens: &'b [Token<'a>], line_number: Option<&BigInt>, start_column: NonZeroUsize)-> Result<Option<(AnyTypeExpression, &'b [Token<'a>], NonZeroUsize)>, Error> {
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
		let (expression_primary, tokens_after_expression_primary, end_column_of_expression_primary) = match parse_expression_primary(remaining_tokens, line_number, start_column)? {
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
		solve_operators_by_precedence(&mut expression_primaries_and_their_unary_operators, &mut operators, Some(binary_operator.get_operator_precedence()), line_number)?;
		operators.push((binary_operator, binary_operator_start_column));
	}
	// Solve
	solve_operators_by_precedence(&mut expression_primaries_and_their_unary_operators, &mut operators, None, line_number)?;
	// Return
	debug_assert_eq!(expression_primaries_and_their_unary_operators.len(), 1);
	debug_assert_eq!(operators.len(), 0);
	debug_assert_eq!(expression_primaries_and_their_unary_operators[0].1.len(), 0);
	Ok(Some((expression_primaries_and_their_unary_operators.pop().unwrap().0, remaining_tokens, end_column_of_last_token)))
}

pub fn solve_operators_by_precedence(expression_stack: &mut Vec<(AnyTypeExpression, Vec<(UnaryOperator, NonZeroUsize)>)>, operator_stack: &mut Vec<(BinaryOperator, NonZeroUsize)>, precedence: Option<u8>, line_number: Option<&BigInt>) -> Result<(), Error> {
	loop {
		// Return if the operator precedence of the operator at the top of the stack is not greater than or equal to the input precedence
		let (binary_operator, binary_operator_start_column) = match operator_stack.last() {
			Some((operator, operator_start_column)) => {
				if let Some(precedence) = precedence && precedence < operator.get_operator_precedence()  {
					return Ok(());
				}
				(*operator, *operator_start_column)
			}
			None => break,
		};
		// Pop the rhs operator operand and parse wrap it in its unary operators
		let (mut rhs_expression, mut rhs_unary_operators) = expression_stack.pop().unwrap();
		while !rhs_unary_operators.is_empty() {
			let unary_operator = rhs_unary_operators.pop().unwrap();
			rhs_expression = unary_operator_to_expression(unary_operator.0, line_number, unary_operator.1, rhs_expression)?;
		}
		// Pop the lhs operator operand and parse wrap it in its unary operators that have a lower than or equal precedence to the binary operator
		let (mut lhs_expression, mut lhs_unary_operators) = expression_stack.pop().unwrap();
		while !lhs_unary_operators.is_empty() {
			if lhs_unary_operators.last().unwrap().0.get_operator_precedence() > binary_operator.get_operator_precedence() {
				break;
			}
			let unary_operator = lhs_unary_operators.pop().unwrap();
			lhs_expression = unary_operator_to_expression(unary_operator.0, line_number, unary_operator.1, lhs_expression)?;
		}
		// Push parsed expressions and unparsed unary operators
		expression_stack.push((binary_operator_to_expression(binary_operator, line_number, binary_operator_start_column, lhs_expression, rhs_expression)?, lhs_unary_operators));
		operator_stack.pop();
	}
	if precedence != None {
		return Ok(());
	}
	let (expression, unary_operators) = match expression_stack.get_mut(0) {
		Some((expression, unary_operators)) => (expression, unary_operators),
		None => return Ok(()),
	};
	while !unary_operators.is_empty() {
		let unary_operator = unary_operators.pop().unwrap();
		*expression = unary_operator_to_expression(unary_operator.0, line_number, unary_operator.1, replace(expression, AnyTypeExpression::PrintComma(1.try_into().unwrap())))?;
	}
	Ok(())
}

pub fn parse_expression_primary<'a, 'b>(tokens: &'b [Token<'a>], line_number: Option<&BigInt>, _start_column: NonZeroUsize)-> Result<Option<(AnyTypeExpression, &'b [Token<'a>], NonZeroUsize)>, Error> {
	// Get the first token or return if there are no more tokens to parse
	let Token { variant: first_token_variant, start_column: first_token_start_column, end_column: first_token_end_column } = match tokens.first() {
		Some(first_token) => first_token,
		None => return Ok(None),
	};
	// Parse the expression primary
	Ok(Some(match first_token_variant {
		// Literals
		TokenVariant::IntegerLiteral(value) =>
			(AnyTypeExpression::Int(IntExpression { variant: IntExpressionVariant::ConstantValue(IntValue::new(Rc::new(value.clone()))), column: *first_token_start_column }), &tokens[1..], *first_token_end_column),
		TokenVariant::FloatLiteral { value, is_imaginary } => match *is_imaginary {
			false =>
				(AnyTypeExpression::Real(RealExpression { variant: RealExpressionVariant::ConstantValue(RealValue::FloatValue(*value)), column: *first_token_start_column }), &tokens[1..], *first_token_end_column),
			true => (AnyTypeExpression::Complex(
				ComplexExpression { variant: ComplexExpressionVariant::ConstantValue(ComplexValue { value: Complex64::new(0., *value) } ), column: *first_token_start_column }
			), &tokens[1..], *first_token_end_column),
		}
		TokenVariant::StringLiteral(value) =>
			(AnyTypeExpression::String(
				StringExpression { variant: StringExpressionVariant::ConstantValue(StringValue::new(Rc::new((*value).into()))), column: *first_token_start_column }
			), &tokens[1..], *first_token_end_column),
		// An expression in parentheses
		TokenVariant::LeftParenthesis => {
			// Get the sub-expression
			let (sub_expression, tokens_after_sub_expression, sub_expression_end_column)
				= match parse_expression(&tokens[1..], line_number, *first_token_end_column)?
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
			if *identifier_is_optional {
				return Err(Error { variant: ErrorVariant::NotYetImplemented("Optional functions".into()), column_number: Some(*token_after_fn_start_column), line_number: line_number.cloned() });
			}
			// Return if there is not a left parenthesis after the identifier
			let tokens_after_identifier = &tokens_after_fn[1..];
			let parenthesis_end_column = match tokens_after_identifier.get(0) {
				Some(Token { variant: TokenVariant::LeftParenthesis, end_column, .. }) => *end_column,
				_ => break 'a (match identifier_type {
					IdentifierType::Integer => AnyTypeExpression::Int(IntExpression { variant: IntExpressionVariant::IntIdentifierOrFunction {
						name: (*identifier_name).into(), arguments: Box::default(), uses_fn_keyword, has_parentheses: false
					}, column: *first_token_start_column }),
					IdentifierType::UnmarkedNumber => AnyTypeExpression::Real(RealExpression { variant: RealExpressionVariant::RealIdentifierOrFunction {
						name: (*identifier_name).into(), arguments: Box::default(), uses_fn_keyword, has_parentheses: false
					}, column: *first_token_start_column }),
					IdentifierType::ComplexNumber => AnyTypeExpression::Complex(ComplexExpression { variant: ComplexExpressionVariant::ComplexIdentifierOrFunction {
						name: (*identifier_name).into(), arguments: Box::default(), uses_fn_keyword, has_parentheses: false
					}, column: *first_token_start_column }),
					IdentifierType::String => AnyTypeExpression::String(StringExpression { variant: StringExpressionVariant::StringIdentifierOrFunction {
						name: (*identifier_name).into(), arguments: Box::default(), uses_fn_keyword, has_parentheses: false
					}, column: *first_token_start_column }),
				}, tokens_after_identifier, *token_after_fn_end_column),
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
				let argument_parse_result = parse_expression(argument_tokens, line_number, end_column_of_token_before_argument)?;
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
			(match identifier_type {
				IdentifierType::Integer => AnyTypeExpression::Int(IntExpression { variant: IntExpressionVariant::IntIdentifierOrFunction {
					name: (*identifier_name).into(), arguments: arguments.into(), uses_fn_keyword, has_parentheses: false
				}, column: *first_token_start_column }),
				IdentifierType::UnmarkedNumber => AnyTypeExpression::Real(RealExpression { variant: RealExpressionVariant::RealIdentifierOrFunction {
					name: (*identifier_name).into(), arguments: arguments.into(), uses_fn_keyword, has_parentheses: false
				}, column: *first_token_start_column }),
				IdentifierType::ComplexNumber => AnyTypeExpression::Complex(ComplexExpression { variant: ComplexExpressionVariant::ComplexIdentifierOrFunction {
					name: (*identifier_name).into(), arguments: arguments.into(), uses_fn_keyword, has_parentheses: false
				}, column: *first_token_start_column }),
				IdentifierType::String => AnyTypeExpression::String(StringExpression { variant: StringExpressionVariant::StringIdentifierOrFunction {
					name: (*identifier_name).into(), arguments: arguments.into(), uses_fn_keyword, has_parentheses: false
				}, column: *first_token_start_column }),
			}, argument_tokens, end_column_of_token_before_argument)
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

pub fn get_l_value_length_old<'a>(tokens: &[Token<'a>]) -> usize {
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

pub fn binary_operator_to_expression(operator: BinaryOperator, line_number: Option<&BigInt>, start_column: NonZeroUsize, lhs: AnyTypeExpression, rhs: AnyTypeExpression) -> Result<AnyTypeExpression, Error> {
	Ok(match operator {
		BinaryOperator::AdditionConcatenation => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) =>
					AnyTypeExpression::Real(RealExpression { variant: RealExpressionVariant::Addition(
						Box::new(lhs.to_real_expression(line_number)?),
						Box::new(rhs.to_real_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Complex(ComplexExpression { variant: ComplexExpressionVariant::Addition(
						Box::new(lhs.to_complex_expression(line_number)?),
						Box::new(rhs.to_complex_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					AnyTypeExpression::String(StringExpression { variant: StringExpressionVariant::Concatenation(
						Box::new(lhs.to_string_expression(line_number)?),
						Box::new(rhs.to_string_expression(line_number)?),
					), column: start_column}),
				_ => unreachable!(),
			}
		},
		BinaryOperator::Concatenation => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					return Err(Error { variant: ErrorVariant::CannotConcatenateNumbers, line_number: line_number.cloned(), column_number: Some(start_column) }),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					AnyTypeExpression::String(StringExpression { variant: StringExpressionVariant::Concatenation(
						Box::new(lhs.to_string_expression(line_number)?),
						Box::new(rhs.to_string_expression(line_number)?),
					), column: start_column}),
				_ => unreachable!(),
			}
		},
		BinaryOperator::Subtraction => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) =>
					AnyTypeExpression::Real(RealExpression { variant: RealExpressionVariant::Subtraction(
						Box::new(lhs.to_real_expression(line_number)?),
						Box::new(rhs.to_real_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Complex(ComplexExpression { variant: ComplexExpressionVariant::Subtraction(
						Box::new(lhs.to_complex_expression(line_number)?),
						Box::new(rhs.to_complex_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					return Err(Error { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column) }),
				_ => unreachable!(),
			}
		},
		BinaryOperator::Multiplication => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) =>
					AnyTypeExpression::Real(RealExpression { variant: RealExpressionVariant::Multiplication(
						Box::new(lhs.to_real_expression(line_number)?),
						Box::new(rhs.to_real_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Complex(ComplexExpression { variant: ComplexExpressionVariant::Multiplication(
						Box::new(lhs.to_complex_expression(line_number)?),
						Box::new(rhs.to_complex_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					return Err(Error { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column) }),
				_ => unreachable!(),
			}
		},
		BinaryOperator::Division => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) =>
					AnyTypeExpression::Real(RealExpression { variant: RealExpressionVariant::Division(
						Box::new(lhs.to_real_expression(line_number)?),
						Box::new(rhs.to_real_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Complex(ComplexExpression { variant: ComplexExpressionVariant::Division(
						Box::new(lhs.to_complex_expression(line_number)?),
						Box::new(rhs.to_complex_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					return Err(Error { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column) }),
				_ => unreachable!(),
			}
		},
		BinaryOperator::Exponentiation => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) =>
					AnyTypeExpression::Real(RealExpression { variant: RealExpressionVariant::Exponentiation(
						Box::new(lhs.to_real_expression(line_number)?),
						Box::new(rhs.to_real_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Complex(ComplexExpression { variant: ComplexExpressionVariant::Exponentiation(
						Box::new(lhs.to_complex_expression(line_number)?),
						Box::new(rhs.to_complex_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					return Err(Error { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column) }),
				_ => unreachable!(),
			}
		},
		BinaryOperator::DoubleSlash => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Real(RealExpression { variant: RealExpressionVariant::FlooredDivision(
						Box::new(lhs.to_real_expression(line_number)?),
						Box::new(rhs.to_real_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					return Err(Error { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column) }),
				_ => unreachable!(),
			}
		},
		BinaryOperator::BackSlash => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Real(RealExpression { variant: RealExpressionVariant::FlooredDivision(
						Box::new(lhs.to_real_expression(line_number)?),
						Box::new(rhs.to_real_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					return Err(Error { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column) }),
				_ => unreachable!(),
			}
		},
		BinaryOperator::Equal => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::BoolEqualTo(
						Box::new(lhs.to_bool_expression(line_number)?),
						Box::new(rhs.to_bool_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::IntEqualTo(
						Box::new(lhs.to_int_expression(line_number)?),
						Box::new(rhs.to_int_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::RealEqualTo(
						Box::new(lhs.to_real_expression(line_number)?),
						Box::new(rhs.to_real_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::ComplexEqualTo(
						Box::new(lhs.to_complex_expression(line_number)?),
						Box::new(rhs.to_complex_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::StringEqualTo(
						Box::new(lhs.to_string_expression(line_number)?),
						Box::new(rhs.to_string_expression(line_number)?),
					), column: start_column}),
				_ => unreachable!(),
			}
		},
		BinaryOperator::NotEqualTo => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::BoolNotEqualTo(
						Box::new(lhs.to_bool_expression(line_number)?),
						Box::new(rhs.to_bool_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::IntNotEqualTo(
						Box::new(lhs.to_int_expression(line_number)?),
						Box::new(rhs.to_int_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::RealNotEqualTo(
						Box::new(lhs.to_real_expression(line_number)?),
						Box::new(rhs.to_real_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::ComplexNotEqualTo(
						Box::new(lhs.to_complex_expression(line_number)?),
						Box::new(rhs.to_complex_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::StringNotEqualTo(
						Box::new(lhs.to_string_expression(line_number)?),
						Box::new(rhs.to_string_expression(line_number)?),
					), column: start_column}),
				_ => unreachable!(),
			}
		},
		BinaryOperator::GreaterThan => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::BoolGreaterThan(
						Box::new(lhs.to_bool_expression(line_number)?),
						Box::new(rhs.to_bool_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::IntGreaterThan(
						Box::new(lhs.to_int_expression(line_number)?),
						Box::new(rhs.to_int_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::RealGreaterThan(
						Box::new(lhs.to_real_expression(line_number)?),
						Box::new(rhs.to_real_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::RealGreaterThan(
						Box::new(lhs.to_real_expression(line_number)?),
						Box::new(rhs.to_real_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::StringGreaterThan(
						Box::new(lhs.to_string_expression(line_number)?),
						Box::new(rhs.to_string_expression(line_number)?),
					), column: start_column}),
				_ => unreachable!(),
			}
		},
		BinaryOperator::GreaterThanOrEqualTo => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::BoolGreaterThanOrEqualTo(
						Box::new(lhs.to_bool_expression(line_number)?),
						Box::new(rhs.to_bool_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::IntGreaterThanOrEqualTo(
						Box::new(lhs.to_int_expression(line_number)?),
						Box::new(rhs.to_int_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::RealGreaterThanOrEqualTo(
						Box::new(lhs.to_real_expression(line_number)?),
						Box::new(rhs.to_real_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::RealGreaterThanOrEqualTo(
						Box::new(lhs.to_real_expression(line_number)?),
						Box::new(rhs.to_real_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::StringGreaterThanOrEqualTo(
						Box::new(lhs.to_string_expression(line_number)?),
						Box::new(rhs.to_string_expression(line_number)?),
					), column: start_column}),
				_ => unreachable!(),
			}
		},
		BinaryOperator::LessThan => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::BoolLessThan(
						Box::new(lhs.to_bool_expression(line_number)?),
						Box::new(rhs.to_bool_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::IntLessThan(
						Box::new(lhs.to_int_expression(line_number)?),
						Box::new(rhs.to_int_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::RealLessThan(
						Box::new(lhs.to_real_expression(line_number)?),
						Box::new(rhs.to_real_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::RealLessThan(
						Box::new(lhs.to_real_expression(line_number)?),
						Box::new(rhs.to_real_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::StringLessThan(
						Box::new(lhs.to_string_expression(line_number)?),
						Box::new(rhs.to_string_expression(line_number)?),
					), column: start_column}),
				_ => unreachable!(),
			}
		},
		BinaryOperator::LessThanOrEqualTo => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::BoolLessThanOrEqualTo(
						Box::new(lhs.to_bool_expression(line_number)?),
						Box::new(rhs.to_bool_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::IntLessThanOrEqualTo(
						Box::new(lhs.to_int_expression(line_number)?),
						Box::new(rhs.to_int_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::RealLessThanOrEqualTo(
						Box::new(lhs.to_real_expression(line_number)?),
						Box::new(rhs.to_real_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::RealLessThanOrEqualTo(
						Box::new(lhs.to_real_expression(line_number)?),
						Box::new(rhs.to_real_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::StringLessThanOrEqualTo(
						Box::new(lhs.to_string_expression(line_number)?),
						Box::new(rhs.to_string_expression(line_number)?),
					), column: start_column}),
				_ => unreachable!(),
			}
		},
		BinaryOperator::And => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::And(
						Box::new(lhs.to_bool_expression(line_number)?),
						Box::new(rhs.to_bool_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Int(IntExpression { variant: IntExpressionVariant::BitwiseAnd(
						Box::new(lhs.to_int_expression(line_number)?),
						Box::new(rhs.to_int_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					return Err(Error { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column) }),
				_ => unreachable!(),
			}
		},
		BinaryOperator::Or => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) =>
					AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::Or(
						Box::new(lhs.to_bool_expression(line_number)?),
						Box::new(rhs.to_bool_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Int(IntExpression { variant: IntExpressionVariant::BitwiseOr(
						Box::new(lhs.to_int_expression(line_number)?),
						Box::new(rhs.to_int_expression(line_number)?),
					), column: start_column}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					return Err(Error { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column) }),
				_ => unreachable!(),
			}
		},
	})
}

pub fn unary_operator_to_expression(operator: UnaryOperator, line_number: Option<&BigInt>, start_column: NonZeroUsize, operand: AnyTypeExpression) -> Result<AnyTypeExpression, Error> {
	Ok(match &operator {
		UnaryOperator::Negation  => match operand {
			AnyTypeExpression::Bool(..) | AnyTypeExpression::Int(..) | AnyTypeExpression::Real(..) =>
				AnyTypeExpression::Real(RealExpression { variant: RealExpressionVariant::Negation(Box::new(operand.to_real_expression(line_number)?)), column: start_column }),
			AnyTypeExpression::Complex(..) =>
				AnyTypeExpression::Complex(ComplexExpression { variant: ComplexExpressionVariant::Negation(Box::new(operand.to_complex_expression(line_number)?)), column: start_column }),
			AnyTypeExpression::String(..) =>
				return Err(Error { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column) }),
			_ => unreachable!(),
		}
		UnaryOperator::UnaryPlus  => operand,
		UnaryOperator::Not  => match operand {
			AnyTypeExpression::Bool(..) =>
				AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::Not(Box::new(operand.to_bool_expression(line_number)?)), column: start_column }),
			AnyTypeExpression::Int(..) | AnyTypeExpression::Real(..) | AnyTypeExpression::Complex(..) =>
				AnyTypeExpression::Int(IntExpression { variant: IntExpressionVariant::BitwiseNot(Box::new(operand.to_int_expression(line_number)?)), column: start_column }),
			AnyTypeExpression::String(..) =>
				return Err(Error { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column) }),
			_ => unreachable!(),
		}
	})
}