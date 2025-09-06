use std::{mem::replace, num::NonZeroUsize, rc::Rc};

use num::{complex::Complex64, BigInt};

use crate::mavagk_basic::{abstract_syntax_tree::{AnyTypeExpression, AnyTypeLValue, BoolExpression, ComplexExpression, ComplexLValue, IntExpression, IntLValue, RealExpression, RealLValue, Statement, StatementVariant, StringExpression, StringLValue}, error::{Error, ErrorVariant}, token::{BinaryOperator, IdentifierType, Keyword, Token, TokenVariant, UnaryOperator}, value::{ComplexValue, IntValue, RealValue, StringValue}};

pub fn parse_line<'a>(mut tokens: &[Token<'a>], line_number: Option<&BigInt>) -> (Box<[Statement]>, Option<Error>) {
	let mut out = Vec::new();
	loop {
		match parse(tokens, line_number) {
			Err(error) => return (out.into(), Some(error)),
			Ok(Some((ast, rest_of_tokens))) => {
				tokens = rest_of_tokens;
				out.push(ast);
			},
			Ok(None) => break,
		}
	}
	(out.into(), None)
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
	if tokens.is_empty() {
		return Ok(None);
	}
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
		(l_value_expression, _, _) = parse_l_value(&tokens[..l_value_length], line_number, tokens[0].start_column)?.unwrap();
		// Get r-value expression
			let r_value_expression;
			let remaining_tokens;
			(r_value_expression, remaining_tokens, _) = match parse_expression(&tokens[l_value_length + 1..], line_number, equal_sign_end_column)? {
				None => return Err(Error { variant: ErrorVariant::ExpectedExpression, line_number: line_number.cloned(), column_number: Some(equal_sign_end_column), line_text: None }),
				Some((l_value_expression, remaining_tokens, expression_end_column)) => (l_value_expression, remaining_tokens, expression_end_column),
			};
			if remaining_tokens.len() != 0 {
				return Err(Error { variant: ErrorVariant::StatementShouldEnd, line_number: line_number.cloned(), column_number: Some(remaining_tokens[0].start_column), line_text: None });
			}
			// Assemble into statement
			let statement = match l_value_expression {
				AnyTypeLValue::Int(l_value) => Statement {
					column: tokens[0].start_column,
					variant: StatementVariant::AssignInt(
						l_value,
						r_value_expression.to_int_expression(line_number)?
					),
				},
				AnyTypeLValue::Real(l_value) => Statement {
					column: tokens[0].start_column,
					variant: StatementVariant::AssignReal(
						l_value,
						r_value_expression.to_real_expression(line_number)?
					),
				},
				AnyTypeLValue::Complex(l_value) => Statement {
					column: tokens[0].start_column,
					variant: StatementVariant::AssignComplex(
						l_value,
						r_value_expression.to_complex_expression(line_number)?
					),
				},
				AnyTypeLValue::String(l_value) => Statement {
					column: tokens[0].start_column,
					variant: StatementVariant::AssignString(
						l_value,
						r_value_expression.to_string_expression(line_number)?
					),
				},
				//_ => unreachable!(),
			};
			return Ok(Some((statement, rest_of_tokens)));
	}
	// Parse depending on keyword
	let (statement_keyword, tokens_after_statement_keyword, statement_keyword_start_column, statement_keyword_end_column) = match get_keyword(tokens) {
		Some(result) => result,
		None => return Err(Error { variant: ErrorVariant::ExpectedStatementKeyword, line_number: line_number.cloned(), column_number: Some(tokens[0].start_column), line_text: None }),
	};
	match statement_keyword {
		// LET
		Keyword::Let => {
			let mut remaining_tokens = tokens_after_statement_keyword;
			// Get l-value expression
			let l_value_length = get_l_value_length_old(remaining_tokens);
			let l_value_expression;
			(l_value_expression, _, _) = match parse_l_value(&remaining_tokens[..l_value_length], line_number, statement_keyword_end_column)? {
				None => return Err(Error { variant: ErrorVariant::ExpectedExpression, line_number: line_number.cloned(), column_number: Some(tokens[0].end_column), line_text: None }),
				Some((l_value_expression, remaining_tokens, expression_end_column)) => (l_value_expression, remaining_tokens, expression_end_column),
			};
			remaining_tokens = &remaining_tokens[l_value_length..];
			// Expect equal sign
			let equal_sign_end_column = match remaining_tokens.get(0) {
				Some(Token { variant: TokenVariant::Operator(Some(BinaryOperator::Equal), _), end_column, .. }) => *end_column,
				Some(Token { start_column, .. }) => return Err(Error { variant: ErrorVariant::ExpectedEqualSign, line_number: line_number.cloned(), column_number: Some(*start_column), line_text: None }),
				None => return Err(Error { variant: ErrorVariant::ExpectedEqualSign, line_number: line_number.cloned(), column_number: Some(tokens[l_value_length].end_column), line_text: None }),
			};
			// Get r-value expression
			let r_value_expression;
			(r_value_expression, remaining_tokens, _) = match parse_expression(&remaining_tokens[1..], line_number, equal_sign_end_column)? {
				None => return Err(Error { variant: ErrorVariant::ExpectedExpression, line_number: line_number.cloned(), column_number: Some(equal_sign_end_column), line_text: None }),
				Some((l_value_expression, remaining_tokens, expression_end_column)) => (l_value_expression, remaining_tokens, expression_end_column),
			};
			if remaining_tokens.len() != 0 {
				return Err(Error { variant: ErrorVariant::StatementShouldEnd, line_number: line_number.cloned(), column_number: Some(remaining_tokens[0].start_column), line_text: None });
			}
			// Assemble into statement
			let statement = match l_value_expression {
				AnyTypeLValue::Int(l_value) => Statement {
					column: statement_keyword_start_column,
					variant: StatementVariant::AssignInt(
						l_value,
						r_value_expression.to_int_expression(line_number)?
					),
				},
				AnyTypeLValue::Real(l_value) => Statement {
					column: statement_keyword_start_column,
					variant: StatementVariant::AssignReal(
						l_value,
						r_value_expression.to_real_expression(line_number)?
					),
				},
				AnyTypeLValue::Complex(l_value) => Statement {
					column: statement_keyword_start_column,
					variant: StatementVariant::AssignComplex(
						l_value,
						r_value_expression.to_complex_expression(line_number)?
					),
				},
				AnyTypeLValue::String(l_value) => Statement {
					column: statement_keyword_start_column,
					variant: StatementVariant::AssignString(
						l_value,
						r_value_expression.to_string_expression(line_number)?
					),
				},
				//_ => unreachable!(),
			};
			Ok(Some((statement, rest_of_tokens)))
		}
		// PRINT
		Keyword::Print => {
			let mut remaining_tokens = tokens_after_statement_keyword;
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
				(expression, remaining_tokens, _) = match parse_expression(remaining_tokens, line_number, statement_keyword_end_column)? {
					None => break,
					Some(result) => result,
				};
				expressions.push(expression);
			}
			debug_assert!(remaining_tokens.is_empty());
			Ok(Some((Statement { column: statement_keyword_start_column, variant: StatementVariant::Print(expressions.into()) }, rest_of_tokens)))
		}
		// RUN / GOTO / GOSUB
		Keyword::Goto | Keyword::Run | Keyword::Gosub => {
			let mut remaining_tokens = tokens_after_statement_keyword;
			let mut expression = None;
			if !remaining_tokens.is_empty() {
				(expression, remaining_tokens, _) = match parse_expression(remaining_tokens, line_number, statement_keyword_end_column)? {
					None => return Err(Error { variant: ErrorVariant::ExpectedExpression, line_number: line_number.cloned(), column_number: Some(remaining_tokens[0].start_column), line_text: None }),
					Some((result, remaining_tokens, expression_end_column)) =>
						(Some(result.to_int_expression(line_number)?), remaining_tokens, expression_end_column),
				};
				if !remaining_tokens.is_empty() {
					return Err(Error { variant: ErrorVariant::StatementShouldEnd, line_number: line_number.cloned(), column_number: Some(remaining_tokens[0].start_column), line_text: None });
				}
			}
			let variant = match statement_keyword {
				Keyword::Run => StatementVariant::Run(expression),
				Keyword::Goto => StatementVariant::Goto(expression),
				Keyword::Gosub => StatementVariant::Gosub(expression),
				_ => unreachable!(),
			};
			Ok(Some((Statement { column: statement_keyword_start_column, variant }, rest_of_tokens)))
		}
		// LIST
		Keyword::List => 'a: {
			// If this is a blank LIST statement
			if tokens_after_statement_keyword.is_empty() {
				break 'a Ok(Some((Statement { variant: StatementVariant::List(None, None), column: statement_keyword_start_column }, rest_of_tokens)));
			}
			// Else find the hyphen
			let hyphen_index = BinaryOperator::Subtraction.find_in(tokens_after_statement_keyword);
			// If there is no hyphen, just parse one expression
			if hyphen_index == None {
				let (sub_expression, tokens_after_sub_expression, sub_expression_end_column) =
					match parse_expression(tokens_after_statement_keyword, line_number, statement_keyword_end_column)?
				{
					Some(result) => result,
					None => return Err(Error { variant: ErrorVariant::ExpectedExpression, line_number: line_number.cloned(), column_number: Some(statement_keyword_end_column), line_text: None }),
				};
				if !tokens_after_sub_expression.is_empty() {
					return Err(Error { variant: ErrorVariant::StatementShouldEnd, line_number: line_number.cloned(), column_number: Some(sub_expression_end_column), line_text: None });
				}
				let sub_expression = sub_expression.to_int_expression(line_number)?;
				break 'a Ok(Some((Statement { variant: StatementVariant::List(Some(sub_expression.clone()), Some(sub_expression)), column: statement_keyword_start_column }, rest_of_tokens)));
			}
			// Else parse range start expression
			let hyphen_index = hyphen_index.unwrap();
			let range_start_expression_tokens = &tokens_after_statement_keyword[..hyphen_index];
			let range_start_expression = match parse_expression(range_start_expression_tokens, line_number, statement_keyword_end_column)? {
				Some((range_start_expression, tokens_after_range_start_expression, sub_expression_end_column)) => {
					if !tokens_after_range_start_expression.is_empty() {
						return Err(Error { variant: ErrorVariant::ExpectedExpression, line_number: line_number.cloned(), column_number: Some(sub_expression_end_column), line_text: None });
					}
					Some(range_start_expression.to_int_expression(line_number)?)
				},
				None => None,
			};
			// Make sure there is not another unparenthesized minus/hyphen after the first one
			let range_end_expression_tokens = &tokens_after_statement_keyword[hyphen_index + 1..];
			match BinaryOperator::Subtraction.find_in(range_end_expression_tokens) {
				Some(second_hyphen_index) =>
					return Err(Error { variant: ErrorVariant::UnexpectedSecondListHyphen, line_number: line_number.cloned(), column_number: Some(tokens_after_statement_keyword[second_hyphen_index].start_column), line_text: None }),
				None => {}
			}
			// Parse range end expression
			let range_end_expression = match parse_expression(range_end_expression_tokens, line_number, tokens_after_statement_keyword[hyphen_index].end_column)? {
				Some((range_end_expression, tokens_after_range_end_expression, sub_expression_end_column)) => {
					if !tokens_after_range_end_expression.is_empty() {
						return Err(Error { variant: ErrorVariant::StatementShouldEnd, line_number: line_number.cloned(), column_number: Some(sub_expression_end_column), line_text: None });
					}
					Some(range_end_expression.to_int_expression(line_number)?)
				},
				None => None,
			};
			// Assemble into LIST statement
			Ok(Some((Statement { column: statement_keyword_start_column, variant: StatementVariant::List(range_start_expression, range_end_expression) }, rest_of_tokens)))
		}
		Keyword::Fn => Err(Error { variant: ErrorVariant::ExpectedStatementKeyword, line_number: line_number.cloned(), column_number: Some(statement_keyword_start_column), line_text: None }),
		Keyword::Go => Err(Error { variant: ErrorVariant::SingleGoKeyword, line_number: line_number.cloned(), column_number: Some(statement_keyword_start_column), line_text: None }),
		_ => return Err(Error { variant: ErrorVariant::NotYetImplemented("Statement".into()), line_number: line_number.cloned(), column_number: Some(statement_keyword_start_column), line_text: None }),
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
	if tokens.is_empty() {
		return Ok(None);
	}
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
					return Err(Error { variant: ErrorVariant::ExpectedExpressionPrimary, column_number: Some(end_column_of_last_token), line_number: line_number.cloned(), line_text: None });
				}
				if !unary_operators_before_expression_primary.is_empty() {
					return Err(Error { variant: ErrorVariant::UnaryOperatorsAtEndOfExpression, column_number: Some(end_column_of_last_token), line_number: line_number.cloned(), line_text: None });
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
			(AnyTypeExpression::Int(IntExpression::ConstantValue { value: IntValue { value: Rc::new(value.clone()) }, start_column: *first_token_start_column }), &tokens[1..], *first_token_end_column),
		TokenVariant::FloatLiteral { value, is_imaginary } => match *is_imaginary {
			false =>
				(AnyTypeExpression::Real(RealExpression::ConstantValue { value: RealValue::FloatValue(*value), start_column: *first_token_start_column }), &tokens[1..], *first_token_end_column),
			true =>
				(AnyTypeExpression::Complex(
					ComplexExpression::ConstantValue { value: ComplexValue { value: Complex64::new(0., *value) }, start_column: *first_token_start_column }
				), &tokens[1..], *first_token_end_column),
		}
		TokenVariant::StringLiteral(value) =>
			(AnyTypeExpression::String(
				StringExpression::ConstantValue { value: StringValue { value: Rc::new(value.to_string().clone()) }, start_column: *first_token_start_column }
			), &tokens[1..], *first_token_end_column),
		// An expression in parentheses
		TokenVariant::LeftParenthesis => {
			// Get the sub-expression
			let (sub_expression, tokens_after_sub_expression, sub_expression_end_column)
				= match parse_expression(&tokens[1..], line_number, *first_token_end_column)?
			{
				None => return Err(Error { variant: ErrorVariant::ExpectedExpression, column_number: Some(*first_token_start_column), line_number: line_number.cloned(), line_text: None }),
				Some((expression, tokens_after_sub_expression, sub_expression_end_column)) => (expression, tokens_after_sub_expression, sub_expression_end_column),
			};
			// Make sure that there is a closing parenthesis after the sub-expression
			let (tokens_after_expression_primary, expression_primary_end_column) = match tokens_after_sub_expression.first() {
				Some(Token { variant: TokenVariant::RightParenthesis, end_column, .. }) => (&tokens_after_sub_expression[1..], end_column),
				Some(Token { variant: _, start_column, .. }) =>
					return Err(Error { variant: ErrorVariant::ExpectedRightParenthesis, column_number: Some(*start_column), line_number: line_number.cloned(), line_text: None }),
				None => return Err(Error { variant: ErrorVariant::ExpectedRightParenthesis, column_number: Some(sub_expression_end_column), line_number: line_number.cloned(), line_text: None }),
			};
			// Return
			(sub_expression, tokens_after_expression_primary, *expression_primary_end_column)
		}
		// There should not be operators
		TokenVariant::Operator(..) => return Err(Error { variant: ErrorVariant::UnexpectedOperator, column_number: Some(*first_token_start_column), line_number: line_number.cloned(), line_text: None }),
		// Identifiers
		TokenVariant::Identifier { .. } => {
			match parse_l_value(tokens, line_number, *first_token_start_column)?.unwrap() {
				(AnyTypeLValue::Int(l_value), remaining_tokens, end_column) =>
					(AnyTypeExpression::Int(IntExpression::LValue(l_value)), remaining_tokens, end_column),
				(AnyTypeLValue::Real(l_value), remaining_tokens, end_column) =>
					(AnyTypeExpression::Real(RealExpression::LValue(l_value)), remaining_tokens, end_column),
				(AnyTypeLValue::Complex(l_value), remaining_tokens, end_column) =>
					(AnyTypeExpression::Complex(ComplexExpression::LValue(l_value)), remaining_tokens, end_column),
				(AnyTypeLValue::String(l_value), remaining_tokens, end_column) =>
					(AnyTypeExpression::String(StringExpression::LValue(l_value)), remaining_tokens, end_column),
			}
		}
		// End of expression
		TokenVariant::Colon | TokenVariant::Comma | TokenVariant::RightParenthesis | TokenVariant::Semicolon => return Ok(None),
		TokenVariant::SingleQuestionMark =>
			return Err(Error { variant: ErrorVariant::NotYetImplemented("Question mark not as type".into()), column_number: Some(*first_token_start_column), line_number: line_number.cloned(), line_text: None }),
	}))
}

pub fn parse_l_value<'a, 'b>(tokens: &'b [Token<'a>], line_number: Option<&BigInt>, _start_column: NonZeroUsize)-> Result<Option<(AnyTypeLValue, &'b [Token<'a>], NonZeroUsize)>, Error> {
	// Get the first token or return if there are no more tokens to parse
	let Token { variant: first_token_variant, start_column: first_token_start_column, end_column: first_token_end_column } = match tokens.first() {
		Some(first_token) => first_token,
		None => return Ok(None),
	};
	let keyword = match first_token_variant {
		TokenVariant::Identifier { keyword, .. } =>
			*keyword,
		_ => return Err(Error { variant: ErrorVariant::InvalidLValue, column_number: Some(*first_token_end_column), line_number: line_number.cloned(), line_text: None }),
	};
	// Get if this is a FN function
	let uses_fn_keyword = keyword == Some(Keyword::Fn);
	let tokens_after_fn = match uses_fn_keyword {
		false => tokens,
		true => &tokens[1..],
	};
	let Token { variant: token_after_fn_variant, start_column: token_after_fn_start_column, end_column: token_after_fn_end_column }
		= match tokens_after_fn.get(0)
	{
		Some(token_after_fn) => token_after_fn,
		None => return Err(Error { variant: ErrorVariant::ExpectedFunctionNameAfterFn, column_number: Some(*first_token_end_column), line_number: line_number.cloned(), line_text: None }),
	};
	// Get identifier name
	let (identifier_name, identifier_type, identifier_is_optional) = match token_after_fn_variant {
		TokenVariant::Identifier { name, identifier_type, is_optional, .. } => (name, identifier_type, is_optional),
		_ => return Err(Error { variant: ErrorVariant::ExpectedFunctionNameAfterFn, column_number: Some(*token_after_fn_start_column), line_number: line_number.cloned(), line_text: None }),
	};
	if *identifier_is_optional {
		return Err(Error { variant: ErrorVariant::NotYetImplemented("Optional functions".into()), column_number: Some(*token_after_fn_start_column), line_number: line_number.cloned(), line_text: None });
	}
	// Return if there is not a left parenthesis after the identifier
	let tokens_after_identifier = &tokens_after_fn[1..];
	let parenthesis_end_column = match tokens_after_identifier.get(0) {
		Some(Token { variant: TokenVariant::LeftParenthesis, end_column, .. }) => *end_column,
		//_ => break 'a (match identifier_type {
		//	IdentifierType::Integer => AnyTypeExpression::Int(IntExpression { variant: IntExpressionVariant::IntIdentifierOrFunction {
		//		name: (*identifier_name).into(), arguments: Box::default(), uses_fn_keyword, has_parentheses: false
		//	}, column: *first_token_start_column }),
		//	IdentifierType::UnmarkedNumber => AnyTypeExpression::Real(RealExpression { variant: RealExpressionVariant::RealIdentifierOrFunction {
		//		name: (*identifier_name).into(), arguments: Box::default(), uses_fn_keyword, has_parentheses: false
		//	}, column: *first_token_start_column }),
		//	IdentifierType::ComplexNumber => AnyTypeExpression::Complex(ComplexExpression { variant: ComplexExpressionVariant::ComplexIdentifierOrFunction {
		//		name: (*identifier_name).into(), arguments: Box::default(), uses_fn_keyword, has_parentheses: false
		//	}, column: *first_token_start_column }),
		//	IdentifierType::String => AnyTypeExpression::String(StringExpression { variant: StringExpressionVariant::StringIdentifierOrFunction {
		//		name: (*identifier_name).into(), arguments: Box::default(), uses_fn_keyword, has_parentheses: false
		//	}, column: *first_token_start_column }),
		//}, tokens_after_identifier, *token_after_fn_end_column),
		_ => return Ok(Some((match identifier_type {
			IdentifierType::Integer => AnyTypeLValue::Int(IntLValue {
				name: (*identifier_name).into(), arguments: Box::default(), uses_fn_keyword, has_parentheses: false, start_column: *first_token_start_column
			}),
			IdentifierType::UnmarkedNumber => AnyTypeLValue::Real(RealLValue {
				name: (*identifier_name).into(), arguments: Box::default(), uses_fn_keyword, has_parentheses: false, start_column: *first_token_start_column
			}),
			IdentifierType::ComplexNumber => AnyTypeLValue::Complex(ComplexLValue {
				name: (*identifier_name).into(), arguments: Box::default(), uses_fn_keyword, has_parentheses: false, start_column: *first_token_start_column
			}),
			IdentifierType::String => AnyTypeLValue::String(StringLValue {
				name: (*identifier_name).into(), arguments: Box::default(), uses_fn_keyword, has_parentheses: false, start_column: *first_token_start_column
			}),
		}, tokens_after_identifier, *token_after_fn_end_column))),
	};
	// Get arguments
	let mut argument_tokens = &tokens_after_identifier[1..];
	let mut end_column_of_token_before_argument = parenthesis_end_column;
	let mut arguments = Vec::new();
	// Make sure there is not a leading comma
	match argument_tokens.get(0) {
		Some(Token { variant: TokenVariant::Comma, start_column, .. }) =>
			return Err(Error { variant: ErrorVariant::LeadingCommaInFunctionArguments, column_number: Some(*start_column), line_number: line_number.cloned(), line_text: None }),
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
						return Err(Error { variant: ErrorVariant::TwoSequentialCommasTogetherInFunctionArguments, column_number: Some(*start_column), line_number: line_number.cloned(), line_text: None }),
					// If it was because we reached a right parenthesis
					Some(Token { variant: TokenVariant::RightParenthesis, end_column, .. }) => {
						argument_tokens = &argument_tokens[1..];
						end_column_of_token_before_argument = *end_column;
						break 'b;
					}
					// If it was because of an invalid separator
					Some(Token { variant: TokenVariant::Colon | TokenVariant::Semicolon, start_column, .. }) =>
						return Err(Error { variant: ErrorVariant::InvalidSeparatorInFunctionArguments, column_number: Some(*start_column), line_number: line_number.cloned(), line_text: None }),
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
	return Ok(Some((match identifier_type {
		IdentifierType::Integer => AnyTypeLValue::Int(IntLValue {
			name: (*identifier_name).into(), arguments: arguments.into(), uses_fn_keyword, has_parentheses: false, start_column: *first_token_start_column
		}),
		IdentifierType::UnmarkedNumber => AnyTypeLValue::Real(RealLValue {
			name: (*identifier_name).into(), arguments: arguments.into(), uses_fn_keyword, has_parentheses: false, start_column: *first_token_start_column
		}),
		IdentifierType::ComplexNumber => AnyTypeLValue::Complex(ComplexLValue {
			name: (*identifier_name).into(), arguments: arguments.into(), uses_fn_keyword, has_parentheses: false, start_column: *first_token_start_column
		}),
		IdentifierType::String => AnyTypeLValue::String(StringLValue {
			name: (*identifier_name).into(), arguments: arguments.into(), uses_fn_keyword, has_parentheses: false, start_column: *first_token_start_column
		}),
	}, argument_tokens, end_column_of_token_before_argument)))
}

// TODO: Remove
/// Gets a one or two word keyword from the start of the list of tokens. Returns:
/// * `None` if there is not a keyword at the start of the list.
/// * `Some((keyword, tokens after keyword, keyword start column, keyword end column))` if it could.
pub fn get_keyword<'a, 'b>(tokens: &'b [Token<'a>]) -> Option<(Keyword, &'b [Token<'a>], NonZeroUsize, NonZeroUsize)> {
	// Get first keyword or return if there is not a first keyword
	let (first_keyword_variant, first_keyword_start_column, first_keyword_end_column) = match tokens.get(0) {
		Some(Token { variant: TokenVariant::Identifier { keyword: Some(keyword), .. }, start_column, end_column }) => (*keyword, *start_column, *end_column),
		_ => return None,
	};
	// Get the second keyword if there is one
	let second_keyword_variant_and_end_column = match tokens.get(1) {
		Some(Token { variant: TokenVariant::Identifier { keyword: Some(keyword), .. }, end_column, .. }) => Some((*keyword, *end_column)),
		_ => None,
	};
	// If there are two keywords and they are in a list of two word keywords, return the two word keyword
	if let Some((second_keyword_variant, second_keyword_end_column)) = second_keyword_variant_and_end_column {
		for (second_keyword_of_double_word_keyword, double_word_keyword) in first_keyword_variant.get_double_word_tokens() {
			if second_keyword_variant == *second_keyword_of_double_word_keyword {
				return Some((*double_word_keyword, &tokens[2..], first_keyword_start_column, second_keyword_end_column));
			}
		}
	}
	// Else return the first keyword variant
	Some((first_keyword_variant, &tokens[1..], first_keyword_start_column, first_keyword_end_column))
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
					AnyTypeExpression::Real(RealExpression::Addition {
						lhs_expression: Box::new(lhs.to_real_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_real_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Complex(ComplexExpression::Addition {
						lhs_expression: Box::new(lhs.to_complex_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_complex_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					AnyTypeExpression::String(StringExpression::Concatenation {
						lhs_expression: Box::new(lhs.to_string_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_string_expression(line_number)?),
						start_column,
					}),
				_ => unreachable!(),
			}
		},
		BinaryOperator::Concatenation => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					return Err(Error { variant: ErrorVariant::CannotConcatenateNumbers, line_number: line_number.cloned(), column_number: Some(start_column), line_text: None }),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					AnyTypeExpression::String(StringExpression::Concatenation {
						lhs_expression: Box::new(lhs.to_string_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_string_expression(line_number)?),
						start_column,
					}),
				_ => unreachable!(),
			}
		},
		BinaryOperator::Subtraction => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) =>
					AnyTypeExpression::Real(RealExpression::Subtraction {
						lhs_expression: Box::new(lhs.to_real_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_real_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Complex(ComplexExpression::Subtraction {
						lhs_expression: Box::new(lhs.to_complex_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_complex_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					return Err(Error { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column), line_text: None }),
				_ => unreachable!(),
			}
		},
		BinaryOperator::Multiplication => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) =>
					AnyTypeExpression::Real(RealExpression::Multiplication {
						lhs_expression: Box::new(lhs.to_real_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_real_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Complex(ComplexExpression::Multiplication {
						lhs_expression: Box::new(lhs.to_complex_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_complex_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					return Err(Error { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column), line_text: None }),
				_ => unreachable!(),
			}
		},
		BinaryOperator::Division => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) =>
					AnyTypeExpression::Real(RealExpression::Division {
						lhs_expression: Box::new(lhs.to_real_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_real_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Complex(ComplexExpression::Division {
						lhs_expression: Box::new(lhs.to_complex_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_complex_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					return Err(Error { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column), line_text: None }),
				_ => unreachable!(),
			}
		},
		BinaryOperator::Exponentiation => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) =>
					AnyTypeExpression::Real(RealExpression::Exponentiation {
						lhs_expression: Box::new(lhs.to_real_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_real_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Complex(ComplexExpression::Exponentiation {
						lhs_expression: Box::new(lhs.to_complex_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_complex_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					return Err(Error { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column), line_text: None }),
				_ => unreachable!(),
			}
		},
		BinaryOperator::DoubleSlash => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Real(RealExpression::FlooredDivision {
						lhs_expression: Box::new(lhs.to_int_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_int_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					return Err(Error { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column), line_text: None }),
				_ => unreachable!(),
			}
		},
		BinaryOperator::BackSlash => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Real(RealExpression::FlooredDivision {
						lhs_expression: Box::new(lhs.to_int_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_int_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					return Err(Error { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column), line_text: None }),
				_ => unreachable!(),
			}
		},
		BinaryOperator::Equal => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) =>
					AnyTypeExpression::Bool(BoolExpression::BoolEqualTo {
						lhs_expression: Box::new(lhs.to_bool_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_bool_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) =>
					AnyTypeExpression::Bool(BoolExpression::IntEqualTo {
						lhs_expression: Box::new(lhs.to_int_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_int_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) =>
					AnyTypeExpression::Bool(BoolExpression::RealEqualTo {
						lhs_expression: Box::new(lhs.to_real_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_real_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Bool(BoolExpression::ComplexEqualTo {
						lhs_expression: Box::new(lhs.to_complex_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_complex_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					AnyTypeExpression::Bool(BoolExpression::StringEqualTo {
						lhs_expression: Box::new(lhs.to_string_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_string_expression(line_number)?),
						start_column,
					}),
				_ => unreachable!(),
			}
		},
		BinaryOperator::NotEqualTo => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) =>
					AnyTypeExpression::Bool(BoolExpression::BoolNotEqualTo {
						lhs_expression: Box::new(lhs.to_bool_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_bool_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) =>
					AnyTypeExpression::Bool(BoolExpression::IntNotEqualTo {
						lhs_expression: Box::new(lhs.to_int_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_int_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) =>
					AnyTypeExpression::Bool(BoolExpression::RealNotEqualTo {
						lhs_expression: Box::new(lhs.to_real_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_real_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Bool(BoolExpression::ComplexNotEqualTo {
						lhs_expression: Box::new(lhs.to_complex_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_complex_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					AnyTypeExpression::Bool(BoolExpression::StringNotEqualTo {
						lhs_expression: Box::new(lhs.to_string_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_string_expression(line_number)?),
						start_column,
					}),
				_ => unreachable!(),
			}
		},
		BinaryOperator::GreaterThan => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) =>
					AnyTypeExpression::Bool(BoolExpression::BoolGreaterThan {
						lhs_expression: Box::new(lhs.to_bool_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_bool_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) =>
					AnyTypeExpression::Bool(BoolExpression::IntGreaterThan {
						lhs_expression: Box::new(lhs.to_int_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_int_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Bool(BoolExpression::RealGreaterThan {
						lhs_expression: Box::new(lhs.to_real_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_real_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					AnyTypeExpression::Bool(BoolExpression::StringGreaterThan {
						lhs_expression: Box::new(lhs.to_string_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_string_expression(line_number)?),
						start_column,
					}),
				_ => unreachable!(),
			}
		},
		BinaryOperator::GreaterThanOrEqualTo => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) =>
					AnyTypeExpression::Bool(BoolExpression::BoolGreaterThanOrEqualTo {
						lhs_expression: Box::new(lhs.to_bool_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_bool_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) =>
					AnyTypeExpression::Bool(BoolExpression::IntGreaterThanOrEqualTo {
						lhs_expression: Box::new(lhs.to_int_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_int_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Bool(BoolExpression::RealGreaterThanOrEqualTo {
						lhs_expression: Box::new(lhs.to_real_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_real_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					AnyTypeExpression::Bool(BoolExpression::StringGreaterThanOrEqualTo {
						lhs_expression: Box::new(lhs.to_string_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_string_expression(line_number)?),
						start_column,
					}),
				_ => unreachable!(),
			}
		},
		BinaryOperator::LessThan => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) =>
					AnyTypeExpression::Bool(BoolExpression::BoolLessThan {
						lhs_expression: Box::new(lhs.to_bool_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_bool_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) =>
					AnyTypeExpression::Bool(BoolExpression::IntLessThan {
						lhs_expression: Box::new(lhs.to_int_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_int_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Bool(BoolExpression::RealLessThan {
						lhs_expression: Box::new(lhs.to_real_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_real_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					AnyTypeExpression::Bool(BoolExpression::StringLessThan {
						lhs_expression: Box::new(lhs.to_string_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_string_expression(line_number)?),
						start_column,
					}),
				_ => unreachable!(),
			}
		},
		BinaryOperator::LessThanOrEqualTo => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) =>
					AnyTypeExpression::Bool(BoolExpression::BoolLessThanOrEqualTo {
						lhs_expression: Box::new(lhs.to_bool_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_bool_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) =>
					AnyTypeExpression::Bool(BoolExpression::IntLessThanOrEqualTo {
						lhs_expression: Box::new(lhs.to_int_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_int_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Bool(BoolExpression::RealLessThanOrEqualTo {
						lhs_expression: Box::new(lhs.to_real_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_real_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					AnyTypeExpression::Bool(BoolExpression::StringLessThanOrEqualTo {
						lhs_expression: Box::new(lhs.to_string_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_string_expression(line_number)?),
						start_column,
					}),
				_ => unreachable!(),
			}
		},
		BinaryOperator::And => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) =>
					AnyTypeExpression::Bool(BoolExpression::And {
						lhs_expression: Box::new(lhs.to_bool_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_bool_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Int(IntExpression::BitwiseAnd {
						lhs_expression: Box::new(lhs.to_int_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_int_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					return Err(Error { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column), line_text: None }),
				_ => unreachable!(),
			}
		},
		BinaryOperator::Or => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) =>
					AnyTypeExpression::Bool(BoolExpression::Or {
						lhs_expression: Box::new(lhs.to_bool_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_bool_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Real(..), AnyTypeExpression::Real(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Int(IntExpression::BitwiseOr {
						lhs_expression: Box::new(lhs.to_int_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_int_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					return Err(Error { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column), line_text: None }),
				_ => unreachable!(),
			}
		},
	})
}

pub fn unary_operator_to_expression(operator: UnaryOperator, line_number: Option<&BigInt>, start_column: NonZeroUsize, operand: AnyTypeExpression) -> Result<AnyTypeExpression, Error> {
	Ok(match &operator {
		UnaryOperator::Negation  => match operand {
			AnyTypeExpression::Bool(..) | AnyTypeExpression::Int(..) | AnyTypeExpression::Real(..) =>
				//AnyTypeExpression::Real(RealExpression { variant: RealExpressionVariant::Negation(Box::new(operand.to_real_expression(line_number)?)), column: start_column }),
				AnyTypeExpression::Real(RealExpression::Negation { sub_expression: Box::new(operand.to_real_expression(line_number)?), start_column }),
			AnyTypeExpression::Complex(..) =>
				//AnyTypeExpression::Complex(ComplexExpression { variant: ComplexExpressionVariant::Negation(Box::new(operand.to_complex_expression(line_number)?)), column: start_column }),
				AnyTypeExpression::Complex(ComplexExpression::Negation { sub_expression: Box::new(operand.to_complex_expression(line_number)?), start_column }),
			AnyTypeExpression::String(..) =>
				return Err(Error { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column), line_text: None }),
			_ => unreachable!(),
		}
		UnaryOperator::UnaryPlus  => operand,
		UnaryOperator::Not  => match operand {
			AnyTypeExpression::Bool(..) =>
				//AnyTypeExpression::Bool(BoolExpression { variant: BoolExpressionVariant::Not(Box::new(operand.to_bool_expression(line_number)?)), column: start_column }),
				AnyTypeExpression::Bool(BoolExpression::Not { sub_expression: Box::new(operand.to_bool_expression(line_number)?), start_column }),
			AnyTypeExpression::Int(..) | AnyTypeExpression::Real(..) | AnyTypeExpression::Complex(..) =>
				//AnyTypeExpression::Real(RealExpression { variant: RealExpressionVariant::BitwiseNot(Box::new(operand.to_int_expression(line_number)?)), column: start_column }),
				AnyTypeExpression::Int(IntExpression::BitwiseNot { sub_expression: Box::new(operand.to_int_expression(line_number)?), start_column }),
			AnyTypeExpression::String(..) =>
				return Err(Error { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column), line_text: None }),
			_ => unreachable!(),
		}
	})
}

pub struct Tokens<'a, 'b> {
	pub tokens: &'b [Token<'a>],
	pub last_removed_token_end_column: NonZeroUsize,
}

impl<'a, 'b> Tokens<'a, 'b> {
	pub fn new(tokens: &'b [Token<'a>]) -> Self {
		Self {
			tokens,
			last_removed_token_end_column: 1.try_into().unwrap(),
		}
	}

	pub fn remove_tokens(&mut self, remove_count: usize) {
		if remove_count == 0 {
			return;
		}
		let tokens_removed;
		(tokens_removed, self.tokens) = self.tokens.split_at(remove_count);
		self.last_removed_token_end_column = tokens_removed.last().unwrap().end_column
	}

	/// Takes a one or two word keyword from the start of the list of tokens. Returns:
	/// * `None` if there is not a keyword at the start of the list, leaves the list unchanged.
	/// * `Some((keyword, keyword start column))` if it could, removes the tokens from the list.
	pub fn take_keyword(&mut self) -> Option<(Keyword, NonZeroUsize)> {
	// Take first keyword or return if there is not a first keyword
	let (first_keyword_variant, first_keyword_start_column) = match self.tokens.get(0) {
		Some(Token { variant: TokenVariant::Identifier { keyword: Some(keyword), .. }, start_column, .. }) => (*keyword, *start_column),
		_ => return None,
	};
	self.remove_tokens(1);
	// Take the second keyword if there is one
	let second_keyword_variant = match self.tokens.get(0) {
		Some(Token { variant: TokenVariant::Identifier { keyword: Some(keyword), .. }, .. }) => {
			self.remove_tokens(1);
			Some(*keyword)
		},
		_ => None,
	};
	// If there are two keywords and they are in a list of two word keywords, return the two word keyword
	if let Some(second_keyword_variant) = second_keyword_variant {
		for (second_keyword_of_double_word_keyword, double_word_keyword) in first_keyword_variant.get_double_word_tokens() {
			if second_keyword_variant == *second_keyword_of_double_word_keyword {
				return Some((*double_word_keyword, first_keyword_start_column));
			}
		}
	}
	// Else return the first keyword variant
	Some((first_keyword_variant, first_keyword_start_column))
}
}