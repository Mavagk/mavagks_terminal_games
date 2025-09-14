use std::{mem::replace, num::NonZeroUsize, rc::Rc};

use num::{complex::Complex64, BigInt};

use crate::mavagk_basic::{abstract_syntax_tree::{AngleOption, AnyTypeExpression, AnyTypeLValue, BoolExpression, ComplexExpression, ComplexLValue, IntExpression, IntLValue, MathOption, OptionVariableAndValue, FloatExpression, FloatLValue, Statement, StatementVariant, StringExpression, StringLValue}, error::{FullError, ErrorVariant}, token::{BinaryOperator, IdentifierType, Keyword, Token, TokenVariant, UnaryOperator}, value::{ComplexValue, IntValue, FloatValue, StringValue}};

pub fn parse_line<'a>(mut tokens: &[Token<'a>], line_number: Option<&BigInt>) -> (Box<[Statement]>, Option<FullError>) {
	let mut out = Vec::new();
	// Parse statements until we reach the end of line
	loop {
		// Strip leading colons
		while matches!(tokens.first(), Some(Token { variant: TokenVariant::Colon, .. })) {
			tokens = &tokens[1..];
		}
		if tokens.is_empty() {
			break;
		}
		// Get the length until the next colon or end of line
		let mut statement_length = None;
		let mut parenthesis_depth = 0usize;
		for (index, token) in tokens.iter().enumerate() {
			match token.variant {
				TokenVariant::LeftParenthesis => parenthesis_depth += 1,
				TokenVariant::RightParenthesis => parenthesis_depth = match parenthesis_depth.checked_sub(1) {
					Some(parenthesis_depth) => parenthesis_depth,
					None => return (out.into(), Some(FullError {
						variant: ErrorVariant::MoreRightParenthesesThanLeftParentheses, line_number: line_number.cloned(), column_number: Some(token.start_column), line_text: None
					})),
				},
				TokenVariant::Colon if parenthesis_depth == 0 => {
					statement_length = Some(index);
					break;
				}
				_ => {}
			}
		};
		let statement_length = match statement_length {
			Some(statement_length) => statement_length,
			None => tokens.len(),
		};
		// Get the tokens until the next colon or end of line
		let statement_tokens;
		(statement_tokens, tokens) = tokens.split_at(statement_length);
		let mut statement_tokens = Tokens::new(statement_tokens);
		// Parse this statement
		let statement = match parse_statement(&mut statement_tokens, line_number, true) {
			Err(error) => return (out.into(), Some(error)),
			Ok(statement) => statement.unwrap(),
		};
		// There should be no tokens left after we parse this statement
		if !statement_tokens.tokens.is_empty() {
			return (out.into(), Some(FullError {
				variant: ErrorVariant::StatementShouldEnd, line_number: line_number.cloned(), column_number: Some(statement_tokens.tokens[0].start_column), line_text: None
			}))
		}
		// Push statement to list
		out.push(statement);
	}
	(out.into(), None)
}

pub fn parse_statement<'a, 'b>(tokens: &mut Tokens, line_number: Option<&BigInt>, is_root_statement: bool) -> Result<Option<Statement>, FullError> {
	// There should be tokens
	if tokens.tokens.is_empty() {
		return Ok(None);
	}
	// Parse assignments without LET
	'a: {
		// Check if this a non-LET assignment
		let l_value_length = get_l_value_length(tokens.tokens);
		if l_value_length == 0 {
			break 'a;
		}
		match tokens.tokens.get(l_value_length) {
			None => break 'a,
			Some(Token { variant: TokenVariant::Operator(Some(BinaryOperator::Equal), _), .. }) => {},
			_ => break 'a,
		}
		// If so then get l-value
		let l_value_expression = parse_l_value(tokens, line_number)?.unwrap();
		// Skip equal sign
		tokens.remove_tokens(1);
		// Get r-value expression
		let r_value_expression = match parse_expression(tokens, line_number)? {
			Some(r_value_expression) => r_value_expression,
			None => return Err(FullError { variant: ErrorVariant::ExpectedExpression, line_number: line_number.cloned(), column_number: Some(tokens.last_removed_token_end_column), line_text: None }),
		};
		// Assemble into statement
		let l_value_start_column = l_value_expression.get_start_column();
		let statement = match l_value_expression {
			AnyTypeLValue::Int(l_value) => Statement {
				column: l_value_start_column,
				variant: StatementVariant::AssignInt(
					l_value,
					r_value_expression.to_int_expression(line_number)?
				),
			},
			AnyTypeLValue::Float(l_value) => Statement {
				column: l_value_start_column,
				variant: StatementVariant::AssignFloat(
					l_value,
					r_value_expression.to_float_expression(line_number)?
				),
			},
			AnyTypeLValue::Complex(l_value) => Statement {
				column: l_value_start_column,
				variant: StatementVariant::AssignComplex(
					l_value,
					r_value_expression.to_complex_expression(line_number)?
				),
			},
			AnyTypeLValue::String(l_value) => Statement {
				column: l_value_start_column,
				variant: StatementVariant::AssignString(
					l_value,
					r_value_expression.to_string_expression(line_number)?
				),
			},
		};
		return Ok(Some(statement));
	}
	// Else get statement keyword
	let (statement_keyword, statement_keyword_start_column) = match tokens.take_keyword() {
		Some(result) => result,
		None => return Err(FullError { variant: ErrorVariant::ExpectedStatementKeyword, line_number: line_number.cloned(), column_number: Some(tokens.last_removed_token_end_column), line_text: None }),
	};
	// Parse depending on keyword
	Ok(Some(match statement_keyword {
		// LET
		Keyword::Let => {
			// Get l-value expression
			let l_value_expression = match parse_l_value(tokens, line_number)? {
				None => return Err(FullError { variant: ErrorVariant::ExpectedExpression, line_number: line_number.cloned(), column_number: Some(tokens.last_removed_token_end_column), line_text: None }),
				Some(l_value_expression) => l_value_expression,
			};
			// Expect equal sign
			match tokens.take_next_token() {
				Some(Token { variant: TokenVariant::Operator(Some(BinaryOperator::Equal), _), ..}) => {},
				Some(token) => return Err(FullError { variant: ErrorVariant::ExpectedEqualSign, line_number: line_number.cloned(), column_number: Some(token.start_column), line_text: None }),
				None => return Err(FullError { variant: ErrorVariant::ExpectedEqualSign, line_number: line_number.cloned(), column_number: Some(tokens.last_removed_token_end_column), line_text: None }),
			}
			// Get r-value expression
			let r_value_expression = match parse_expression(tokens, line_number)? {
				None => return Err(FullError { variant: ErrorVariant::ExpectedExpression, line_number: line_number.cloned(), column_number: Some(tokens.last_removed_token_end_column), line_text: None }),
				Some(l_value_expression) => l_value_expression,
			};
			// Assemble into statement
			match l_value_expression {
				AnyTypeLValue::Int(l_value) => Statement {
					column: statement_keyword_start_column,
					variant: StatementVariant::AssignInt(
						l_value,
						r_value_expression.to_int_expression(line_number)?
					),
				},
				AnyTypeLValue::Float(l_value) => Statement {
					column: statement_keyword_start_column,
					variant: StatementVariant::AssignFloat(
						l_value,
						r_value_expression.to_float_expression(line_number)?
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
			}
		}
		// PRINT
		Keyword::Print => {
			let mut expressions = Vec::new();
			// Parse all PRINT arguments
			while !tokens.tokens.is_empty() {
				// Parse comma and semicolon arguments
				match &tokens.tokens[0] {
					Token { variant: TokenVariant::Comma, start_column, end_column: _ } => {
						expressions.push(AnyTypeExpression::PrintComma(*start_column));
						tokens.remove_tokens(1);
						continue;
					}
					Token { variant: TokenVariant::Semicolon, start_column, end_column: _ } => {
						expressions.push(AnyTypeExpression::PrintSemicolon(*start_column));
						tokens.remove_tokens(1);
						continue;
					}
					_ => {}
				}
				// Parse expression arguments
				let expression = match parse_expression(tokens, line_number)? {
					None => break,
					Some(result) => result,
				};
				expressions.push(expression);
			}
			// Assemble into statement
			Statement {
				column: statement_keyword_start_column,
				variant: StatementVariant::Print(expressions.into()),
			}
		}
		// Statements with 0-1 integer arguments
		Keyword::Goto | Keyword::Run | Keyword::Gosub => {
			// Get the argument expression if it exists
			let expression = match parse_expression(tokens, line_number)? {
				Some(expression) => Some(expression.to_int_expression(line_number)?),
				None => None,
			};
			// Parse depending on keyword
			let variant = match statement_keyword {
				Keyword::Run => StatementVariant::Run(expression),
				Keyword::Goto => StatementVariant::Goto(expression),
				Keyword::Gosub => StatementVariant::Gosub(expression),
				_ => unreachable!(),
			};
			// Assemble into statement
			Statement {
				column: statement_keyword_start_column,
				variant
			}
		}
		// LIST
		Keyword::List => 'a: {
			// If this is a blank LIST statement
			if tokens.tokens.is_empty() {
				break 'a Statement {
					variant: StatementVariant::List(None, None),
					column: statement_keyword_start_column,
				};
			}
			// Else find the hyphen
			let hyphen_index = BinaryOperator::Subtraction.find_in(tokens.tokens);
			// If there is no hyphen, just parse one expression
			if hyphen_index == None {
				// Get the argument expression
				let sub_expression = parse_expression(tokens, line_number)?.unwrap().to_int_expression(line_number)?;
				// There should be no tokens after said expression
				if !tokens.tokens.is_empty() {
					return Err(FullError { variant: ErrorVariant::StatementShouldEnd, line_number: line_number.cloned(), column_number: Some(tokens.last_removed_token_end_column), line_text: None });
				}
				// Assemble into statement
				break 'a Statement {
					variant: StatementVariant::List(Some(sub_expression.clone()), Some(sub_expression)),
					column: statement_keyword_start_column
				};
			}
			// Else split expression at the hyphen
			let mut tokens_left_of_hyphen;
			(tokens_left_of_hyphen, *tokens) = tokens.split_at(hyphen_index.unwrap());
			// Parse the range start expression if it exists
			let range_start_expression = match parse_expression(&mut tokens_left_of_hyphen, line_number)? {
				Some(range_start_expression) => Some(range_start_expression.to_int_expression(line_number)?),
				None => None,
			};
			// There should not be any tokens between the range start expression and the hyphen
			if !tokens_left_of_hyphen.tokens.is_empty() {
				return Err(FullError {
					variant: ErrorVariant::StatementShouldEnd, line_number: line_number.cloned(), column_number: Some(tokens_left_of_hyphen.tokens[0].start_column), line_text: None
				});
			}
			// Skip the hyphen
			tokens.take_next_token();
			// Make sure there is not another unparenthesized minus/hyphen after the first one
			match BinaryOperator::Subtraction.find_in(tokens.tokens) {
				Some(second_hyphen_index) =>
					return Err(FullError {
						variant: ErrorVariant::UnexpectedSecondListHyphen, line_number: line_number.cloned(), column_number: Some(tokens.tokens[second_hyphen_index].start_column), line_text: None
					}),
				None => {}
			}
			// Parse range end expression if it exists
			let range_end_expression = match parse_expression(tokens, line_number)? {
				Some(range_end_expression) => Some(range_end_expression.to_int_expression(line_number)?),
				None => None,
			};
			// Assemble into LIST statement
			Statement {
				column: statement_keyword_start_column,
				variant: StatementVariant::List(range_start_expression, range_end_expression)
			}
		}
		Keyword::If => {
			if !is_root_statement {
				return Err(FullError { variant: ErrorVariant::StatementCannotBeNested, line_number: line_number.cloned(), column_number: Some(statement_keyword_start_column), line_text: None });
			}
			// Parse condition
			let condition_expression = match parse_expression(tokens, line_number)? {
				Some(condition_expression) => condition_expression.to_bool_expression(line_number)?,
				None => return Err(FullError { variant: ErrorVariant::ExpectedExpression, line_number: line_number.cloned(), column_number: Some(tokens.last_removed_token_end_column), line_text: None }),
			};
			// Get then/goto statement
			let then_statement = Box::new(match tokens.take_keyword() {
				Some((Keyword::Then, then_start_column)) => {
					match tokens.tokens.get(0) {
						Some(Token { variant: TokenVariant::IntegerLiteral(value), start_column, .. }) => {
							tokens.take_next_token();
							Statement { variant: StatementVariant::Goto(Some(IntExpression::ConstantValue {
								value: IntValue { value: Rc::new(value.clone()) }, start_column: *start_column
							})), column: then_start_column }
						}
						_ => match parse_statement(tokens, line_number, false)? {
							Some(statement) => statement,
							None => return Err(FullError {
								variant: ErrorVariant::ExpectedStatementKeyword, line_number: line_number.cloned(), column_number: Some(tokens.last_removed_token_end_column), line_text: None
							}),
						}
					}
				}
				Some((Keyword::Goto, goto_start_column)) => {
					match parse_expression(tokens, line_number)? {
						Some(line_number_expression) => Statement { variant: StatementVariant::Goto(Some(line_number_expression.to_int_expression(line_number)?)), column: goto_start_column },
						None => return Err(FullError { variant: ErrorVariant::ExpectedExpression, line_number: line_number.cloned(), column_number: Some(tokens.last_removed_token_end_column), line_text: None }),
					}
				}
				Some((_, start_column)) => return Err(FullError {
					variant: ErrorVariant::ExpectedThenKeyword, line_number: line_number.cloned(), column_number: Some(start_column), line_text: None
				}),
				None => return Err(FullError { variant: ErrorVariant::ExpectedThenKeyword, line_number: line_number.cloned(), column_number: Some(tokens.last_removed_token_end_column), line_text: None }),
			});
			// Get else statement if it exists
			let else_statement = match tokens.take_keyword() {
				Some((Keyword::Else, else_start_column)) => {
					Some(Box::new(match tokens.tokens.get(0) {
						Some(Token { variant: TokenVariant::IntegerLiteral(value), start_column, .. }) => {
							tokens.take_next_token();
							Statement { variant: StatementVariant::Goto(Some(IntExpression::ConstantValue {
								value: IntValue { value: Rc::new(value.clone()) }, start_column: *start_column
							})), column: else_start_column }
						}
						_ => match parse_statement(tokens, line_number, false)? {
							Some(statement) => statement,
							None => return Err(FullError {
								variant: ErrorVariant::ExpectedStatementKeyword, line_number: line_number.cloned(), column_number: Some(tokens.last_removed_token_end_column), line_text: None
							}),
						}
					}))
				}
				None => None,
				Some((_, invalid_keyword_start_column)) => return Err(FullError {
					variant: ErrorVariant::ExpectedElseOrStatementEnd, line_number: line_number.cloned(), column_number: Some(invalid_keyword_start_column), line_text: None
				}),
			};
			// Assemble into statement
			Statement {
				column: statement_keyword_start_column,
				variant: StatementVariant::OneLineIf { condition_expression, then_statement, else_statement }
			}
		}
		Keyword::Option => {
			// Get the next two keywords
			let (option_variable, option_variable_start_column) = match tokens.take_keyword() {
				Some(option_variable) => option_variable,
				None => return Err(FullError { variant: ErrorVariant::ExpectedOptionArguments, line_number: line_number.cloned(), column_number: Some(tokens.last_removed_token_end_column), line_text: None }),
			};
			let (option_value, _) = match tokens.take_keyword() {
				Some(option_value) => option_value,
				None => return Err(FullError { variant: ErrorVariant::ExpectedOptionArguments, line_number: line_number.cloned(), column_number: Some(tokens.last_removed_token_end_column), line_text: None }),
			};
			// Get the option variable/value pair
			let option_variable_and_value = match (option_variable, option_value) {
				(Keyword::Angle, Keyword::Radians) => OptionVariableAndValue::Angle(AngleOption::Radians),
				(Keyword::Angle, Keyword::Degrees) => OptionVariableAndValue::Angle(AngleOption::Degrees),
				(Keyword::Angle, Keyword::Gradians) => OptionVariableAndValue::Angle(AngleOption::Gradians),
				(Keyword::Angle, Keyword::Revolutions) => OptionVariableAndValue::Angle(AngleOption::Revolutions),
				(Keyword::Arithmetic, Keyword::Decimal) => OptionVariableAndValue::ArithmeticDecimal,
				(Keyword::Arithmetic, Keyword::Native) => OptionVariableAndValue::ArithmeticNative,
				(Keyword::Math, Keyword::Ansi) => OptionVariableAndValue::Math(MathOption::Ansi),
				(Keyword::Math, Keyword::Ieee) => OptionVariableAndValue::Math(MathOption::Ieee),
				_ => return Err(FullError { variant: ErrorVariant::InvalidOptionVariableOrValue, line_number: line_number.cloned(), column_number: Some(option_variable_start_column), line_text: None }),
			};
			// Assemble into statement
			Statement {
				column: statement_keyword_start_column,
				variant: StatementVariant::Option(option_variable_and_value),
			}
		}
		Keyword::Fn => return Err(FullError { variant: ErrorVariant::ExpectedStatementKeyword, line_number: line_number.cloned(), column_number: Some(statement_keyword_start_column), line_text: None }),
		Keyword::Go => return Err(FullError { variant: ErrorVariant::SingleGoKeyword, line_number: line_number.cloned(), column_number: Some(statement_keyword_start_column), line_text: None }),
		_ => return Err(FullError { variant: ErrorVariant::NotYetImplemented("Statement".into()), line_number: line_number.cloned(), column_number: Some(statement_keyword_start_column), line_text: None }),
	}))
}

pub fn parse_expression<'a, 'b>(tokens: &mut Tokens, line_number: Option<&BigInt>)-> Result<Option<AnyTypeExpression>, FullError> {
	// Return None if there are no tokens left
	if tokens.tokens.is_empty() {
		return Ok(None);
	}

	let mut expression_primaries_and_their_unary_operators = Vec::new();
	let mut operators = Vec::new();
	'a: loop {
		// Get any unary operators before the expression primary
		let mut unary_operators_before_expression_primary = Vec::new();
		'b: loop {
			match tokens.tokens.get(0) {
				Some(Token { variant: TokenVariant::Operator(_, Some(unary_operator)), start_column, .. }) |
				Some(Token { variant: TokenVariant::Identifier { unary_operator: Some(unary_operator), .. }, start_column, .. }) => {
					unary_operators_before_expression_primary.push((*unary_operator, *start_column));
					tokens.take_next_token();
				}
				_ => break 'b,
			}
		}
		// Get expression primary
		let expression_primary = match parse_expression_primary(tokens, line_number)? {
			Some(expression_primary) => expression_primary,
			None => {
				if !expression_primaries_and_their_unary_operators.is_empty() {
					return Err(FullError { variant: ErrorVariant::ExpectedExpressionPrimary, column_number: Some(tokens.last_removed_token_end_column), line_number: line_number.cloned(), line_text: None });
				}
				if !unary_operators_before_expression_primary.is_empty() {
					return Err(FullError { variant: ErrorVariant::UnaryOperatorsAtEndOfExpression, column_number: Some(tokens.last_removed_token_end_column), line_number: line_number.cloned(), line_text: None });
				}
				break 'a;
			}
		};
		// Get binary operator or break
		let (binary_operator, binary_operator_start_column) = match tokens.tokens.get(0) {
			Some(Token { variant: TokenVariant::Operator(Some(binary_operator), _), start_column, .. }) |
			Some(Token { variant: TokenVariant::Identifier { binary_operator: Some(binary_operator), .. }, start_column, .. }) => {
				tokens.take_next_token();
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
	// Return None if this is a blank expression
	if expression_primaries_and_their_unary_operators.is_empty() {
		return Ok(None);
	}
	// Solve
	solve_operators_by_precedence(&mut expression_primaries_and_their_unary_operators, &mut operators, None, line_number)?;
	// Return
	debug_assert_eq!(expression_primaries_and_their_unary_operators.len(), 1);
	debug_assert_eq!(operators.len(), 0);
	debug_assert_eq!(expression_primaries_and_their_unary_operators[0].1.len(), 0);
	Ok(Some(expression_primaries_and_their_unary_operators.pop().unwrap().0))
}

pub fn solve_operators_by_precedence(expression_stack: &mut Vec<(AnyTypeExpression, Vec<(UnaryOperator, NonZeroUsize)>)>, operator_stack: &mut Vec<(BinaryOperator, NonZeroUsize)>, precedence: Option<u8>, line_number: Option<&BigInt>) -> Result<(), FullError> {
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

pub fn parse_expression_primary<'a, 'b>(tokens: &mut Tokens, line_number: Option<&BigInt>)-> Result<Option<AnyTypeExpression>, FullError> {
	// Get the first token or return if there are no more tokens to parse
	let Token { variant: first_token_variant, start_column: first_token_start_column, end_column: _ } = match tokens.tokens.first() {
		Some(first_token) => first_token,
		None => return Ok(None),
	};
	// Parse the expression primary
	Ok(Some(match first_token_variant {
		// Literals
		TokenVariant::IntegerLiteral(value) => {
			tokens.take_next_token();
			AnyTypeExpression::Int(IntExpression::ConstantValue { value: IntValue { value: Rc::new(value.clone()) }, start_column: *first_token_start_column })
		}
		TokenVariant::FloatLiteral { value, is_imaginary } => {
			tokens.take_next_token();
			match *is_imaginary {
				false => AnyTypeExpression::Float(FloatExpression::ConstantValue { value: FloatValue::new(*value), start_column: *first_token_start_column }),
				true => AnyTypeExpression::Complex(ComplexExpression::ConstantValue { value: ComplexValue { value: Complex64::new(0., *value) }, start_column: *first_token_start_column }),
			}
		}
		TokenVariant::StringLiteral(value) => {
			tokens.take_next_token();
			AnyTypeExpression::String(StringExpression::ConstantValue { value: StringValue { value: Rc::new(value.to_string().clone()) }, start_column: *first_token_start_column })
		}
		// An expression in parentheses
		TokenVariant::LeftParenthesis => {
			tokens.take_next_token();
			// Get the sub-expression
			let sub_expression = match parse_expression(tokens, line_number)? {
				None => return Err(FullError { variant: ErrorVariant::ExpectedExpression, column_number: Some(tokens.last_removed_token_end_column), line_number: line_number.cloned(), line_text: None }),
				Some(sub_expression) => sub_expression,
			};
			// Make sure that there is a closing parenthesis after the sub-expression
			match tokens.take_next_token() {
				Some(Token { variant: TokenVariant::RightParenthesis, .. }) => {}
				Some(Token { variant: _, start_column, .. }) =>
					return Err(FullError { variant: ErrorVariant::ExpectedRightParenthesis, column_number: Some(*start_column), line_number: line_number.cloned(), line_text: None }),
				None => return Err(FullError { variant: ErrorVariant::ExpectedRightParenthesis, column_number: Some(tokens.last_removed_token_end_column), line_number: line_number.cloned(), line_text: None }),
			}
			// Return
			sub_expression
		}
		// There should not be operators
		TokenVariant::Operator(..) => return Err(FullError { variant: ErrorVariant::UnexpectedOperator, column_number: Some(*first_token_start_column), line_number: line_number.cloned(), line_text: None }),
		// Identifiers
		TokenVariant::Identifier { .. } => {
			match parse_l_value(tokens, line_number)? {
				Some(AnyTypeLValue::Int(l_value)) => AnyTypeExpression::Int(IntExpression::LValue(l_value)),
				Some(AnyTypeLValue::Float(l_value)) => AnyTypeExpression::Float(FloatExpression::LValue(l_value)),
				Some(AnyTypeLValue::Complex(l_value)) => AnyTypeExpression::Complex(ComplexExpression::LValue(l_value)),
				Some(AnyTypeLValue::String(l_value)) => AnyTypeExpression::String(StringExpression::LValue(l_value)),
				None => return Ok(None),
			}
		}
		// End of expression
		TokenVariant::Colon | TokenVariant::Comma | TokenVariant::RightParenthesis | TokenVariant::Semicolon => return Ok(None),
		TokenVariant::SingleQuestionMark =>
			return Err(FullError { variant: ErrorVariant::NotYetImplemented("Question mark not as type".into()), column_number: Some(*first_token_start_column), line_number: line_number.cloned(), line_text: None }),
	}))
}

pub fn parse_l_value<'a, 'b>(tokens: &mut Tokens, line_number: Option<&BigInt>)-> Result<Option<AnyTypeLValue>, FullError> {
	// Get the first token or return if there are no more tokens to parse
	let Token { variant: first_token_variant, start_column: first_token_start_column, end_column: first_token_end_column } = match tokens.tokens.first() {
		Some(first_token) => first_token,
		None => return Ok(None),
	};
	let (keyword, is_reserved) = match first_token_variant {
		TokenVariant::Identifier { keyword, is_reserved_keyword, .. } => (*keyword, *is_reserved_keyword),
		_ => return Err(FullError { variant: ErrorVariant::InvalidLValue, column_number: Some(*first_token_end_column), line_number: line_number.cloned(), line_text: None }),
	};
	// Return if the identifier name is a reserved keyword
	if is_reserved {
		return Ok(None);
	}
	// Get if this is a FN function
	let uses_fn_keyword = keyword == Some(Keyword::Fn);
	match uses_fn_keyword {
		false => {}
		true => {
			tokens.take_next_token();
		}
	}
	// Get identifier name
	let Token { variant: token_after_fn_variant, start_column: token_after_fn_start_column, end_column: _ } = match tokens.tokens.get(0) {
		Some(token_after_fn) => token_after_fn,
		None => return Err(FullError { variant: ErrorVariant::ExpectedFunctionNameAfterFn, column_number: Some(*first_token_end_column), line_number: line_number.cloned(), line_text: None }),
	};
	let (identifier_name, identifier_type, identifier_is_optional, supplied_function) = match token_after_fn_variant {
		TokenVariant::Identifier { name, identifier_type, is_optional, supplied_function, .. } => (name, identifier_type, is_optional, *supplied_function),
		_ => return Err(FullError { variant: ErrorVariant::ExpectedFunctionNameAfterFn, column_number: Some(*token_after_fn_start_column), line_number: line_number.cloned(), line_text: None }),
	};
	if *identifier_is_optional {
		return Err(FullError { variant: ErrorVariant::NotYetImplemented("Optional functions".into()), column_number: Some(*token_after_fn_start_column), line_number: line_number.cloned(), line_text: None });
	}
	tokens.take_next_token();
	// Return if there is not a left parenthesis after the identifier
	match tokens.tokens.get(0) {
		Some(Token { variant: TokenVariant::LeftParenthesis, .. }) => {},
		_ => return Ok(Some(match identifier_type {
			IdentifierType::Integer => AnyTypeLValue::Int(IntLValue {
				name: (*identifier_name).into(), arguments: Box::default(), uses_fn_keyword, has_parentheses: false, start_column: *first_token_start_column, supplied_function
			}),
			IdentifierType::UnmarkedNumber => AnyTypeLValue::Float(FloatLValue {
				name: (*identifier_name).into(), arguments: Box::default(), uses_fn_keyword, has_parentheses: false, start_column: *first_token_start_column, supplied_function
			}),
			IdentifierType::ComplexNumber => AnyTypeLValue::Complex(ComplexLValue {
				name: (*identifier_name).into(), arguments: Box::default(), uses_fn_keyword, has_parentheses: false, start_column: *first_token_start_column, supplied_function
			}),
			IdentifierType::String => AnyTypeLValue::String(StringLValue {
				name: (*identifier_name).into(), arguments: Box::default(), uses_fn_keyword, has_parentheses: false, start_column: *first_token_start_column, supplied_function
			}),
		})),
	}
	// Skip opening parenthesis
	tokens.take_next_token();
	// Get arguments
	let mut arguments = Vec::new();
	// Make sure there is not a leading comma
	match tokens.tokens.get(0) {
		Some(Token { variant: TokenVariant::Comma, start_column, .. }) =>
			return Err(FullError { variant: ErrorVariant::LeadingCommaInFunctionArguments, column_number: Some(*start_column), line_number: line_number.cloned(), line_text: None }),
		_ => {},
	}
	// Parse each argument
	'b: loop {
		// If we reach a non-expression token
		match tokens.tokens.get(0) {
			// Comma
			Some(Token { variant: TokenVariant::Comma, start_column, ..}) =>
				return Err(FullError { variant: ErrorVariant::TwoSequentialCommasTogetherInFunctionArguments, column_number: Some(*start_column), line_number: line_number.cloned(), line_text: None }),
			// Right parenthesis
			Some(Token { variant: TokenVariant::RightParenthesis, .. }) => {
				tokens.take_next_token();
				break 'b;
			}
			// Colon / semicolon
			Some(Token { variant: TokenVariant::Colon | TokenVariant::Semicolon, start_column, .. }) =>
				return Err(FullError { variant: ErrorVariant::InvalidSeparatorInFunctionArguments, column_number: Some(*start_column), line_number: line_number.cloned(), line_text: None }),
			// End of statement without closing parenthesis
			None =>
				return Err(FullError { variant: ErrorVariant::ExpectedRightParenthesis, column_number: Some(tokens.last_removed_token_end_column), line_number: line_number.cloned(), line_text: None }),
			_ => {}
		}
		// Parse argument
		let argument_expression = parse_expression(tokens, line_number)?.unwrap();
		arguments.push(argument_expression);
		// Parse comma or right parentheses
		match tokens.tokens.get(0) {
			Some(Token { variant: TokenVariant::Comma, ..}) => {
				tokens.take_next_token();
			}
			Some(Token { variant: TokenVariant::RightParenthesis, .. }) => {
				tokens.take_next_token();
				break 'b;
			}
			_ => {}
		};
	}
	// Return
	return Ok(Some(match identifier_type {
		IdentifierType::Integer => AnyTypeLValue::Int(IntLValue {
			name: (*identifier_name).into(), arguments: arguments.into(), uses_fn_keyword, has_parentheses: true, start_column: *first_token_start_column, supplied_function
		}),
		IdentifierType::UnmarkedNumber => AnyTypeLValue::Float(FloatLValue {
			name: (*identifier_name).into(), arguments: arguments.into(), uses_fn_keyword, has_parentheses: true, start_column: *first_token_start_column, supplied_function
		}),
		IdentifierType::ComplexNumber => AnyTypeLValue::Complex(ComplexLValue {
			name: (*identifier_name).into(), arguments: arguments.into(), uses_fn_keyword, has_parentheses: true, start_column: *first_token_start_column, supplied_function
		}),
		IdentifierType::String => AnyTypeLValue::String(StringLValue {
			name: (*identifier_name).into(), arguments: arguments.into(), uses_fn_keyword, has_parentheses: true, start_column: *first_token_start_column, supplied_function
		}),
	}))
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

pub fn binary_operator_to_expression(operator: BinaryOperator, line_number: Option<&BigInt>, start_column: NonZeroUsize, lhs: AnyTypeExpression, rhs: AnyTypeExpression) -> Result<AnyTypeExpression, FullError> {
	Ok(match operator {
		BinaryOperator::AdditionConcatenation => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) =>
					AnyTypeExpression::Int(IntExpression::Addition {
						lhs_expression: Box::new(lhs.to_int_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_int_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) =>
					AnyTypeExpression::Float(FloatExpression::Addition {
						lhs_expression: Box::new(lhs.to_float_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_float_expression(line_number)?),
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
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					return Err(FullError { variant: ErrorVariant::CannotConcatenateNumbers, line_number: line_number.cloned(), column_number: Some(start_column), line_text: None }),
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
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) =>
					AnyTypeExpression::Int(IntExpression::Subtraction {
						lhs_expression: Box::new(lhs.to_int_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_int_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) =>
					AnyTypeExpression::Float(FloatExpression::Subtraction {
						lhs_expression: Box::new(lhs.to_float_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_float_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Complex(ComplexExpression::Subtraction {
						lhs_expression: Box::new(lhs.to_complex_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_complex_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					return Err(FullError { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column), line_text: None }),
				_ => unreachable!(),
			}
		},
		BinaryOperator::Multiplication => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) =>
					AnyTypeExpression::Int(IntExpression::Multiplication {
						lhs_expression: Box::new(lhs.to_int_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_int_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) =>
					AnyTypeExpression::Float(FloatExpression::Multiplication {
						lhs_expression: Box::new(lhs.to_float_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_float_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Complex(ComplexExpression::Multiplication {
						lhs_expression: Box::new(lhs.to_complex_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_complex_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					return Err(FullError { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column), line_text: None }),
				_ => unreachable!(),
			}
		},
		BinaryOperator::Division => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) =>
					AnyTypeExpression::Float(FloatExpression::Division {
						lhs_expression: Box::new(lhs.to_float_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_float_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Complex(ComplexExpression::Division {
						lhs_expression: Box::new(lhs.to_complex_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_complex_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					return Err(FullError { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column), line_text: None }),
				_ => unreachable!(),
			}
		},
		BinaryOperator::Exponentiation => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) =>
					AnyTypeExpression::Float(FloatExpression::Exponentiation {
						lhs_expression: Box::new(lhs.to_float_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_float_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Complex(ComplexExpression::Exponentiation {
						lhs_expression: Box::new(lhs.to_complex_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_complex_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					return Err(FullError { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column), line_text: None }),
				_ => unreachable!(),
			}
		},
		BinaryOperator::DoubleSlash => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Int(IntExpression::FlooredDivision {
						lhs_expression: Box::new(lhs.to_int_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_int_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					return Err(FullError { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column), line_text: None }),
				_ => unreachable!(),
			}
		},
		BinaryOperator::BackSlash => {
			let (lhs, rhs) = lhs.upcast(rhs, line_number)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Int(IntExpression::FlooredDivision {
						lhs_expression: Box::new(lhs.to_int_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_int_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					return Err(FullError { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column), line_text: None }),
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
				(AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) =>
					AnyTypeExpression::Bool(BoolExpression::FloatEqualTo {
						lhs_expression: Box::new(lhs.to_float_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_float_expression(line_number)?),
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
				(AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) =>
					AnyTypeExpression::Bool(BoolExpression::FloatNotEqualTo {
						lhs_expression: Box::new(lhs.to_float_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_float_expression(line_number)?),
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
				(AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Bool(BoolExpression::FloatGreaterThan {
						lhs_expression: Box::new(lhs.to_float_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_float_expression(line_number)?),
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
				(AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Bool(BoolExpression::FloatGreaterThanOrEqualTo {
						lhs_expression: Box::new(lhs.to_float_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_float_expression(line_number)?),
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
				(AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Bool(BoolExpression::FloatLessThan {
						lhs_expression: Box::new(lhs.to_float_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_float_expression(line_number)?),
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
				(AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Bool(BoolExpression::FloatLessThanOrEqualTo {
						lhs_expression: Box::new(lhs.to_float_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_float_expression(line_number)?),
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
				(AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Int(IntExpression::BitwiseAnd {
						lhs_expression: Box::new(lhs.to_int_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_int_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					return Err(FullError { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column), line_text: None }),
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
				(AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Int(IntExpression::BitwiseOr {
						lhs_expression: Box::new(lhs.to_int_expression(line_number)?),
						rhs_expression: Box::new(rhs.to_int_expression(line_number)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					return Err(FullError { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column), line_text: None }),
				_ => unreachable!(),
			}
		},
	})
}

pub fn unary_operator_to_expression(operator: UnaryOperator, line_number: Option<&BigInt>, start_column: NonZeroUsize, operand: AnyTypeExpression) -> Result<AnyTypeExpression, FullError> {
	Ok(match &operator {
		UnaryOperator::Negation  => match operand {
			AnyTypeExpression::Bool(..) | AnyTypeExpression::Int(..) =>
				AnyTypeExpression::Int(IntExpression::Negation { sub_expression: Box::new(operand.to_int_expression(line_number)?), start_column }),
			AnyTypeExpression::Float(..) =>
				AnyTypeExpression::Float(FloatExpression::Negation { sub_expression: Box::new(operand.to_float_expression(line_number)?), start_column }),
			AnyTypeExpression::Complex(..) =>
				AnyTypeExpression::Complex(ComplexExpression::Negation { sub_expression: Box::new(operand.to_complex_expression(line_number)?), start_column }),
			AnyTypeExpression::String(..) =>
				return Err(FullError { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column), line_text: None }),
			_ => unreachable!(),
		}
		UnaryOperator::UnaryPlus  => operand,
		UnaryOperator::Not  => match operand {
			AnyTypeExpression::Bool(..) =>
				AnyTypeExpression::Bool(BoolExpression::Not { sub_expression: Box::new(operand.to_bool_expression(line_number)?), start_column }),
			AnyTypeExpression::Int(..) | AnyTypeExpression::Float(..) | AnyTypeExpression::Complex(..) =>
				AnyTypeExpression::Int(IntExpression::BitwiseNot { sub_expression: Box::new(operand.to_int_expression(line_number)?), start_column }),
			AnyTypeExpression::String(..) =>
				return Err(FullError { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line_number.cloned(), column_number: Some(start_column), line_text: None }),
			_ => unreachable!(),
		}
	})
}


#[derive(Clone, Copy)]
pub struct Tokens<'a, 'b> {
	pub tokens: &'b [Token<'a>],
	pub last_removed_token_end_column: NonZeroUsize,
}

impl<'a, 'b> Tokens<'a, 'b> {
	/// Tokens must be at least one token long
	pub fn new(tokens: &'b [Token<'a>]) -> Self {
		Self {
			tokens,
			last_removed_token_end_column: tokens[0].start_column,
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

	/// Returns the token and it's start column.
	pub fn take_next_token(&mut self) -> Option<&Token<'_>> {
		let tokens_removed;
		(tokens_removed, self.tokens) = match self.tokens.split_at_checked(1) {
			Some(result) => result,
			None => return None,
		};
		self.last_removed_token_end_column = tokens_removed[0].end_column;
		Some(&tokens_removed[0])
	}

	pub fn split_at(self, index: usize) -> (Self, Self) {
		let (left_tokens, right_tokens) = self.tokens.split_at(index);
		let split_column = match index {
			0 => self.last_removed_token_end_column,
			_ => left_tokens.last().unwrap().end_column,
		};
		(Self {
			tokens: left_tokens,
			last_removed_token_end_column: self.last_removed_token_end_column,
		},
		Self {
			tokens: right_tokens,
			last_removed_token_end_column: split_column,
		})
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
			Some(*keyword)
		},
		_ => None,
	};
	// If there are two keywords and they are in a list of two word keywords, return the two word keyword
	if let Some(second_keyword_variant) = second_keyword_variant {
		for (second_keyword_of_double_word_keyword, double_word_keyword) in first_keyword_variant.get_double_word_tokens() {
			if second_keyword_variant == *second_keyword_of_double_word_keyword {
				self.remove_tokens(1);
				return Some((*double_word_keyword, first_keyword_start_column));
			}
		}
	}
	// Else return the first keyword variant
	Some((first_keyword_variant, first_keyword_start_column))
}
}