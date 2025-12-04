use std::{collections::HashSet, mem::replace, num::NonZeroUsize, rc::Rc};

use num::{bigint::ToBigInt, complex::Complex64, Zero, One};
use strum::IntoDiscriminant;

use crate::mavagk_basic::{abstract_syntax_tree::{AnyTypeExpression, AnyTypeLValue, ArrayDimension, BoolExpression, ComplexExpression, ComplexLValue, ComplexSuppliedFunction, Datum, FloatExpression, FloatLValue, FloatSuppliedFunction, IntExpression, IntLValue, IntSuppliedFunction, OptionVariableAndValue, PrintOperand, Statement, StatementVariant, StringExpression, StringLValue, StringSuppliedFunction}, error::{Error, ErrorVariant}, options::{AngleOption, BaseOption, CollateOption, MachineOption, MathOption}, token::{BinaryOperator, IdentifierType, Keyword, SuppliedFunctionIdentifier, Token, TokenVariant, UnaryOperator}, value::{ComplexValue, FloatValue, IntValue, StringValue}};

/// Parses the a line or tokens into a list of statements and an error if the line has an error. Takes in the tokens received by tokenizing the line.
pub fn parse_line<'a>(tokens: &mut Tokens) -> (Box<[Statement]>, Option<Error>) {
	let mut parsed_tokens = Vec::new();
	// Parse statements until we reach the end of line
	loop {
		// Strip leading colons
		while matches!(tokens.tokens.first(), Some(Token { variant: TokenVariant::Colon, .. })) {
			tokens.take_next_token();
		}
		// Return once there are no more tokens left
		if tokens.tokens.is_empty() {
			return (parsed_tokens.into(), None);
		}
		// Parse this statement
		let statement = match parse_statement(tokens, true) {
			Err(error) => return (parsed_tokens.into(), Some(error)),
			Ok(statement) => statement.unwrap(),
		};
		// There should be either no tokens or a colon left after we parse this statement
		if !tokens.tokens.is_empty() && !matches!(tokens.tokens.first(), Some(Token { variant: TokenVariant::Colon, .. })) {
			return (parsed_tokens.into(), Some(ErrorVariant::StatementShouldEnd.at_column(tokens.tokens[0].start_column)))
		}
		// Push statement to list
		parsed_tokens.push(statement);
	}
}

/// Parses a single statement from a list of tokens, removing the tokens parsed, `is_root_statement` is false if this statement is nested inside another statement such as "IF A THEN PRINT "Hello"".
fn parse_statement<'a, 'b>(tokens: &mut Tokens, is_root_statement: bool) -> Result<Option<Statement>, Error> {
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
		let l_value_expression = parse_l_value(tokens)?.unwrap();
		// Skip equal sign
		tokens.remove_tokens(1);
		// Get r-value expression
		let r_value_expression = match parse_expression(tokens)? {
			Some(r_value_expression) => r_value_expression,
			None => return Err(ErrorVariant::ExpectedExpression.at_column(tokens.last_removed_token_end_column)),
		};
		// Assemble into statement
		let l_value_start_column = l_value_expression.get_start_column();
		let statement = match l_value_expression {
			AnyTypeLValue::Int(l_value) => Statement {
				column: l_value_start_column,
				variant: StatementVariant::NumericAssignment(
					Box::new([AnyTypeLValue::Int(l_value)]),
					AnyTypeExpression::Int(cast_to_int_expression(r_value_expression)?),
				),
			},
			AnyTypeLValue::Float(l_value) => Statement {
				column: l_value_start_column,
				variant: StatementVariant::NumericAssignment(
					Box::new([AnyTypeLValue::Float(l_value)]),
					AnyTypeExpression::Float(cast_to_float_expression(r_value_expression)?),
				),
			},
			AnyTypeLValue::Complex(l_value) => Statement {
				column: l_value_start_column,
				variant: StatementVariant::NumericAssignment(
					Box::new([AnyTypeLValue::Complex(l_value)]),
					AnyTypeExpression::Complex(cast_to_complex_expression(r_value_expression)?),
				),
			},
			AnyTypeLValue::String(l_value) => Statement {
				column: l_value_start_column,
				variant: StatementVariant::StringAssignment(
					Box::new([l_value]),
					cast_to_string_expression(r_value_expression)?,
				),
			},
		};
		return Ok(Some(statement));
	}
	// Else get statement keyword
	let (statement_keyword, statement_keyword_start_column) = match tokens.take_keyword() {
		Some(result) => result,
		None => return Err(ErrorVariant::ExpectedStatementKeyword.at_column(tokens.last_removed_token_end_column)),
	};
	// Parse depending on keyword
	Ok(Some(match statement_keyword {
		// LET
		Keyword::Let => {
			// Get comma separated l_values
			let mut l_value_expressions = Vec::new();
			loop {
				// Get l-value expression
				let l_value_expression = match parse_l_value(tokens)? {
					None => return Err(ErrorVariant::ExpectedExpression.at_column(tokens.last_removed_token_end_column)),
					Some(l_value_expression) => l_value_expression,
				};
				l_value_expressions.push(l_value_expression);
				// If there is not a comma after the input, end the statement
				if !matches!(tokens.tokens.first(), Some(Token { variant: TokenVariant::Comma, .. })) {
					break;
				}
				// If there is a comma, take it and continue
				tokens.take_next_token();
			}
			// Expect equal sign
			expect_and_remove_equal_sign(tokens)?;
			// Get r-value expression
			let r_value_expression = match parse_expression(tokens)? {
				None => return Err(ErrorVariant::ExpectedExpression.at_column(tokens.last_removed_token_end_column)),
				Some(l_value_expression) => l_value_expression,
			};
			// Assemble into statement
			match r_value_expression {
				// String assignments
				AnyTypeExpression::String(r_value_expression) => Statement {
					column: statement_keyword_start_column,
					variant: StatementVariant::StringAssignment(
						{
							let mut string_l_value_expressions = Vec::new();
							for l_value_expression in l_value_expressions {
								match l_value_expression {
									AnyTypeLValue::String(l_value_expression) => string_l_value_expressions.push(l_value_expression),
									_ => return Err(ErrorVariant::NumberCastToString.at_column(l_value_expression.get_start_column())),
								}
							}
							string_l_value_expressions.into()
						},
						r_value_expression,
					),
				},
				// If all l-values are floats
				_ if l_value_expressions.iter().all(|l_value_expression| matches!(l_value_expression, AnyTypeLValue::Float(..))) => Statement {
					column: statement_keyword_start_column,
					variant: StatementVariant::NumericAssignment(
						l_value_expressions.into(),
						AnyTypeExpression::Float(cast_to_float_expression(r_value_expression)?),
					),
				},
				// If all l-values are ints
				_ if l_value_expressions.iter().all(|l_value_expression| matches!(l_value_expression, AnyTypeLValue::Float(..))) => Statement {
					column: statement_keyword_start_column,
					variant: StatementVariant::NumericAssignment(
						l_value_expressions.into(),
						AnyTypeExpression::Int(cast_to_int_expression(r_value_expression)?),
					),
				},
				// If all l-values are complexes
				_ if l_value_expressions.iter().all(|l_value_expression| matches!(l_value_expression, AnyTypeLValue::Float(..))) => Statement {
					column: statement_keyword_start_column,
					variant: StatementVariant::NumericAssignment(
						l_value_expressions.into(),
						AnyTypeExpression::Complex(cast_to_complex_expression(r_value_expression)?),
					),
				},
				// Mixed numeric l-value types
				_ => Statement {
					column: statement_keyword_start_column,
					variant: StatementVariant::NumericAssignment(
						{
							for l_value_expression in l_value_expressions.iter() {
								if matches!(l_value_expression, AnyTypeLValue::String(..)) {
									return Err(ErrorVariant::StringCastToNumber.at_column(l_value_expression.get_start_column()));
								}
							}
							l_value_expressions.into()
						},
						r_value_expression,
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
						expressions.push(PrintOperand::Comma(*start_column));
						tokens.remove_tokens(1);
						continue;
					}
					Token { variant: TokenVariant::Semicolon, start_column, end_column: _ } => {
						expressions.push(PrintOperand::Semicolon(*start_column));
						tokens.remove_tokens(1);
						continue;
					}
					_ => {}
				}
				// Parse expression arguments
				let expression = match parse_expression(tokens)? {
					None => break,
					Some(result) => PrintOperand::Expression(result),
				};
				expressions.push(expression);
			}
			// Assemble into statement
			Statement {
				column: statement_keyword_start_column,
				variant: StatementVariant::Print(expressions.into()),
			}
		}
		// INPUT
		Keyword::Input => {
			let mut prompt = None;
			let mut timeout = None;
			let mut elapsed = None;
			let mut inputs = Vec::new();
			// Get prompt/timeout/elapsed
			// Commodore style prompt
			if matches!(tokens.tokens.first(), Some(Token { variant: TokenVariant::StringLiteral(..), .. })) {
				// Parse prompt
				prompt = Some(parse_expression(tokens)?.unwrap());
				// Expect colon/semicolon
				match tokens.take_next_token() {
					None => return Err(ErrorVariant::ExpectedColonAfterInputPrompt.at_column(tokens.last_removed_token_end_column)),
					Some(Token { variant: TokenVariant::Colon | TokenVariant::Semicolon, .. }) => {},
					Some(Token { start_column, .. }) => return Err(ErrorVariant::ExpectedColonAfterInputPrompt.at_column(*start_column)),
				}
			}
			// ANSI style prompt
			else { 'a: {
				// Make sure this is an ANSI style prompt
				let mut test_tokens = tokens.clone();
				if !matches!(test_tokens.take_keyword(), Some((Keyword::Prompt | Keyword::Timeout | Keyword::Elapsed, _))) {
					break 'a;
				}
				if !matches!(parse_expression(&mut test_tokens), Ok(Some(_))) {
					break 'a;
				}
				// Get each prompt/timeout/elapsed
				loop {
					// Get prompt/timeout/elapsed
					match tokens.take_keyword() {
						None => return Err(ErrorVariant::ExpectedInputPrompt.at_column(tokens.last_removed_token_end_column)),
						Some((Keyword::Prompt, start_column)) => {
							let prompt_expression = match parse_expression(tokens)? {
								Some(prompt_expression) => prompt_expression,
								None => return Err(ErrorVariant::ExpectedExpression.at_column(tokens.last_removed_token_end_column)),
							};
							if replace(&mut prompt, Some(prompt_expression)).is_some() {
								return Err(ErrorVariant::MultiplePromptsForInput.at_column(start_column));
							}
						}
						Some((Keyword::Timeout, start_column)) => {
							let timeout_expression = match parse_expression(tokens)? {
								Some(timeout_expression) => timeout_expression,
								None => return Err(ErrorVariant::ExpectedExpression.at_column(tokens.last_removed_token_end_column)),
							};
							if replace(&mut timeout, Some(timeout_expression)).is_some() {
								return Err(ErrorVariant::MultipleTimeoutsForInput.at_column(start_column));
							}
						}
						Some((Keyword::Elapsed, start_column)) => {
							let elapsed_expression = match parse_expression(tokens)? {
								Some(elapsed_expression) => elapsed_expression,
								None => return Err(ErrorVariant::ExpectedExpression.at_column(tokens.last_removed_token_end_column)),
							};
							if replace(&mut elapsed, Some(elapsed_expression)).is_some() {
								return Err(ErrorVariant::MultipleElapsedsForInput.at_column(start_column));
							}
						}
						Some((_, start_column)) => return Err(ErrorVariant::ExpectedInputPrompt.at_column(start_column)),
					}
					// Expect a comma to indicate there is another prompt/timeout/elapsed or a colon/semicolon to indicate the end of said list.
					match tokens.take_next_token() {
						None => return Err(ErrorVariant::ExpectedColonAfterInputPrompt.at_column(tokens.last_removed_token_end_column)),
						Some(Token { variant: TokenVariant::Colon | TokenVariant::Semicolon, .. }) => break,
						Some(Token { variant: TokenVariant::Comma, .. }) => {},
						Some(Token { start_column, .. }) => return Err(ErrorVariant::ExpectedColonAfterInputPrompt.at_column(*start_column)),
					}
				}
			}}
			// Get inputs
			loop {
				// Get input
				let input_l_value = match parse_l_value(tokens)? {
					Some(input_l_value) => input_l_value,
					None => return Err(ErrorVariant::InvalidLValue.at_column(tokens.last_removed_token_end_column)),
				};
				inputs.push(input_l_value);
				// If there is not a comma after the input, end the statement
				if !matches!(tokens.tokens.first(), Some(Token { variant: TokenVariant::Comma, .. })) {
					break;
				}
				tokens.take_next_token();
			}
			// Assemble into statement
			Statement {
				column: statement_keyword_start_column,
				variant: StatementVariant::Input { prompt, timeout, elapsed, inputs: inputs.into() },
			}
		}
		// FOR
		Keyword::For => {
			// Get the loop variable
			let loop_variable = match parse_l_value(tokens)? {
				Some(loop_variable) => loop_variable,
				None => return Err(ErrorVariant::InvalidLValue.at_column(tokens.last_removed_token_end_column)),
			};
			// The loop variable should just be a simple variable
			if loop_variable.has_parentheses() {
				return Err(ErrorVariant::LoopVariableNotSimpleVar.at_column(loop_variable.get_start_column()));
			}
			// Expect equal sign
			expect_and_remove_equal_sign(tokens)?;
			// Get initial value
			let initial_value_expression = match parse_expression(tokens)? {
				Some(initial_value_expression) => initial_value_expression,
				None => return Err(ErrorVariant::ExpectedExpression.at_column(tokens.last_removed_token_end_column)),
			};
			// Expect TO token
			match tokens.take_next_token() {
				Some(Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::To), .. }, ..}) => {},
				Some(token) => return Err(ErrorVariant::ExpectedEqualSign.at_column(token.start_column)),
				None => return Err(ErrorVariant::ExpectedEqualSign.at_column(tokens.last_removed_token_end_column)),
			}
			// Get limit
			let limit_value_expression = match parse_expression(tokens)? {
				Some(limit_value_expression) => limit_value_expression,
				None => return Err(ErrorVariant::ExpectedExpression.at_column(tokens.last_removed_token_end_column)),
			};
			// Get STEP part if it exists
			let step_value_expression = if matches!(tokens.tokens.first(), Some(Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::Step), .. }, .. })) {
				tokens.take_next_token();
				// Get STEP expression
				Some(match parse_expression(tokens)? {
					Some(step_value_expression) => step_value_expression,
					None => return Err(ErrorVariant::ExpectedExpression.at_column(tokens.last_removed_token_end_column)),
				})
			}
			else {
				None
			};
			// Assemble into statement
			match loop_variable {
				AnyTypeLValue::Int(loop_variable) => {
					Statement {
						variant: StatementVariant::ForInt {
							loop_variable,
							initial: cast_to_int_expression(initial_value_expression)?,
							limit: cast_to_int_expression(limit_value_expression)?,
							step: match step_value_expression {
								Some(step_value_expression) => Some(cast_to_int_expression(step_value_expression)?),
								None => None,
							}
						},
						column: statement_keyword_start_column,
					}
				}
				AnyTypeLValue::Float(loop_variable) => {
					Statement {
						variant: StatementVariant::ForFloat {
							loop_variable,
							initial: cast_to_float_expression(initial_value_expression)?,
							limit: cast_to_float_expression(limit_value_expression)?,
							step: match step_value_expression {
								Some(step_value_expression) => Some(cast_to_float_expression(step_value_expression)?),
								None => None,
							}
						},
						column: statement_keyword_start_column,
					}
				}
				AnyTypeLValue::Complex(loop_variable) => return Err(ErrorVariant::Unimplemented("Complex FOR loops".into()).at_column(loop_variable.start_column)),
				AnyTypeLValue::String(loop_variable) => return Err(ErrorVariant::Unimplemented("String FOR loops".into()).at_column(loop_variable.start_column)),
			}
		}
		// NEXT
		Keyword::Next => {
			// If this is a next without any arguments
			if !matches!(tokens.tokens.first(), Some(Token { variant: TokenVariant::Identifier { is_reserved_keyword: false, .. }, .. })) {
				return Ok(Some(Statement {
					column: statement_keyword_start_column,
					variant: StatementVariant::Next(Box::default()),
				}));
			}
			// Get each comma-separated loop variable
			let mut loop_variables = Vec::new();
			loop {
				// Parse loop variable
				let loop_variable = match parse_l_value(tokens)? {
					Some(initial_value_expression) => initial_value_expression,
					None => return Err(ErrorVariant::InvalidLValue.at_column(tokens.last_removed_token_end_column)),
				};
				// Check that the l-value is valid
				match loop_variable {
					AnyTypeLValue::Complex(loop_variable) => return Err(ErrorVariant::Unimplemented("Complex FOR loops".into()).at_column(loop_variable.start_column)),
					AnyTypeLValue::String(loop_variable) => return Err(ErrorVariant::Unimplemented("String FOR loops".into()).at_column(loop_variable.start_column)),
					_ => {}
				}
				if loop_variable.has_parentheses() {
					return Err(ErrorVariant::LoopVariableNotSimpleVar.at_column(loop_variable.get_start_column()));
				}
				// Push variable
				loop_variables.push(loop_variable);
				// The loop variable list ends if there is not a comma after the loop variable
				if !matches!(tokens.tokens.first(), Some(Token { variant: TokenVariant::Comma, .. })) {
					break;
				}
				// If it does have a comma, remove it
				tokens.take_next_token();
			}
			// Assemble into statement
			Statement {
				column: statement_keyword_start_column,
				variant: StatementVariant::Next(loop_variables.into()),
			}
		}
		// Statements with 0-1 integer arguments
		Keyword::Goto | Keyword::Run | Keyword::Gosub => {
			// Get the argument expression if it exists
			let expression = match parse_expression(tokens)? {
				Some(expression) => Some(cast_to_int_expression(expression)?),
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
				let sub_expression = cast_to_int_expression(parse_expression(tokens)?.unwrap())?;
				// There should be no tokens after said expression
				if !tokens.tokens.is_empty() {
					return Err(ErrorVariant::StatementShouldEnd.at_column(tokens.last_removed_token_end_column));
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
			let range_start_expression = match parse_expression(&mut tokens_left_of_hyphen)? {
				Some(range_start_expression) => Some(cast_to_int_expression(range_start_expression)?),
				None => None,
			};
			// There should not be any tokens between the range start expression and the hyphen
			if !tokens_left_of_hyphen.tokens.is_empty() {
				return Err(ErrorVariant::StatementShouldEnd.at_column(tokens_left_of_hyphen.tokens[0].start_column));
			}
			// Skip the hyphen
			tokens.take_next_token();
			// Make sure there is not another unparenthesized minus/hyphen after the first one
			match BinaryOperator::Subtraction.find_in(tokens.tokens) {
				Some(second_hyphen_index) =>
					return Err(ErrorVariant::UnexpectedSecondListHyphen.at_column(tokens.tokens[second_hyphen_index].start_column)),
				None => {}
			}
			// Parse range end expression if it exists
			let range_end_expression = match parse_expression(tokens)? {
				Some(range_end_expression) => Some(cast_to_int_expression(range_end_expression)?),
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
				return Err(ErrorVariant::StatementCannotBeNested.at_column(statement_keyword_start_column));
			}
			// Parse condition
			let condition_expression = match parse_expression(tokens)? {
				Some(condition_expression) => cast_to_bool_expression(condition_expression)?,
				None => return Err(ErrorVariant::ExpectedExpression.at_column(tokens.last_removed_token_end_column)),
			};
			// Get then/goto statement
			let then_statement = Box::new(match tokens.take_keyword() {
				Some((Keyword::Then, then_start_column)) => {
					match tokens.tokens.get(0) {
						Some(Token { variant: TokenVariant::IntegerLiteral(value), start_column, .. }) => {
							tokens.take_next_token();
							Statement { variant: StatementVariant::Goto(Some(IntExpression::ConstantValue {
								value: IntValue { value: Rc::new(value.clone().into()) }, start_column: *start_column
							})), column: then_start_column }
						}
						_ => match parse_statement(tokens, false)? {
							Some(statement) => statement,
							None => return Err(ErrorVariant::ExpectedStatementKeyword.at_column(tokens.last_removed_token_end_column)),
						}
					}
				}
				Some((Keyword::Goto, goto_start_column)) => {
					match parse_expression(tokens)? {
						Some(line_number_expression) => Statement { variant: StatementVariant::Goto(Some(cast_to_int_expression(line_number_expression)?)), column: goto_start_column },
						None => return Err(ErrorVariant::ExpectedExpression.at_column(tokens.last_removed_token_end_column)),
					}
				}
				Some((_, start_column)) => return Err(ErrorVariant::ExpectedThenKeyword.at_column(start_column)),
				None => return Err(ErrorVariant::ExpectedThenKeyword.at_column(tokens.last_removed_token_end_column)),
			});
			// Get else statement if it exists
			let else_statement = match tokens.take_keyword() {
				Some((Keyword::Else, else_start_column)) => {
					Some(Box::new(match tokens.tokens.get(0) {
						Some(Token { variant: TokenVariant::IntegerLiteral(value), start_column, .. }) => {
							tokens.take_next_token();
							Statement { variant: StatementVariant::Goto(Some(IntExpression::ConstantValue {
								value: IntValue { value: Rc::new(value.clone().into()) }, start_column: *start_column
							})), column: else_start_column }
						}
						_ => match parse_statement(tokens, false)? {
							Some(statement) => statement,
							None => return Err(ErrorVariant::ExpectedStatementKeyword.at_column(tokens.last_removed_token_end_column)),
						}
					}))
				}
				None => None,
				Some((_, invalid_keyword_start_column)) => return Err(ErrorVariant::ExpectedElseOrStatementEnd.at_column(invalid_keyword_start_column)),
			};
			// Assemble into statement
			Statement {
				column: statement_keyword_start_column,
				variant: StatementVariant::OneLineIf { condition_expression, then_statement, else_statement }
			}
		}
		Keyword::Option => {
			// Should be a root statement
			if !is_root_statement {
				return Err(ErrorVariant::ShouldBeRootStatement.at_column(statement_keyword_start_column));
			}
			// Get comma separated OPTIONs
			let mut option_variables_set = HashSet::new();
			let mut options = Vec::new();
			loop {
				// Get the next two keywords
				let (option_variable, option_variable_start_column) = match tokens.take_keyword() {
					Some(option_variable) => option_variable.clone(),
					None => return Err(ErrorVariant::ExpectedOptionArguments.at_column(tokens.last_removed_token_end_column)),
				};
				if tokens.tokens.is_empty() {
					return Err(ErrorVariant::ExpectedOptionArguments.at_column(tokens.last_removed_token_end_column));
				}
				// Get the option variable/value pair
				let (option_variable_and_value, token_count_to_pop) = match (option_variable, tokens.tokens.get(0).unwrap(), tokens.tokens.get(1)) {
					(Keyword::Angle, Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::Radians), .. }, .. }, _) => (OptionVariableAndValue::Angle(Some(AngleOption::Radians)), 1),
					(Keyword::Angle, Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::Degrees), .. }, .. }, _) => (OptionVariableAndValue::Angle(Some(AngleOption::Degrees)), 1),
					(Keyword::Angle, Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::Gradians), .. }, .. }, _) => (OptionVariableAndValue::Angle(Some(AngleOption::Gradians)), 1),
					(Keyword::Angle, Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::Revolutions), .. }, .. }, _) => (OptionVariableAndValue::Angle(Some(AngleOption::Revolutions)), 1),
					(Keyword::Angle, Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::Default), .. }, .. }, _) => (OptionVariableAndValue::Angle(None), 1),
					(Keyword::Arithmetic, Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::Decimal), .. }, .. }, _) => (OptionVariableAndValue::ArithmeticDecimal, 1),
					(Keyword::Arithmetic, Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::Native), .. }, .. }, _) => (OptionVariableAndValue::ArithmeticNative, 1),
					(Keyword::Arithmetic, Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::Default), .. }, .. }, _) => (OptionVariableAndValue::ArithmeticDefault, 1),
					(
						Keyword::Math,
						Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::Ansi), .. }, .. },
						Some(Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::Full), .. }, .. })
					) => (OptionVariableAndValue::Math(Some(MathOption::AnsiFull)), 2),
					(
						Keyword::Math,
						Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::Ecma), .. }, .. },
						Some(Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::Minimal), .. }, .. })
					) => (OptionVariableAndValue::Math(Some(MathOption::EcmaMinimal)), 2),
					(Keyword::Math, Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::Ieee), .. }, .. }, _) => (OptionVariableAndValue::Math(Some(MathOption::Ieee)), 1),
					(Keyword::Math, Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::Default), .. }, .. }, _) => (OptionVariableAndValue::Math(None), 1),
					(
						Keyword::Machine,
						Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::Ansi), .. }, .. },
						Some(Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::Full), .. }, .. })
					) => (OptionVariableAndValue::Machine(Some(MachineOption::AnsiFull)), 2),
					(
						Keyword::Machine,
						Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::Ecma), .. }, .. },
						Some(Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::Minimal), .. }, .. })
					) => (OptionVariableAndValue::Machine(Some(MachineOption::EcmaMinimal)), 2),
					(Keyword::Machine, Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::C64), .. }, .. }, _) => (OptionVariableAndValue::Machine(Some(MachineOption::C64)), 1),
					(Keyword::Machine, Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::Default), .. }, .. }, _) => (OptionVariableAndValue::Machine(None), 1),
					(Keyword::Base, Token { variant: TokenVariant::IntegerLiteral(value), .. }, _) if value.is_zero() => (OptionVariableAndValue::Base(Some(BaseOption::Zero)), 1),
					(Keyword::Base, Token { variant: TokenVariant::IntegerLiteral(value), .. }, _) if value.is_one() => (OptionVariableAndValue::Base(Some(BaseOption::One)), 1),
					(Keyword::Base, Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::Default), .. }, .. }, _) => (OptionVariableAndValue::Base(None), 1),
					(Keyword::Collate, Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::Native | Keyword::Unicode), .. }, .. }, _)
						=> (OptionVariableAndValue::Collate(Some(CollateOption::Native)), 1),
					(Keyword::Collate, Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::Default), .. }, .. }, _) => (OptionVariableAndValue::Collate(None), 1),
					(Keyword::Collate, Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::Standard | Keyword::Ascii), .. }, .. }, _)
						=> (OptionVariableAndValue::Collate(Some(CollateOption::Standard)), 1),
					_ => return Err(ErrorVariant::InvalidOptionVariableOrValue.at_column(option_variable_start_column)),
				};
				tokens.tokens = &tokens.tokens[token_count_to_pop..];
				// Make sure this OPTION does not have the variable set twice
				let option_variable_discriminant = option_variable_and_value.discriminant();
				if option_variables_set.contains(&option_variable_discriminant) {
					return Err(ErrorVariant::OptionStatementWithTwoOfAnOptionVariable.at_column(option_variable_start_column));
				}
				option_variables_set.insert(option_variable_discriminant);
				// Push the variable/value pair to the OPTION list
				options.push((option_variable_and_value, option_variable_start_column));
				// If there is not a comma after the OPTION, end the statement
				if !matches!(tokens.tokens.first(), Some(Token { variant: TokenVariant::Comma, .. })) {
					break;
				}
				tokens.take_next_token();
			}
			// Assemble into statement
			Statement {
				column: statement_keyword_start_column,
				variant: StatementVariant::Option(options.into()),
			}
		}
		Keyword::Load | Keyword::Save => {
			// Get the filename expression if it exists or return a LOAD statement without an argument if there is no expression
			let filename_expression = match parse_expression(tokens)? {
				Some(filename_expression) => cast_to_string_expression(filename_expression)?,
				None => return Ok(Some(Statement {
					column: statement_keyword_start_column,
					variant: match statement_keyword {
						Keyword::Load => StatementVariant::Load(None),
						Keyword::Save => StatementVariant::Save(None),
						_ => unreachable!(),
					},
				})),
			};
			// Make sure we have at most one argument
			match tokens.tokens.first() {
				Some(Token { variant: TokenVariant::Comma, start_column, .. }) =>
					return Err(ErrorVariant::Unimplemented("LOAD or SAVE statement with more than one argument".into()).at_column(*start_column)),
				_ => {},
			}
			// Assemble into statement
			Statement {
				column: statement_keyword_start_column,
				variant: match statement_keyword {
						Keyword::Load => StatementVariant::Load(Some(filename_expression)),
						Keyword::Save => StatementVariant::Save(Some(filename_expression)),
						_ => unreachable!(),
					},
			}
		}
		// Statements without any arguments
		Keyword::End | Keyword::Stop | Keyword::Return | Keyword::Randomize /*| Keyword::Clear*/ | Keyword::Clr | Keyword::New => Statement {
			column: statement_keyword_start_column,
			variant: match statement_keyword {
				Keyword::End       => StatementVariant::End,
				Keyword::Stop      => StatementVariant::Stop,
				Keyword::Return    => StatementVariant::Return,
				Keyword::Randomize => StatementVariant::Randomize,
				/*Keyword::Clear     => StatementVariant::Clear,*/
				Keyword::Clr       => StatementVariant::Clr,
				Keyword::New       => StatementVariant::New,
				_ => unreachable!()
			},
		},
		// DATA
		Keyword::Data => 'a: {
			if !is_root_statement {
				return Err(ErrorVariant::StatementCannotBeNested.at_column(statement_keyword_start_column));
			}
			let mut data_values = Vec::new();
			// Get the first datum if it exists
			match tokens.tokens.first() {
				Some(Token { variant: TokenVariant::Datum(datum), start_column, .. }) => {
					data_values.push((datum.clone(), *start_column));
					tokens.take_next_token();
				}
				_ => break 'a Statement {
					column: statement_keyword_start_column,
					variant: StatementVariant::Data(data_values.into_boxed_slice()),
				}
			}
			// For each datum after the first datum
			loop {
				// The statement ends if there is not a comma
				if !matches!(tokens.tokens.first(), Some(Token { variant: TokenVariant::Comma, .. })) {
					break 'a Statement {
						column: statement_keyword_start_column,
						variant: StatementVariant::Data(data_values.into_boxed_slice()),
					}
				}
				// Else remove the comma
				tokens.take_next_token();
				// If this is an empty datum, treat it as an empty string
				if matches!(tokens.tokens.first(), Some(Token { variant: TokenVariant::Comma, .. })) {
					data_values.push((Datum { as_string: StringValue::empty(), as_float: None, as_integer: None, as_complex: None }, tokens.last_removed_token_end_column));
					continue;
				}
				// Take datum
				match tokens.take_next_token() {
					Some(Token { variant: TokenVariant::Datum(datum), start_column, .. }) => data_values.push((datum.clone(), *start_column)),
					Some(Token { start_column, .. }) => return Err(ErrorVariant::ExpectedDatum.at_column(*start_column)),
					_ => return Err(ErrorVariant::ExpectedDatum.at_column(tokens.last_removed_token_end_column)),
				}
			}
		}
		Keyword::Read => {
			// If there is a missing recovery statement
			let missing_recovery_statement = if matches!(tokens.tokens.first(), Some(Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::If), .. }, .. })) &&
				matches!(tokens.tokens.iter().nth(1), Some(Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::Missing), .. }, .. }))
			{
				// Take IF and MISSING tokens
				tokens.remove_tokens(2);
				// Expect and take THEN keyword
				match tokens.take_keyword() {
					Some((Keyword::Then, _)) => {}
					None => return Err(ErrorVariant::ExpectedThenKeyword.at_column(tokens.last_removed_token_end_column)),
					Some((_, start_column)) => return Err(ErrorVariant::ExpectedThenKeyword.at_column(start_column)),
				}
				// Parse missing recovery statement
				let result = match tokens.clone().take_keyword() {
					// TODO: EXIT FOR, EXIT DO
					Some((Keyword::Goto | Keyword::Return, _)) => parse_statement(tokens, false)?,
					_ if matches!(tokens.tokens.first(), Some(Token { variant: TokenVariant::IntegerLiteral(..), .. })) => {
						match tokens.tokens.first().unwrap() {
							Token { variant: TokenVariant::IntegerLiteral(goto_line_number), start_column, .. } =>
								Some(Statement {
									variant: StatementVariant::Goto(Some(IntExpression::ConstantValue { value: IntValue::new(Rc::new(goto_line_number.to_bigint().unwrap())), start_column: *start_column })),
									column: *start_column
								}),
							_ => unreachable!(),
						}
					}
					None => return Err(ErrorVariant::InvalidIfMissingThenStatement.at_column(tokens.last_removed_token_end_column)),
					Some((_, start_column)) => return Err(ErrorVariant::InvalidIfMissingThenStatement.at_column(start_column)),
				};
				let result = match result {
					None => return Err(ErrorVariant::InvalidIfMissingThenStatement.at_column(tokens.last_removed_token_end_column)),
					Some(result) => result,
				};
				// Expect colon/semicolon
				match tokens.take_next_token() {
					None => return Err(ErrorVariant::ExpectedColonAfterIfMissingThen.at_column(tokens.last_removed_token_end_column)),
					Some(Token { variant: TokenVariant::Colon | TokenVariant::Semicolon, .. }) => {},
					Some(Token { start_column, .. }) => return Err(ErrorVariant::ExpectedColonAfterIfMissingThen.at_column(*start_column)),
				}
				Some(Box::new(result))
			}
			else {
				None
			};
			// Read l-values to read to
			let mut l_values_to_read_to = Vec::new();
			loop {
				// Get l-value
				let l_value = match parse_l_value(tokens)? {
					None => match l_values_to_read_to.is_empty() {
						true => break,
						false => return Err(ErrorVariant::TrailingComma.at_column(tokens.last_removed_token_end_column)),
					}
					Some(l_value) => l_value,
				};
				l_values_to_read_to.push(l_value);
				// Take comma or end statement
				match tokens.tokens.first() {
					Some(Token { variant: TokenVariant::Comma, .. }) => {
						tokens.take_next_token();
					}
					_ => break,
				}
			}
			Statement {
				column: statement_keyword_start_column,
				variant: StatementVariant::Read { to_do_when_data_missing_statement: missing_recovery_statement, variables: l_values_to_read_to.into_boxed_slice() },
			}
		}
		Keyword::Restore => {
			let restore_to = match parse_expression(tokens)? {
				Some(expression) => Some(cast_to_int_expression(expression)?),
				None => None,
			};
			Statement {
				column: statement_keyword_start_column,
				variant: StatementVariant::Restore(restore_to),
			}
		}
		Keyword::Dim => {
			// Read each array
			let mut arrays = Vec::new();
			loop {
				// Remove and parse name and type
				let (array_name, array_type, is_optional, array_start_column) = match tokens.take_next_token() {
					Some(Token { variant: TokenVariant::Identifier { name, identifier_type, is_optional, .. }, start_column, .. }) =>
						(name.clone(), *identifier_type, *is_optional, *start_column),
					Some(Token { start_column, .. }) => return Err(ErrorVariant::ExpectedIdentifier.at_column(*start_column)),
					None => return Err(ErrorVariant::ExpectedIdentifier.at_column(tokens.last_removed_token_end_column)),
				};
				if is_optional {
					return Err(ErrorVariant::Unimplemented("Optional types".into()).at_column(array_start_column));
				}
				// Remove left parenthesis
				match tokens.take_next_token() {
					Some(Token { variant: TokenVariant::LeftParenthesis, .. }) => {}
					Some(Token { start_column, .. }) => return Err(ErrorVariant::ExpectedLeftParenthesis.at_column(*start_column)),
					None => return Err(ErrorVariant::ExpectedLeftParenthesis.at_column(tokens.last_removed_token_end_column)),
				}
				// Get each dimension
				let mut dimensions = Vec::new();
				loop {
					// Read the first expression of the dimension
					let first_dimension_bound = match parse_expression(tokens)? {
						Some(first_dimension_bound) => cast_to_int_expression(first_dimension_bound)?,
						None => return Err(ErrorVariant::ExpectedExpression.at_column(tokens.last_removed_token_end_column)),
					};
					// If there is no TO keyword, add the dimension to the list and go on to the next dimension
					if !matches!(tokens.tokens.first(), Some(Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::To), .. }, .. })) {
						dimensions.push((None, first_dimension_bound));
						// End the dimension list if there is not a comma
						if !matches!(tokens.tokens.first(), Some(Token { variant: TokenVariant::Comma, .. })) {
							break;
						}
						// If there is a comma take it and go on to parse the next dimension
						tokens.take_next_token();
						continue;
					}
					// Remove the TO keyword
					tokens.take_next_token();
					// Read the second expression of the dimension
					let second_dimension_bound = match parse_expression(tokens)? {
						Some(second_dimension_bound) => cast_to_int_expression(second_dimension_bound)?,
						None => return Err(ErrorVariant::ExpectedExpression.at_column(tokens.last_removed_token_end_column)),
					};
					// Push bound to list
					dimensions.push((Some(first_dimension_bound), second_dimension_bound));
					// End the dimension list if there is not a comma
					if !matches!(tokens.tokens.first(), Some(Token { variant: TokenVariant::Comma, .. })) {
						break;
					}
					// If there is a comma take it and go on to parse the next dimension
					tokens.take_next_token();
				}
				// Remove right parenthesis
				match tokens.take_next_token() {
					Some(Token { variant: TokenVariant::RightParenthesis, .. }) => {}
					Some(Token { start_column, .. }) => return Err(ErrorVariant::ExpectedRightParenthesis.at_column(*start_column)),
					None => return Err(ErrorVariant::ExpectedRightParenthesis.at_column(tokens.last_removed_token_end_column)),
				}
				// Assemble into array
				arrays.push(ArrayDimension {
					array_type: array_type,
					name: array_name,
					dimensions: dimensions.into_boxed_slice(),
					start_column: array_start_column,
				});
				// If the next token is not a comma, this is the end of the array list
				if !matches!(tokens.tokens.first(), Some(Token { variant: TokenVariant::Comma, .. })) {
					break;
				}
				// If there is a comma take it and go on to parse the next array
				tokens.take_next_token();
			}
			// Assemble into statement
			Statement {
				column: statement_keyword_start_column,
				variant: StatementVariant::Dimension(arrays.into_boxed_slice()),
			}
		}
		Keyword::Def => {
			// Get l-value
			let l_value = match parse_l_value(tokens)? {
				Some(l_value) => l_value,
				None => return Err(ErrorVariant::InvalidLValue.at_column(tokens.last_removed_token_end_column)),
			};
			// Expect equal sign
			expect_and_remove_equal_sign(tokens)?;
			// Get expression
			let expression = match parse_expression(tokens)? {
				Some(l_value) => l_value,
				None => return Err(ErrorVariant::ExpectedExpression.at_column(tokens.last_removed_token_end_column)),
			};
			// Assemble into statement
			match l_value {
				AnyTypeLValue::Int(l_value) => Statement {
					column: statement_keyword_start_column,
					variant: StatementVariant::DefInt(
						l_value,
						cast_to_int_expression(expression)?
					),
				},
				AnyTypeLValue::Float(l_value) => Statement {
					column: statement_keyword_start_column,
					variant: StatementVariant::DefFloat(
						l_value,
						cast_to_float_expression(expression)?
					),
				},
				AnyTypeLValue::Complex(l_value) => Statement {
					column: statement_keyword_start_column,
					variant: StatementVariant::DefComplex(
						l_value,
						cast_to_complex_expression(expression)?
					),
				},
				AnyTypeLValue::String(l_value) => Statement {
					column: statement_keyword_start_column,
					variant: StatementVariant::DefString(
						l_value,
						cast_to_string_expression(expression)?
					),
				},
			}
		}
		// ON GOTO/GOSUB
		Keyword::On => {
			// Get the index expression
			let index_expression = match parse_expression(tokens)? {
				Some(index_expression) => cast_to_int_expression(index_expression)?,
				None => return Err(ErrorVariant::ExpectedExpression.at_column(tokens.last_removed_token_end_column)),
			};
			// Get if this is a ON GOTO or a ON GOSUB statement
			let is_gosub = match tokens.take_keyword() {
				None => return Err(ErrorVariant::ExpectedGotoOrGosub.at_column(tokens.last_removed_token_end_column)),
				Some((Keyword::Goto, _)) => false,
				Some((Keyword::Gosub, _)) => true,
				Some((_, start_column)) => return Err(ErrorVariant::ExpectedGotoOrGosub.at_column(start_column)),
			};
			// Get the line numbers
			let mut line_numbers = Vec::new();
			loop {
				// Get line number
				let line_number_expression = match parse_expression(tokens)? {
					Some(line_number_expression) => cast_to_int_expression(line_number_expression)?,
					None => return Err(ErrorVariant::ExpectedExpression.at_column(tokens.last_removed_token_end_column)),
				};
				line_numbers.push(line_number_expression);
				// If the next token is not a comma, this is the end of the line number list
				if !matches!(tokens.tokens.first(), Some(Token { variant: TokenVariant::Comma, .. })) {
					break;
				}
				// If there is a comma take it and go on to parse the next line number
				tokens.take_next_token();
			}
			// Get the else statement if it exists
			let else_statement = match matches!(tokens.tokens.first(), Some(Token { variant: TokenVariant::Identifier { keyword: Some(Keyword::Else), .. }, .. })) {
				true => {
					tokens.take_next_token();
					match parse_statement(tokens, false)? {
						Some(else_statement) => Some(Box::new(else_statement)),
						None => return Err(ErrorVariant::ExpectedStatementKeyword.at_column(tokens.last_removed_token_end_column)),
					}
				}
				false => None,
			};
			// Assemble into statement
			Statement {
				column: statement_keyword_start_column,
				variant: match is_gosub {
					false => StatementVariant::OnGoto {
						index: index_expression,
						line_numbers: line_numbers.into_boxed_slice(),
						else_statement,
					},
					true => StatementVariant::OnGosub {
						index: index_expression,
						line_numbers: line_numbers.into_boxed_slice(),
						else_statement,
					},
				}
			}
		}
		Keyword::Go => return Err(ErrorVariant::SingleGoKeyword.at_column(statement_keyword_start_column)),
		Keyword::Help => {
			let token_after_help_keyword = tokens.take_next_token();
			Statement {
				column: statement_keyword_start_column,
				variant: StatementVariant::Help(token_after_help_keyword.cloned()),
			}
		}
		other_keyword => return Err(ErrorVariant::NotYetImplemented(format!("{} Statement", other_keyword.get_names()[0].0)).at_column(statement_keyword_start_column)),
	}))
}

/// Parses an expression of any type from the start of a list of tokens, removing the tokens it parsed from the token list.
fn parse_expression<'a, 'b>(tokens: &mut Tokens)-> Result<Option<AnyTypeExpression>, Error> {
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
		let expression_primary = match parse_expression_primary(tokens)? {
			Some(expression_primary) => expression_primary,
			None => {
				if !expression_primaries_and_their_unary_operators.is_empty() {
					return Err(ErrorVariant::ExpectedExpressionPrimary.at_column(tokens.last_removed_token_end_column));
				}
				if !unary_operators_before_expression_primary.is_empty() {
					return Err(ErrorVariant::UnaryOperatorsAtEndOfExpression.at_column(tokens.last_removed_token_end_column));
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
		solve_operators_by_precedence(&mut expression_primaries_and_their_unary_operators, &mut operators, Some(binary_operator.get_operator_precedence()))?;
		operators.push((binary_operator, binary_operator_start_column));
	}
	// Return None if this is a blank expression
	if expression_primaries_and_their_unary_operators.is_empty() {
		return Ok(None);
	}
	// Solve
	solve_operators_by_precedence(&mut expression_primaries_and_their_unary_operators, &mut operators, None)?;
	// Return
	debug_assert_eq!(expression_primaries_and_their_unary_operators.len(), 1);
	debug_assert_eq!(operators.len(), 0);
	debug_assert_eq!(expression_primaries_and_their_unary_operators[0].1.len(), 0);
	Ok(Some(expression_primaries_and_their_unary_operators.pop().unwrap().0))
}

/// Takes in a stack of operands and operators, solves operators of lower precedence.
fn solve_operators_by_precedence(expression_stack: &mut Vec<(AnyTypeExpression, Vec<(UnaryOperator, NonZeroUsize)>)>, operator_stack: &mut Vec<(BinaryOperator, NonZeroUsize)>, precedence: Option<u8>)
	-> Result<(), Error>
{
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
			rhs_expression = unary_operator_to_expression(unary_operator.0, unary_operator.1, rhs_expression)?;
		}
		// Pop the lhs operator operand and parse wrap it in its unary operators that have a lower than or equal precedence to the binary operator
		let (mut lhs_expression, mut lhs_unary_operators) = expression_stack.pop().unwrap();
		while !lhs_unary_operators.is_empty() {
			if lhs_unary_operators.last().unwrap().0.get_operator_precedence() > binary_operator.get_operator_precedence() {
				break;
			}
			let unary_operator = lhs_unary_operators.pop().unwrap();
			lhs_expression = unary_operator_to_expression(unary_operator.0, unary_operator.1, lhs_expression)?;
		}
		// Push parsed expressions and unparsed unary operators
		expression_stack.push((binary_operator_to_expression(binary_operator, binary_operator_start_column, lhs_expression, rhs_expression)?, lhs_unary_operators));
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
		*expression = unary_operator_to_expression(
			unary_operator.0, unary_operator.1,
			replace(expression, AnyTypeExpression::Float(FloatExpression::ConstantValue { value: FloatValue::ZERO, start_column: 1.try_into().unwrap() }))
		)?;
	}
	Ok(())
}

/// Parse the parts of an expression that are separated by operators from a list of tokens, removes the parsed tokens. Eg. "2", "(3 + 2)", "f(x)", not "1 + 2".
fn parse_expression_primary<'a, 'b>(tokens: &mut Tokens) -> Result<Option<AnyTypeExpression>, Error> {
	// Get the expression before any string slicings
	let mut expression_primary = {
		// Get the first token or return if there are no more tokens to parse
		let Token { variant: first_token_variant, start_column: first_token_start_column, end_column: _ } = match tokens.tokens.first() {
			Some(first_token) => first_token,
			None => return Ok(None),
		};
		// Parse the expression primary
		match first_token_variant {
			// Literals
			TokenVariant::IntegerLiteral(value) => {
				tokens.take_next_token();
				AnyTypeExpression::Int(IntExpression::ConstantValue { value: IntValue { value: Rc::new(value.clone().into()) }, start_column: *first_token_start_column })
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
				let sub_expression = match parse_expression(tokens)? {
					None => return Err(ErrorVariant::ExpectedExpression.at_column(tokens.last_removed_token_end_column)),
					Some(sub_expression) => sub_expression,
				};
				// Make sure that there is a closing parenthesis after the sub-expression
				match tokens.take_next_token() {
					Some(Token { variant: TokenVariant::RightParenthesis, .. }) => {}
					Some(Token { variant: _, start_column, .. }) => return Err(ErrorVariant::ExpectedRightParenthesis.at_column(*start_column)),
					None => return Err(ErrorVariant::ExpectedRightParenthesis.at_column(tokens.last_removed_token_end_column)),
				}
				// Return
				sub_expression
			}
			// There should not be operators
			TokenVariant::Operator(..) => return Err(ErrorVariant::UnexpectedOperator.at_column(*first_token_start_column)),
			// Identifiers
			TokenVariant::Identifier { .. } => {
				match parse_l_value(tokens)? {
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
				return Err(ErrorVariant::NotYetImplemented("Question mark not as type".into()).at_column(*first_token_start_column)),
			TokenVariant::Datum(_) => unreachable!(),
		}
	};
	// Parse slicings
	loop {
		if let AnyTypeExpression::String(expression_primary_before_slicing) = &mut expression_primary {
			// Get left parenthesis or break
			let slice_operator_start_column = match tokens.tokens.get(0) {
				Some(Token { variant: TokenVariant::LeftParenthesis, start_column, .. }) => start_column,
				_ => break,
			};
			tokens.take_next_token();
			// Get slice start
			let slice_start_index_expression = Box::new(cast_to_int_expression(parse_expression(tokens)?.unwrap())?);
			// Expect colon
			match tokens.tokens.get(0) {
				Some(Token { variant: TokenVariant::Colon, .. }) => {},
				Some(Token { start_column, .. }) => return Err(ErrorVariant::ExpectedColonAfterIfMissingThen.at_column(*start_column)),
				None => return Err(ErrorVariant::ExpectedColonAfterIfMissingThen.at_column(tokens.last_removed_token_end_column)),
			};
			tokens.take_next_token();
			// Get slice end
			let slice_end_index_expression = Box::new(cast_to_int_expression(parse_expression(tokens)?.unwrap())?);
			// Expect closing parenthesis
			match tokens.tokens.get(0) {
				Some(Token { variant: TokenVariant::RightParenthesis, .. }) => {},
				Some(Token { start_column, .. }) => return Err(ErrorVariant::ExpectedRightParenthesis.at_column(*start_column)),
				None => return Err(ErrorVariant::ExpectedRightParenthesis.at_column(tokens.last_removed_token_end_column)),
			};
			tokens.take_next_token();
			// Replace expression with slicing expression containing the expression
			expression_primary = AnyTypeExpression::String(StringExpression::StringSlicing {
				to_slice_expression: Box::new(expression_primary_before_slicing.clone()),
				range_start_expression: slice_start_index_expression,
				range_end_expression: slice_end_index_expression,
				start_column: *slice_operator_start_column,
			})
		}
		else {
			break;
		}
	}
	// Return
	Ok(Some(expression_primary))
}

/// Will remove a equal sign from the start of `tokens`. Will return an error if it is not found and will not remove the token from the start.
fn expect_and_remove_equal_sign(tokens: &mut Tokens) -> Result<(), Error> {
	match tokens.take_next_token() {
		Some(Token { variant: TokenVariant::Operator(Some(BinaryOperator::Equal), _), ..}) => Ok(()),
		Some(token) => Err(ErrorVariant::ExpectedEqualSign.at_column(token.start_column)),
		None => Err(ErrorVariant::ExpectedEqualSign.at_column(tokens.last_removed_token_end_column)),
	}
}

/// Parses a l-value of any type for a list of tokens, removes the parsed tokens from the list.
fn parse_l_value<'a, 'b>(tokens: &mut Tokens) -> Result<Option<AnyTypeLValue>, Error> {
	// Get the part of the l-value before any slicing
	let mut l_value_expression = 'a: {
		// Get the first token or return if there are no more tokens to parse
		let Token { variant: first_token_variant, start_column: first_token_start_column, end_column: first_token_end_column } = match tokens.tokens.first() {
			Some(first_token) => first_token,
			None => return Ok(None),
		};
		let (_keyword, is_reserved) = match first_token_variant {
			TokenVariant::Identifier { keyword, is_reserved_keyword, .. } => (*keyword, *is_reserved_keyword),
			_ => return Err(ErrorVariant::InvalidLValue.at_column(*first_token_start_column)),
		};
		// Return if the identifier name is a reserved keyword
		if is_reserved {
			return Ok(None);
		}
		// Get identifier name
		let Token { variant: token_after_fn_variant, start_column: token_after_fn_start_column, end_column: _ } = match tokens.tokens.get(0) {
			Some(token_after_fn) => token_after_fn,
			None => return Err(ErrorVariant::ExpectedFunctionNameAfterFn.at_column(*first_token_end_column)),
		};
		let (identifier_name, identifier_type, identifier_is_optional, supplied_function_identifier) = match token_after_fn_variant {
			TokenVariant::Identifier { name, identifier_type, is_optional, supplied_function, .. } => (name, identifier_type, is_optional, *supplied_function),
			_ => return Err(ErrorVariant::ExpectedFunctionNameAfterFn.at_column(*token_after_fn_start_column)),
		};
		if *identifier_is_optional {
			return Err(ErrorVariant::Unimplemented("Optional functions".into()).at_column(*token_after_fn_start_column));
		}
		tokens.take_next_token();
		// Get if any parentheses after this identifier are part of a string slicing operator
		let mut has_slicing_next = false;
		if matches!(tokens.tokens.get(0), Some(Token { variant: TokenVariant::LeftParenthesis, .. })) {
			let mut temp_tokens = tokens.clone();
			temp_tokens.take_next_token();
			if temp_tokens.has_colon_before_closing_parenthesis() {
				has_slicing_next = true;
			}
		}
		// Return if there is not a left parenthesis after the identifier or if said parenthesis is part of a string slicing operator
		match tokens.tokens.get(0) {
			Some(Token { variant: TokenVariant::LeftParenthesis, .. }) if !has_slicing_next => {},
			_ => break 'a match identifier_type {
				IdentifierType::Integer => AnyTypeLValue::Int(IntLValue {
					name: identifier_name.clone(), arguments: Box::default(), has_parentheses: false, start_column: *first_token_start_column,
					supplied_function: match supplied_function_identifier {
						Some(supplied_function_identifier) => parse_int_supplied_function(supplied_function_identifier, Default::default()),
						None => None,
					}
				}),
				IdentifierType::UnmarkedOrFloat => AnyTypeLValue::Float(FloatLValue {
					name: identifier_name.clone(), arguments: Box::default(), has_parentheses: false, start_column: *first_token_start_column,
					supplied_function: match supplied_function_identifier {
						Some(supplied_function_identifier) => parse_float_supplied_function(supplied_function_identifier, Default::default()),
						None => None,
					}
				}),
				IdentifierType::ComplexNumber => AnyTypeLValue::Complex(ComplexLValue {
					name: identifier_name.clone(), arguments: Box::default(), has_parentheses: false, start_column: *first_token_start_column,
					supplied_function: match supplied_function_identifier {
						Some(supplied_function_identifier) => parse_complex_supplied_function(supplied_function_identifier, Default::default()),
						None => None,
					}
				}),
				IdentifierType::String => AnyTypeLValue::String(StringLValue {
					name: identifier_name.clone(), arguments: Box::default(), has_parentheses: false, start_column: *first_token_start_column, string_slicings: Box::default(),
					supplied_function: match supplied_function_identifier {
						Some(supplied_function_identifier) => parse_string_supplied_function(supplied_function_identifier, Default::default()),
						None => None,
					}
				}),
			},
		}
		// Skip opening parenthesis
		tokens.take_next_token();
		// Get arguments
		let mut arguments = Vec::new();
		// Make sure there is not a leading comma
		match tokens.tokens.get(0) {
			Some(Token { variant: TokenVariant::Comma, start_column, .. }) => return Err(ErrorVariant::LeadingCommaInFunctionArguments.at_column(*start_column)),
			_ => {},
		}
		// Parse each argument
		'b: loop {
			// If we reach a non-expression token
			match tokens.tokens.get(0) {
				// Comma
				Some(Token { variant: TokenVariant::Comma, start_column, ..}) => return Err(ErrorVariant::TwoSequentialCommasTogetherInFunctionArguments.at_column(*start_column)),
				// Right parenthesis
				Some(Token { variant: TokenVariant::RightParenthesis, .. }) if arguments.len() == 0 => {
					tokens.take_next_token();
					break 'b;
				}
				Some(Token { variant: TokenVariant::RightParenthesis, start_column, .. }) => return Err(ErrorVariant::TrailingComma.at_column(*start_column)),
				// Colon / semicolon
				Some(Token { variant: TokenVariant::Colon | TokenVariant::Semicolon, start_column, .. }) =>
					return Err(ErrorVariant::InvalidSeparatorInFunctionArguments.at_column(*start_column)),
				// End of statement without closing parenthesis
					None => return Err(ErrorVariant::ExpectedRightParenthesis.at_column(tokens.last_removed_token_end_column)),
				_ => {}
			}
			// Parse argument
			let argument_expression = parse_expression(tokens)?.unwrap();
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
				// End of statement without closing parenthesis
				None => return Err(ErrorVariant::ExpectedRightParenthesis.at_column(tokens.last_removed_token_end_column)),
				_ => {}
			};
		}
		// Return
		break 'a match identifier_type {
			IdentifierType::Integer => AnyTypeLValue::Int(IntLValue {
				supplied_function: match supplied_function_identifier {
					Some(supplied_function_identifier) => parse_int_supplied_function(supplied_function_identifier, &arguments),
					None => None,
				},
				name: identifier_name.clone(), arguments: arguments.into(), has_parentheses: true, start_column: *first_token_start_column
			}),
			IdentifierType::UnmarkedOrFloat => AnyTypeLValue::Float(FloatLValue {
				supplied_function: match supplied_function_identifier {
					Some(supplied_function_identifier) => parse_float_supplied_function(supplied_function_identifier, &arguments),
					None => None,
				},
				name: identifier_name.clone(), arguments: arguments.into(), has_parentheses: true, start_column: *first_token_start_column,
			}),
			IdentifierType::ComplexNumber => AnyTypeLValue::Complex(ComplexLValue {
				supplied_function: match supplied_function_identifier {
					Some(supplied_function_identifier) => parse_complex_supplied_function(supplied_function_identifier, &arguments),
					None => None,
				},
				name: identifier_name.clone(), arguments: arguments.into(), has_parentheses: true, start_column: *first_token_start_column
			}),
			IdentifierType::String => AnyTypeLValue::String(StringLValue {
				supplied_function: match supplied_function_identifier {
					Some(supplied_function_identifier) => parse_string_supplied_function(supplied_function_identifier, &arguments),
					None => None,
				},
				name: identifier_name.clone(), arguments: arguments.into(), has_parentheses: true, start_column: *first_token_start_column, string_slicings: Box::default(),
			}),
		}
	};
	// Parse slicings
	if let AnyTypeLValue::String(l_value_expression) = &mut l_value_expression {
		let mut string_slicings = Vec::new();
		loop {
			// Get left parenthesis or break
			let slice_operator_start_column = match tokens.tokens.get(0) {
				Some(Token { variant: TokenVariant::LeftParenthesis, start_column, .. }) => start_column,
				_ => break,
			};
			tokens.take_next_token();
			// Get slice start
			let slice_start_index_expression = Box::new(cast_to_int_expression(parse_expression(tokens)?.unwrap())?);
			// Expect colon
			match tokens.tokens.get(0) {
				Some(Token { variant: TokenVariant::Colon, .. }) => {},
				Some(Token { start_column, .. }) => return Err(ErrorVariant::ExpectedColonAfterIfMissingThen.at_column(*start_column)),
				None => return Err(ErrorVariant::ExpectedColonAfterIfMissingThen.at_column(tokens.last_removed_token_end_column)),
			};
			tokens.take_next_token();
			// Get slice end
			let slice_end_index_expression = Box::new(cast_to_int_expression(parse_expression(tokens)?.unwrap())?);
			// Expect closing parenthesis
			match tokens.tokens.get(0) {
				Some(Token { variant: TokenVariant::RightParenthesis, .. }) => {},
				Some(Token { start_column, .. }) => return Err(ErrorVariant::ExpectedRightParenthesis.at_column(*start_column)),
				None => return Err(ErrorVariant::ExpectedRightParenthesis.at_column(tokens.last_removed_token_end_column)),
			};
			tokens.take_next_token();
			// Push
			string_slicings.push((slice_start_index_expression, slice_end_index_expression, *slice_operator_start_column));
		}
		l_value_expression.string_slicings = string_slicings.into_boxed_slice();
	}
	// Return
	Ok(Some(l_value_expression))
}

fn parse_float_supplied_function(identifier: SuppliedFunctionIdentifier, arguments: &[AnyTypeExpression]) -> Option<FloatSuppliedFunction> {
	let argument_count = arguments.len();
	Some(match (identifier, argument_count) {
		(SuppliedFunctionIdentifier::Tab, 1) => FloatSuppliedFunction::Tab,

		(SuppliedFunctionIdentifier::Pi,     0) => FloatSuppliedFunction::Pi,
		(SuppliedFunctionIdentifier::E,      0) => FloatSuppliedFunction::E,
		(SuppliedFunctionIdentifier::Tau,    0) => FloatSuppliedFunction::Tau,
		(SuppliedFunctionIdentifier::Phi,    0) => FloatSuppliedFunction::Phi,
		(SuppliedFunctionIdentifier::EGamma, 0) => FloatSuppliedFunction::EGamma,
		(SuppliedFunctionIdentifier::MaxNum, 0) => FloatSuppliedFunction::MaxNum,
		(SuppliedFunctionIdentifier::NaN,    0) => FloatSuppliedFunction::NaN,
		(SuppliedFunctionIdentifier::Inf,    0) => FloatSuppliedFunction::Inf,
		(SuppliedFunctionIdentifier::NInf,   0) => FloatSuppliedFunction::NInf,
		(SuppliedFunctionIdentifier::True,   0) => FloatSuppliedFunction::True,
		(SuppliedFunctionIdentifier::False,  0) => FloatSuppliedFunction::False,

		(SuppliedFunctionIdentifier::Time,   0) => FloatSuppliedFunction::Time,
		(SuppliedFunctionIdentifier::Date,   0) => FloatSuppliedFunction::Date,
		(SuppliedFunctionIdentifier::Second, 0) => FloatSuppliedFunction::Second,
		(SuppliedFunctionIdentifier::Minute, 0) => FloatSuppliedFunction::Minute,
		(SuppliedFunctionIdentifier::Hour,   0) => FloatSuppliedFunction::Hour,
		(SuppliedFunctionIdentifier::Day,    0) => FloatSuppliedFunction::Day,
		(SuppliedFunctionIdentifier::Month,  0) => FloatSuppliedFunction::Month,
		(SuppliedFunctionIdentifier::Year,   0) => FloatSuppliedFunction::Year,

		(SuppliedFunctionIdentifier::Floor | SuppliedFunctionIdentifier::Int, 1) => FloatSuppliedFunction::Floor,
		(SuppliedFunctionIdentifier::Ip,                                      1) => FloatSuppliedFunction::Ip,
		(SuppliedFunctionIdentifier::Ceil,                                    1) => FloatSuppliedFunction::Ceil,
		(SuppliedFunctionIdentifier::Truncate,                                2) => FloatSuppliedFunction::Truncate,
		(SuppliedFunctionIdentifier::Round,                                   2) => FloatSuppliedFunction::Round,

		(SuppliedFunctionIdentifier::Abs,       1) if !arguments[0].is_complex() => FloatSuppliedFunction::Abs,
		(SuppliedFunctionIdentifier::Sgn,       1)                               => FloatSuppliedFunction::Signum,
		(SuppliedFunctionIdentifier::Fp,        1)                               => FloatSuppliedFunction::Fp,
		(SuppliedFunctionIdentifier::Deg,       1)                               => FloatSuppliedFunction::Deg,
		(SuppliedFunctionIdentifier::Rad,       1)                               => FloatSuppliedFunction::Rad,
		(SuppliedFunctionIdentifier::Eps,       1)                               => FloatSuppliedFunction::Eps,
		(SuppliedFunctionIdentifier::Sqr,       1)                               => FloatSuppliedFunction::Sqrt,
		(SuppliedFunctionIdentifier::Exp,       1)                               => FloatSuppliedFunction::Exp,
		(SuppliedFunctionIdentifier::Mod,       2)                               => FloatSuppliedFunction::Modulo,
		(SuppliedFunctionIdentifier::Remainder, 2)                               => FloatSuppliedFunction::Remainder,
		(SuppliedFunctionIdentifier::Min,       n) if n > 0               => FloatSuppliedFunction::Min,
		(SuppliedFunctionIdentifier::Max,       n) if n > 0               => FloatSuppliedFunction::Max,
		(SuppliedFunctionIdentifier::Rnd,       0)                               => FloatSuppliedFunction::Rad,
		(SuppliedFunctionIdentifier::Rnd,       1)                               => FloatSuppliedFunction::CommodoreRandom,

		(SuppliedFunctionIdentifier::Log | SuppliedFunctionIdentifier::Ln, 1) => FloatSuppliedFunction::LogE,
		(SuppliedFunctionIdentifier::Log10, 1)                                => FloatSuppliedFunction::Log10,
		(SuppliedFunctionIdentifier::Log2,  1)                                => FloatSuppliedFunction::Log2,
		(SuppliedFunctionIdentifier::Log,   2)                                => FloatSuppliedFunction::LogN,

		(SuppliedFunctionIdentifier::Sin,   1) => FloatSuppliedFunction::Sin,
		(SuppliedFunctionIdentifier::Cos,   1) => FloatSuppliedFunction::Cos,
		(SuppliedFunctionIdentifier::Tan,   1) => FloatSuppliedFunction::Tan,
		(SuppliedFunctionIdentifier::Cot,   1) => FloatSuppliedFunction::Cot,
		(SuppliedFunctionIdentifier::Sec,   1) => FloatSuppliedFunction::Sec,
		(SuppliedFunctionIdentifier::Csc,   1) => FloatSuppliedFunction::Csc,
		(SuppliedFunctionIdentifier::Asin,  1) => FloatSuppliedFunction::Asin,
		(SuppliedFunctionIdentifier::Acos,  1) => FloatSuppliedFunction::Acos,
		(SuppliedFunctionIdentifier::Atan,  1) => FloatSuppliedFunction::Atan,
		(SuppliedFunctionIdentifier::Acot,  1) => FloatSuppliedFunction::Acot,
		(SuppliedFunctionIdentifier::Asec,  1) => FloatSuppliedFunction::Asec,
		(SuppliedFunctionIdentifier::Acsc,  1) => FloatSuppliedFunction::Acsc,
		(SuppliedFunctionIdentifier::Angle, 2) => FloatSuppliedFunction::Angle,
		(SuppliedFunctionIdentifier::Atan2, 2) => FloatSuppliedFunction::Atan2,

		(SuppliedFunctionIdentifier::Sinh,  1) => FloatSuppliedFunction::Sinh,
		(SuppliedFunctionIdentifier::Cosh,  1) => FloatSuppliedFunction::Cosh,
		(SuppliedFunctionIdentifier::Tanh,  1) => FloatSuppliedFunction::Tanh,
		(SuppliedFunctionIdentifier::Coth,  1) => FloatSuppliedFunction::Coth,
		(SuppliedFunctionIdentifier::Sech,  1) => FloatSuppliedFunction::Sech,
		(SuppliedFunctionIdentifier::Csch,  1) => FloatSuppliedFunction::Csch,
		(SuppliedFunctionIdentifier::Asinh, 1) => FloatSuppliedFunction::Asinh,
		(SuppliedFunctionIdentifier::Acosh, 1) => FloatSuppliedFunction::Acosh,
		(SuppliedFunctionIdentifier::Atanh, 1) => FloatSuppliedFunction::Atanh,
		(SuppliedFunctionIdentifier::Acoth, 1) => FloatSuppliedFunction::Acoth,
		(SuppliedFunctionIdentifier::Asech, 1) => FloatSuppliedFunction::Asech,
		(SuppliedFunctionIdentifier::Acsch, 1) => FloatSuppliedFunction::Acsch,

		(SuppliedFunctionIdentifier::Real, 1)                              => FloatSuppliedFunction::Real,
		(SuppliedFunctionIdentifier::Imag, 1)                              => FloatSuppliedFunction::Imag,
		(SuppliedFunctionIdentifier::Arg,  1)                              => FloatSuppliedFunction::Arg,
		(SuppliedFunctionIdentifier::Abs,  1) if arguments[0].is_complex() => FloatSuppliedFunction::AbsComplex,

		(SuppliedFunctionIdentifier::Len,    1) => FloatSuppliedFunction::Len,
		(SuppliedFunctionIdentifier::Ord,    1) => FloatSuppliedFunction::Ord,
		(SuppliedFunctionIdentifier::Asc,    1) => FloatSuppliedFunction::Asc,
		(SuppliedFunctionIdentifier::Val,    1) => FloatSuppliedFunction::Val,
		(SuppliedFunctionIdentifier::MaxLen, 1) => FloatSuppliedFunction::MaxLen,
		(SuppliedFunctionIdentifier::Pos,    1) => FloatSuppliedFunction::Pos,
		_ => return None,
	})
}

pub fn get_float_supplied_functions(identifier: SuppliedFunctionIdentifier) -> &'static [FloatSuppliedFunction] {
	match identifier {
		SuppliedFunctionIdentifier::Tab => &[FloatSuppliedFunction::Tab],

		SuppliedFunctionIdentifier::Pi     => &[FloatSuppliedFunction::Pi],
		SuppliedFunctionIdentifier::E      => &[FloatSuppliedFunction::E],
		SuppliedFunctionIdentifier::Tau    => &[FloatSuppliedFunction::Tau],
		SuppliedFunctionIdentifier::Phi    => &[FloatSuppliedFunction::Phi],
		SuppliedFunctionIdentifier::EGamma => &[FloatSuppliedFunction::EGamma],
		SuppliedFunctionIdentifier::MaxNum => &[FloatSuppliedFunction::MaxNum],
		SuppliedFunctionIdentifier::NaN    => &[FloatSuppliedFunction::NaN],
		SuppliedFunctionIdentifier::Inf    => &[FloatSuppliedFunction::Inf],
		SuppliedFunctionIdentifier::NInf   => &[FloatSuppliedFunction::NInf],
		SuppliedFunctionIdentifier::True   => &[FloatSuppliedFunction::True],
		SuppliedFunctionIdentifier::False  => &[FloatSuppliedFunction::False],

		SuppliedFunctionIdentifier::Time   => &[FloatSuppliedFunction::Time],
		SuppliedFunctionIdentifier::Date   => &[FloatSuppliedFunction::Date],
		SuppliedFunctionIdentifier::Second => &[FloatSuppliedFunction::Second],
		SuppliedFunctionIdentifier::Minute => &[FloatSuppliedFunction::Minute],
		SuppliedFunctionIdentifier::Hour   => &[FloatSuppliedFunction::Hour],
		SuppliedFunctionIdentifier::Day    => &[FloatSuppliedFunction::Day],
		SuppliedFunctionIdentifier::Month  => &[FloatSuppliedFunction::Month],
		SuppliedFunctionIdentifier::Year   => &[FloatSuppliedFunction::Year],

		SuppliedFunctionIdentifier::Floor | SuppliedFunctionIdentifier::Int => &[FloatSuppliedFunction::Floor],
		SuppliedFunctionIdentifier::Ip                                      => &[FloatSuppliedFunction::Ip],
		SuppliedFunctionIdentifier::Ceil                                    => &[FloatSuppliedFunction::Ceil],
		SuppliedFunctionIdentifier::Truncate                                => &[FloatSuppliedFunction::Truncate],
		SuppliedFunctionIdentifier::Round                                   => &[FloatSuppliedFunction::Round],

		SuppliedFunctionIdentifier::Abs       => &[FloatSuppliedFunction::Abs, FloatSuppliedFunction::AbsComplex],
		SuppliedFunctionIdentifier::Sgn       => &[FloatSuppliedFunction::Signum],
		SuppliedFunctionIdentifier::Fp        => &[FloatSuppliedFunction::Fp],
		SuppliedFunctionIdentifier::Deg       => &[FloatSuppliedFunction::Deg],
		SuppliedFunctionIdentifier::Rad       => &[FloatSuppliedFunction::Rad],
		SuppliedFunctionIdentifier::Eps       => &[FloatSuppliedFunction::Eps],
		SuppliedFunctionIdentifier::Sqr       => &[FloatSuppliedFunction::Sqrt],
		SuppliedFunctionIdentifier::Exp       => &[FloatSuppliedFunction::Exp],
		SuppliedFunctionIdentifier::Mod       => &[FloatSuppliedFunction::Modulo],
		SuppliedFunctionIdentifier::Remainder => &[FloatSuppliedFunction::Remainder],
		SuppliedFunctionIdentifier::Min       => &[FloatSuppliedFunction::Min],
		SuppliedFunctionIdentifier::Max       => &[FloatSuppliedFunction::Max],
		SuppliedFunctionIdentifier::Rnd       => &[FloatSuppliedFunction::Rad, FloatSuppliedFunction::CommodoreRandom],

		SuppliedFunctionIdentifier::Ln    => &[FloatSuppliedFunction::LogE],
		SuppliedFunctionIdentifier::Log10 => &[FloatSuppliedFunction::Log10],
		SuppliedFunctionIdentifier::Log2  => &[FloatSuppliedFunction::Log2],
		SuppliedFunctionIdentifier::Log   => &[FloatSuppliedFunction::LogN, FloatSuppliedFunction::LogE],

		SuppliedFunctionIdentifier::Sin   => &[FloatSuppliedFunction::Sin],
		SuppliedFunctionIdentifier::Cos   => &[FloatSuppliedFunction::Cos],
		SuppliedFunctionIdentifier::Tan   => &[FloatSuppliedFunction::Tan],
		SuppliedFunctionIdentifier::Cot   => &[FloatSuppliedFunction::Cot],
		SuppliedFunctionIdentifier::Sec   => &[FloatSuppliedFunction::Sec],
		SuppliedFunctionIdentifier::Csc   => &[FloatSuppliedFunction::Csc],
		SuppliedFunctionIdentifier::Asin  => &[FloatSuppliedFunction::Asin],
		SuppliedFunctionIdentifier::Acos  => &[FloatSuppliedFunction::Acos],
		SuppliedFunctionIdentifier::Atan  => &[FloatSuppliedFunction::Atan],
		SuppliedFunctionIdentifier::Acot  => &[FloatSuppliedFunction::Acot],
		SuppliedFunctionIdentifier::Asec  => &[FloatSuppliedFunction::Asec],
		SuppliedFunctionIdentifier::Acsc  => &[FloatSuppliedFunction::Acsc],
		SuppliedFunctionIdentifier::Angle => &[FloatSuppliedFunction::Angle],
		SuppliedFunctionIdentifier::Atan2 => &[FloatSuppliedFunction::Atan2],

		SuppliedFunctionIdentifier::Sinh  => &[FloatSuppliedFunction::Sinh],
		SuppliedFunctionIdentifier::Cosh  => &[FloatSuppliedFunction::Cosh],
		SuppliedFunctionIdentifier::Tanh  => &[FloatSuppliedFunction::Tanh],
		SuppliedFunctionIdentifier::Coth  => &[FloatSuppliedFunction::Coth],
		SuppliedFunctionIdentifier::Sech  => &[FloatSuppliedFunction::Sech],
		SuppliedFunctionIdentifier::Csch  => &[FloatSuppliedFunction::Csch],
		SuppliedFunctionIdentifier::Asinh => &[FloatSuppliedFunction::Asinh],
		SuppliedFunctionIdentifier::Acosh => &[FloatSuppliedFunction::Acosh],
		SuppliedFunctionIdentifier::Atanh => &[FloatSuppliedFunction::Atanh],
		SuppliedFunctionIdentifier::Acoth => &[FloatSuppliedFunction::Acoth],
		SuppliedFunctionIdentifier::Asech => &[FloatSuppliedFunction::Asech],
		SuppliedFunctionIdentifier::Acsch => &[FloatSuppliedFunction::Acsch],

		SuppliedFunctionIdentifier::Real => &[FloatSuppliedFunction::Real],
		SuppliedFunctionIdentifier::Imag => &[FloatSuppliedFunction::Imag],
		SuppliedFunctionIdentifier::Arg  => &[FloatSuppliedFunction::Arg],

		SuppliedFunctionIdentifier::Len    => &[FloatSuppliedFunction::Len],
		SuppliedFunctionIdentifier::Ord    => &[FloatSuppliedFunction::Ord],
		SuppliedFunctionIdentifier::Asc    => &[FloatSuppliedFunction::Asc],
		SuppliedFunctionIdentifier::Val    => &[FloatSuppliedFunction::Val],
		SuppliedFunctionIdentifier::MaxLen => &[FloatSuppliedFunction::MaxLen],
		SuppliedFunctionIdentifier::Pos    => &[FloatSuppliedFunction::Pos],

		_ => &[],
	}
}

fn parse_int_supplied_function(identifier: SuppliedFunctionIdentifier, arguments: &[AnyTypeExpression]) -> Option<IntSuppliedFunction> {
	let argument_count = arguments.len();
	Some(match (identifier, argument_count) {
		// Constant
		(SuppliedFunctionIdentifier::True,  0) => IntSuppliedFunction::True,
		(SuppliedFunctionIdentifier::False, 0) => IntSuppliedFunction::False,
		// Time
		(SuppliedFunctionIdentifier::Time,   0) => IntSuppliedFunction::Time,
		(SuppliedFunctionIdentifier::Date,   0) => IntSuppliedFunction::Date,
		(SuppliedFunctionIdentifier::Second, 0) => IntSuppliedFunction::Second,
		(SuppliedFunctionIdentifier::Minute, 0) => IntSuppliedFunction::Minute,
		(SuppliedFunctionIdentifier::Hour,   0) => IntSuppliedFunction::Hour,
		(SuppliedFunctionIdentifier::Day,    0) => IntSuppliedFunction::Day,
		(SuppliedFunctionIdentifier::Month,  0) => IntSuppliedFunction::Month,
		(SuppliedFunctionIdentifier::Year,   0) => IntSuppliedFunction::Year,
		// Other Math
		(SuppliedFunctionIdentifier::Sqr,   1)                 => IntSuppliedFunction::Sqr,
		(SuppliedFunctionIdentifier::Abs,   1)                 => IntSuppliedFunction::Abs,
		(SuppliedFunctionIdentifier::Log2,  1)                 => IntSuppliedFunction::Log2,
		(SuppliedFunctionIdentifier::Log10, 1)                 => IntSuppliedFunction::Log10,
		(SuppliedFunctionIdentifier::Xor,   n) if n > 1 => IntSuppliedFunction::Xor,
		(SuppliedFunctionIdentifier::Min,   n) if n > 1 => IntSuppliedFunction::Min,
		(SuppliedFunctionIdentifier::Max,   n) if n > 1 => IntSuppliedFunction::Max,
		(SuppliedFunctionIdentifier::Sgn,   1)                 => IntSuppliedFunction::Sgn,
		// Rounding
		(SuppliedFunctionIdentifier::Int | SuppliedFunctionIdentifier::Floor, 1) => IntSuppliedFunction::Floor,
		(SuppliedFunctionIdentifier::Ceil,                                    1) => IntSuppliedFunction::Ceil,
		(SuppliedFunctionIdentifier::Ip,                                      1) => IntSuppliedFunction::Ip,
		// String to Int
		(SuppliedFunctionIdentifier::Len, 1) => IntSuppliedFunction::Len,

		_ => return None,
	})
}

pub fn get_int_supplied_functions(identifier: SuppliedFunctionIdentifier) -> &'static [IntSuppliedFunction] {
	match identifier {
		SuppliedFunctionIdentifier::True  => &[IntSuppliedFunction::True],
		SuppliedFunctionIdentifier::False => &[IntSuppliedFunction::False],
		
		SuppliedFunctionIdentifier::Time   => &[IntSuppliedFunction::Time],
		SuppliedFunctionIdentifier::Date   => &[IntSuppliedFunction::Date],
		SuppliedFunctionIdentifier::Second => &[IntSuppliedFunction::Second],
		SuppliedFunctionIdentifier::Minute => &[IntSuppliedFunction::Minute],
		SuppliedFunctionIdentifier::Hour   => &[IntSuppliedFunction::Hour],
		SuppliedFunctionIdentifier::Day    => &[IntSuppliedFunction::Day],
		SuppliedFunctionIdentifier::Month  => &[IntSuppliedFunction::Month],
		SuppliedFunctionIdentifier::Year   => &[IntSuppliedFunction::Year],
		
		SuppliedFunctionIdentifier::Sqr   => &[IntSuppliedFunction::Sqr],
		SuppliedFunctionIdentifier::Abs   => &[IntSuppliedFunction::Abs],
		SuppliedFunctionIdentifier::Log2  => &[IntSuppliedFunction::Log2],
		SuppliedFunctionIdentifier::Log10 => &[IntSuppliedFunction::Log10],
		SuppliedFunctionIdentifier::Xor   => &[IntSuppliedFunction::Xor],
		SuppliedFunctionIdentifier::Min   => &[IntSuppliedFunction::Min],
		SuppliedFunctionIdentifier::Max   => &[IntSuppliedFunction::Max],
		SuppliedFunctionIdentifier::Sgn   => &[IntSuppliedFunction::Sgn],
		
		SuppliedFunctionIdentifier::Int | SuppliedFunctionIdentifier::Floor => &[IntSuppliedFunction::Floor],
		SuppliedFunctionIdentifier::Ceil                                    => &[IntSuppliedFunction::Ceil],
		SuppliedFunctionIdentifier::Ip                                      => &[IntSuppliedFunction::Ip],
		
		SuppliedFunctionIdentifier::Len => &[IntSuppliedFunction::Len],

		_ => return &[],
	}
}

fn parse_complex_supplied_function(identifier: SuppliedFunctionIdentifier, arguments: &[AnyTypeExpression]) -> Option<ComplexSuppliedFunction> {
	let argument_count = arguments.len();
	Some(match (identifier, argument_count) {
		// Constants
		(SuppliedFunctionIdentifier::I, 0) => ComplexSuppliedFunction::I,
		// Other Math
		(SuppliedFunctionIdentifier::Sqr,  1) => ComplexSuppliedFunction::Sqr,
		(SuppliedFunctionIdentifier::Exp,  1) => ComplexSuppliedFunction::Exp,
		(SuppliedFunctionIdentifier::Conj, 1) => ComplexSuppliedFunction::Conj,
		// Logarithm
		(SuppliedFunctionIdentifier::Log | SuppliedFunctionIdentifier::Ln, 1) => ComplexSuppliedFunction::LogE,
		(SuppliedFunctionIdentifier::Log2,                                 1) => ComplexSuppliedFunction::Log2,
		(SuppliedFunctionIdentifier::Log10,                                1) => ComplexSuppliedFunction::Log10,
		(SuppliedFunctionIdentifier::Log,                                  2) => ComplexSuppliedFunction::LogN,
		// Trigonometry
		(SuppliedFunctionIdentifier::Sin,  1) => ComplexSuppliedFunction::Sin,
		(SuppliedFunctionIdentifier::Cos,  1) => ComplexSuppliedFunction::Cos,
		(SuppliedFunctionIdentifier::Tan,  1) => ComplexSuppliedFunction::Tan,
		(SuppliedFunctionIdentifier::Cot,  1) => ComplexSuppliedFunction::Cot,
		(SuppliedFunctionIdentifier::Sec,  1) => ComplexSuppliedFunction::Sec,
		(SuppliedFunctionIdentifier::Csc,  1) => ComplexSuppliedFunction::Csc,
		(SuppliedFunctionIdentifier::Asin, 1) => ComplexSuppliedFunction::Asin,
		(SuppliedFunctionIdentifier::Acos, 1) => ComplexSuppliedFunction::Acos,
		(SuppliedFunctionIdentifier::Atan, 1) => ComplexSuppliedFunction::Atan,
		(SuppliedFunctionIdentifier::Acot, 1) => ComplexSuppliedFunction::Acot,
		(SuppliedFunctionIdentifier::Asec, 1) => ComplexSuppliedFunction::Asec,
		(SuppliedFunctionIdentifier::Acsc, 1) => ComplexSuppliedFunction::Acsc,
		// Hyperbolic Trigonometry
		(SuppliedFunctionIdentifier::Sinh,  1) => ComplexSuppliedFunction::Sinh,
		(SuppliedFunctionIdentifier::Cosh,  1) => ComplexSuppliedFunction::Cosh,
		(SuppliedFunctionIdentifier::Tanh,  1) => ComplexSuppliedFunction::Tanh,
		(SuppliedFunctionIdentifier::Coth,  1) => ComplexSuppliedFunction::Coth,
		(SuppliedFunctionIdentifier::Sech,  1) => ComplexSuppliedFunction::Sech,
		(SuppliedFunctionIdentifier::Csch,  1) => ComplexSuppliedFunction::Csch,
		(SuppliedFunctionIdentifier::Asinh, 1) => ComplexSuppliedFunction::Asinh,
		(SuppliedFunctionIdentifier::Acosh, 1) => ComplexSuppliedFunction::Acosh,
		(SuppliedFunctionIdentifier::Atanh, 1) => ComplexSuppliedFunction::Atanh,
		(SuppliedFunctionIdentifier::Acoth, 1) => ComplexSuppliedFunction::Acoth,
		(SuppliedFunctionIdentifier::Asech, 1) => ComplexSuppliedFunction::Asech,
		(SuppliedFunctionIdentifier::Acsch, 1) => ComplexSuppliedFunction::Acsch,
		_ => return None,
	})
}

pub fn get_complex_supplied_functions(identifier: SuppliedFunctionIdentifier) -> &'static [ComplexSuppliedFunction] {
	match identifier {
		SuppliedFunctionIdentifier::I => &[ComplexSuppliedFunction::I],
		
		SuppliedFunctionIdentifier::Sqr  => &[ComplexSuppliedFunction::Sqr],
		SuppliedFunctionIdentifier::Exp  => &[ComplexSuppliedFunction::Exp],
		SuppliedFunctionIdentifier::Conj => &[ComplexSuppliedFunction::Conj],
		
		SuppliedFunctionIdentifier::Ln    => &[ComplexSuppliedFunction::LogE],
		SuppliedFunctionIdentifier::Log2  => &[ComplexSuppliedFunction::Log2],
		SuppliedFunctionIdentifier::Log10 => &[ComplexSuppliedFunction::Log10],
		SuppliedFunctionIdentifier::Log   => &[ComplexSuppliedFunction::LogN, ComplexSuppliedFunction::LogE],
		
		SuppliedFunctionIdentifier::Sin  => &[ComplexSuppliedFunction::Sin],
		SuppliedFunctionIdentifier::Cos  => &[ComplexSuppliedFunction::Cos],
		SuppliedFunctionIdentifier::Tan  => &[ComplexSuppliedFunction::Tan],
		SuppliedFunctionIdentifier::Cot  => &[ComplexSuppliedFunction::Cot],
		SuppliedFunctionIdentifier::Sec  => &[ComplexSuppliedFunction::Sec],
		SuppliedFunctionIdentifier::Csc  => &[ComplexSuppliedFunction::Csc],
		SuppliedFunctionIdentifier::Asin => &[ComplexSuppliedFunction::Asin],
		SuppliedFunctionIdentifier::Acos => &[ComplexSuppliedFunction::Acos],
		SuppliedFunctionIdentifier::Atan => &[ComplexSuppliedFunction::Atan],
		SuppliedFunctionIdentifier::Acot => &[ComplexSuppliedFunction::Acot],
		SuppliedFunctionIdentifier::Asec => &[ComplexSuppliedFunction::Asec],
		SuppliedFunctionIdentifier::Acsc => &[ComplexSuppliedFunction::Acsc],
		
		SuppliedFunctionIdentifier::Sinh  => &[ComplexSuppliedFunction::Sinh],
		SuppliedFunctionIdentifier::Cosh  => &[ComplexSuppliedFunction::Cosh],
		SuppliedFunctionIdentifier::Tanh  => &[ComplexSuppliedFunction::Tanh],
		SuppliedFunctionIdentifier::Coth  => &[ComplexSuppliedFunction::Coth],
		SuppliedFunctionIdentifier::Sech  => &[ComplexSuppliedFunction::Sech],
		SuppliedFunctionIdentifier::Csch  => &[ComplexSuppliedFunction::Csch],
		SuppliedFunctionIdentifier::Asinh => &[ComplexSuppliedFunction::Asinh],
		SuppliedFunctionIdentifier::Acosh => &[ComplexSuppliedFunction::Acosh],
		SuppliedFunctionIdentifier::Atanh => &[ComplexSuppliedFunction::Atanh],
		SuppliedFunctionIdentifier::Acoth => &[ComplexSuppliedFunction::Acoth],
		SuppliedFunctionIdentifier::Asech => &[ComplexSuppliedFunction::Asech],
		SuppliedFunctionIdentifier::Acsch => &[ComplexSuppliedFunction::Acsch],
		_ => &[],
	}
}

fn parse_string_supplied_function(identifier: SuppliedFunctionIdentifier, arguments: &[AnyTypeExpression]) -> Option<StringSuppliedFunction> {
	let argument_count = arguments.len();
	Some(match (identifier, argument_count) {
		// Time
		(SuppliedFunctionIdentifier::Time, 0) => StringSuppliedFunction::Time,
		(SuppliedFunctionIdentifier::Date, 0) => StringSuppliedFunction::Date,
		// String Editing
		(SuppliedFunctionIdentifier::UCase,  1) => StringSuppliedFunction::UCase,
		(SuppliedFunctionIdentifier::LCase,  1) => StringSuppliedFunction::LCase,
		(SuppliedFunctionIdentifier::LTrim,  1) => StringSuppliedFunction::LTrim,
		(SuppliedFunctionIdentifier::RTrim,  1) => StringSuppliedFunction::RTrim,
		(SuppliedFunctionIdentifier::Left,   1) => StringSuppliedFunction::Left1Arg,
		(SuppliedFunctionIdentifier::Right,  1) => StringSuppliedFunction::Right1Arg,
		(SuppliedFunctionIdentifier::Repeat, 2) => StringSuppliedFunction::Repeat,
		(SuppliedFunctionIdentifier::Left,   2) => StringSuppliedFunction::Left2Args,
		(SuppliedFunctionIdentifier::Right,  2) => StringSuppliedFunction::Right2Args,
		(SuppliedFunctionIdentifier::Mid,    2) => StringSuppliedFunction::Mid2Args,
		(SuppliedFunctionIdentifier::Mid,    3) => StringSuppliedFunction::Mid3Args,
		// Int to String
		(SuppliedFunctionIdentifier::Chr, 1) => StringSuppliedFunction::Chr,
		// Any value to String
		(SuppliedFunctionIdentifier::Str, 1) => StringSuppliedFunction::Str,
		_ => return None,
	})
}

pub fn get_string_supplied_functions(identifier: SuppliedFunctionIdentifier) -> &'static [StringSuppliedFunction] {
	match identifier {
		SuppliedFunctionIdentifier::Time => &[StringSuppliedFunction::Time],
		SuppliedFunctionIdentifier::Date => &[StringSuppliedFunction::Date],
		
		SuppliedFunctionIdentifier::UCase  => &[StringSuppliedFunction::UCase],
		SuppliedFunctionIdentifier::LCase  => &[StringSuppliedFunction::LCase],
		SuppliedFunctionIdentifier::LTrim  => &[StringSuppliedFunction::LTrim],
		SuppliedFunctionIdentifier::RTrim  => &[StringSuppliedFunction::RTrim],
		SuppliedFunctionIdentifier::Left   => &[StringSuppliedFunction::Left1Arg, StringSuppliedFunction::Left2Args],
		SuppliedFunctionIdentifier::Right  => &[StringSuppliedFunction::Right1Arg, StringSuppliedFunction::Right2Args],
		SuppliedFunctionIdentifier::Repeat => &[StringSuppliedFunction::Repeat],
		SuppliedFunctionIdentifier::Mid    => &[StringSuppliedFunction::Mid2Args, StringSuppliedFunction::Mid3Args],
		
		SuppliedFunctionIdentifier::Chr => &[StringSuppliedFunction::Chr],
		
		SuppliedFunctionIdentifier::Str => &[StringSuppliedFunction::Str],
		_ => &[],
	}
}

/// Gets the length of an l-value
// TODO: Remove
fn get_l_value_length(tokens: &[Token]) -> usize {
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

/// Take a binary operator and its left and right operands and convert them to a single expression object.
fn binary_operator_to_expression(operator: BinaryOperator, start_column: NonZeroUsize, lhs: AnyTypeExpression, rhs: AnyTypeExpression) -> Result<AnyTypeExpression, Error> {
	Ok(match operator {
		BinaryOperator::AdditionConcatenation => {
			let (lhs, rhs) = upcast(lhs, rhs)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) =>
					AnyTypeExpression::Int(IntExpression::Addition {
						lhs_expression: Box::new(cast_to_int_expression(lhs)?),
						rhs_expression: Box::new(cast_to_int_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) =>
					AnyTypeExpression::Float(FloatExpression::Addition {
						lhs_expression: Box::new(cast_to_float_expression(lhs)?),
						rhs_expression: Box::new(cast_to_float_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Complex(ComplexExpression::Addition {
						lhs_expression: Box::new(cast_to_complex_expression(lhs)?),
						rhs_expression: Box::new(cast_to_complex_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					AnyTypeExpression::String(StringExpression::Concatenation {
						lhs_expression: Box::new(cast_to_string_expression(lhs)?),
						rhs_expression: Box::new(cast_to_string_expression(rhs)?),
						start_column,
					}),
				_ => unreachable!(),
			}
		},
		BinaryOperator::Concatenation => {
			let (lhs, rhs) = upcast(lhs, rhs)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) |
				(AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					return Err(ErrorVariant::CannotConcatenateNumbers.at_column(start_column)),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					AnyTypeExpression::String(StringExpression::Concatenation {
						lhs_expression: Box::new(cast_to_string_expression(lhs)?),
						rhs_expression: Box::new(cast_to_string_expression(rhs)?),
						start_column,
					}),
				_ => unreachable!(),
			}
		},
		BinaryOperator::Subtraction => {
			let (lhs, rhs) = upcast(lhs, rhs)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) =>
					AnyTypeExpression::Int(IntExpression::Subtraction {
						lhs_expression: Box::new(cast_to_int_expression(lhs)?),
						rhs_expression: Box::new(cast_to_int_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) =>
					AnyTypeExpression::Float(FloatExpression::Subtraction {
						lhs_expression: Box::new(cast_to_float_expression(lhs)?),
						rhs_expression: Box::new(cast_to_float_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Complex(ComplexExpression::Subtraction {
						lhs_expression: Box::new(cast_to_complex_expression(lhs)?),
						rhs_expression: Box::new(cast_to_complex_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) => return Err(ErrorVariant::CannotUseThisOperatorOnAString.at_column(start_column)),
				_ => unreachable!(),
			}
		},
		BinaryOperator::Multiplication => {
			let (lhs, rhs) = upcast(lhs, rhs)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) =>
					AnyTypeExpression::Int(IntExpression::Multiplication {
						lhs_expression: Box::new(cast_to_int_expression(lhs)?),
						rhs_expression: Box::new(cast_to_int_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) =>
					AnyTypeExpression::Float(FloatExpression::Multiplication {
						lhs_expression: Box::new(cast_to_float_expression(lhs)?),
						rhs_expression: Box::new(cast_to_float_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Complex(ComplexExpression::Multiplication {
						lhs_expression: Box::new(cast_to_complex_expression(lhs)?),
						rhs_expression: Box::new(cast_to_complex_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) => return Err(ErrorVariant::CannotUseThisOperatorOnAString.at_column(start_column)),
				_ => unreachable!(),
			}
		},
		BinaryOperator::Division => {
			let (lhs, rhs) = upcast(lhs, rhs)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) =>
					AnyTypeExpression::Float(FloatExpression::Division {
						lhs_expression: Box::new(cast_to_float_expression(lhs)?),
						rhs_expression: Box::new(cast_to_float_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Complex(ComplexExpression::Division {
						lhs_expression: Box::new(cast_to_complex_expression(lhs)?),
						rhs_expression: Box::new(cast_to_complex_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) => return Err(ErrorVariant::CannotUseThisOperatorOnAString.at_column(start_column)),
				_ => unreachable!(),
			}
		},
		BinaryOperator::Exponentiation => {
			let (lhs, rhs) = upcast(lhs, rhs)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) =>
					AnyTypeExpression::Float(FloatExpression::Exponentiation {
						lhs_expression: Box::new(cast_to_float_expression(lhs)?),
						rhs_expression: Box::new(cast_to_float_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Complex(ComplexExpression::Exponentiation {
						lhs_expression: Box::new(cast_to_complex_expression(lhs)?),
						rhs_expression: Box::new(cast_to_complex_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) => return Err(ErrorVariant::CannotUseThisOperatorOnAString.at_column(start_column)),
				_ => unreachable!(),
			}
		},
		BinaryOperator::DoubleSlash => {
			let (lhs, rhs) = upcast(lhs, rhs)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Int(IntExpression::FlooredDivision {
						lhs_expression: Box::new(cast_to_int_expression(lhs)?),
						rhs_expression: Box::new(cast_to_int_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					return Err(ErrorVariant::CannotUseThisOperatorOnAString.at_column(start_column)),
				_ => unreachable!(),
			}
		},
		BinaryOperator::BackSlash => {
			let (lhs, rhs) = upcast(lhs, rhs)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Int(IntExpression::FlooredDivision {
						lhs_expression: Box::new(cast_to_int_expression(lhs)?),
						rhs_expression: Box::new(cast_to_int_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					return Err(ErrorVariant::CannotUseThisOperatorOnAString.at_column(start_column)),
				_ => unreachable!(),
			}
		},
		BinaryOperator::Equal => {
			let (lhs, rhs) = upcast(lhs, rhs)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) =>
					AnyTypeExpression::Bool(BoolExpression::BoolEqualTo {
						lhs_expression: Box::new(cast_to_bool_expression(lhs)?),
						rhs_expression: Box::new(cast_to_bool_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) =>
					AnyTypeExpression::Bool(BoolExpression::IntEqualTo {
						lhs_expression: Box::new(cast_to_int_expression(lhs)?),
						rhs_expression: Box::new(cast_to_int_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) =>
					AnyTypeExpression::Bool(BoolExpression::FloatEqualTo {
						lhs_expression: Box::new(cast_to_float_expression(lhs)?),
						rhs_expression: Box::new(cast_to_float_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Bool(BoolExpression::ComplexEqualTo {
						lhs_expression: Box::new(cast_to_complex_expression(lhs)?),
						rhs_expression: Box::new(cast_to_complex_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					AnyTypeExpression::Bool(BoolExpression::StringEqualTo {
						lhs_expression: Box::new(cast_to_string_expression(lhs)?),
						rhs_expression: Box::new(cast_to_string_expression(rhs)?),
						start_column,
					}),
				_ => unreachable!(),
			}
		},
		BinaryOperator::NotEqualTo => {
			let (lhs, rhs) = upcast(lhs, rhs)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) =>
					AnyTypeExpression::Bool(BoolExpression::BoolNotEqualTo {
						lhs_expression: Box::new(cast_to_bool_expression(lhs)?),
						rhs_expression: Box::new(cast_to_bool_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) =>
					AnyTypeExpression::Bool(BoolExpression::IntNotEqualTo {
						lhs_expression: Box::new(cast_to_int_expression(lhs)?),
						rhs_expression: Box::new(cast_to_int_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) =>
					AnyTypeExpression::Bool(BoolExpression::FloatNotEqualTo {
						lhs_expression: Box::new(cast_to_float_expression(lhs)?),
						rhs_expression: Box::new(cast_to_float_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Bool(BoolExpression::ComplexNotEqualTo {
						lhs_expression: Box::new(cast_to_complex_expression(lhs)?),
						rhs_expression: Box::new(cast_to_complex_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					AnyTypeExpression::Bool(BoolExpression::StringNotEqualTo {
						lhs_expression: Box::new(cast_to_string_expression(lhs)?),
						rhs_expression: Box::new(cast_to_string_expression(rhs)?),
						start_column,
					}),
				_ => unreachable!(),
			}
		},
		BinaryOperator::GreaterThan => {
			let (lhs, rhs) = upcast(lhs, rhs)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) =>
					AnyTypeExpression::Bool(BoolExpression::BoolGreaterThan {
						lhs_expression: Box::new(cast_to_bool_expression(lhs)?),
						rhs_expression: Box::new(cast_to_bool_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) =>
					AnyTypeExpression::Bool(BoolExpression::IntGreaterThan {
						lhs_expression: Box::new(cast_to_int_expression(lhs)?),
						rhs_expression: Box::new(cast_to_int_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Bool(BoolExpression::FloatGreaterThan {
						lhs_expression: Box::new(cast_to_float_expression(lhs)?),
						rhs_expression: Box::new(cast_to_float_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					AnyTypeExpression::Bool(BoolExpression::StringGreaterThan {
						lhs_expression: Box::new(cast_to_string_expression(lhs)?),
						rhs_expression: Box::new(cast_to_string_expression(rhs)?),
						start_column,
					}),
				_ => unreachable!(),
			}
		},
		BinaryOperator::GreaterThanOrEqualTo => {
			let (lhs, rhs) = upcast(lhs, rhs)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) =>
					AnyTypeExpression::Bool(BoolExpression::BoolGreaterThanOrEqualTo {
						lhs_expression: Box::new(cast_to_bool_expression(lhs)?),
						rhs_expression: Box::new(cast_to_bool_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) =>
					AnyTypeExpression::Bool(BoolExpression::IntGreaterThanOrEqualTo {
						lhs_expression: Box::new(cast_to_int_expression(lhs)?),
						rhs_expression: Box::new(cast_to_int_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Bool(BoolExpression::FloatGreaterThanOrEqualTo {
						lhs_expression: Box::new(cast_to_float_expression(lhs)?),
						rhs_expression: Box::new(cast_to_float_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					AnyTypeExpression::Bool(BoolExpression::StringGreaterThanOrEqualTo {
						lhs_expression: Box::new(cast_to_string_expression(lhs)?),
						rhs_expression: Box::new(cast_to_string_expression(rhs)?),
						start_column,
					}),
				_ => unreachable!(),
			}
		},
		BinaryOperator::LessThan => {
			let (lhs, rhs) = upcast(lhs, rhs)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) =>
					AnyTypeExpression::Bool(BoolExpression::BoolLessThan {
						lhs_expression: Box::new(cast_to_bool_expression(lhs)?),
						rhs_expression: Box::new(cast_to_bool_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) =>
					AnyTypeExpression::Bool(BoolExpression::IntLessThan {
						lhs_expression: Box::new(cast_to_int_expression(lhs)?),
						rhs_expression: Box::new(cast_to_int_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Bool(BoolExpression::FloatLessThan {
						lhs_expression: Box::new(cast_to_float_expression(lhs)?),
						rhs_expression: Box::new(cast_to_float_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					AnyTypeExpression::Bool(BoolExpression::StringLessThan {
						lhs_expression: Box::new(cast_to_string_expression(lhs)?),
						rhs_expression: Box::new(cast_to_string_expression(rhs)?),
						start_column,
					}),
				_ => unreachable!(),
			}
		},
		BinaryOperator::LessThanOrEqualTo => {
			let (lhs, rhs) = upcast(lhs, rhs)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) =>
					AnyTypeExpression::Bool(BoolExpression::BoolLessThanOrEqualTo {
						lhs_expression: Box::new(cast_to_bool_expression(lhs)?),
						rhs_expression: Box::new(cast_to_bool_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) =>
					AnyTypeExpression::Bool(BoolExpression::IntLessThanOrEqualTo {
						lhs_expression: Box::new(cast_to_int_expression(lhs)?),
						rhs_expression: Box::new(cast_to_int_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Bool(BoolExpression::FloatLessThanOrEqualTo {
						lhs_expression: Box::new(cast_to_float_expression(lhs)?),
						rhs_expression: Box::new(cast_to_float_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) =>
					AnyTypeExpression::Bool(BoolExpression::StringLessThanOrEqualTo {
						lhs_expression: Box::new(cast_to_string_expression(lhs)?),
						rhs_expression: Box::new(cast_to_string_expression(rhs)?),
						start_column,
					}),
				_ => unreachable!(),
			}
		},
		BinaryOperator::And => {
			let (lhs, rhs) = upcast(lhs, rhs)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) =>
					AnyTypeExpression::Bool(BoolExpression::And {
						lhs_expression: Box::new(cast_to_bool_expression(lhs)?),
						rhs_expression: Box::new(cast_to_bool_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Int(IntExpression::BitwiseAnd {
						lhs_expression: Box::new(cast_to_int_expression(lhs)?),
						rhs_expression: Box::new(cast_to_int_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) => return Err(ErrorVariant::CannotUseThisOperatorOnAString.at_column(start_column)),
				_ => unreachable!(),
			}
		},
		BinaryOperator::Or => {
			let (lhs, rhs) = upcast(lhs, rhs)?;
			match (&lhs, &rhs) {
				(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) =>
					AnyTypeExpression::Bool(BoolExpression::Or {
						lhs_expression: Box::new(cast_to_bool_expression(lhs)?),
						rhs_expression: Box::new(cast_to_bool_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) | (AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) =>
					AnyTypeExpression::Int(IntExpression::BitwiseOr {
						lhs_expression: Box::new(cast_to_int_expression(lhs)?),
						rhs_expression: Box::new(cast_to_int_expression(rhs)?),
						start_column,
					}),
				(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) => return Err(ErrorVariant::CannotUseThisOperatorOnAString.at_column(start_column)),
				_ => unreachable!(),
			}
		},
	})
}

/// Take a unary operator and its right operand and convert them to a single expression object.
fn unary_operator_to_expression(operator: UnaryOperator, start_column: NonZeroUsize, operand: AnyTypeExpression) -> Result<AnyTypeExpression, Error> {
	Ok(match &operator {
		UnaryOperator::Negation  => match operand {
			AnyTypeExpression::Bool(..) | AnyTypeExpression::Int(..) => AnyTypeExpression::Int(IntExpression::Negation { sub_expression: Box::new(cast_to_int_expression(operand)?), start_column }),
			AnyTypeExpression::Float(..) => AnyTypeExpression::Float(FloatExpression::Negation { sub_expression: Box::new(cast_to_float_expression(operand)?), start_column }),
			AnyTypeExpression::Complex(..) => AnyTypeExpression::Complex(ComplexExpression::Negation { sub_expression: Box::new(cast_to_complex_expression(operand)?), start_column }),
			AnyTypeExpression::String(..) => return Err(ErrorVariant::CannotUseThisOperatorOnAString.at_column(start_column)),
		}
		UnaryOperator::UnaryPlus  => operand,
		UnaryOperator::Not  => match operand {
			AnyTypeExpression::Bool(..) => AnyTypeExpression::Bool(BoolExpression::Not { sub_expression: Box::new(cast_to_bool_expression(operand)?), start_column }),
			AnyTypeExpression::Int(..) | AnyTypeExpression::Float(..) | AnyTypeExpression::Complex(..) =>
				AnyTypeExpression::Int(IntExpression::BitwiseNot { sub_expression: Box::new(cast_to_int_expression(operand)?), start_column }),
			AnyTypeExpression::String(..) => return Err(ErrorVariant::CannotUseThisOperatorOnAString.at_column(start_column)),
		}
	})
}

/// Creates a cast from an expression of any type to an expression of integer type.
fn cast_to_int_expression(any_type_expression: AnyTypeExpression) -> Result<IntExpression, Error> {
	Ok(match any_type_expression {
		AnyTypeExpression::Int(expression) => expression,
		AnyTypeExpression::Float(expression) => IntExpression::CastFromFloat(Box::new(expression)),
		AnyTypeExpression::Complex(expression) => IntExpression::CastFromFloat(Box::new(FloatExpression::CastFromComplex(Box::new(expression)))),
		AnyTypeExpression::Bool(expression) => IntExpression::CastFromBool(Box::new(expression)),
		AnyTypeExpression::String(expression) => return Err(ErrorVariant::StringCastToNumber.at_column(expression.get_start_column())),
	})
}

/// Creates a cast from an expression of any type to an expression of float type.
fn cast_to_float_expression(any_type_expression: AnyTypeExpression) -> Result<FloatExpression, Error> {
	Ok(match any_type_expression {
		AnyTypeExpression::Int(expression) => FloatExpression::CastFromInt(Box::new(expression)),
		AnyTypeExpression::Float(expression) => expression,
		AnyTypeExpression::Complex(expression) => FloatExpression::CastFromComplex(Box::new(expression)),
		AnyTypeExpression::Bool(expression) => FloatExpression::CastFromInt(Box::new(IntExpression::CastFromBool(Box::new(expression)))),
		AnyTypeExpression::String(expression) => return Err(ErrorVariant::StringCastToNumber.at_column(expression.get_start_column())),
	})
}

/// Creates a cast from an expression of any type to an expression of complex type.
fn cast_to_complex_expression(any_type_expression: AnyTypeExpression) -> Result<ComplexExpression, Error> {
	Ok(match any_type_expression {
		AnyTypeExpression::Int(expression) => ComplexExpression::CastFromFloat(Box::new(FloatExpression::CastFromInt(Box::new(expression)))),
		AnyTypeExpression::Float(expression) => ComplexExpression::CastFromFloat(Box::new(expression)),
		AnyTypeExpression::Complex(expression) => expression,
		AnyTypeExpression::Bool(expression) => ComplexExpression::CastFromFloat(Box::new(FloatExpression::CastFromInt(Box::new(IntExpression::CastFromBool(Box::new(expression)))))),
		AnyTypeExpression::String(expression) => return Err(ErrorVariant::StringCastToNumber.at_column(expression.get_start_column())),
	})
}

/// Creates a cast from an expression of any type to an expression of string type.
fn cast_to_string_expression(any_type_expression: AnyTypeExpression) -> Result<StringExpression, Error> {
	Ok(match any_type_expression {
		AnyTypeExpression::Int(..) | AnyTypeExpression::Float(..) | AnyTypeExpression::Complex(..) | AnyTypeExpression::Bool(..) =>
			return Err(ErrorVariant::NumberCastToString.at_column(any_type_expression.get_start_column())),
		AnyTypeExpression::String(value) => value,
	})
}

/// Creates a cast from an expression of any type to an expression of boolean type.
fn cast_to_bool_expression(any_type_expression: AnyTypeExpression) -> Result<BoolExpression, Error> {
	Ok(match any_type_expression {
		AnyTypeExpression::Bool(value) => value,
		AnyTypeExpression::Int(expression) => BoolExpression::IntIsNonZero(Box::new(expression)),
		AnyTypeExpression::Float(expression) => BoolExpression::FloatIsNonZero(Box::new(expression)),
		AnyTypeExpression::Complex(expression) => BoolExpression::ComplexIsNonZero(Box::new(expression)),
		AnyTypeExpression::String(expression) => BoolExpression::StringIsNotEmpty(Box::new(expression)),
	})
}

/// Takes in two expressions and upcasts the first one in the bool -> int -> float -> complex chain to be of the same type as the other one.
/// Eg. upcast(complex, int) will cast the second argument to type complex and return expressions of type (complex, complex).
fn upcast(lhs: AnyTypeExpression, rhs: AnyTypeExpression) -> Result<(AnyTypeExpression, AnyTypeExpression), Error> {
	Ok(match (&lhs, &rhs) {
		(AnyTypeExpression::Bool(..), AnyTypeExpression::Bool(..)) | (AnyTypeExpression::Int(..), AnyTypeExpression::Int(..)) |
		(AnyTypeExpression::Float(..), AnyTypeExpression::Float(..)) | (AnyTypeExpression::Complex(..), AnyTypeExpression::Complex(..)) |
		(AnyTypeExpression::String(..), AnyTypeExpression::String(..)) => (lhs, rhs),
		(AnyTypeExpression::Int(..), AnyTypeExpression::Bool(..)) => (lhs, AnyTypeExpression::Int(cast_to_int_expression(rhs)?)),
		(AnyTypeExpression::Bool(..), AnyTypeExpression::Int(..)) => (AnyTypeExpression::Int(cast_to_int_expression(lhs)?), rhs),
		(AnyTypeExpression::Float(..), AnyTypeExpression::Bool(..) | AnyTypeExpression::Int(..)) => (lhs, AnyTypeExpression::Float(cast_to_float_expression(rhs)?)),
		(AnyTypeExpression::Bool(..) | AnyTypeExpression::Int(..), AnyTypeExpression::Float(..)) => (AnyTypeExpression::Float(cast_to_float_expression(lhs)?), rhs),
		(AnyTypeExpression::Complex(..), AnyTypeExpression::Bool(..) | AnyTypeExpression::Int(..) | AnyTypeExpression::Float(..)) => (lhs, AnyTypeExpression::Complex(cast_to_complex_expression(rhs)?)),
		(AnyTypeExpression::Bool(..) | AnyTypeExpression::Int(..) | AnyTypeExpression::Float(..), AnyTypeExpression::Complex(..)) => (AnyTypeExpression::Complex(cast_to_complex_expression(lhs)?), rhs),
		(AnyTypeExpression::String(..), rhs) => return Err(ErrorVariant::NumberCastToString.at_column(rhs.get_start_column())),
		(lhs, AnyTypeExpression::String(..)) => return Err(ErrorVariant::NumberCastToString.at_column(lhs.get_start_column())),
	})
}

#[derive(Clone, Copy)]
/// A struct that contains a borrowed slice of tokens. Tokens can be parsed and removed from the front of the slice.
pub struct Tokens<'b> {
	/// The borrowed slice.
	pub tokens: &'b [Token],
	/// The end column of the last removed token.
	pub last_removed_token_end_column: NonZeroUsize,
}

impl<'b> Tokens<'b> {
	/// Create from a slice of tokens.
	pub fn new(tokens: &'b [Token]) -> Self {
		Self {
			tokens,
			last_removed_token_end_column: 1.try_into().unwrap(),
		}
	}

	/// Removes an amount of tokens from the start of the token slice.
	pub fn remove_tokens(&mut self, remove_count: usize) {
		if remove_count == 0 {
			return;
		}
		let tokens_removed;
		(tokens_removed, self.tokens) = self.tokens.split_at(remove_count);
		self.last_removed_token_end_column = tokens_removed.last().unwrap().end_column
	}

	/// Removes the next token from the start of the list.
	pub fn take_next_token(&mut self) -> Option<&Token> {
		let tokens_removed;
		(tokens_removed, self.tokens) = match self.tokens.split_at_checked(1) {
			Some(result) => result,
			None => return None,
		};
		self.last_removed_token_end_column = tokens_removed[0].end_column;
		Some(&tokens_removed[0])
	}

	/// Split the list into two an an index.
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

	/// Returns `true` if there is an unparenthesized colon token before the next closing parenthesis that is'nt closing an opening parenthesis in the remaining tokens.
	pub fn has_colon_before_closing_parenthesis(&self) -> bool {
		let mut parenthesis_depth = 0usize;
		for token in self.tokens.iter() {
			if matches!(token.variant, TokenVariant::LeftParenthesis) {
				parenthesis_depth += 1;
			}
			if matches!(token.variant, TokenVariant::RightParenthesis) {
				parenthesis_depth = match parenthesis_depth.checked_sub(1) {
					Some(parenthesis_depth) => parenthesis_depth,
					None => return false,
				}
			}
			if parenthesis_depth == 0 {
				if matches!(token, Token { variant: TokenVariant::Colon, .. }) {
					return true;
				}
			}
		}
		false
	}
}