use std::{mem::replace, num::NonZeroUsize, rc::Rc};

use num::complex::Complex64;

use crate::mavagk_basic::{abstract_syntax_tree::{AngleOption, AnyTypeExpression, AnyTypeLValue, BoolExpression, ComplexExpression, ComplexLValue, FloatExpression, FloatLValue, IntExpression, IntLValue, MachineOption, MathOption, OptionVariableAndValue, PrintOperand, Statement, StatementVariant, StringExpression, StringLValue}, error::{Error, ErrorVariant}, token::{BinaryOperator, IdentifierType, Keyword, Token, TokenVariant, UnaryOperator}, value::{ComplexValue, FloatValue, IntValue, StringValue}};

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
				variant: StatementVariant::AssignInt(
					l_value,
					cast_to_int_expression(r_value_expression)?
				),
			},
			AnyTypeLValue::Float(l_value) => Statement {
				column: l_value_start_column,
				variant: StatementVariant::AssignFloat(
					l_value,
					cast_to_float_expression(r_value_expression)?
				),
			},
			AnyTypeLValue::Complex(l_value) => Statement {
				column: l_value_start_column,
				variant: StatementVariant::AssignComplex(
					l_value,
					cast_to_complex_expression(r_value_expression)?
				),
			},
			AnyTypeLValue::String(l_value) => Statement {
				column: l_value_start_column,
				variant: StatementVariant::AssignString(
					l_value,
					cast_to_string_expression(r_value_expression)?
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
			// Get l-value expression
			let l_value_expression = match parse_l_value(tokens)? {
				None => return Err(ErrorVariant::ExpectedExpression.at_column(tokens.last_removed_token_end_column)),
				Some(l_value_expression) => l_value_expression,
			};
			// Expect equal sign
			expect_and_remove_equal_sign(tokens)?;
			// Get r-value expression
			let r_value_expression = match parse_expression(tokens)? {
				None => return Err(ErrorVariant::ExpectedExpression.at_column(tokens.last_removed_token_end_column)),
				Some(l_value_expression) => l_value_expression,
			};
			// Assemble into statement
			match l_value_expression {
				AnyTypeLValue::Int(l_value) => Statement {
					column: statement_keyword_start_column,
					variant: StatementVariant::AssignInt(
						l_value,
						cast_to_int_expression(r_value_expression)?
					),
				},
				AnyTypeLValue::Float(l_value) => Statement {
					column: statement_keyword_start_column,
					variant: StatementVariant::AssignFloat(
						l_value,
						cast_to_float_expression(r_value_expression)?
					),
				},
				AnyTypeLValue::Complex(l_value) => Statement {
					column: statement_keyword_start_column,
					variant: StatementVariant::AssignComplex(
						l_value,
						cast_to_complex_expression(r_value_expression)?
					),
				},
				AnyTypeLValue::String(l_value) => Statement {
					column: statement_keyword_start_column,
					variant: StatementVariant::AssignString(
						l_value,
						cast_to_string_expression(r_value_expression)?
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
			if !is_root_statement {
				return Err(ErrorVariant::ShouldBeRootStatement.at_column(statement_keyword_start_column));
			}
			// Get the next two keywords
			let (option_variable, option_variable_start_column) = match tokens.take_keyword() {
				Some(option_variable) => option_variable,
				None => return Err(ErrorVariant::ExpectedOptionArguments.at_column(tokens.last_removed_token_end_column)),
			};
			let (option_value, _) = match tokens.take_keyword() {
				Some(option_value) => option_value,
				None => return Err(ErrorVariant::ExpectedOptionArguments.at_column(tokens.last_removed_token_end_column)),
			};
			// Get the option variable/value pair
			let option_variable_and_value = match (option_variable, option_value) {
				(Keyword::Angle, Keyword::Radians) => OptionVariableAndValue::Angle(Some(AngleOption::Radians)),
				(Keyword::Angle, Keyword::Degrees) => OptionVariableAndValue::Angle(Some(AngleOption::Degrees)),
				(Keyword::Angle, Keyword::Gradians) => OptionVariableAndValue::Angle(Some(AngleOption::Gradians)),
				(Keyword::Angle, Keyword::Revolutions) => OptionVariableAndValue::Angle(Some(AngleOption::Revolutions)),
				(Keyword::Angle, Keyword::Default) => OptionVariableAndValue::Angle(None),
				(Keyword::Arithmetic, Keyword::Decimal) => OptionVariableAndValue::ArithmeticDecimal,
				(Keyword::Arithmetic, Keyword::Native) => OptionVariableAndValue::ArithmeticNative,
				(Keyword::Arithmetic, Keyword::Default) => OptionVariableAndValue::ArithmeticDefault,
				(Keyword::Math, Keyword::Ansi) => OptionVariableAndValue::Math(Some(MathOption::Ansi)),
				(Keyword::Math, Keyword::Ieee) => OptionVariableAndValue::Math(Some(MathOption::Ieee)),
				(Keyword::Math, Keyword::Default) => OptionVariableAndValue::Math(None),
				(Keyword::Machine, Keyword::Ansi) => OptionVariableAndValue::Machine(Some(MachineOption::Ansi)),
				(Keyword::Machine, Keyword::C64) => OptionVariableAndValue::Machine(Some(MachineOption::C64)),
				(Keyword::Machine, Keyword::Default) => OptionVariableAndValue::Machine(None),
				_ => return Err(ErrorVariant::InvalidOptionVariableOrValue.at_column(option_variable_start_column)),
			};
			// Assemble into statement
			Statement {
				column: statement_keyword_start_column,
				variant: StatementVariant::Option(option_variable_and_value),
			}
		}
		Keyword::Load => {
			// Get the filename expression if it exists or return a LOAD statement without an argument if there is no expression
			let filename_expression = match parse_expression(tokens)? {
				Some(filename_expression) => cast_to_string_expression(filename_expression)?,
				None => return Ok(Some(Statement {
					column: statement_keyword_start_column,
					variant: StatementVariant::Load(None),
				})),
			};
			// Make sure we have at most one argument
			match tokens.tokens.first() {
				Some(Token { variant: TokenVariant::Comma, start_column, .. }) =>
					return Err(ErrorVariant::Unimplemented("LOAD statement with more than one argument".into()).at_column(*start_column)),
				_ => {},
			}
			// Assemble into statement
			Statement {
				column: statement_keyword_start_column,
				variant: StatementVariant::Load(Some(filename_expression)),
			}
		}
		Keyword::End => Statement {
			column: statement_keyword_start_column,
			variant: StatementVariant::End,
		},
		Keyword::Stop => Statement {
			column: statement_keyword_start_column,
			variant: StatementVariant::Stop,
		},
		//Keyword::Fn => return Err(ErrorVariant::ExpectedStatementKeyword.at_column(statement_keyword_start_column)),
		Keyword::Go => return Err(ErrorVariant::SingleGoKeyword.at_column(statement_keyword_start_column)),
		_ => return Err(ErrorVariant::NotYetImplemented("Statement".into()).at_column(statement_keyword_start_column)),
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
	}))
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
fn parse_l_value<'a, 'b>(tokens: &mut Tokens)-> Result<Option<AnyTypeLValue>, Error> {
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
	// Get if this is a FN function
	//let uses_fn_keyword = keyword == Some(Keyword::Fn);
	//match uses_fn_keyword {
	//	false => {}
	//	true => {
	//		tokens.take_next_token();
	//	}
	//}
	// Get identifier name
	let Token { variant: token_after_fn_variant, start_column: token_after_fn_start_column, end_column: _ } = match tokens.tokens.get(0) {
		Some(token_after_fn) => token_after_fn,
		None => return Err(ErrorVariant::ExpectedFunctionNameAfterFn.at_column(*first_token_end_column)),
	};
	let (identifier_name, identifier_type, identifier_is_optional, supplied_function) = match token_after_fn_variant {
		TokenVariant::Identifier { name, identifier_type, is_optional, supplied_function, .. } => (name, identifier_type, is_optional, *supplied_function),
		_ => return Err(ErrorVariant::ExpectedFunctionNameAfterFn.at_column(*token_after_fn_start_column)),
	};
	if *identifier_is_optional {
		return Err(ErrorVariant::NotYetImplemented("Optional functions".into()).at_column(*token_after_fn_start_column));
	}
	tokens.take_next_token();
	// Return if there is not a left parenthesis after the identifier
	match tokens.tokens.get(0) {
		Some(Token { variant: TokenVariant::LeftParenthesis, .. }) => {},
		_ => return Ok(Some(match identifier_type {
			IdentifierType::Integer => AnyTypeLValue::Int(IntLValue {
				name: (*identifier_name).into(), arguments: Box::default()/*, uses_fn_keyword*/, has_parentheses: false, start_column: *first_token_start_column, supplied_function
			}),
			IdentifierType::UnmarkedOrFloat => AnyTypeLValue::Float(FloatLValue {
				name: (*identifier_name).into(), arguments: Box::default()/*, uses_fn_keyword*/, has_parentheses: false, start_column: *first_token_start_column, supplied_function
			}),
			IdentifierType::ComplexNumber => AnyTypeLValue::Complex(ComplexLValue {
				name: (*identifier_name).into(), arguments: Box::default()/*, uses_fn_keyword*/, has_parentheses: false, start_column: *first_token_start_column, supplied_function
			}),
			IdentifierType::String => AnyTypeLValue::String(StringLValue {
				name: (*identifier_name).into(), arguments: Box::default()/*, uses_fn_keyword*/, has_parentheses: false, start_column: *first_token_start_column, supplied_function
			}),
		})),
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
			Some(Token { variant: TokenVariant::RightParenthesis, .. }) => {
				tokens.take_next_token();
				break 'b;
			}
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
			_ => {}
		};
	}
	// Return
	return Ok(Some(match identifier_type {
		IdentifierType::Integer => AnyTypeLValue::Int(IntLValue {
			name: (*identifier_name).into(), arguments: arguments.into()/*, uses_fn_keyword*/, has_parentheses: true, start_column: *first_token_start_column, supplied_function
		}),
		IdentifierType::UnmarkedOrFloat => AnyTypeLValue::Float(FloatLValue {
			name: (*identifier_name).into(), arguments: arguments.into()/*, uses_fn_keyword*/, has_parentheses: true, start_column: *first_token_start_column, supplied_function
		}),
		IdentifierType::ComplexNumber => AnyTypeLValue::Complex(ComplexLValue {
			name: (*identifier_name).into(), arguments: arguments.into()/*, uses_fn_keyword*/, has_parentheses: true, start_column: *first_token_start_column, supplied_function
		}),
		IdentifierType::String => AnyTypeLValue::String(StringLValue {
			name: (*identifier_name).into(), arguments: arguments.into()/*, uses_fn_keyword*/, has_parentheses: true, start_column: *first_token_start_column, supplied_function
		}),
	}))
}

/// Gets the length of an l-value
// TODO: Remove
fn get_l_value_length<'a>(tokens: &[Token<'a>]) -> usize {
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
		(AnyTypeExpression::String(..), _) => (lhs, AnyTypeExpression::Complex(cast_to_complex_expression(rhs)?)),
		(_, AnyTypeExpression::String(..)) => (AnyTypeExpression::Complex(cast_to_complex_expression(lhs)?), rhs),
	})
}

#[derive(Clone, Copy)]
/// A struct that contains a borrowed slice of tokens. Tokens can be parsed and removed from the front of the slice.
pub struct Tokens<'a, 'b> {
	/// The borrowed slice.
	pub tokens: &'b [Token<'a>],
	/// The end column of the last removed token.
	pub last_removed_token_end_column: NonZeroUsize,
}

impl<'a, 'b> Tokens<'a, 'b> {
	/// Create from a slice of tokens.
	pub fn new(tokens: &'b [Token<'a>]) -> Self {
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
	pub fn take_next_token(&mut self) -> Option<&Token<'_>> {
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
}