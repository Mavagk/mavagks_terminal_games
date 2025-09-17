use std::{collections::HashMap, io::{stdin, stdout, Write}, mem::take, num::NonZeroUsize, ops::{RangeFrom, RangeFull, RangeInclusive, RangeToInclusive}, rc::Rc};

use crossterm::{execute, style::{Color, ContentStyle, PrintStyledContent, StyledContent}};
use num::{bigint::Sign, BigInt, FromPrimitive, Signed, Zero};

use crate::mavagk_basic::{abstract_syntax_tree::{AngleOption, AnyTypeExpression, AnyTypeLValue, BoolExpression, ComplexExpression, ComplexLValue, FloatExpression, FloatLValue, IntExpression, IntLValue, MathOption, OptionVariableAndValue, PrintOperand, Statement, StatementVariant, StringExpression, StringLValue}, error::{handle_error, Error, ErrorVariant, FullError}, optimize::optimize_statement, parse::{parse_line, Tokens}, program::Program, token::{SuppliedFunction, Token}, value::{AnyTypeValue, BoolValue, ComplexValue, FloatValue, IntValue, StringValue}};

pub struct Machine {
	// Program counter
	line_executing: Option<Rc<BigInt>>,
	execution_source: ExecutionSource,
	// Variables
	float_variables: HashMap<Box<str>, FloatValue>,
	complex_variables: HashMap<Box<str>, ComplexValue>,
	int_variables: HashMap<Box<str>, IntValue>,
	string_variables: HashMap<Box<str>, StringValue>,
	// Options
	angle_option: AngleOption,
	math_option: MathOption,
}

impl Machine {
	pub fn new() -> Self {
		Self {
			line_executing: None,
			execution_source: ExecutionSource::ProgramEnded,
			int_variables: HashMap::new(),
			float_variables: HashMap::new(),
			complex_variables: HashMap::new(),
			string_variables: HashMap::new(),
			angle_option: AngleOption::Gradians,
			math_option: MathOption::Ansi,
		}
	}

	fn set_line_executing(&mut self, program: &Program, goto_line_number: Option<Rc<BigInt>>, column_number: NonZeroUsize) -> Result<(), Error> {
		self.line_executing = match goto_line_number {
			Some(goto_line_number) =>{
				if !program.lines.contains_key(&goto_line_number) {
					return Err(ErrorVariant::InvalidLineNumber((*goto_line_number).clone()).at_column(column_number));
				}
				Some(goto_line_number)
			}
			None => None,
		};
		self.execution_source = ExecutionSource::Program;
		Ok(())
	}

	fn clear_machine_state(&mut self) {
		self.int_variables = HashMap::new();
		self.float_variables = HashMap::new();
		self.complex_variables = HashMap::new();
		self.string_variables = HashMap::new();
	}

	pub fn line_of_text_entered(&mut self, line_text: Box<str>, program: &mut Program) -> Result<(), FullError> {
		// Parse line
		let (line_number, tokens, error) = match Token::tokenize_line(&*line_text) {
			(line_number, Ok(tokens)) => (line_number, tokens, None),
			(line_number, Err(error)) => (line_number, Box::default(), Some(error)),
		};
		let (mut statements, error) = match error {
			None => parse_line(&mut Tokens::new(&tokens)),
			Some(error) => (Box::default(), Some(error)),
		};
		for statement in statements.iter_mut() {
			optimize_statement(statement);
		}
		// Enter line number into program and run if it does not have a line number
		match line_number {
			// If the line has a line number
			Some(line_number) => {
				if let Some(error) = &error {
					let error = error.clone().to_full_error(Some(line_number.clone()), Some(line_text.clone().into()));
					handle_error::<()>(Err(error));
				}
				if statements.is_empty() && error.is_none() {
					program.lines.remove(&line_number);
				}
				else {
					program.lines.insert(Rc::new(line_number), (statements, error, line_text));
				}
			}
			// Run the line in direct mode if it does not have a line number
			None => {
				if let Some(error) = error {
					//error.line_text = Some(line_text.into());
					return Err(error.clone().to_full_error(None, Some(line_text.clone().into())));
				}
				self.execution_source = ExecutionSource::DirectModeLine;
				self.execute(program, &statements, &line_text)?;
			}
		}
		Ok(())
	}

	/// Returns true if taking the real square root of a negative number should throw an error, returns false if it should return NaN.
	const fn real_square_root_of_negative_is_error(&self) -> bool {
		match self.math_option {
			MathOption::Ieee => false,
			MathOption::Ansi => true,
		}
	}

	/// Returns true if numeric overflow should throw an error, returns false if it should return a non finite value.
	const fn overflow_is_error(&self) -> bool {
		match self.math_option {
			MathOption::Ieee => false,
			MathOption::Ansi => true,
		}
	}

	fn execute(&mut self, program: &mut Program, direct_mode_statements: &Box<[Statement]>, direct_mode_line_text: &str) -> Result<(), FullError> {
		// For each line
		'lines_loop: loop {
			match self.execution_source {
				// If we are executing a line in the program
				ExecutionSource::Program => {
					// No line executing means that we should execute the first line
					if self.line_executing.is_none() {
						self.line_executing = match program.lines.first_key_value() {
							Some(first_entry) => Some(first_entry.0.clone()),
							None => {
								self.execution_source = ExecutionSource::ProgramEnded;
								continue 'lines_loop;
							}
						};
					}
					let line_number = self.line_executing.as_ref().unwrap().clone();
					// Execute each statement in the line
					let (statements, line_error, line_text) = program.lines.get(&line_number).unwrap();
					for statement in statements {
						let flow_control_used = match self.execute_statement(statement, program) {
							Ok(flow_control_used) => flow_control_used,
							Err(error) => return Err(error.to_full_error(Some((*line_number).clone()), Some(line_text.clone().into_string()))),
						};
						if flow_control_used || self.execution_source != ExecutionSource::Program {
							continue 'lines_loop;
						}
					}
					// If there is an error at the end of the line, throw the error
					if let Some(line_error) = line_error {
						return Err(line_error.clone().to_full_error(Some((&*line_number).clone()), Some(line_text.clone().into_string())));
					}
					// Jump to the next line
					self.line_executing = match program.lines.range(line_number..).nth(1) {
						Some((line_number, _)) => Some(line_number.clone()),
						None => {
							self.execution_source = ExecutionSource::ProgramEnded;
							continue 'lines_loop;
						}
					}.clone();
				}
				// If we are executing a direct mode line
				ExecutionSource::DirectModeLine => {
					// Execute each statement in the direct mode line
					for direct_mode_statement in direct_mode_statements {
						let flow_control_used = match self.execute_direct_mode_statement(&direct_mode_statement, program) {
							Ok(flow_control_used) => flow_control_used,
							Err(error) => return Err(error.to_full_error(None, Some(direct_mode_line_text.into()))),
						};
						if flow_control_used || self.execution_source != ExecutionSource::DirectModeLine {
							continue 'lines_loop;
						}
					}
					// If we did not jump out of the direct mode line, such as with a RUN, end execution
					self.execution_source = ExecutionSource::ProgramEnded;
				}
				// Return if the program has ended
				ExecutionSource::ProgramEnded => return Ok(()),
			}
		}
	}

	fn execute_direct_mode_statement(&mut self, statement: &Statement, program: &mut Program) -> Result<bool, Error> {
		let Statement { variant, column: _ } = &statement;
		match variant {
			_ => self.execute_statement(statement, program),
		}
	}

	fn execute_statement(&mut self, statement: &Statement, program: &Program) -> Result<bool, Error> {
		let Statement { variant, column } = &statement;
		match variant {
			StatementVariant::Print(sub_expressions) => {
				for sub_expression in sub_expressions {
					match sub_expression {
						PrintOperand::Expression(expression) => print!("{}", self.execute_any_type_expression(expression)?),
						PrintOperand::Comma(sub_expression_column) | PrintOperand::Semicolon(sub_expression_column) =>
							return Err(ErrorVariant::NotYetImplemented(", and ; in PRINT statement".into()).at_column(*sub_expression_column))
					}
				}
				println!();
			}
			StatementVariant::Input { prompt, timeout, elapsed, inputs } => {
				// TODO
				if let Some(timeout) = timeout {
					return Err(ErrorVariant::NotYetImplemented("TIMEOUT".into()).at_column(timeout.get_start_column()))
				}
				if let Some(elapsed) = elapsed {
					return Err(ErrorVariant::NotYetImplemented("ELAPSED".into()).at_column(elapsed.get_start_column()))
				}
				// Loop until all inputs have been entered
				let mut is_redoing = false;
				'a: loop {
					// Print if text needs to be re-entered
					if is_redoing {
						println!("Input error, please re-enter.")
					}
					is_redoing = true;
					// Print the prompt
					// TODO: OPTION for setting if "? " should be auto printed
					if let Some(prompt_expression) = prompt {
						print!("{}", self.execute_any_type_expression(prompt_expression)?);
					}
					else {
						print!("? ");
					}
					// Get inputs
					stdout().flush().unwrap();
					let mut input_buffer = String::new();
					if stdin().read_line(&mut input_buffer).is_err() {
						continue;
					}
					// Break if a blank string was entered
					if !input_buffer.contains(|chr: char| !chr.is_ascii_whitespace()) {
						break;
					}
					// For each input
					let mut inputs_left = &**inputs;
					let mut input_buffer_left = input_buffer.as_str();
					while !inputs_left.is_empty() {
						let next_input = &inputs_left[0];
						inputs_left = &inputs_left[1..];
						let next_input_text_byte_length = input_buffer_left.find(',');
						let next_input_text = match next_input_text_byte_length {
							Some(next_input_text_byte_length) => {
								let (next_input_text, text_after_next_input_text) = input_buffer_left.split_at(next_input_text_byte_length);
								input_buffer_left = &text_after_next_input_text[1..];
								next_input_text
							}
							None => {
								if !inputs_left.is_empty() {
									continue 'a;
								}
								take(&mut input_buffer_left)
							}
						}.trim_ascii();
						match next_input {
							AnyTypeLValue::Int(l_value) => {
								let parsed_value = match next_input_text.parse() {
									Ok(parsed_value) => IntValue::new(Rc::new(parsed_value)),
									Err(_) => continue 'a,
								};
								self.execute_int_l_value_write(l_value, parsed_value)?;
							}
							AnyTypeLValue::Float(l_value) => {
								let parsed_value = match next_input_text.parse() {
									Ok(parsed_value) => FloatValue::new(parsed_value),
									Err(_) => continue 'a,
								};
								self.execute_float_l_value_write(l_value, parsed_value)?;
							}
							AnyTypeLValue::Complex(l_value) => {
								let parsed_value = match next_input_text.parse() {
									Ok(parsed_value) => ComplexValue::new(parsed_value),
									Err(_) => continue 'a,
								};
								self.execute_complex_l_value_write(l_value, parsed_value)?;
							}
							AnyTypeLValue::String(l_value) => {
								if next_input_text.contains('"') {
									// TODO: Quoted text input
									continue 'a;
								}
								self.execute_string_l_value_write(l_value, StringValue::new(Rc::new(next_input_text.into())))?;
							}
						}
					}
					if input_buffer_left.contains(|chr: char| !chr.is_ascii_whitespace()) {
						println!("Extra inputs ignored.");
					}
					break;
				}
			}
			StatementVariant::Goto(sub_expression) | StatementVariant::Run(sub_expression) => {
				// Set the line to be executed next
				match sub_expression {
					Some(sub_expression) => {
						let line_number_to_jump_to = self.execute_int_expression(sub_expression)?.value;
						self.set_line_executing(program, Some(line_number_to_jump_to), sub_expression.get_start_column())?;
					}
					None => self.set_line_executing(program, None, *column)?,
				}
				// Clear if this is a RUN statement
				if matches!(variant, StatementVariant::Run(..)) {
					self.clear_machine_state();
				}
				// Next statement
				return Ok(true);
			}
			StatementVariant::Gosub(_) => return Err(ErrorVariant::NotYetImplemented("GOSUB statement".into()).at_column(*column)),
			StatementVariant::AssignInt(l_value, r_value_expression) =>
				self.execute_int_l_value_write(l_value, self.execute_int_expression(r_value_expression)?)?,
			StatementVariant::AssignFloat(l_value, r_value_expression) =>
				self.execute_float_l_value_write(l_value, self.execute_float_expression(r_value_expression)?)?,
			StatementVariant::AssignComplex(l_value, r_value_expression) =>
				self.execute_complex_l_value_write(l_value, self.execute_complex_expression(r_value_expression)?)?,
			StatementVariant::AssignString(l_value, r_value_expression) =>
				self.execute_string_l_value_write(l_value, self.execute_string_expression(r_value_expression)?)?,
			StatementVariant::List(range_start, range_end) => {
				let range_start_value = match range_start {
					Some(range_start) => Some(&*self.execute_int_expression(range_start)?.value),
					None => None,
				};
				let range_end_value = match range_end {
					Some(range_end) => Some(&*self.execute_int_expression(range_end)?.value),
					None => None,
				};
				let range = match (range_start_value, range_end_value) {
					(None, None) => program.lines.range::<BigInt, RangeFull>(..),
					(Some(range_start_value), None) => program.lines.range::<BigInt, RangeFrom<&BigInt>>(range_start_value..),
					(None, Some(range_end_value)) => program.lines.range::<BigInt, RangeToInclusive<&BigInt>>(..=range_end_value),
					(Some(range_start_value), Some(range_end_value)) => program.lines.range::<BigInt, RangeInclusive<&BigInt>>(range_start_value..=range_end_value),
				};
				for (_line, (_statements, error, code_text)) in range {
					if let Some(_) = error {
						execute!(stdout(), PrintStyledContent(StyledContent::new(ContentStyle { foreground_color: Some(Color::Red), ..Default::default() }, format!("{code_text}\n")))).unwrap()
					}
					else {
						println!("{code_text}");
					}
				}
			}
			StatementVariant::OneLineIf { condition_expression: condition, then_statement, else_statement } => {
				// Execute condition
				let condition_value = self.execute_bool_expression(condition)?;
				// Execute statement
				if condition_value.value {
					return self.execute_statement(then_statement, program);
				}
				else {
					if let Some(else_statement) = else_statement {
						return self.execute_statement(else_statement, program);
					}
				}
			}
			StatementVariant::Option(option_variable_and_value) => {
				match option_variable_and_value {
					OptionVariableAndValue::ArithmeticDecimal | OptionVariableAndValue::ArithmeticNative => {},
					OptionVariableAndValue::Angle(angle_option) => self.angle_option = *angle_option,
					OptionVariableAndValue::Math(math_option) => self.math_option = *math_option,
				}
			}
		}
		Ok(false)
	}

	fn execute_int_expression(&self, expression: &IntExpression) -> Result<IntValue, Error> {
		Ok(match expression {
			IntExpression::ConstantValue { value, .. } => value.clone(),
			IntExpression::CastFromBool(sub_expression) => self.execute_bool_expression(sub_expression)?.to_int(),
			IntExpression::CastFromFloat(sub_expression) =>
				self.execute_float_expression(sub_expression)?.to_int().map_err(|error| error.at_column(sub_expression.get_start_column()))?,
			IntExpression::BitwiseAnd { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression)?.and(self.execute_int_expression(rhs_expression)?),
			IntExpression::BitwiseOr { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression)?.or(self.execute_int_expression(rhs_expression)?),
			IntExpression::Addition { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression)?.add(self.execute_int_expression(rhs_expression)?),
			IntExpression::Subtraction { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression)?.sub(self.execute_int_expression(rhs_expression)?),
			IntExpression::Multiplication { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression)?.mul(self.execute_int_expression(rhs_expression)?),
			IntExpression::FlooredDivision { lhs_expression, rhs_expression, start_column } =>
				self.execute_int_expression(lhs_expression)?.floored_div(self.execute_int_expression(rhs_expression)?)
					.map_err(|error| error.at_column(*start_column))?,
			IntExpression::BitwiseNot { sub_expression, .. } => self.execute_int_expression(sub_expression)?.not(),
			IntExpression::Negation { sub_expression, .. } => self.execute_int_expression(sub_expression)?.neg(),
			IntExpression::LValue(l_value) => self.execute_int_l_value_read(l_value)?,
		})
	}

	fn execute_float_expression(&self, expression: &FloatExpression) -> Result<FloatValue, Error> {
		Ok(match expression {
			FloatExpression::ConstantValue { value, .. } => value.clone(),
			FloatExpression::CastFromInt(sub_expression) => self.execute_int_expression(sub_expression)?.to_float(),
			FloatExpression::CastFromComplex(sub_expression) =>
				self.execute_complex_expression(sub_expression)?.to_float().map_err(|error| error.at_column(sub_expression.get_start_column()))?,
			FloatExpression::Addition { lhs_expression, rhs_expression, start_column } =>
				self.execute_float_expression(lhs_expression)?.add(self.execute_float_expression(rhs_expression)?, self.math_option == MathOption::Ieee)
					.map_err(|error| error.at_column(*start_column))?,
			FloatExpression::Subtraction { lhs_expression, rhs_expression, start_column } =>
				self.execute_float_expression(lhs_expression)?.sub(self.execute_float_expression(rhs_expression)?, self.math_option == MathOption::Ieee)
					.map_err(|error| error.at_column(*start_column))?,
			FloatExpression::Multiplication { lhs_expression, rhs_expression, start_column } =>
				self.execute_float_expression(lhs_expression)?.mul(self.execute_float_expression(rhs_expression)?, self.math_option == MathOption::Ieee)
					.map_err(|error| error.at_column(*start_column))?,
			FloatExpression::Division { lhs_expression, rhs_expression, start_column } =>
				self.execute_float_expression(lhs_expression)?
					.div(self.execute_float_expression(rhs_expression)?, self.math_option == MathOption::Ieee, self.math_option == MathOption::Ieee)
					.map_err(|error| error.at_column(*start_column))?,
			FloatExpression::Exponentiation { lhs_expression, rhs_expression, start_column } =>
				self.execute_float_expression(lhs_expression)?
					.pow(self.execute_float_expression(rhs_expression)?, self.math_option == MathOption::Ieee, self.math_option == MathOption::Ieee)
					.map_err(|error| error.at_column(*start_column))?,
			FloatExpression::Negation { sub_expression, .. } => self.execute_float_expression(&sub_expression)?.neg(),
			FloatExpression::LValue(l_value) => self.execute_float_l_value_read(l_value)?,
		})
	}

	fn execute_complex_expression(&self, expression: &ComplexExpression) -> Result<ComplexValue, Error> {
		Ok(match expression {
			ComplexExpression::ConstantValue { value, .. } => *value,
			ComplexExpression::CastFromFloat(sub_expression) => self.execute_float_expression(sub_expression)?.to_complex(),
			ComplexExpression::Addition { lhs_expression, rhs_expression, start_column } =>
				self.execute_complex_expression(lhs_expression)?.add(self.execute_complex_expression(rhs_expression)?, self.math_option == MathOption::Ieee)
					.map_err(|error| error.at_column(*start_column))?,
			ComplexExpression::Subtraction { lhs_expression, rhs_expression, start_column } =>
				self.execute_complex_expression(lhs_expression)?.sub(self.execute_complex_expression(rhs_expression)?, self.math_option == MathOption::Ieee)
					.map_err(|error| error.at_column(*start_column))?,
			ComplexExpression::Multiplication { lhs_expression, rhs_expression, start_column } =>
				self.execute_complex_expression(lhs_expression)?.mul(self.execute_complex_expression(rhs_expression)?, self.math_option == MathOption::Ieee)
					.map_err(|error| error.at_column(*start_column))?,
			ComplexExpression::Division { lhs_expression, rhs_expression, start_column } =>
				self.execute_complex_expression(lhs_expression)?
					.div(self.execute_complex_expression(rhs_expression)?, self.math_option == MathOption::Ieee, self.math_option == MathOption::Ieee)
					.map_err(|error| error.at_column(*start_column))?,
			ComplexExpression::Exponentiation { lhs_expression, rhs_expression, start_column } =>
				self.execute_complex_expression(lhs_expression)?.pow(self.execute_complex_expression(rhs_expression)?, self.math_option == MathOption::Ieee)
					.map_err(|error| error.at_column(*start_column))?,
			ComplexExpression::Negation { sub_expression, .. } => self.execute_complex_expression(sub_expression)?.neg(),
			ComplexExpression::LValue(l_value) => self.execute_complex_l_value_read(l_value)?,
		})
	}

	fn execute_bool_expression(&self, expression: &BoolExpression) -> Result<BoolValue, Error> {
		Ok(match expression {
			BoolExpression::ConstantValue { value, .. } => *value,
			BoolExpression::IntIsNonZero(int_expression) => BoolValue::new(!self.execute_int_expression(&int_expression)?.value.is_zero()),
			BoolExpression::FloatIsNonZero(float_expression) => BoolValue::new(!self.execute_float_expression(&float_expression)?.is_zero()),
			BoolExpression::ComplexIsNonZero(complex_expression) => BoolValue::new(!self.execute_complex_expression(&complex_expression)?.value.is_zero()),
			BoolExpression::StringIsNotEmpty(string_expression) => BoolValue::new(!self.execute_string_expression(&string_expression)?.value.is_empty()),

			BoolExpression::And { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression)?.and(self.execute_bool_expression(rhs_expression)?),
			BoolExpression::Or { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression)?.or(self.execute_bool_expression(rhs_expression)?),
			BoolExpression::Not { sub_expression, .. } => self.execute_bool_expression(sub_expression)?.not(),

			BoolExpression::BoolEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression)?.equal_to(self.execute_bool_expression(rhs_expression)?),
			BoolExpression::BoolNotEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression)?.not_equal_to(self.execute_bool_expression(rhs_expression)?),
			BoolExpression::BoolLessThan { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression)?.less_than(self.execute_bool_expression(rhs_expression)?),
			BoolExpression::BoolLessThanOrEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression)?.less_than_or_equal_to(self.execute_bool_expression(rhs_expression)?),
			BoolExpression::BoolGreaterThan { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression)?.greater_than(self.execute_bool_expression(rhs_expression)?),
			BoolExpression::BoolGreaterThanOrEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression)?.greater_than_or_equal_to(self.execute_bool_expression(rhs_expression)?),
			
			BoolExpression::IntEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression)?.equal_to(&self.execute_int_expression(rhs_expression)?),
			BoolExpression::IntNotEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression)?.not_equal_to(&self.execute_int_expression(rhs_expression)?),
			BoolExpression::IntLessThan { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression)?.less_than(&self.execute_int_expression(rhs_expression)?),
			BoolExpression::IntLessThanOrEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression)?.less_than_or_equal_to(&self.execute_int_expression(rhs_expression)?),
			BoolExpression::IntGreaterThan { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression)?.greater_than(&self.execute_int_expression(rhs_expression)?),
			BoolExpression::IntGreaterThanOrEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression)?.greater_than_or_equal_to(&self.execute_int_expression(rhs_expression)?),

			BoolExpression::FloatEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_float_expression(lhs_expression)?.equal_to(&self.execute_float_expression(rhs_expression)?),
			BoolExpression::FloatNotEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_float_expression(lhs_expression)?.not_equal_to(&self.execute_float_expression(rhs_expression)?),
			BoolExpression::FloatLessThan { lhs_expression, rhs_expression, .. } =>
				self.execute_float_expression(lhs_expression)?.less_than(&self.execute_float_expression(rhs_expression)?),
			BoolExpression::FloatLessThanOrEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_float_expression(lhs_expression)?.less_than_or_equal_to(&self.execute_float_expression(rhs_expression)?),
			BoolExpression::FloatGreaterThan { lhs_expression, rhs_expression, .. } =>
				self.execute_float_expression(lhs_expression)?.greater_than(&self.execute_float_expression(rhs_expression)?),
			BoolExpression::FloatGreaterThanOrEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_float_expression(lhs_expression)?.greater_than_or_equal_to(&self.execute_float_expression(rhs_expression)?),

			BoolExpression::ComplexEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_complex_expression(lhs_expression)?.equal_to(self.execute_complex_expression(rhs_expression)?),
			BoolExpression::ComplexNotEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_complex_expression(lhs_expression)?.not_equal_to(self.execute_complex_expression(rhs_expression)?),

			BoolExpression::StringEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_string_expression(lhs_expression)?.equal_to(&self.execute_string_expression(rhs_expression)?),
			BoolExpression::StringNotEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_string_expression(lhs_expression)?.not_equal_to(&self.execute_string_expression(rhs_expression)?),
			BoolExpression::StringLessThan { lhs_expression: _, rhs_expression: _, start_column } =>
				return Err(ErrorVariant::NotYetImplemented("String <, <=, >, >= operators".into()).at_column(*start_column)),
			BoolExpression::StringLessThanOrEqualTo { lhs_expression: _, rhs_expression: _, start_column } =>
				return Err(ErrorVariant::NotYetImplemented("String <, <=, >, >= operators".into()).at_column(*start_column)),
			BoolExpression::StringGreaterThan { lhs_expression: _, rhs_expression: _, start_column } =>
				return Err(ErrorVariant::NotYetImplemented("String <, <=, >, >= operators".into()).at_column(*start_column)),
			BoolExpression::StringGreaterThanOrEqualTo { lhs_expression: _, rhs_expression: _, start_column } =>
				return Err(ErrorVariant::NotYetImplemented("String <, <=, >, >= operators".into()).at_column(*start_column)),
		})
	}

	fn execute_string_expression(&self, expression: &StringExpression) -> Result<StringValue, Error> {
		Ok(match expression {
			StringExpression::ConstantValue { value, .. } => value.clone(),
			StringExpression::Concatenation { lhs_expression, rhs_expression, .. } =>
				self.execute_string_expression(lhs_expression)?.concat(self.execute_string_expression(rhs_expression)?),
			StringExpression::LValue(l_value) => self.execute_string_l_value_read(l_value)?,
		})
	}

	fn execute_any_type_expression(&self, expression: &AnyTypeExpression) -> Result<AnyTypeValue, Error> {
		Ok(match expression {
			AnyTypeExpression::Bool(expression) => AnyTypeValue::Bool(self.execute_bool_expression(expression)?),
			AnyTypeExpression::Int(expression) => AnyTypeValue::Int(self.execute_int_expression(expression)?),
			AnyTypeExpression::Float(expression) => AnyTypeValue::Float(self.execute_float_expression(expression)?),
			AnyTypeExpression::Complex(expression) => AnyTypeValue::Complex(self.execute_complex_expression(expression)?),
			AnyTypeExpression::String(expression) => AnyTypeValue::String(self.execute_string_expression(expression)?),
			//_ => unreachable!(),
		})
	}

	fn execute_int_l_value_read(&self, l_value: &IntLValue) -> Result<IntValue, Error> {
		// Unpack
		let IntLValue { name, arguments, /*uses_fn_keyword,*/ has_parentheses, start_column, supplied_function } = l_value;
		// If it is a user defined variable that has been defined, get it
		if !*has_parentheses /*&& !*uses_fn_keyword*/ && let Some(variable) = self.int_variables.get(name) {
			return Ok(variable.clone());
		}
		// Else try to execute a supplied (built-in) function
		if /* !*uses_fn_keyword && */ let Some(supplied_function) = supplied_function {
			match (supplied_function, arguments) {
				// SQR%(X)
				(SuppliedFunction::Sqr, arguments) if arguments.len() == 1 && arguments[0].is_numeric() => {
					let argument = &arguments[0];
					return match self.execute_any_type_expression(&arguments[0])?.to_int().map_err(|error| error.at_column(argument.get_start_column()))? {
						result if result.is_negative() => Err(ErrorVariant::IntSquareRootOfNegativeNumber.at_column(*start_column)),
						result => Ok(IntValue::new(Rc::new(result.value.sqrt())))
					};
				}
				// ABS%(X)
				(SuppliedFunction::Abs, arguments) if arguments.len() == 1 && arguments[0].is_numeric() => {
					let argument = &arguments[0];
					return Ok(IntValue::new(Rc::new(
						self.execute_any_type_expression(argument)?.to_int().map_err(|error| error.at_column(argument.get_start_column()))?.value.abs()
					)))
				}
				// TRUE%
				(SuppliedFunction::Abs, _) if !has_parentheses => return Ok(IntValue::new(Rc::new((-1i8).into()))),
				// FALSE%
				(SuppliedFunction::False, _) if !has_parentheses => return Ok(IntValue::zero()),
				// INT%(X)
				(SuppliedFunction::Int, arguments) if arguments.len() == 1 && arguments[0].is_numeric() =>
					return Ok({
						let argument = &arguments[0];
						let value = self.execute_any_type_expression(argument)?;
						match value {
							AnyTypeValue::Bool(_) | AnyTypeValue::Int(_) => value.to_int().map_err(|error| error.at_column(argument.get_start_column()))?,
							_ => {
								let float_value = value.to_float().map_err(|error| error.at_column(argument.get_start_column()))?.value.floor();
								match BigInt::from_f64(float_value) {
									Some(result) => IntValue::new(Rc::new(result)),
									None => return Err(ErrorVariant::NonNumberValueCastToInt(float_value).at_column(*start_column)),
								}
							}
						}
					}),
				// LEN%(X$)
				(SuppliedFunction::Len, arguments) if arguments.len() == 1 => match &arguments[0] {
					AnyTypeExpression::String(string_expression) =>
						return Ok(IntValue::new(Rc::new(BigInt::from_usize(self.execute_string_expression(string_expression)?.count_chars()).unwrap()))),
					_ => {},
				}
				// SGN%(X)
				(SuppliedFunction::Sgn, arguments) if arguments.len() == 1 && arguments[0].is_numeric() =>
					return Ok(IntValue::new(Rc::new({
						let argument = &arguments[0];
						let value = self.execute_any_type_expression(argument)?;
						match value {
							AnyTypeValue::Bool(_) | AnyTypeValue::Int(_) => match value.to_int().map_err(|error| error.at_column(argument.get_start_column()))?.value.sign() {
								Sign::Minus => (-1).into(),
								Sign::NoSign => 0.into(),
								Sign::Plus => 1.into(),
							},
							_ => {
								match value.to_float().map_err(|error| error.at_column(argument.get_start_column()))? {
									value if value.is_zero() => 0.into(),
									value if value.is_negative() => (-1).into(),
									value if value.is_positive() => 1.into(),
									value => return Err(ErrorVariant::NonNumberValueCastToInt(value.value).at_column(*start_column)),
								}
							}
						}
					}))),
				_ => {}
			}
		}
		// TODO
		if *has_parentheses /*|| *uses_fn_keyword*/ {
			return Err(ErrorVariant::NotYetImplemented("Arrays and user defined functions".into()).at_column(*start_column));
		}
		// Else return zero
		Ok(IntValue::zero())
	}

	fn execute_float_l_value_read(&self, l_value: &FloatLValue) -> Result<FloatValue, Error> {
		// Unpack
		let FloatLValue { name, arguments/*, uses_fn_keyword*/, has_parentheses, start_column, supplied_function } = l_value;
		// If it is a user defined variable that has been defined, get it
		if !*has_parentheses/* && !*uses_fn_keyword*/ && let Some(variable) = self.float_variables.get(name) {
			return Ok(variable.clone());
		}
		// Else try to execute a supplied (built-in) function
		if /* !*uses_fn_keyword && */ let Some(supplied_function) = supplied_function {
			match (supplied_function, arguments) {
				// SQR(X)
				(SuppliedFunction::Sqr, arguments) if arguments.len() == 1 && arguments[0].is_numeric() => {
					let argument = &arguments[0];
					return match self.execute_any_type_expression(argument)?.to_float().map_err(|error| error.at_column(argument.get_start_column()))? {
						// If the input is negative and that is not allowed
						value if value.is_negative() && self.real_square_root_of_negative_is_error() => Err(ErrorVariant::SquareRootOfNegative.at_column(*start_column)),
						// Else square root floats
						value => Ok(FloatValue::new(value.value.sqrt())),
					}
				}
				// ABS(X#)
				(SuppliedFunction::Abs, arguments) if arguments.len() == 1 && arguments[0].is_complex() => {
					let argument = &arguments[0];
					return self.execute_any_type_expression(argument)?.to_complex().map_err(|error| error.at_column(argument.get_start_column()))?
						.abs(!self.overflow_is_error()).map_err(|error| error.at_column(argument.get_start_column()));
				}
				// ABS(X)
				(SuppliedFunction::Abs, arguments) if arguments.len() == 1 && arguments[0].is_numeric() => {
					let argument = &arguments[0];
					return Ok(self.execute_any_type_expression(argument)?.to_float().map_err(|error| error.at_column(argument.get_start_column()))?.abs());
				}
				// LEN(X$)
				(SuppliedFunction::Len, arguments) if arguments.len() == 1 => match &arguments[0] {
					AnyTypeExpression::String(string_expression) =>
						return Ok(FloatValue::new(self.execute_string_expression(string_expression)?.count_chars() as f64)),
					_ => {},
				}
				// SGN(X)
				(SuppliedFunction::Sgn, arguments) if arguments.len() == 1 && arguments[0].is_numeric() =>
					return Ok(FloatValue::new({
						let argument = &arguments[0];
						let value = self.execute_any_type_expression(argument)?;
						match value {
							AnyTypeValue::Bool(_) | AnyTypeValue::Int(_) => match value.to_int().map_err(|error| error.at_column(argument.get_start_column()))?.value.sign() {
								Sign::Minus => -1.,
								Sign::NoSign => 0.,
								Sign::Plus => 1.,
							},
							_ => {
								match value.to_float().map_err(|error| error.at_column(argument.get_start_column()))? {
									value if value.is_zero() => 0.,
									value if value.is_negative() => -1.,
									value if value.is_positive() => 1.,
									value => return Err(ErrorVariant::NonNumberValueCastToInt(value.value).at_column(*start_column)),
								}
							}
						}
					})),
				_ => {}
			}
		}
		// TODO
		if *has_parentheses/* || *uses_fn_keyword*/ {
			return Err(ErrorVariant::NotYetImplemented("Arrays and user defined functions".into()).at_column(*start_column));
		}
		// Else return zero
		Ok(FloatValue::zero())
	}

	fn execute_complex_l_value_read(&self, l_value: &ComplexLValue) -> Result<ComplexValue, Error> {
		// Unpack
		let ComplexLValue { name, arguments/*, uses_fn_keyword*/, has_parentheses, start_column, supplied_function } = l_value;
		// If it is a user defined variable that has been defined, get it
		if !*has_parentheses/* && !*uses_fn_keyword*/ && let Some(variable) = self.complex_variables.get(name) {
			return Ok(variable.clone());
		}
		// Else try to execute a supplied (built-in) function
		if /* !*uses_fn_keyword &&*/ let Some(supplied_function) = supplied_function {
			match (supplied_function, arguments) {
				// SQR#(X)
				(SuppliedFunction::Sqr, arguments) if arguments.len() == 1 && arguments[0].is_numeric() => {
					let argument = &arguments[0];
					return self.execute_any_type_expression(argument)?.to_complex().map_err(|error| error.at_column(argument.get_start_column()));
				}
				_ => {}
			}
		}
		// TODO
		if *has_parentheses/* || *uses_fn_keyword*/ {
			return Err(ErrorVariant::NotYetImplemented("Arrays and user defined functions".into()).at_column(*start_column));
		}
		// Else return zero
		Ok(ComplexValue::zero())
	}

	fn execute_string_l_value_read(&self, l_value: &StringLValue) -> Result<StringValue, Error> {
		// Unpack
		let StringLValue { name, arguments/*, uses_fn_keyword*/, has_parentheses, start_column, supplied_function } = l_value;
		// If it is a user defined variable that has been defined, get it
		if !*has_parentheses /* && !*uses_fn_keyword*/ && let Some(variable) = self.string_variables.get(name) {
			return Ok(variable.clone());
		}
		// Else try to execute a supplied (built-in) function
		if /* !*uses_fn_keyword &&*/ let Some(supplied_function) = supplied_function {
			match (supplied_function, arguments) {
				_ => {}
			}
		}
		// TODO
		if *has_parentheses/* || *uses_fn_keyword*/ {
			return Err(ErrorVariant::NotYetImplemented("Arrays and user defined functions".into()).at_column(*start_column));
		}
		// Else return zero
		Ok(StringValue::empty())
	}

	fn execute_int_l_value_write(&mut self, l_value: &IntLValue, value: IntValue) -> Result<(), Error> {
		// Unpack
		let IntLValue { name, arguments, has_parentheses, start_column, .. } = l_value;
		// TODO
		if !arguments.is_empty() || *has_parentheses {
			return Err(ErrorVariant::NotYetImplemented("Arrays and user defined functions".into()).at_column(*start_column));
		}
		// Assign to global variable
		self.int_variables.insert(name.clone(), value);
		// Return
		Ok(())
	}

	fn execute_float_l_value_write(&mut self, l_value: &FloatLValue, value: FloatValue) -> Result<(), Error> {
		// Unpack
		let FloatLValue { name, arguments, has_parentheses, start_column, .. } = l_value;
		// TODO
		if !arguments.is_empty() || *has_parentheses {
			return Err(ErrorVariant::NotYetImplemented("Arrays and user defined functions".into()).at_column(*start_column));
		}
		// Assign to global variable
		self.float_variables.insert(name.clone(), value);
		// Return
		Ok(())
	}

	fn execute_complex_l_value_write(&mut self, l_value: &ComplexLValue, value: ComplexValue) -> Result<(), Error> {
		// Unpack
		let ComplexLValue { name, arguments, has_parentheses, start_column, .. } = l_value;
		// TODO
		if !arguments.is_empty() || *has_parentheses {
			return Err(ErrorVariant::NotYetImplemented("Arrays and user defined functions".into()).at_column(*start_column));
		}
		// Assign to global variable
		self.complex_variables.insert(name.clone(), value);
		// Return
		Ok(())
	}

	fn execute_string_l_value_write(&mut self, l_value: &StringLValue, value: StringValue) -> Result<(), Error> {
		// Unpack
		let StringLValue { name, arguments, has_parentheses, start_column, .. } = l_value;
		// TODO
		if !arguments.is_empty() || *has_parentheses {
			return Err(ErrorVariant::NotYetImplemented("Arrays and user defined functions".into()).at_column(*start_column));
		}
		// Assign to global variable
		self.string_variables.insert(name.clone(), value);
		// Return
		Ok(())
	}
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum ExecutionSource {
	Program,
	DirectModeLine,
	ProgramEnded,
}