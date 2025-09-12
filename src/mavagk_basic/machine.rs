use std::{collections::HashMap, io::stdout, num::NonZeroUsize, ops::{RangeFrom, RangeFull, RangeInclusive, RangeToInclusive}, rc::Rc};

use crossterm::{execute, style::{Color, ContentStyle, PrintStyledContent, StyledContent}};
use num::{BigInt, Zero};

use crate::mavagk_basic::{abstract_syntax_tree::{AngleOption, AnyTypeExpression, BoolExpression, ComplexExpression, ComplexLValue, IntExpression, IntLValue, MathOption, OptionVariableAndValue, RealExpression, RealLValue, Statement, StatementVariant, StringExpression, StringLValue}, error::{handle_error, Error, ErrorVariant}, exception::Exception, optimize::optimize_statement, parse::parse_line, program::Program, token::{SuppliedFunction, Token}, value::{int_to_float, AnyTypeValue, BoolValue, ComplexValue, IntValue, RealValue, StringValue}};

pub struct Machine {
	// Program counter
	line_executing: Option<Rc<BigInt>>,
	execution_source: ExecutionSource,
	// Variables
	real_variables: HashMap<Box<str>, RealValue>,
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
			real_variables: HashMap::new(),
			complex_variables: HashMap::new(),
			string_variables: HashMap::new(),
			angle_option: AngleOption::Gradians,
			math_option: MathOption::Ansi,
		}
	}

	fn set_line_executing(&mut self, program: &Program, goto_line_number: Option<Rc<BigInt>>, line_number_executed_in: Option<&BigInt>, column_number: NonZeroUsize) -> Result<(), Error> {
		self.line_executing = match goto_line_number {
			Some(goto_line_number) =>{
				if !program.lines.contains_key(&goto_line_number) {
					return Err(Error {
						variant: ErrorVariant::InvalidLineNumber((*goto_line_number).clone()), line_number: line_number_executed_in.cloned(), column_number: Some(column_number), line_text: None
					});
				}
				Some(goto_line_number)
			}
			None => None,
		};
		self.execution_source = ExecutionSource::Program;
		Ok(())
	}

	fn clear_machine_state(&mut self) {
		// TODO
		self.int_variables = HashMap::new();
		self.real_variables = HashMap::new();
		self.complex_variables = HashMap::new();
		self.string_variables = HashMap::new();
	}

	pub fn line_of_text_entered(&mut self, line_text: Box<str>, program: &mut Program) -> Result<(), Error> {
		// Parse line
		let (line_number, tokens, error) = match Token::tokenize_line(&*line_text) {
			(line_number, Ok(tokens)) => (line_number, tokens, None),
			(line_number, Err(error)) => (line_number, Box::default(), Some(error)),
		};
		let (mut statements, error) = match error {
			None => parse_line(&*tokens, line_number.as_ref()),
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
					let mut error = error.clone();
					error.line_text = Some(line_text.clone().into());
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
				if let Some(mut error) = error {
					error.line_text = Some(line_text.into());
					return Err(error);
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

	fn execute(&mut self, program: &mut Program, direct_mode_statements: &Box<[Statement]>, direct_mode_line_text: &str) -> Result<(), Error> {
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
						let flow_control_used = match self.execute_statement(statement, Some(&line_number), program) {
							Ok(flow_control_used) => flow_control_used,
							Err(mut error) => {
								error.line_text = Some(line_text.clone().into_string());
								return Err(error);
							}
						};
						if flow_control_used || self.execution_source != ExecutionSource::Program {
							continue 'lines_loop;
						}
					}
					// If there is an error at the end of the line, throw the error
					if let Some(line_error) = line_error {
						let mut line_error = line_error.clone();
						line_error.line_text = Some(line_text.clone().into_string());
						return Err(line_error);
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
							Err(mut error) => {
								error.line_text = Some(direct_mode_line_text.into());
								return Err(error);
							}
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
			_ => self.execute_statement(statement, None, program),
		}
	}

	fn execute_statement(&mut self, statement: &Statement, line_number: Option<&BigInt>, program: &Program) -> Result<bool, Error> {
		let Statement { variant, column } = &statement;
		match variant {
			StatementVariant::Print(sub_expressions) => {
				for sub_expression in sub_expressions {
					match sub_expression {
						AnyTypeExpression::Bool(sub_expression) => print!("{}", self.execute_bool_expression(sub_expression, line_number)?),
						AnyTypeExpression::Int(sub_expression) => print!("{}", self.execute_int_expression(sub_expression, line_number)?),
						AnyTypeExpression::Real(sub_expression) => print!("{}", self.execute_real_expression(sub_expression, line_number)?),
						AnyTypeExpression::Complex(sub_expression) => print!("{}", self.execute_complex_expression(sub_expression, line_number)?),
						AnyTypeExpression::String(sub_expression) => print!("{}", self.execute_string_expression(sub_expression, line_number)?),
						AnyTypeExpression::PrintComma(sub_expression_column) | AnyTypeExpression::PrintSemicolon(sub_expression_column) =>
							return Err(Error {
								variant: ErrorVariant::NotYetImplemented(", and ; in PRINT statement".into()), line_number: line_number.cloned(), column_number: Some(*sub_expression_column),
								line_text: None
							}),
					}
				}
				println!();
			}
			StatementVariant::Goto(sub_expression) | StatementVariant::Run(sub_expression) => {
				// Set the line to be executed next
				match sub_expression {
					Some(sub_expression) => {
						let line_number_to_jump_to = self.execute_int_expression(sub_expression, line_number)?.value;
						self.set_line_executing(program, Some(line_number_to_jump_to), line_number, sub_expression.get_start_column())?;
					}
					None => self.set_line_executing(program, None, line_number, *column)?,
				}
				// Clear if this is a RUN statement
				if matches!(variant, StatementVariant::Run(..)) {
					self.clear_machine_state();
				}
				// Next statement
				return Ok(true);
			}
			StatementVariant::Gosub(_) => return Err(Error { variant: ErrorVariant::NotYetImplemented("GOSUB statement".into()), line_number: line_number.cloned(), column_number: Some(*column), line_text: None }),
			StatementVariant::AssignInt(l_value, r_value_expression) => {
				// Get what to assign to
				let IntLValue { name, arguments: _, uses_fn_keyword: _, has_parentheses, start_column: _, .. } = l_value;
				// Get r-value
				let r_value = Self::execute_int_expression(&self, r_value_expression, line_number)?;
				// Assign
				if *has_parentheses {
					return Err(Error { variant: ErrorVariant::NotYetImplemented("Arrays".into()), line_number: line_number.cloned(), column_number: Some(*column), line_text: None });
				}
				self.int_variables.insert(name.clone(), r_value);
			}
			StatementVariant::AssignReal(l_value, r_value_expression) => {
				// Get what to assign to
				let RealLValue { name, arguments: _, uses_fn_keyword: _, has_parentheses, start_column: _, .. } = l_value;
				// Get r-value
				let r_value = Self::execute_real_expression(&self, r_value_expression, line_number)?;
				// Assign
				if *has_parentheses {
					return Err(Error { variant: ErrorVariant::NotYetImplemented("Arrays".into()), line_number: line_number.cloned(), column_number: Some(*column), line_text: None });
				}
				self.real_variables.insert(name.clone(), r_value);
			}
			StatementVariant::AssignComplex(l_value, r_value_expression) => {
				// Get what to assign to
				let ComplexLValue { name, arguments: _, uses_fn_keyword: _, has_parentheses, start_column: _, .. } = l_value;
				// Get r-value
				let r_value = Self::execute_complex_expression(&self, r_value_expression, line_number)?;
				// Assign
				if *has_parentheses {
					return Err(Error { variant: ErrorVariant::NotYetImplemented("Arrays".into()), line_number: line_number.cloned(), column_number: Some(*column), line_text: None });
				}
				self.complex_variables.insert(name.clone(), r_value);
			}
			StatementVariant::AssignString(l_value, r_value_expression) => {
				// Get what to assign to
				let StringLValue { name, arguments: _, uses_fn_keyword: _, has_parentheses, start_column: _, .. } = l_value;
				// Get r-value
				let r_value = Self::execute_string_expression(&self, r_value_expression, line_number)?;
				// Assign
				if *has_parentheses {
					return Err(Error { variant: ErrorVariant::NotYetImplemented("Arrays".into()), line_number: line_number.cloned(), column_number: Some(*column), line_text: None });
				}
				self.string_variables.insert(name.clone(), r_value);
			}
			StatementVariant::List(range_start, range_end) => {
				let range_start_value = match range_start {
					Some(range_start) => Some(&*self.execute_int_expression(range_start, line_number)?.value),
					None => None,
				};
				let range_end_value = match range_end {
					Some(range_end) => Some(&*self.execute_int_expression(range_end, line_number)?.value),
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
				let condition_value = self.execute_bool_expression(condition, line_number)?;
				// Execute statement
				if condition_value.value {
					return self.execute_statement(then_statement, line_number, program);
				}
				else {
					if let Some(else_statement) = else_statement {
						return self.execute_statement(else_statement, line_number, program);
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

	fn execute_int_expression(&self, expression: &IntExpression, line_number: Option<&BigInt>) -> Result<IntValue, Error> {
		Ok(match expression {
			IntExpression::ConstantValue { value, .. } => value.clone(),
			IntExpression::CastFromBool(sub_expression) => self.execute_bool_expression(sub_expression, line_number)?.to_int(),
			IntExpression::CastFromReal(sub_expression) => self.execute_real_expression(sub_expression, line_number)?.to_int(line_number, sub_expression.get_start_column())?,
			IntExpression::BitwiseAnd { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression, line_number)?.and(self.execute_int_expression(rhs_expression, line_number)?),
			IntExpression::BitwiseOr { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression, line_number)?.or(self.execute_int_expression(rhs_expression, line_number)?),
			IntExpression::BitwiseNot { sub_expression, .. } => self.execute_int_expression(sub_expression, line_number)?.not(),
			IntExpression::LValue(l_value) => self.execute_int_l_value_read(l_value, line_number)?,
		})
	}

	fn execute_real_expression(&self, expression: &RealExpression, line_number: Option<&BigInt>) -> Result<RealValue, Error> {
		Ok(match expression {
			RealExpression::ConstantValue { value, .. } => value.clone(),
			RealExpression::CastFromInt(sub_expression) => self.execute_int_expression(sub_expression, line_number)?.to_real(),
			RealExpression::CastFromComplex(sub_expression) =>
				self.execute_complex_expression(sub_expression, line_number)?.to_real(line_number, sub_expression.get_start_column())?,
			RealExpression::Addition { lhs_expression, rhs_expression, .. } =>
				match self.execute_real_expression(lhs_expression, line_number)?.add(self.execute_real_expression(rhs_expression, line_number)?, self.math_option == MathOption::Ieee) {
					Some(result) => result,
					None => return Err(Error { variant: ErrorVariant::Exception(Exception::ValueOverflow), line_number: line_number.cloned(), column_number: Some(expression.get_start_column()), line_text: None }),
				}
			RealExpression::Subtraction { lhs_expression, rhs_expression, .. } =>
				match self.execute_real_expression(lhs_expression, line_number)?.sub(self.execute_real_expression(rhs_expression, line_number)?, self.math_option == MathOption::Ieee) {
					Some(result) => result,
					None => return Err(Error { variant: ErrorVariant::Exception(Exception::ValueOverflow), line_number: line_number.cloned(), column_number: Some(expression.get_start_column()), line_text: None }),
				}
			RealExpression::Multiplication { lhs_expression, rhs_expression, .. } =>
				match self.execute_real_expression(lhs_expression, line_number)?.mul(self.execute_real_expression(rhs_expression, line_number)?, self.math_option == MathOption::Ieee) {
					Some(result) => result,
					None => return Err(Error { variant: ErrorVariant::Exception(Exception::ValueOverflow), line_number: line_number.cloned(), column_number: Some(expression.get_start_column()), line_text: None }),
				}
			RealExpression::Division { lhs_expression, rhs_expression, .. } => {
				let rhs = self.execute_real_expression(rhs_expression, line_number)?;
				let rhs_is_zero = rhs.is_zero();
				match self.execute_real_expression(lhs_expression, line_number)?.div(rhs, self.math_option == MathOption::Ieee) {
					Some(result) => result,
					None => match rhs_is_zero {
						true => return Err(Error {
							variant: ErrorVariant::Exception(Exception::DivisionByZero), line_number: line_number.cloned(), column_number: Some(expression.get_start_column()), line_text: None
						}),
						false => return Err(Error {
							variant: ErrorVariant::Exception(Exception::ValueOverflow), line_number: line_number.cloned(), column_number: Some(expression.get_start_column()), line_text: None
						}),
					}
				}
			}
			RealExpression::Exponentiation { lhs_expression, rhs_expression, .. } => {
				let lhs_value = self.execute_real_expression(lhs_expression, line_number)?;
				let lhs_is_negative = lhs_value.is_negative();
				let rhs_value = self.execute_real_expression(rhs_expression, line_number)?;
				let rhs_is_int = lhs_value.is_integer();
				match lhs_value.pow(rhs_value, self.math_option == MathOption::Ieee) {
					Some(result) => result,
					None => match lhs_is_negative && rhs_is_int {
						true => return Err(Error {
							variant: ErrorVariant::Exception(Exception::NegativeNumberRaisedToNonIntegerPower), line_number: line_number.cloned(), column_number: Some(expression.get_start_column()), line_text: None
						}),
						false => return Err(Error {
							variant: ErrorVariant::Exception(Exception::ValueOverflow), line_number: line_number.cloned(), column_number: Some(expression.get_start_column()), line_text: None
						}),
					}
				}
			}
			RealExpression::Negation { sub_expression, .. } => self.execute_real_expression(&sub_expression, line_number)?.neg(),
			RealExpression::FlooredDivision { lhs_expression, rhs_expression, start_column } =>
				match self.execute_int_expression(lhs_expression, line_number)?.floored_div(self.execute_int_expression(rhs_expression, line_number)?) {
					Some(result) => result,
					None => return Err(Error { variant: ErrorVariant::FlooredDivisionByZero, line_number: line_number.cloned(), column_number: Some(*start_column), line_text: None }),
				}
			RealExpression::LValue(l_value) => self.execute_real_l_value_read(l_value, line_number)?,
		})
	}

	fn execute_complex_expression(&self, expression: &ComplexExpression, line_number: Option<&BigInt>) -> Result<ComplexValue, Error> {
		Ok(match expression {
			ComplexExpression::ConstantValue { value, .. } => *value,
			ComplexExpression::CastFromReal(sub_expression) => self.execute_real_expression(sub_expression, line_number)?.to_complex(),
			ComplexExpression::Addition { lhs_expression, rhs_expression, .. } =>
				match self.execute_complex_expression(lhs_expression, line_number)?.add(self.execute_complex_expression(rhs_expression, line_number)?, self.math_option == MathOption::Ieee) {
					Some(result) => result,
					None => return Err(Error { variant: ErrorVariant::Exception(Exception::ValueOverflow), line_number: line_number.cloned(), column_number: Some(expression.get_start_column()), line_text: None }),
				}
			ComplexExpression::Subtraction { lhs_expression, rhs_expression, .. } =>
				match self.execute_complex_expression(lhs_expression, line_number)?.sub(self.execute_complex_expression(rhs_expression, line_number)?, self.math_option == MathOption::Ieee) {
					Some(result) => result,
					None => return Err(Error { variant: ErrorVariant::Exception(Exception::ValueOverflow), line_number: line_number.cloned(), column_number: Some(expression.get_start_column()), line_text: None }),
				}
			ComplexExpression::Multiplication { lhs_expression, rhs_expression, .. } =>
				match self.execute_complex_expression(lhs_expression, line_number)?.mul(self.execute_complex_expression(rhs_expression, line_number)?, self.math_option == MathOption::Ieee) {
					Some(result) => result,
					None => return Err(Error { variant: ErrorVariant::Exception(Exception::ValueOverflow), line_number: line_number.cloned(), column_number: Some(expression.get_start_column()), line_text: None }),
				}
			ComplexExpression::Division { lhs_expression, rhs_expression, .. } => {
				let rhs_value = self.execute_complex_expression(rhs_expression, line_number)?;
				match self.execute_complex_expression(lhs_expression, line_number)?.div(rhs_value, self.math_option == MathOption::Ieee) {
					Some(result) => result,
					None => return Err(Error {
						variant: match rhs_value.is_zero() {
							true => ErrorVariant::Exception(Exception::DivisionByZero),
							false => ErrorVariant::Exception(Exception::ValueOverflow)
						},
						line_number: line_number.cloned(), column_number: Some(expression.get_start_column()), line_text: None
					}),
				}
			}
			ComplexExpression::Exponentiation { lhs_expression, rhs_expression, .. } =>
				match self.execute_complex_expression(lhs_expression, line_number)?.pow(self.execute_complex_expression(rhs_expression, line_number)?, self.math_option == MathOption::Ieee) {
					Some(result) => result,
					None => return Err(Error { variant: ErrorVariant::Exception(Exception::ValueOverflow), line_number: line_number.cloned(), column_number: Some(expression.get_start_column()), line_text: None }),
				}
			ComplexExpression::Negation { sub_expression, .. } => self.execute_complex_expression(sub_expression, line_number)?.neg(),
			ComplexExpression::LValue(l_value) => self.execute_complex_l_value_read(l_value, line_number)?,
		})
	}

	fn execute_bool_expression(&self, expression: &BoolExpression, line_number: Option<&BigInt>) -> Result<BoolValue, Error> {
		Ok(match expression {
			BoolExpression::ConstantValue { value, .. } => *value,
			BoolExpression::IntIsNonZero(int_expression) => BoolValue::new(!self.execute_int_expression(&int_expression, line_number)?.value.is_zero()),
			BoolExpression::RealIsNonZero(real_expression) => BoolValue::new(!self.execute_real_expression(&real_expression, line_number)?.is_zero()),
			BoolExpression::ComplexIsNonZero(complex_expression) => BoolValue::new(!self.execute_complex_expression(&complex_expression, line_number)?.value.is_zero()),
			BoolExpression::StringIsNotEmpty(string_expression) => BoolValue::new(!self.execute_string_expression(&string_expression, line_number)?.value.is_empty()),

			BoolExpression::And { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression, line_number)?.and(self.execute_bool_expression(rhs_expression, line_number)?),
			BoolExpression::Or { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression, line_number)?.or(self.execute_bool_expression(rhs_expression, line_number)?),
			BoolExpression::Not { sub_expression, .. } => self.execute_bool_expression(sub_expression, line_number)?.not(),

			BoolExpression::BoolEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression, line_number)?.equal_to(self.execute_bool_expression(rhs_expression, line_number)?),
			BoolExpression::BoolNotEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression, line_number)?.not_equal_to(self.execute_bool_expression(rhs_expression, line_number)?),
			BoolExpression::BoolLessThan { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression, line_number)?.less_than(self.execute_bool_expression(rhs_expression, line_number)?),
			BoolExpression::BoolLessThanOrEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression, line_number)?.less_than_or_equal_to(self.execute_bool_expression(rhs_expression, line_number)?),
			BoolExpression::BoolGreaterThan { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression, line_number)?.greater_than(self.execute_bool_expression(rhs_expression, line_number)?),
			BoolExpression::BoolGreaterThanOrEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression, line_number)?.greater_than_or_equal_to(self.execute_bool_expression(rhs_expression, line_number)?),
			
			BoolExpression::IntEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression, line_number)?.equal_to(&self.execute_int_expression(rhs_expression, line_number)?),
			BoolExpression::IntNotEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression, line_number)?.not_equal_to(&self.execute_int_expression(rhs_expression, line_number)?),
			BoolExpression::IntLessThan { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression, line_number)?.less_than(&self.execute_int_expression(rhs_expression, line_number)?),
			BoolExpression::IntLessThanOrEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression, line_number)?.less_than_or_equal_to(&self.execute_int_expression(rhs_expression, line_number)?),
			BoolExpression::IntGreaterThan { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression, line_number)?.greater_than(&self.execute_int_expression(rhs_expression, line_number)?),
			BoolExpression::IntGreaterThanOrEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression, line_number)?.greater_than_or_equal_to(&self.execute_int_expression(rhs_expression, line_number)?),

			BoolExpression::RealEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_real_expression(lhs_expression, line_number)?.equal_to(&self.execute_real_expression(rhs_expression, line_number)?),
			BoolExpression::RealNotEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_real_expression(lhs_expression, line_number)?.not_equal_to(&self.execute_real_expression(rhs_expression, line_number)?),
			BoolExpression::RealLessThan { lhs_expression, rhs_expression, .. } =>
				self.execute_real_expression(lhs_expression, line_number)?.less_than(&self.execute_real_expression(rhs_expression, line_number)?),
			BoolExpression::RealLessThanOrEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_real_expression(lhs_expression, line_number)?.less_than_or_equal_to(&self.execute_real_expression(rhs_expression, line_number)?),
			BoolExpression::RealGreaterThan { lhs_expression, rhs_expression, .. } =>
				self.execute_real_expression(lhs_expression, line_number)?.greater_than(&self.execute_real_expression(rhs_expression, line_number)?),
			BoolExpression::RealGreaterThanOrEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_real_expression(lhs_expression, line_number)?.greater_than_or_equal_to(&self.execute_real_expression(rhs_expression, line_number)?),

			BoolExpression::ComplexEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_complex_expression(lhs_expression, line_number)?.equal_to(self.execute_complex_expression(rhs_expression, line_number)?),
			BoolExpression::ComplexNotEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_complex_expression(lhs_expression, line_number)?.not_equal_to(self.execute_complex_expression(rhs_expression, line_number)?),

			BoolExpression::StringEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_string_expression(lhs_expression, line_number)?.equal_to(&self.execute_string_expression(rhs_expression, line_number)?),
			BoolExpression::StringNotEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_string_expression(lhs_expression, line_number)?.not_equal_to(&self.execute_string_expression(rhs_expression, line_number)?),
			BoolExpression::StringLessThan { lhs_expression: _, rhs_expression: _, start_column } =>
				return Err(Error { variant: ErrorVariant::NotYetImplemented("String <, <=, >, >= operators".into()), line_number: line_number.cloned(), column_number: Some(*start_column), line_text: None }),
			BoolExpression::StringLessThanOrEqualTo { lhs_expression: _, rhs_expression: _, start_column } =>
				return Err(Error { variant: ErrorVariant::NotYetImplemented("String <, <=, >, >= operators".into()), line_number: line_number.cloned(), column_number: Some(*start_column), line_text: None }),
			BoolExpression::StringGreaterThan { lhs_expression: _, rhs_expression: _, start_column } =>
				return Err(Error { variant: ErrorVariant::NotYetImplemented("String <, <=, >, >= operators".into()), line_number: line_number.cloned(), column_number: Some(*start_column), line_text: None }),
			BoolExpression::StringGreaterThanOrEqualTo { lhs_expression: _, rhs_expression: _, start_column } =>
				return Err(Error { variant: ErrorVariant::NotYetImplemented("String <, <=, >, >= operators".into()), line_number: line_number.cloned(), column_number: Some(*start_column), line_text: None }),
		})
	}

	fn execute_string_expression(&self, expression: &StringExpression, line_number: Option<&BigInt>) -> Result<StringValue, Error> {
		Ok(match expression {
			StringExpression::ConstantValue { value, .. } => value.clone(),
			StringExpression::Concatenation { lhs_expression, rhs_expression, .. } =>
				self.execute_string_expression(lhs_expression, line_number)?.concat(self.execute_string_expression(rhs_expression, line_number)?),
			StringExpression::LValue(l_value) => self.execute_string_l_value_read(l_value, line_number)?,
		})
	}

	fn execute_any_type_expression(&self, expression: &AnyTypeExpression, line_number: Option<&BigInt>) -> Result<AnyTypeValue, Error> {
		Ok(match expression {
			AnyTypeExpression::Bool(expression) => AnyTypeValue::Bool(self.execute_bool_expression(expression, line_number)?),
			AnyTypeExpression::Int(expression) => AnyTypeValue::Int(self.execute_int_expression(expression, line_number)?),
			AnyTypeExpression::Real(expression) => AnyTypeValue::Real(self.execute_real_expression(expression, line_number)?),
			AnyTypeExpression::Complex(expression) => AnyTypeValue::Complex(self.execute_complex_expression(expression, line_number)?),
			AnyTypeExpression::String(expression) => AnyTypeValue::String(self.execute_string_expression(expression, line_number)?),
			_ => unreachable!(),
		})
	}

	fn execute_int_l_value_read(&self, l_value: &IntLValue, line_number: Option<&BigInt>) -> Result<IntValue, Error> {
		// Unpack
		let IntLValue { name, arguments, uses_fn_keyword, has_parentheses, start_column, supplied_function } = l_value;
		// If it is a user defined variable that has been defined, get it
		if !*has_parentheses && !*uses_fn_keyword && let Some(variable) = self.int_variables.get(name) {
			return Ok(variable.clone());
		}
		// Else try to execute a supplied (built-in) function
		if !*uses_fn_keyword && let Some(supplied_function) = supplied_function {
			match (supplied_function, arguments) {
				// SQR%(X)
				(SuppliedFunction::Sqr, arguments) if arguments.len() == 1 && arguments[0].is_numeric() =>
					return match self.execute_any_type_expression(&arguments[0], line_number)?.to_int(line_number, *start_column)? {
						result if result.is_negative() =>
							Err(Error { variant: ErrorVariant::IntSquareRootOfNegativeNumber, line_number: line_number.cloned(), column_number: Some(*start_column), line_text: None }),
						result => Ok(IntValue::new(Rc::new(result.value.sqrt())))
					},
				_ => {}
			}
		}
		// TODO
		if *has_parentheses || *uses_fn_keyword {
			return Err(Error { variant: ErrorVariant::NotYetImplemented("Arrays and user defined functions".into()), line_number: line_number.cloned(), column_number: Some(*start_column), line_text: None });
		}
		// Else return zero
		Ok(IntValue::zero())
	}

	fn execute_real_l_value_read(&self, l_value: &RealLValue, line_number: Option<&BigInt>) -> Result<RealValue, Error> {
		// Unpack
		let RealLValue { name, arguments, uses_fn_keyword, has_parentheses, start_column, supplied_function } = l_value;
		// If it is a user defined variable that has been defined, get it
		if !*has_parentheses && !*uses_fn_keyword && let Some(variable) = self.real_variables.get(name) {
			return Ok(variable.clone());
		}
		// Else try to execute a supplied (built-in) function
		if !*uses_fn_keyword && let Some(supplied_function) = supplied_function {
			match (supplied_function, arguments) {
				// SQR(X)
				(SuppliedFunction::Sqr, arguments) if arguments.len() == 1 && arguments[0].is_numeric() =>
					return match self.execute_any_type_expression(&arguments[0], line_number)?.to_real(line_number, *start_column)? {
						// If the input is negative and that is not allowed
						value if value.is_negative() && self.real_square_root_of_negative_is_error() =>
							Err(Error { variant: ErrorVariant::Exception(Exception::SquareRootOfNegative), line_number: line_number.cloned(), column_number: Some(*start_column), line_text: None }),
						// Else square root floats
						RealValue::FloatValue(value) => Ok(RealValue::FloatValue(value.sqrt())),
						// Try to get the exact integer square root of an integer
						RealValue::IntValue(value) => {
							let floored_sqrt = value.sqrt();
							match (&*value) == &(floored_sqrt.pow(2u32)) {
								// If it exists
								true => Ok(RealValue::IntValue(Rc::new(floored_sqrt))),
								// Else get the float square root
								false => match int_to_float(&value).sqrt() {
									// If the integer input is larger than what can be stored in a float and overflow is an error
									value if !value.is_finite() && self.overflow_is_error() =>
										Err(Error { variant: ErrorVariant::Exception(Exception::ValueOverflow), line_number: line_number.cloned(), column_number: Some(*start_column), line_text: None }),
									// Else get the float square root
									value => Ok(RealValue::FloatValue(value)),
								}
							}
						}
					},
				_ => {}
			}
		}
		// TODO
		if *has_parentheses || *uses_fn_keyword {
			return Err(Error { variant: ErrorVariant::NotYetImplemented("Arrays and user defined functions".into()), line_number: line_number.cloned(), column_number: Some(*start_column), line_text: None });
		}
		// Else return zero
		Ok(RealValue::zero())
	}

	fn execute_complex_l_value_read(&self, l_value: &ComplexLValue, line_number: Option<&BigInt>) -> Result<ComplexValue, Error> {
		// Unpack
		let ComplexLValue { name, arguments, uses_fn_keyword, has_parentheses, start_column, supplied_function } = l_value;
		// If it is a user defined variable that has been defined, get it
		if !*has_parentheses && !*uses_fn_keyword && let Some(variable) = self.complex_variables.get(name) {
			return Ok(variable.clone());
		}
		// Else try to execute a supplied (built-in) function
		if !*uses_fn_keyword && let Some(supplied_function) = supplied_function {
			match (supplied_function, arguments) {
				// SQR#(X)
				(SuppliedFunction::Sqr, arguments) if arguments.len() == 1 && arguments[0].is_numeric() =>
					return match self.execute_any_type_expression(&arguments[0], line_number)?.to_complex(line_number, *start_column)?.value.sqrt() {
						value if !value.is_finite() && self.overflow_is_error() =>
							Err(Error { variant: ErrorVariant::Exception(Exception::ValueOverflow), line_number: line_number.cloned(), column_number: Some(*start_column), line_text: None }),
						value => Ok(ComplexValue::new(value)),
					},
				_ => {}
			}
		}
		// TODO
		if *has_parentheses || *uses_fn_keyword {
			return Err(Error { variant: ErrorVariant::NotYetImplemented("Arrays and user defined functions".into()), line_number: line_number.cloned(), column_number: Some(*start_column), line_text: None });
		}
		// Else return zero
		Ok(ComplexValue::zero())
	}

	fn execute_string_l_value_read(&self, l_value: &StringLValue, line_number: Option<&BigInt>) -> Result<StringValue, Error> {
		// Unpack
		let StringLValue { name, arguments, uses_fn_keyword, has_parentheses, start_column, supplied_function } = l_value;
		// If it is a user defined variable that has been defined, get it
		if !*has_parentheses && !*uses_fn_keyword && let Some(variable) = self.string_variables.get(name) {
			return Ok(variable.clone());
		}
		// Else try to execute a supplied (built-in) function
		if !*uses_fn_keyword && let Some(supplied_function) = supplied_function {
			match (supplied_function, arguments) {
				_ => {}
			}
		}
		// TODO
		if *has_parentheses || *uses_fn_keyword {
			return Err(Error { variant: ErrorVariant::NotYetImplemented("Arrays and user defined functions".into()), line_number: line_number.cloned(), column_number: Some(*start_column), line_text: None });
		}
		// Else return zero
		Ok(StringValue::empty())
	}
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum ExecutionSource {
	Program,
	DirectModeLine,
	ProgramEnded,
}