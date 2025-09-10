use std::{collections::HashMap, io::stdout, num::NonZeroUsize, ops::{RangeFrom, RangeFull, RangeInclusive, RangeToInclusive}, rc::Rc};

use crossterm::{execute, style::{Color, ContentStyle, PrintStyledContent, StyledContent}};
use num::{BigInt, Complex, FromPrimitive, Integer, BigUint, Zero, Signed};
use num_traits::Pow;

use crate::mavagk_basic::{abstract_syntax_tree::{AngleOption, AnyTypeExpression, BoolExpression, ComplexExpression, ComplexLValue, IntExpression, IntLValue, MathOption, OptionVariableAndValue, RealExpression, RealLValue, Statement, StatementVariant, StringExpression, StringLValue}, error::{handle_error, Error, ErrorVariant}, exception::Exception, parse::parse_line, program::Program, token::{SuppliedFunction, Token}, value::{int_to_float, AnyTypeValue, BoolValue, ComplexValue, IntValue, RealValue, StringValue}};

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
		let (statements, error) = match error {
			None => parse_line(&*tokens, line_number.as_ref()),
			Some(error) => (Box::default(), Some(error)),
		};
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
			IntExpression::CastFromBool(sub_expression) => IntValue {
				value: Rc::new(match Self::execute_bool_expression(&self, sub_expression, line_number)?.value {
					true => -1,
					false => 0,
				}.try_into().unwrap()),
			},
			IntExpression::CastFromReal(sub_expression) => IntValue {
				value: match Self::execute_real_expression(&self, sub_expression, line_number)? {
					RealValue::IntValue(value) => value,
					RealValue::FloatValue(value) => Rc::new(match BigInt::from_f64(value) {
						Some(value) => value,
						None => return Err(Error { variant: ErrorVariant::NonNumberValueCastToInt(value), line_number: line_number.cloned(), column_number: Some(sub_expression.get_start_column()), line_text: None }),
					}),
				}
			},
			IntExpression::BitwiseAnd { lhs_expression, rhs_expression, .. } => IntValue {
				value: {
					let mut lhs_value = Self::execute_int_expression(self, lhs_expression, line_number)?.value;
					let int = Rc::<BigInt>::make_mut(&mut lhs_value);
					(*int) &= &*Self::execute_int_expression(self, rhs_expression, line_number)?.value;
					lhs_value
				},
			},
			IntExpression::BitwiseOr { lhs_expression, rhs_expression, .. } => IntValue {
				value: {
					let mut lhs_value = Self::execute_int_expression(self, lhs_expression, line_number)?.value;
					let int = Rc::<BigInt>::make_mut(&mut lhs_value);
					(*int) |= &*Self::execute_int_expression(self, rhs_expression, line_number)?.value;
					lhs_value
				},
			},
			IntExpression::BitwiseNot { sub_expression, .. } => IntValue { 
				value: Rc::new(!&*Self::execute_int_expression(self, sub_expression, line_number)?.value),
			},
			IntExpression::LValue(l_value) => self.execute_int_l_value_read(l_value, line_number)?,
		})
	}

	fn execute_real_expression(&self, expression: &RealExpression, line_number: Option<&BigInt>) -> Result<RealValue, Error> {
		Ok(match expression {
			RealExpression::ConstantValue { value, .. } => value.clone(),
			RealExpression::CastFromInt(sub_expression) => RealValue::IntValue(
				Self::execute_int_expression(&self, sub_expression, line_number)?.value
			),
			RealExpression::CastFromComplex(sub_expression) => RealValue::FloatValue({
				let value = Self::execute_complex_expression(&self, sub_expression, line_number)?.value;
				if value.im != 0. {
					return Err(Error { variant: ErrorVariant::NonRealComplexValueCastToReal(value), line_number: line_number.cloned(), column_number: Some(sub_expression.get_start_column()), line_text: None });
				}
				value.re
			}),
			RealExpression::Addition { lhs_expression, rhs_expression, .. } => {
				match self.execute_real_expression(lhs_expression, line_number)?.add(self.execute_real_expression(rhs_expression, line_number)?, self.math_option == MathOption::Ieee) {
					Some(result) => result,
					None => return Err(Error { variant: ErrorVariant::Exception(Exception::ValueOverflow), line_number: line_number.cloned(), column_number: Some(expression.get_start_column()), line_text: None }),
				}
			}
			RealExpression::Subtraction { lhs_expression, rhs_expression, .. } => {
				match (self.execute_real_expression(lhs_expression, line_number)?, self.execute_real_expression(rhs_expression, line_number)?) {
					(RealValue::IntValue(mut lhs_value), RealValue::IntValue(rhs_value)) => RealValue::IntValue({
						let int = Rc::<BigInt>::make_mut(&mut lhs_value);
						(*int) -= &*rhs_value;
						lhs_value
					}),
					(lhs_value, rhs_value) => RealValue::FloatValue(lhs_value.get_float() - rhs_value.get_float()),
				}
			}
			RealExpression::Multiplication { lhs_expression, rhs_expression, .. } => {
				match (self.execute_real_expression(lhs_expression, line_number)?, self.execute_real_expression(rhs_expression, line_number)?) {
					(RealValue::IntValue(mut lhs_value), RealValue::IntValue(rhs_value)) => RealValue::IntValue({
						let int = Rc::<BigInt>::make_mut(&mut lhs_value);
						(*int) *= &*rhs_value;
						lhs_value
					}),
					(lhs_value, rhs_value) => RealValue::FloatValue(lhs_value.get_float() * rhs_value.get_float()),
				}
			}
			RealExpression::Division { lhs_expression, rhs_expression, .. } => {
				match (self.execute_real_expression(lhs_expression, line_number)?, self.execute_real_expression(rhs_expression, line_number)?) {
					(lhs_value, rhs_value) if rhs_value.is_zero() => RealValue::FloatValue(lhs_value.get_float() / rhs_value.get_float()),
					(lhs_value, rhs_value) if matches!((&lhs_value, &rhs_value), (RealValue::IntValue(lhs_int), RealValue::IntValue(rhs_int)) if !lhs_int.is_multiple_of(rhs_int)) =>
						RealValue::FloatValue(lhs_value.get_float() / rhs_value.get_float()),
					(RealValue::IntValue(mut lhs_value), RealValue::IntValue(rhs_value)) => RealValue::IntValue({
						let int = Rc::<BigInt>::make_mut(&mut lhs_value);
						(*int) /= &*rhs_value;
						lhs_value
					}),
					(lhs_value, rhs_value) => RealValue::FloatValue(lhs_value.get_float() / rhs_value.get_float()),
				}
			}
			RealExpression::Exponentiation { lhs_expression, rhs_expression, .. } => {
				let lhs_value = self.execute_real_expression(lhs_expression, line_number)?;
				let rhs_value = self.execute_real_expression(rhs_expression, line_number)?;
				match (&lhs_value, &rhs_value) {
					(RealValue::IntValue(lhs_int), RealValue::IntValue(rhs_int)) => {
						match rhs_int.to_biguint() {
							Some(rhs_uint) => RealValue::IntValue(Rc::new(Pow::<BigUint>::pow(&**lhs_int, rhs_uint))),
							None => RealValue::FloatValue((&lhs_value).get_float().powf((&rhs_value).get_float())),
						}
					},
					(lhs_value, rhs_value) => RealValue::FloatValue(lhs_value.get_float().powf(rhs_value.get_float())),
				}
			}
			RealExpression::Negation { sub_expression, .. } => match self.execute_real_expression(&sub_expression, line_number)? {
				RealValue::IntValue(int_value) => RealValue::IntValue(Rc::new(-&*int_value)),
				RealValue::FloatValue(float_value) => RealValue::FloatValue(-float_value),
			}
			RealExpression::FlooredDivision { lhs_expression, rhs_expression, start_column } => {
				let mut lhs_value = self.execute_int_expression(lhs_expression, line_number)?.value;
				let rhs_value = self.execute_int_expression(rhs_expression, line_number)?.value;
				if rhs_value.is_zero() {
					return Err(Error { variant: ErrorVariant::FlooredDivisionByZero, line_number: line_number.cloned(), column_number: Some(*start_column), line_text: None });
				}
				let int = Rc::<BigInt>::make_mut(&mut lhs_value);
				(*int) /= &*rhs_value;
				RealValue::IntValue(lhs_value)
			}
			RealExpression::LValue(l_value) => self.execute_real_l_value_read(l_value, line_number)?,
		})
	}

	fn execute_complex_expression(&self, expression: &ComplexExpression, line_number: Option<&BigInt>) -> Result<ComplexValue, Error> {
		Ok(match expression {
			ComplexExpression::ConstantValue { value, .. } => *value,
			ComplexExpression::CastFromReal(sub_expression) => ComplexValue {
				value: Complex { re: self.execute_real_expression(sub_expression, line_number)?.get_float(), im: 0. }
			},
			ComplexExpression::Addition { lhs_expression, rhs_expression, .. } => ComplexValue {
				value: self.execute_complex_expression(lhs_expression, line_number)?.value + self.execute_complex_expression(rhs_expression, line_number)?.value
			},
			ComplexExpression::Subtraction { lhs_expression, rhs_expression, .. } => ComplexValue {
				value: self.execute_complex_expression(lhs_expression, line_number)?.value - self.execute_complex_expression(rhs_expression, line_number)?.value
			},
			ComplexExpression::Multiplication { lhs_expression, rhs_expression, .. } => ComplexValue {
				value: self.execute_complex_expression(lhs_expression, line_number)?.value * self.execute_complex_expression(rhs_expression, line_number)?.value
			},
			ComplexExpression::Division { lhs_expression, rhs_expression, .. } => ComplexValue {
				value: self.execute_complex_expression(lhs_expression, line_number)?.value / self.execute_complex_expression(rhs_expression, line_number)?.value
			},
			ComplexExpression::Exponentiation { lhs_expression, rhs_expression, .. } => ComplexValue {
				value: self.execute_complex_expression(lhs_expression, line_number)?.value.powc(self.execute_complex_expression(rhs_expression, line_number)?.value)
			},
			ComplexExpression::Negation { sub_expression, .. } => ComplexValue {
				value: -self.execute_complex_expression(sub_expression, line_number)?.value
			},
			ComplexExpression::LValue(l_value) => self.execute_complex_l_value_read(l_value, line_number)?,
		})
	}

	fn execute_bool_expression(&self, expression: &BoolExpression, line_number: Option<&BigInt>) -> Result<BoolValue, Error> {
		Ok(match expression {
			BoolExpression::ConstantValue { value, .. } => *value,
			BoolExpression::IntIsNonZero(int_expression) => BoolValue { value: !self.execute_int_expression(&int_expression, line_number)?.value.is_zero() },
			BoolExpression::RealIsNonZero(real_expression) => BoolValue { value: !self.execute_real_expression(&real_expression, line_number)?.is_zero() },
			BoolExpression::ComplexIsNonZero(complex_expression) => BoolValue { value: !self.execute_complex_expression(&complex_expression, line_number)?.value.is_zero() },
			BoolExpression::StringIsNotEmpty(string_expression) => BoolValue { value: !self.execute_string_expression(&string_expression, line_number)?.value.is_empty() },

			BoolExpression::And { lhs_expression, rhs_expression, .. } => BoolValue {
				value: self.execute_bool_expression(lhs_expression, line_number)?.value && self.execute_bool_expression(rhs_expression, line_number)?.value
			},
			BoolExpression::Or { lhs_expression, rhs_expression, .. } => BoolValue {
				value: self.execute_bool_expression(lhs_expression, line_number)?.value || self.execute_bool_expression(rhs_expression, line_number)?.value
			},
			BoolExpression::Not { sub_expression, .. } => BoolValue {
				value: !self.execute_bool_expression(sub_expression, line_number)?.value
			},

			BoolExpression::BoolEqualTo { lhs_expression, rhs_expression, .. } => BoolValue {
				value: self.execute_bool_expression(lhs_expression, line_number)?.value == self.execute_bool_expression(rhs_expression, line_number)?.value
			},
			BoolExpression::BoolNotEqualTo { lhs_expression, rhs_expression, .. } => BoolValue {
				value: self.execute_bool_expression(lhs_expression, line_number)?.value != self.execute_bool_expression(rhs_expression, line_number)?.value
			},
			BoolExpression::BoolLessThan { lhs_expression, rhs_expression, .. } => BoolValue {
				value: self.execute_bool_expression(lhs_expression, line_number)?.value < self.execute_bool_expression(rhs_expression, line_number)?.value
			},
			BoolExpression::BoolLessThanOrEqualTo { lhs_expression, rhs_expression, .. } => BoolValue {
				value: self.execute_bool_expression(lhs_expression, line_number)?.value <= self.execute_bool_expression(rhs_expression, line_number)?.value
			},
			BoolExpression::BoolGreaterThan { lhs_expression, rhs_expression, .. } => BoolValue {
				value: self.execute_bool_expression(lhs_expression, line_number)?.value > self.execute_bool_expression(rhs_expression, line_number)?.value
			},
			BoolExpression::BoolGreaterThanOrEqualTo { lhs_expression, rhs_expression, .. } => BoolValue {
				value: self.execute_bool_expression(lhs_expression, line_number)?.value >= self.execute_bool_expression(rhs_expression, line_number)?.value
			},
			
			BoolExpression::IntEqualTo { lhs_expression, rhs_expression, .. } => BoolValue {
				value: *self.execute_int_expression(lhs_expression, line_number)?.value == *self.execute_int_expression(rhs_expression, line_number)?.value
			},
			BoolExpression::IntNotEqualTo { lhs_expression, rhs_expression, .. } => BoolValue {
				value: *self.execute_int_expression(lhs_expression, line_number)?.value != *self.execute_int_expression(rhs_expression, line_number)?.value
			},
			BoolExpression::IntLessThan { lhs_expression, rhs_expression, .. } => BoolValue {
				value: *self.execute_int_expression(lhs_expression, line_number)?.value < *self.execute_int_expression(rhs_expression, line_number)?.value
			},
			BoolExpression::IntLessThanOrEqualTo { lhs_expression, rhs_expression, .. } => BoolValue {
				value: *self.execute_int_expression(lhs_expression, line_number)?.value <= *self.execute_int_expression(rhs_expression, line_number)?.value
			},
			BoolExpression::IntGreaterThan { lhs_expression, rhs_expression, .. } => BoolValue {
				value: *self.execute_int_expression(lhs_expression, line_number)?.value > *self.execute_int_expression(rhs_expression, line_number)?.value
			},
			BoolExpression::IntGreaterThanOrEqualTo { lhs_expression, rhs_expression, .. } => BoolValue {
				value: *self.execute_int_expression(lhs_expression, line_number)?.value >= *self.execute_int_expression(rhs_expression, line_number)?.value
			},

			BoolExpression::RealEqualTo { lhs_expression, rhs_expression, .. } => BoolValue {
				value: {
					let lhs_value = self.execute_real_expression(lhs_expression, line_number)?;
					let rhs_value = self.execute_real_expression(rhs_expression, line_number)?;
					match (&lhs_value, &rhs_value) {
						(RealValue::IntValue(lhs_int), RealValue::IntValue(rhs_int)) => **lhs_int == **rhs_int,
						(_, _) => lhs_value.get_float() == rhs_value.get_float(),
					}
				}
			},
			BoolExpression::RealNotEqualTo { lhs_expression, rhs_expression, .. } => BoolValue {
				value: {
					let lhs_value = self.execute_real_expression(lhs_expression, line_number)?;
					let rhs_value = self.execute_real_expression(rhs_expression, line_number)?;
					match (&lhs_value, &rhs_value) {
						(RealValue::IntValue(lhs_int), RealValue::IntValue(rhs_int)) => **lhs_int != **rhs_int,
						(_, _) => lhs_value.get_float() != rhs_value.get_float(),
					}
				}
			},
			BoolExpression::RealLessThan { lhs_expression, rhs_expression, .. } => BoolValue {
				value: {
					let lhs_value = self.execute_real_expression(lhs_expression, line_number)?;
					let rhs_value = self.execute_real_expression(rhs_expression, line_number)?;
					match (&lhs_value, &rhs_value) {
						(RealValue::IntValue(lhs_int), RealValue::IntValue(rhs_int)) => **lhs_int < **rhs_int,
						(_, _) => lhs_value.get_float() < rhs_value.get_float(),
					}
				}
			},
			BoolExpression::RealLessThanOrEqualTo { lhs_expression, rhs_expression, .. } => BoolValue {
				value: {
					let lhs_value = self.execute_real_expression(lhs_expression, line_number)?;
					let rhs_value = self.execute_real_expression(rhs_expression, line_number)?;
					match (&lhs_value, &rhs_value) {
						(RealValue::IntValue(lhs_int), RealValue::IntValue(rhs_int)) => **lhs_int <= **rhs_int,
						(_, _) => lhs_value.get_float() <= rhs_value.get_float(),
					}
				}
			},
			BoolExpression::RealGreaterThan { lhs_expression, rhs_expression, .. } => BoolValue {
				value: {
					let lhs_value = self.execute_real_expression(lhs_expression, line_number)?;
					let rhs_value = self.execute_real_expression(rhs_expression, line_number)?;
					match (&lhs_value, &rhs_value) {
						(RealValue::IntValue(lhs_int), RealValue::IntValue(rhs_int)) => **lhs_int > **rhs_int,
						(_, _) => lhs_value.get_float() > rhs_value.get_float(),
					}
				}
			},
			BoolExpression::RealGreaterThanOrEqualTo { lhs_expression, rhs_expression, .. } => BoolValue {
				value: {
					let lhs_value = self.execute_real_expression(lhs_expression, line_number)?;
					let rhs_value = self.execute_real_expression(rhs_expression, line_number)?;
					match (&lhs_value, &rhs_value) {
						(RealValue::IntValue(lhs_int), RealValue::IntValue(rhs_int)) => **lhs_int >= **rhs_int,
						(_, _) => lhs_value.get_float() >= rhs_value.get_float(),
					}
				}
			},

			BoolExpression::ComplexEqualTo { lhs_expression, rhs_expression, .. } => BoolValue {
				value: self.execute_complex_expression(lhs_expression, line_number)?.value == self.execute_complex_expression(rhs_expression, line_number)?.value
			},
			BoolExpression::ComplexNotEqualTo { lhs_expression, rhs_expression, .. } => BoolValue {
				value: self.execute_complex_expression(lhs_expression, line_number)?.value != self.execute_complex_expression(rhs_expression, line_number)?.value
			},

			BoolExpression::StringEqualTo { lhs_expression, rhs_expression, .. } => BoolValue {
				value: *self.execute_string_expression(lhs_expression, line_number)?.value == *self.execute_string_expression(rhs_expression, line_number)?.value
			},
			BoolExpression::StringNotEqualTo { lhs_expression, rhs_expression, .. } => BoolValue {
				value: *self.execute_string_expression(lhs_expression, line_number)?.value != *self.execute_string_expression(rhs_expression, line_number)?.value
			},
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
			StringExpression::Concatenation { lhs_expression, rhs_expression, .. } => StringValue {
				value: {
					let mut lhs_value = Self::execute_string_expression(self, lhs_expression, line_number)?.value;
					let string = Rc::<String>::make_mut(&mut lhs_value);
					string.push_str(&*Self::execute_string_expression(self, rhs_expression, line_number)?.value);
					lhs_value
				},
			},
			StringExpression::LValue(l_value) => self.execute_string_l_value_read(l_value, line_number)?,
		})
	}

	fn _execute_any_type_expression(&self, expression: &AnyTypeExpression, line_number: Option<&BigInt>) -> Result<AnyTypeValue, Error> {
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
				(SuppliedFunction::Sqr, arguments) if arguments.len() == 1 => 'a: {
					let argument_value = match &arguments[0] {
						AnyTypeExpression::Bool(expression) => return Ok(self.execute_bool_expression(expression, line_number)?.to_int()),
						AnyTypeExpression::Int(expression) => self.execute_int_expression(expression, line_number)?,
						AnyTypeExpression::Real(expression) => self.execute_real_expression(expression, line_number)?.to_int(line_number, *start_column)?,
						AnyTypeExpression::Complex(expression) => self.execute_complex_expression(expression, line_number)?.to_int(line_number, *start_column)?,
						AnyTypeExpression::String(_) => break 'a,
						_ => unreachable!(),
					};
					if argument_value.value.is_negative() {
						return Err(Error { variant: ErrorVariant::NonIntSquareRootOfNegativeNumber, line_number: line_number.cloned(), column_number: Some(*start_column), line_text: None });
					}
					return Ok(IntValue::new(Rc::new(argument_value.value.sqrt())));
				}
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
				(SuppliedFunction::Sqr, arguments) if arguments.len() == 1 => 'a: {
					let argument_value = match &arguments[0] {
						AnyTypeExpression::Bool(expression) => return Ok(self.execute_bool_expression(expression, line_number)?.to_real()),
						AnyTypeExpression::Int(expression) => self.execute_int_expression(expression, line_number)?.to_real(),
						AnyTypeExpression::Real(expression) => self.execute_real_expression(expression, line_number)?,
						AnyTypeExpression::Complex(expression) => self.execute_complex_expression(expression, line_number)?.to_real(line_number, *start_column)?,
						AnyTypeExpression::String(_) => break 'a,
						_ => unreachable!(),
					};
					return Ok(match argument_value {
						RealValue::FloatValue(value) => RealValue::FloatValue(value.sqrt()),
						RealValue::IntValue(value) if value.is_negative() => RealValue::FloatValue(int_to_float(&value).sqrt()),
						RealValue::IntValue(value) => {
							let floored_sqrt = value.sqrt();
							match (&*value) == &((&floored_sqrt).pow(2u32)) {
								true => RealValue::IntValue(Rc::new(floored_sqrt)),
								false => RealValue::FloatValue(int_to_float(&value).sqrt())
							}
						}
					})
				}
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
				(SuppliedFunction::Sqr, arguments) if arguments.len() == 1 => 'a: {
					let argument_value = match &arguments[0] {
						AnyTypeExpression::Bool(expression) => return Ok(self.execute_bool_expression(expression, line_number)?.to_complex()),
						AnyTypeExpression::Int(expression) => self.execute_int_expression(expression, line_number)?.to_complex(),
						AnyTypeExpression::Real(expression) => self.execute_real_expression(expression, line_number)?.to_complex(),
						AnyTypeExpression::Complex(expression) => self.execute_complex_expression(expression, line_number)?,
						AnyTypeExpression::String(_) => break 'a,
						_ => unreachable!(),
					};
					return Ok(ComplexValue::new(argument_value.value.sqrt()))
				}
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