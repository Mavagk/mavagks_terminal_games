use std::{collections::HashMap, io::stdout, rc::Rc};

use crossterm::{execute, style::{Color, ContentStyle, PrintStyledContent, StyledContent}};
use num::{BigInt, Complex, FromPrimitive, Integer, BigUint, Zero};
use num_traits::Pow;

use crate::mavagk_basic::{abstract_syntax_tree::{AnyTypeExpression, BoolExpression, BoolExpressionVariant, ComplexExpression, ComplexExpressionVariant, IntExpression, IntExpressionVariant, RealExpression, RealExpressionVariant, Statement, StatementVariant, StringExpression, StringExpressionVariant}, error::{handle_error, Error, ErrorVariant}, parse::parse_line, program::Program, token::Token, value::{BoolValue, ComplexValue, IntValue, RealValue, StringValue}};

pub struct Machine {
	is_executing_unnumbered_line: bool,
	line_executing: BigInt,
	real_variables: HashMap<Box<str>, RealValue>,
	complex_variables: HashMap<Box<str>, ComplexValue>,
	int_variables: HashMap<Box<str>, IntValue>,
	string_variables: HashMap<Box<str>, StringValue>,
}

impl Machine {
	pub fn new() -> Self {
		Self {
			is_executing_unnumbered_line: false,
			line_executing: 0.into(),
			int_variables: HashMap::new(),
			real_variables: HashMap::new(),
			complex_variables: HashMap::new(),
			string_variables: HashMap::new(),
		}
	}

	fn set_line_executing_to_first_line(&mut self, program: &Program) {
		self.line_executing = match program.lines.first_key_value() {
			Some(first_entry) => first_entry.0.clone(),
			None => BigInt::ZERO,
		};
		self.is_executing_unnumbered_line = false;
	}

	fn clear_machine_state(&mut self) {
		// TODO
		self.int_variables = HashMap::new();
		self.real_variables = HashMap::new();
		self.complex_variables = HashMap::new();
		self.string_variables = HashMap::new();
	}

	pub fn line_of_text_entered(&mut self, line: Box<str>, program: &mut Program) -> Result<(), Error> {
		// Parse line
		let (line_number, tokens, error) = match Token::tokenize_line(&*line) {
			(line_number, Ok(tokens)) => (line_number, tokens, None),
			(line_number, Err(error)) => (line_number, Box::default(), Some(error)),
		};
		let (statements, error) = match error {
			None => parse_line(&*tokens, line_number.as_ref()),
			Some(error) => (Box::default(), Some(error)),
		};
		// Enter line number into program and run if it does not have a line number
		match line_number {
			Some(line_number) => {
				if let Some(error) = &error {
					let mut error = error.clone();
					error.line_text = Some(line.clone().into());
					handle_error::<()>(Err(error));
				}
				if statements.is_empty() && error.is_none() {
					program.lines.remove(&line_number);
				}
				else {
					program.lines.insert(line_number, (statements, error, line));
				}
			}
			None => {
				if let Some(mut error) = error {
					error.line_text = Some(line.into());
					return Err(error);
				}
				program.unnumbered_line = statements;
				program.unnumbered_line_string = line;
				self.is_executing_unnumbered_line = true;
				self.execute(program)?;
			}
		}
		Ok(())
	}

	fn execute(&mut self, program: &mut Program) -> Result<(), Error> {
		'lines_loop: loop {
			// Get the line number to be executed
			let line_number = match self.is_executing_unnumbered_line {
				false => Some(self.line_executing.clone()),
				true => None,
			};
			// Get the statements to execute
			let (statements, mut error, line_text) = match self.is_executing_unnumbered_line {
				true => (&program.unnumbered_line, None, Some(&*program.unnumbered_line_string)),
				false => match program.lines.get(&self.line_executing) {
					Some((statements, error, line_text)) => (statements, error.clone(), Some(&**line_text)),
					None => return Err(Error { variant: ErrorVariant::InvalidLineNumber, line_number: line_number, column_number: None, line_text: None })
				}
			};
			// Execute statements
			for statement in statements {
				let do_skip_other_statements_on_line = match self.execute_statement(statement, line_number.as_ref(), program) {
					Ok(do_skip_other_statements_on_line) => do_skip_other_statements_on_line,
					Err(statement_error) => {
						error = Some(statement_error);
						break;
					}
				};
				if do_skip_other_statements_on_line {
					continue 'lines_loop;
				}
			}
			// Throw the error if there is one
			if let Some(error) = error {
				if let Some(line_text) = line_text {
					let mut error = error.clone();
					error.line_text = Some(line_text.into());
					return Err(error.clone())
				}
			}
			// Decide what to execute next
			if self.is_executing_unnumbered_line {
				return Ok(());
			}
			self.line_executing = match program.lines.range(&self.line_executing..).nth(1) {
				Some((line_number, _)) => line_number,
				None => return Ok(()),
			}.clone();
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
						let line_number = self.execute_int_expression(sub_expression, line_number)?.value;
						self.line_executing = (&*line_number).clone();
						self.is_executing_unnumbered_line = false;
					}
					None => self.set_line_executing_to_first_line(&program),
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
				let (name, _arguments, has_parentheses) = match l_value {
					IntExpression { variant: IntExpressionVariant::IntIdentifierOrFunction { name, arguments, uses_fn_keyword: false, has_parentheses }, .. }
						=> (name, arguments, *has_parentheses),
					IntExpression { variant: _, column } => return Err(Error { variant: ErrorVariant::InvalidLValue, line_number: line_number.cloned(), column_number: Some(*column), line_text: None }),
				};
				// Get r-value
				let r_value = Self::execute_int_expression(&self, r_value_expression, line_number)?;
				// Assign
				if has_parentheses {
					return Err(Error { variant: ErrorVariant::NotYetImplemented("Arrays".into()), line_number: line_number.cloned(), column_number: Some(*column), line_text: None });
				}
				self.int_variables.insert(name.clone(), r_value);
			}
			StatementVariant::AssignReal(l_value, r_value_expression) => {
				// Get what to assign to
				let (name, _arguments, has_parentheses) = match l_value {
					RealExpression { variant: RealExpressionVariant::RealIdentifierOrFunction { name, arguments, uses_fn_keyword: false, has_parentheses }, .. }
						=> (name, arguments, *has_parentheses),
					RealExpression { variant: _, column } => return Err(Error { variant: ErrorVariant::InvalidLValue, line_number: line_number.cloned(), column_number: Some(*column), line_text: None }),
				};
				// Get r-value
				let r_value = Self::execute_real_expression(&self, r_value_expression, line_number)?;
				// Assign
				if has_parentheses {
					return Err(Error { variant: ErrorVariant::NotYetImplemented("Arrays".into()), line_number: line_number.cloned(), column_number: Some(*column), line_text: None });
				}
				self.real_variables.insert(name.clone(), r_value);
			}
			StatementVariant::AssignComplex(l_value, r_value_expression) => {
				// Get what to assign to
				let (name, _arguments, has_parentheses) = match l_value {
					ComplexExpression { variant: ComplexExpressionVariant::ComplexIdentifierOrFunction { name, arguments, uses_fn_keyword: false, has_parentheses }, .. }
						=> (name, arguments, *has_parentheses),
					ComplexExpression { variant: _, column } => return Err(Error { variant: ErrorVariant::InvalidLValue, line_number: line_number.cloned(), column_number: Some(*column), line_text: None }),
				};
				// Get r-value
				let r_value = Self::execute_complex_expression(&self, r_value_expression, line_number)?;
				// Assign
				if has_parentheses {
					return Err(Error { variant: ErrorVariant::NotYetImplemented("Arrays".into()), line_number: line_number.cloned(), column_number: Some(*column), line_text: None });
				}
				self.complex_variables.insert(name.clone(), r_value);
			}
			StatementVariant::AssignString(l_value, r_value_expression) => {
				// Get what to assign to
				let (name, _arguments, has_parentheses) = match l_value {
					StringExpression { variant: StringExpressionVariant::StringIdentifierOrFunction { name, arguments, uses_fn_keyword: false, has_parentheses }, .. }
						=> (name, arguments, *has_parentheses),
					StringExpression { variant: _, column } => return Err(Error { variant: ErrorVariant::InvalidLValue, line_number: line_number.cloned(), column_number: Some(*column), line_text: None }),
				};
				// Get r-value
				let r_value = Self::execute_string_expression(&self, r_value_expression, line_number)?;
				// Assign
				if has_parentheses {
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
					(None, None) => program.lines.range(..),
					(Some(range_start_value), None) => program.lines.range(range_start_value..),
					(None, Some(range_end_value)) => program.lines.range(..=range_end_value),
					(Some(range_start_value), Some(range_end_value)) => program.lines.range(range_start_value..=range_end_value),
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
		}
		Ok(false)
	}

	fn execute_int_expression(&self, expression: &IntExpression, line_number: Option<&BigInt>) -> Result<IntValue, Error> {
		let IntExpression { variant: expression_variant, column: expression_column } = expression;
		Ok(match expression_variant {
			IntExpressionVariant::ConstantValue(value) => value.clone(),
			IntExpressionVariant::CastFromBool(sub_expression) => IntValue {
				value: Rc::new(match Self::execute_bool_expression(&self, sub_expression, line_number)?.value {
					true => -1,
					false => 0,
				}.try_into().unwrap()),
			},
			IntExpressionVariant::CastFromReal(sub_expression) => IntValue {
				value: match Self::execute_real_expression(&self, sub_expression, line_number)? {
					RealValue::IntValue(value) => value,
					RealValue::FloatValue(value) => Rc::new(match BigInt::from_f64(value) {
						Some(value) => value,
						None => return Err(Error { variant: ErrorVariant::NonNumberValueCastToInt(value), line_number: line_number.cloned(), column_number: Some(*expression_column), line_text: None }),
					}),
				}
			},
			IntExpressionVariant::BitwiseAnd(lhs, rhs) => IntValue {
				value: {
					let mut lhs_value = Self::execute_int_expression(self, lhs, line_number)?.value;
					let int = Rc::<BigInt>::make_mut(&mut lhs_value);
					(*int) &= &*Self::execute_int_expression(self, rhs, line_number)?.value;
					lhs_value
				},
			},
			IntExpressionVariant::BitwiseOr(lhs, rhs) => IntValue {
				value: {
					let mut lhs_value = Self::execute_int_expression(self, lhs, line_number)?.value;
					let int = Rc::<BigInt>::make_mut(&mut lhs_value);
					(*int) |= &*Self::execute_int_expression(self, rhs, line_number)?.value;
					lhs_value
				},
			},
			IntExpressionVariant::IntIdentifierOrFunction { name, arguments: _, uses_fn_keyword, has_parentheses } => {
				if *has_parentheses || *uses_fn_keyword {
					return Err(Error { variant: ErrorVariant::NotYetImplemented("Arrays and functions".into()), line_number: line_number.cloned(), column_number: Some(*expression_column), line_text: None });
				}
				match self.int_variables.get(name) {
					Some(int_variable) => int_variable.clone(),
					None => return Err(Error { variant: ErrorVariant::VariableNotFound, line_number: line_number.cloned(), column_number: Some(*expression_column), line_text: None }),
				}
			}
		})
	}

	fn execute_real_expression(&self, expression: &RealExpression, line_number: Option<&BigInt>) -> Result<RealValue, Error> {
		let RealExpression { variant: expression_variant, column: expression_column } = expression;
		Ok(match expression_variant {
			RealExpressionVariant::ConstantValue(value) => value.clone(),
			RealExpressionVariant::CastFromInt(sub_expression) => RealValue::IntValue(
				Self::execute_int_expression(&self, sub_expression, line_number)?.value
			),
			RealExpressionVariant::CastFromComplex(sub_expression) => RealValue::FloatValue({
				let value = Self::execute_complex_expression(&self, sub_expression, line_number)?.value;
				if value.im != 0. {
					return Err(Error { variant: ErrorVariant::NonRealComplexValueCastToReal(value), line_number: line_number.cloned(), column_number: Some(*expression_column), line_text: None });
				}
				value.re
			}),
			RealExpressionVariant::Addition(lhs, rhs) => {
				match (self.execute_real_expression(lhs, line_number)?, self.execute_real_expression(rhs, line_number)?) {
					(RealValue::IntValue(mut lhs_value), RealValue::IntValue(rhs_value)) => RealValue::IntValue({
						let int = Rc::<BigInt>::make_mut(&mut lhs_value);
						(*int) += &*rhs_value;
						lhs_value
					}),
					(lhs_value, rhs_value) => RealValue::FloatValue(lhs_value.get_float() + rhs_value.get_float()),
				}
			}
			RealExpressionVariant::Subtraction(lhs, rhs) => {
				match (self.execute_real_expression(lhs, line_number)?, self.execute_real_expression(rhs, line_number)?) {
					(RealValue::IntValue(mut lhs_value), RealValue::IntValue(rhs_value)) => RealValue::IntValue({
						let int = Rc::<BigInt>::make_mut(&mut lhs_value);
						(*int) -= &*rhs_value;
						lhs_value
					}),
					(lhs_value, rhs_value) => RealValue::FloatValue(lhs_value.get_float() - rhs_value.get_float()),
				}
			}
			RealExpressionVariant::Multiplication(lhs, rhs) => {
				match (self.execute_real_expression(lhs, line_number)?, self.execute_real_expression(rhs, line_number)?) {
					(RealValue::IntValue(mut lhs_value), RealValue::IntValue(rhs_value)) => RealValue::IntValue({
						let int = Rc::<BigInt>::make_mut(&mut lhs_value);
						(*int) *= &*rhs_value;
						lhs_value
					}),
					(lhs_value, rhs_value) => RealValue::FloatValue(lhs_value.get_float() * rhs_value.get_float()),
				}
			}
			RealExpressionVariant::Division(lhs, rhs) => {
				match (self.execute_real_expression(lhs, line_number)?, self.execute_real_expression(rhs, line_number)?) {
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
			RealExpressionVariant::Exponentiation(lhs, rhs) => {
				let lhs_value = self.execute_real_expression(lhs, line_number)?;
				let rhs_value = self.execute_real_expression(rhs, line_number)?;
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
			RealExpressionVariant::BitwiseNot(sub_expression) => RealValue::IntValue(
				Rc::new(!&*Self::execute_int_expression(self, sub_expression, line_number)?.value)
			),
			RealExpressionVariant::Negation(sub_expression) => match self.execute_real_expression(&sub_expression, line_number)? {
				RealValue::IntValue(int_value) => RealValue::IntValue(Rc::new(-&*int_value)),
				RealValue::FloatValue(float_value) => RealValue::FloatValue(-float_value),
			}
			RealExpressionVariant::FlooredDivision(lhs, rhs) => {
				let mut lhs_value = self.execute_int_expression(lhs, line_number)?.value;
				let rhs_value = self.execute_int_expression(rhs, line_number)?.value;
				if rhs_value.is_zero() {
					return Err(Error { variant: ErrorVariant::FlooredDivisionByZero, line_number: line_number.cloned(), column_number: Some(*expression_column), line_text: None });
				}
				let int = Rc::<BigInt>::make_mut(&mut lhs_value);
				(*int) /= &*rhs_value;
				RealValue::IntValue(lhs_value)
			}
			RealExpressionVariant::RealIdentifierOrFunction { name, arguments: _, uses_fn_keyword, has_parentheses } => {
				if *has_parentheses || *uses_fn_keyword {
					return Err(Error { variant: ErrorVariant::NotYetImplemented("Arrays and functions".into()), line_number: line_number.cloned(), column_number: Some(*expression_column), line_text: None });
				}
				match self.real_variables.get(name) {
					Some(real_variable) => real_variable.clone(),
					None => return Err(Error { variant: ErrorVariant::VariableNotFound, line_number: line_number.cloned(), column_number: Some(*expression_column), line_text: None }),
				}
			}
		})
	}

	fn execute_complex_expression(&self, expression: &ComplexExpression, line_number: Option<&BigInt>) -> Result<ComplexValue, Error> {
		let ComplexExpression { variant: expression_variant, column: expression_column } = expression;
		Ok(match expression_variant {
			ComplexExpressionVariant::ConstantValue(value) => *value,
			ComplexExpressionVariant::CastFromReal(sub_expression) => ComplexValue {
				value: Complex { re: self.execute_real_expression(sub_expression, line_number)?.get_float(), im: 0. }
			},
			ComplexExpressionVariant::Addition(lhs, rhs) => ComplexValue {
				value: self.execute_complex_expression(lhs, line_number)?.value + self.execute_complex_expression(rhs, line_number)?.value
			},
			ComplexExpressionVariant::Subtraction(lhs, rhs) => ComplexValue {
				value: self.execute_complex_expression(lhs, line_number)?.value - self.execute_complex_expression(rhs, line_number)?.value
			},
			ComplexExpressionVariant::Multiplication(lhs, rhs) => ComplexValue {
				value: self.execute_complex_expression(lhs, line_number)?.value * self.execute_complex_expression(rhs, line_number)?.value
			},
			ComplexExpressionVariant::Division(lhs, rhs) => ComplexValue {
				value: self.execute_complex_expression(lhs, line_number)?.value / self.execute_complex_expression(rhs, line_number)?.value
			},
			ComplexExpressionVariant::Exponentiation(lhs, rhs) => ComplexValue {
				value: self.execute_complex_expression(lhs, line_number)?.value.powc(self.execute_complex_expression(rhs, line_number)?.value)
			},
			ComplexExpressionVariant::Negation(sub_expression) => ComplexValue {
				value: -self.execute_complex_expression(sub_expression, line_number)?.value
			},
			ComplexExpressionVariant::ComplexIdentifierOrFunction { name, arguments: _, uses_fn_keyword, has_parentheses } => {
				if *has_parentheses || *uses_fn_keyword {
					return Err(Error { variant: ErrorVariant::NotYetImplemented("Arrays and functions".into()), line_number: line_number.cloned(), column_number: Some(*expression_column), line_text: None });
				}
				match self.complex_variables.get(name) {
					Some(complex_variable) => complex_variable.clone(),
					None => return Err(Error { variant: ErrorVariant::VariableNotFound, line_number: line_number.cloned(), column_number: Some(*expression_column), line_text: None }),
				}
			}
		})
	}

	fn execute_bool_expression(&self, expression: &BoolExpression, line_number: Option<&BigInt>) -> Result<BoolValue, Error> {
		let BoolExpression { variant: expression_variant, column: expression_column } = expression;
		Ok(match expression_variant {
			BoolExpressionVariant::ConstantValue(value) => *value,
			BoolExpressionVariant::And(lhs, rhs) => BoolValue {
				value: self.execute_bool_expression(lhs, line_number)?.value && self.execute_bool_expression(rhs, line_number)?.value
			},
			BoolExpressionVariant::Or(lhs, rhs) => BoolValue {
				value: self.execute_bool_expression(lhs, line_number)?.value || self.execute_bool_expression(rhs, line_number)?.value
			},
			BoolExpressionVariant::Not(sub_expression) => BoolValue {
				value: !self.execute_bool_expression(sub_expression, line_number)?.value
			},

			BoolExpressionVariant::BoolEqualTo(lhs, rhs) => BoolValue {
				value: self.execute_bool_expression(lhs, line_number)?.value == self.execute_bool_expression(rhs, line_number)?.value
			},
			BoolExpressionVariant::BoolNotEqualTo(lhs, rhs) => BoolValue {
				value: self.execute_bool_expression(lhs, line_number)?.value != self.execute_bool_expression(rhs, line_number)?.value
			},
			BoolExpressionVariant::BoolLessThan(lhs, rhs) => BoolValue {
				value: self.execute_bool_expression(lhs, line_number)?.value < self.execute_bool_expression(rhs, line_number)?.value
			},
			BoolExpressionVariant::BoolLessThanOrEqualTo(lhs, rhs) => BoolValue {
				value: self.execute_bool_expression(lhs, line_number)?.value <= self.execute_bool_expression(rhs, line_number)?.value
			},
			BoolExpressionVariant::BoolGreaterThan(lhs, rhs) => BoolValue {
				value: self.execute_bool_expression(lhs, line_number)?.value > self.execute_bool_expression(rhs, line_number)?.value
			},
			BoolExpressionVariant::BoolGreaterThanOrEqualTo(lhs, rhs) => BoolValue {
				value: self.execute_bool_expression(lhs, line_number)?.value >= self.execute_bool_expression(rhs, line_number)?.value
			},
			
			BoolExpressionVariant::IntEqualTo(lhs, rhs) => BoolValue {
				value: *self.execute_int_expression(lhs, line_number)?.value == *self.execute_int_expression(rhs, line_number)?.value
			},
			BoolExpressionVariant::IntNotEqualTo(lhs, rhs) => BoolValue {
				value: *self.execute_int_expression(lhs, line_number)?.value != *self.execute_int_expression(rhs, line_number)?.value
			},
			BoolExpressionVariant::IntLessThan(lhs, rhs) => BoolValue {
				value: *self.execute_int_expression(lhs, line_number)?.value < *self.execute_int_expression(rhs, line_number)?.value
			},
			BoolExpressionVariant::IntLessThanOrEqualTo(lhs, rhs) => BoolValue {
				value: *self.execute_int_expression(lhs, line_number)?.value <= *self.execute_int_expression(rhs, line_number)?.value
			},
			BoolExpressionVariant::IntGreaterThan(lhs, rhs) => BoolValue {
				value: *self.execute_int_expression(lhs, line_number)?.value > *self.execute_int_expression(rhs, line_number)?.value
			},
			BoolExpressionVariant::IntGreaterThanOrEqualTo(lhs, rhs) => BoolValue {
				value: *self.execute_int_expression(lhs, line_number)?.value >= *self.execute_int_expression(rhs, line_number)?.value
			},

			BoolExpressionVariant::RealEqualTo(lhs, rhs) => BoolValue {
				value: {
					let lhs_value = self.execute_real_expression(lhs, line_number)?;
					let rhs_value = self.execute_real_expression(rhs, line_number)?;
					match (&lhs_value, &rhs_value) {
						(RealValue::IntValue(lhs_int), RealValue::IntValue(rhs_int)) => **lhs_int == **rhs_int,
						(_, _) => lhs_value.get_float() == rhs_value.get_float(),
					}
				}
			},
			BoolExpressionVariant::RealNotEqualTo(lhs, rhs) => BoolValue {
				value: {
					let lhs_value = self.execute_real_expression(lhs, line_number)?;
					let rhs_value = self.execute_real_expression(rhs, line_number)?;
					match (&lhs_value, &rhs_value) {
						(RealValue::IntValue(lhs_int), RealValue::IntValue(rhs_int)) => **lhs_int != **rhs_int,
						(_, _) => lhs_value.get_float() != rhs_value.get_float(),
					}
				}
			},
			BoolExpressionVariant::RealLessThan(lhs, rhs) => BoolValue {
				value: {
					let lhs_value = self.execute_real_expression(lhs, line_number)?;
					let rhs_value = self.execute_real_expression(rhs, line_number)?;
					match (&lhs_value, &rhs_value) {
						(RealValue::IntValue(lhs_int), RealValue::IntValue(rhs_int)) => **lhs_int < **rhs_int,
						(_, _) => lhs_value.get_float() < rhs_value.get_float(),
					}
				}
			},
			BoolExpressionVariant::RealLessThanOrEqualTo(lhs, rhs) => BoolValue {
				value: {
					let lhs_value = self.execute_real_expression(lhs, line_number)?;
					let rhs_value = self.execute_real_expression(rhs, line_number)?;
					match (&lhs_value, &rhs_value) {
						(RealValue::IntValue(lhs_int), RealValue::IntValue(rhs_int)) => **lhs_int <= **rhs_int,
						(_, _) => lhs_value.get_float() <= rhs_value.get_float(),
					}
				}
			},
			BoolExpressionVariant::RealGreaterThan(lhs, rhs) => BoolValue {
				value: {
					let lhs_value = self.execute_real_expression(lhs, line_number)?;
					let rhs_value = self.execute_real_expression(rhs, line_number)?;
					match (&lhs_value, &rhs_value) {
						(RealValue::IntValue(lhs_int), RealValue::IntValue(rhs_int)) => **lhs_int > **rhs_int,
						(_, _) => lhs_value.get_float() > rhs_value.get_float(),
					}
				}
			},
			BoolExpressionVariant::RealGreaterThanOrEqualTo(lhs, rhs) => BoolValue {
				value: {
					let lhs_value = self.execute_real_expression(lhs, line_number)?;
					let rhs_value = self.execute_real_expression(rhs, line_number)?;
					match (&lhs_value, &rhs_value) {
						(RealValue::IntValue(lhs_int), RealValue::IntValue(rhs_int)) => **lhs_int >= **rhs_int,
						(_, _) => lhs_value.get_float() >= rhs_value.get_float(),
					}
				}
			},

			BoolExpressionVariant::ComplexEqualTo(lhs, rhs) => BoolValue {
				value: self.execute_complex_expression(lhs, line_number)?.value == self.execute_complex_expression(rhs, line_number)?.value
			},
			BoolExpressionVariant::ComplexNotEqualTo(lhs, rhs) => BoolValue {
				value: self.execute_complex_expression(lhs, line_number)?.value != self.execute_complex_expression(rhs, line_number)?.value
			},

			BoolExpressionVariant::StringEqualTo(lhs, rhs) => BoolValue {
				value: *self.execute_string_expression(lhs, line_number)?.value == *self.execute_string_expression(rhs, line_number)?.value
			},
			BoolExpressionVariant::StringNotEqualTo(lhs, rhs) => BoolValue {
				value: *self.execute_string_expression(lhs, line_number)?.value != *self.execute_string_expression(rhs, line_number)?.value
			},
			BoolExpressionVariant::StringLessThan(_lhs, _rhs) =>
				return Err(Error { variant: ErrorVariant::NotYetImplemented("String <, <=, >, >= operators".into()), line_number: line_number.cloned(), column_number: Some(*expression_column), line_text: None }),
			BoolExpressionVariant::StringLessThanOrEqualTo(_lhs, _rhs) =>
				return Err(Error { variant: ErrorVariant::NotYetImplemented("String <, <=, >, >= operators".into()), line_number: line_number.cloned(), column_number: Some(*expression_column), line_text: None }),
			BoolExpressionVariant::StringGreaterThan(_lhs, _rhs) =>
				return Err(Error { variant: ErrorVariant::NotYetImplemented("String <, <=, >, >= operators".into()), line_number: line_number.cloned(), column_number: Some(*expression_column), line_text: None }),
			BoolExpressionVariant::StringGreaterThanOrEqualTo(_lhs, _rhs) =>
				return Err(Error { variant: ErrorVariant::NotYetImplemented("String <, <=, >, >= operators".into()), line_number: line_number.cloned(), column_number: Some(*expression_column), line_text: None }),
		})
	}

	fn execute_string_expression(&self, expression: &StringExpression, line_number: Option<&BigInt>) -> Result<StringValue, Error> {
		let StringExpression { variant: expression_variant, column: expression_column } = expression;
		Ok(match expression_variant {
			StringExpressionVariant::ConstantValue(value) => value.clone(),
			StringExpressionVariant::Concatenation(lhs, rhs) => StringValue {
				value: {
					let mut lhs_value = Self::execute_string_expression(self, lhs, line_number)?.value;
					let string = Rc::<String>::make_mut(&mut lhs_value);
					string.push_str(&*Self::execute_string_expression(self, rhs, line_number)?.value);
					lhs_value
				},
			},
			StringExpressionVariant::StringIdentifierOrFunction { name, arguments: _, uses_fn_keyword, has_parentheses } => {
				if *has_parentheses || *uses_fn_keyword {
					return Err(Error { variant: ErrorVariant::NotYetImplemented("Arrays and functions".into()), line_number: line_number.cloned(), column_number: Some(*expression_column), line_text: None });
				}
				match self.string_variables.get(name) {
					Some(int_variable) => int_variable.clone(),
					None => return Err(Error { variant: ErrorVariant::VariableNotFound, line_number: line_number.cloned(), column_number: Some(*expression_column), line_text: None }),
				}
			}
		})
	}
}