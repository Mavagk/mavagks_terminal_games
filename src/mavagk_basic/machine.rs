use std::{collections::HashMap, f64::{INFINITY, NAN, NEG_INFINITY}, fmt::{self, Display, Formatter}, num::NonZeroUsize, rc::Rc};

use num::{complex::Complex64, BigInt, Complex, FromPrimitive, ToPrimitive};

use crate::mavagk_basic::{abstract_syntax_tree::{AnyTypeExpression, BoolExpression, ComplexExpression, ComplexExpressionVariant, IntExpression, IntExpressionVariant, RealExpression, RealExpressionVariant, Statement, StatementVariant, StringExpression, StringExpressionVariant}, error::{Error, ErrorVariant}, parse::parse_line, program::Program, token::{IdentifierType, Token}, value::{BoolValue, ComplexValue, IntValue, RealValue, StringValue}};

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
		let (line_number, tokens) = Token::tokenize_line(&*line)?;
		let statements = parse_line(&*tokens, line_number.as_ref())?;
		// Enter line number into program and run if it does not have a line number
		match line_number {
			Some(line_number) => {
				if statements.is_empty() {
					program.lines.remove(&line_number);
				}
				else {
					program.lines.insert(line_number, (statements, line));
				}
			}
			None => {
				program.unnumbered_line = statements;
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
				false => Some(&self.line_executing),
				true => None,
			};
			// Get the statements to execute
			let statements = match self.is_executing_unnumbered_line {
				true => &program.unnumbered_line,
				false => match program.lines.get(&self.line_executing) {
					Some(line) => &line.0,
					None => return Err(Error { variant: ErrorVariant::InvalidLineNumber, line_number: Some(line_number.cloned().unwrap()), column_number: None })
				}
			};
			// Execute statements
			for Statement { variant, column } in statements {
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
										variant: ErrorVariant::NotYetImplemented(", and ; in PRINT statement".into()), line_number: line_number.cloned(), column_number: Some(*sub_expression_column)
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
						continue 'lines_loop;
					}
					StatementVariant::Gosub(_) => return Err(Error { variant: ErrorVariant::NotYetImplemented("GOSUB statement".into()), line_number: Some(line_number.cloned().unwrap()), column_number: Some(*column) }),
					StatementVariant::AssignInt(l_value, r_value_expression) => {
						// Get what to assign to
						let (name, _arguments, has_parentheses) = match l_value {
							IntExpression { variant: IntExpressionVariant::IntIdentifierOrFunction { name, arguments, uses_fn_keyword: false, has_parentheses }, .. }
								=> (name, arguments, *has_parentheses),
							IntExpression { variant: _, column } => return Err(Error { variant: ErrorVariant::InvalidLValue, line_number: line_number.cloned(), column_number: Some(*column) }),
						};
						// Get r-value
						let r_value = Self::execute_int_expression(&self, r_value_expression, line_number)?;
						// Assign
						if has_parentheses {
							return Err(Error { variant: ErrorVariant::NotYetImplemented("Arrays".into()), line_number: line_number.cloned(), column_number: Some(*column) });
						}
						self.int_variables.insert(name.clone(), r_value);
					}
					StatementVariant::AssignReal(l_value, r_value_expression) => {
						// Get what to assign to
						let (name, _arguments, has_parentheses) = match l_value {
							RealExpression { variant: RealExpressionVariant::RealIdentifierOrFunction { name, arguments, uses_fn_keyword: false, has_parentheses }, .. }
								=> (name, arguments, *has_parentheses),
							RealExpression { variant: _, column } => return Err(Error { variant: ErrorVariant::InvalidLValue, line_number: line_number.cloned(), column_number: Some(*column) }),
						};
						// Get r-value
						let r_value = Self::execute_real_expression(&self, r_value_expression, line_number)?;
						// Assign
						if has_parentheses {
							return Err(Error { variant: ErrorVariant::NotYetImplemented("Arrays".into()), line_number: line_number.cloned(), column_number: Some(*column) });
						}
						self.real_variables.insert(name.clone(), r_value);
					}
					StatementVariant::AssignComplex(l_value, r_value_expression) => {
						// Get what to assign to
						let (name, _arguments, has_parentheses) = match l_value {
							ComplexExpression { variant: ComplexExpressionVariant::ComplexIdentifierOrFunction { name, arguments, uses_fn_keyword: false, has_parentheses }, .. }
								=> (name, arguments, *has_parentheses),
							ComplexExpression { variant: _, column } => return Err(Error { variant: ErrorVariant::InvalidLValue, line_number: line_number.cloned(), column_number: Some(*column) }),
						};
						// Get r-value
						let r_value = Self::execute_complex_expression(&self, r_value_expression, line_number)?;
						// Assign
						if has_parentheses {
							return Err(Error { variant: ErrorVariant::NotYetImplemented("Arrays".into()), line_number: line_number.cloned(), column_number: Some(*column) });
						}
						self.complex_variables.insert(name.clone(), r_value);
					}
					StatementVariant::AssignString(l_value, r_value_expression) => {
						// Get what to assign to
						let (name, _arguments, has_parentheses) = match l_value {
							StringExpression { variant: StringExpressionVariant::StringIdentifierOrFunction { name, arguments, uses_fn_keyword: false, has_parentheses }, .. }
								=> (name, arguments, *has_parentheses),
							StringExpression { variant: _, column } => return Err(Error { variant: ErrorVariant::InvalidLValue, line_number: line_number.cloned(), column_number: Some(*column) }),
						};
						// Get r-value
						let r_value = Self::execute_string_expression(&self, r_value_expression, line_number)?;
						// Assign
						if has_parentheses {
							return Err(Error { variant: ErrorVariant::NotYetImplemented("Arrays".into()), line_number: line_number.cloned(), column_number: Some(*column) });
						}
						self.string_variables.insert(name.clone(), r_value);
					}
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
						None => return Err(Error { variant: ErrorVariant::NonNumberValueCastToInt(value), line_number: line_number.cloned(), column_number: Some(*expression_column) }),
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
			IntExpressionVariant::BitwiseNot(sub_expression) => IntValue {
				value: Rc::new(!&*Self::execute_int_expression(self, sub_expression, line_number)?.value),
			},
			IntExpressionVariant::IntIdentifierOrFunction { name, arguments: _, uses_fn_keyword, has_parentheses } => {
				if *has_parentheses || *uses_fn_keyword {
					return Err(Error { variant: ErrorVariant::NotYetImplemented("Arrays and functions".into()), line_number: line_number.cloned(), column_number: Some(*expression_column) });
				}
				match self.int_variables.get(name) {
					Some(int_variable) => int_variable.clone(),
					None => return Err(Error { variant: ErrorVariant::VariableNotFound, line_number: line_number.cloned(), column_number: Some(*expression_column) }),
				}
			}
		})
	}

	fn execute_bool_expression(&self, expression: &BoolExpression, line: Option<&BigInt>) -> Result<BoolValue, Error> {
		let BoolExpression { variant: expression_variant, column: expression_column } = expression;
		Ok(match expression_variant {
			_ => todo!(),
		})
	}

	fn execute_real_expression(&self, expression: &RealExpression, line: Option<&BigInt>) -> Result<RealValue, Error> {
		let RealExpression { variant: expression_variant, column: expression_column } = expression;
		Ok(match expression_variant {
			_ => todo!(),
		})
	}

	fn execute_complex_expression(&self, expression: &ComplexExpression, line: Option<&BigInt>) -> Result<ComplexValue, Error> {
		let ComplexExpression { variant: expression_variant, column: expression_column } = expression;
		Ok(match expression_variant {
			_ => todo!(),
		})
	}

	fn execute_string_expression(&self, expression: &StringExpression, line: Option<&BigInt>) -> Result<StringValue, Error> {
		let StringExpression { variant: expression_variant, column: expression_column } = expression;
		Ok(match expression_variant {
			_ => todo!(),
		})
	}

	/*fn execute_expression(&self, expression: &Expression, line: Option<&BigInt>) -> Result<Value, Error> {
		let Expression { variant, column } = expression;
		Ok(match variant {
			ExpressionVariant::StringLiteral(string) => Value::String(string.clone()),
			ExpressionVariant::IntegerLiteral(value) => Value::Int(value.clone()),
			ExpressionVariant::FloatLiteral { value, is_imaginary } => match is_imaginary {
				false => Value::Float(*value),
				true => Value::Complex(Complex64::new(0., *value)),
			}
			ExpressionVariant::AdditionConcatenation(lhs_expression, rhs_expression) => {
				let lhs = self.execute_expression(&lhs_expression, line)?;
				let rhs = self.execute_expression(&rhs_expression, line)?;
				let (lhs, rhs) = lhs.upcast(rhs, line, *column)?;
				match (lhs, rhs) {
					(Value::Bool(lhs), Value::Bool(rhs)) => Value::Int(Rc::new(BigInt::from_u8(lhs as u8).unwrap() + BigInt::from_u8(rhs as u8).unwrap())),
					(Value::Int(mut lhs), Value::Int(rhs)) => {
						let int = Rc::<BigInt>::make_mut(&mut lhs);
						(*int) += &*rhs;
						Value::Int(lhs)
					}
					(Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs + rhs),
					(Value::Complex(lhs), Value::Complex(rhs)) => Value::Complex(lhs + rhs),
					(Value::String(mut lhs), Value::String(rhs)) => {
						let string = Rc::<String>::make_mut(&mut lhs);
						string.push_str(rhs.as_str());
						Value::String(lhs)
					}
					_ => unreachable!(),
				}
			}
			ExpressionVariant::Subtraction(lhs_expression, rhs_expression) => {
				let lhs = self.execute_expression(&lhs_expression, line)?;
				let rhs = self.execute_expression(&rhs_expression, line)?;
				let (lhs, rhs) = lhs.upcast(rhs, line, *column)?;
				match (lhs, rhs) {
					(Value::Bool(lhs), Value::Bool(rhs)) => Value::Int(Rc::new(BigInt::from_u8(lhs as u8).unwrap() - BigInt::from_u8(rhs as u8).unwrap())),
					(Value::Int(mut lhs), Value::Int(rhs)) => {
						let int = Rc::<BigInt>::make_mut(&mut lhs);
						(*int) -= &*rhs;
						Value::Int(lhs)
					}
					(Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs - rhs),
					(Value::Complex(lhs), Value::Complex(rhs)) => Value::Complex(lhs - rhs),
					(Value::String(_), Value::String(_)) => return Err(Error { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line.cloned(), column_number: Some(*column) }),
					_ => unreachable!(),
				}
			}
			ExpressionVariant::Concatenation(lhs_expression, rhs_expression) => {
				let lhs = self.execute_expression(&lhs_expression, line)?;
				let rhs = self.execute_expression(&rhs_expression, line)?;
				match (lhs, rhs) {
					(Value::String(mut lhs), Value::String(rhs)) => {
						let string = Rc::<String>::make_mut(&mut lhs);
						string.push_str(rhs.as_str());
						Value::String(lhs)
					}
					_ => return Err(Error { variant: ErrorVariant::CannotConcatenateNumbers, line_number: line.cloned(), column_number: Some(*column) }),
				}
			}
			ExpressionVariant::Multiplication(lhs_expression, rhs_expression) => {
				let lhs = self.execute_expression(&lhs_expression, line)?;
				let rhs = self.execute_expression(&rhs_expression, line)?;
				let (lhs, rhs) = lhs.upcast(rhs, line, *column)?;
				match (lhs, rhs) {
					(Value::Bool(lhs), Value::Bool(rhs)) => Value::Bool(lhs && rhs),
					(Value::Int(mut lhs), Value::Int(rhs)) => {
						let int = Rc::<BigInt>::make_mut(&mut lhs);
						(*int) *= &*rhs;
						Value::Int(lhs)
					}
					(Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs * rhs),
					(Value::Complex(lhs), Value::Complex(rhs)) => Value::Complex(lhs * rhs),
					(Value::String(_), Value::String(_)) => return Err(Error { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line.cloned(), column_number: Some(*column) }),
					_ => unreachable!(),
				}
			}
			ExpressionVariant::Division(lhs_expression, rhs_expression) => {
				let lhs = self.execute_expression(&lhs_expression, line)?;
				let rhs = self.execute_expression(&rhs_expression, line)?;
				let (lhs, rhs) = lhs.upcast(rhs, line, *column)?;
				match (lhs, rhs) {
					(Value::Bool(lhs), Value::Bool(rhs)) => match (lhs, rhs) {
						(true, true) => Value::Bool(true),
						(true, false) => Value::Float(INFINITY),
						(false, true) => Value::Bool(false),
						(false, false) => Value::Float(NAN),
					}
					(Value::Int(mut lhs), Value::Int(rhs)) => {
						if *rhs == BigInt::ZERO || &*lhs % &*rhs == BigInt::ZERO {
							return Ok(Value::Float(Value::Int(lhs).cast_to_float(line, *column)?.unwrap_float() / Value::Int(rhs).cast_to_float(line, *column)?.unwrap_float()))
						}
						let int = Rc::<BigInt>::make_mut(&mut lhs);
						(*int) /= &*rhs;
						Value::Int(lhs)
					}
					(Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs / rhs),
					(Value::Complex(lhs), Value::Complex(rhs)) => Value::Complex(lhs / rhs),
					(Value::String(_), Value::String(_)) => return Err(Error { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line.cloned(), column_number: Some(*column) }),
					_ => unreachable!(),
				}
			}
			ExpressionVariant::FlooredDivision(lhs_expression, rhs_expression) => {
				let mut lhs = self.execute_expression(&lhs_expression, line)?.cast_to_int(line, *column)?.unwrap_int();
				let rhs = self.execute_expression(&rhs_expression, line)?.cast_to_int(line, *column)?.unwrap_int();
				if *rhs == BigInt::ZERO {
					return Err(Error { variant: ErrorVariant::FlooredDivisionByZero, line_number: line.cloned(), column_number: Some(*column) })
				}
				let int = Rc::<BigInt>::make_mut(&mut lhs);
				(*int) /= &*rhs;
				Value::Int(lhs)
			}
			ExpressionVariant::EqualTo(lhs_expression, rhs_expression) => {
				let lhs = self.execute_expression(&lhs_expression, line)?;
				let rhs = self.execute_expression(&rhs_expression, line)?;
				let (lhs, rhs) = lhs.upcast(rhs, line, *column)?;
				match (lhs, rhs) {
					(Value::Bool(lhs), Value::Bool(rhs)) => Value::Bool(lhs == rhs),
					(Value::Int(lhs), Value::Int(rhs)) => Value::Bool(lhs == rhs),
					(Value::Float(lhs), Value::Float(rhs)) => Value::Bool(lhs == rhs),
					(Value::Complex(lhs), Value::Complex(rhs)) => Value::Bool(lhs == rhs),
					(Value::String(lhs), Value::String(rhs)) => Value::Bool(lhs == rhs),
					_ => unreachable!(),
				}
			}
			ExpressionVariant::Exponentiation(lhs_expression, rhs_expression) => {
				let lhs = self.execute_expression(&lhs_expression, line)?;
				let rhs = self.execute_expression(&rhs_expression, line)?;
				let (lhs, rhs) = lhs.upcast(rhs, line, *column)?;
				match (lhs, rhs) {
					(Value::Bool(lhs), Value::Bool(rhs)) => match (lhs, rhs) {
						(true, true) => Value::Bool(true),
						(true, false) => Value::Bool(true),
						(false, true) => Value::Bool(false),
						(false, false) => Value::Bool(true),
					}
					(Value::Int(lhs), Value::Int(rhs)) => {
						match rhs.to_u32() {
							Some(rhs_as_u32) => Value::Int(Rc::new(lhs.pow(rhs_as_u32))),
							None => Value::Float(Value::Int(lhs).cast_to_float(line, *column).unwrap().unwrap_float().powf(Value::Int(rhs).cast_to_float(line, *column).unwrap().unwrap_float())),
						}
					}
					(Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs.powf(rhs)),
					(Value::Complex(lhs), Value::Complex(rhs)) => Value::Complex(lhs.powc(rhs)),
					(Value::String(_), Value::String(_)) => return Err(Error { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line.cloned(), column_number: Some(*column) }),
					_ => unreachable!(),
				}
			}
			ExpressionVariant::NotEqualTo(lhs_expression, rhs_expression) => {
				let lhs = self.execute_expression(&lhs_expression, line)?;
				let rhs = self.execute_expression(&rhs_expression, line)?;
				let (lhs, rhs) = lhs.upcast(rhs, line, *column)?;
				match (lhs, rhs) {
					(Value::Bool(lhs), Value::Bool(rhs)) => Value::Bool(lhs != rhs),
					(Value::Int(lhs), Value::Int(rhs)) => Value::Bool(lhs != rhs),
					(Value::Float(lhs), Value::Float(rhs)) => Value::Bool(lhs != rhs),
					(Value::Complex(lhs), Value::Complex(rhs)) => Value::Bool(lhs != rhs),
					(Value::String(lhs), Value::String(rhs)) => Value::Bool(lhs != rhs),
					_ => unreachable!(),
				}
			}
			ExpressionVariant::LessThan(lhs_expression, rhs_expression) => {
				let lhs = self.execute_expression(&lhs_expression, line)?;
				let rhs = self.execute_expression(&rhs_expression, line)?;
				let (lhs, rhs) = lhs.upcast(rhs, line, *column)?;
				match (lhs, rhs) {
					(Value::Bool(lhs), Value::Bool(rhs)) => Value::Bool(lhs < rhs),
					(Value::Int(lhs), Value::Int(rhs)) => Value::Bool(lhs < rhs),
					(Value::Float(lhs), Value::Float(rhs)) => Value::Bool(lhs < rhs),
					(Value::Complex(lhs), Value::Complex(rhs)) => match lhs.im != 0. || rhs.im != 0. {
						false => Value::Bool(lhs.re < rhs.re),
						true => return Err(Error { variant: ErrorVariant::NonRealComparison(lhs, rhs), line_number: line.cloned(), column_number: Some(*column) }),
					}
					(Value::String(_), Value::String(_))=>
						return Err(Error { variant: ErrorVariant::NotYetImplemented("String <, <=, >, >=".into()), line_number: line.cloned(), column_number: Some(*column) }),
					_ => unreachable!(),
				}
			}
			ExpressionVariant::LessThanOrEqualTo(lhs_expression, rhs_expression) => {
				let lhs = self.execute_expression(&lhs_expression, line)?;
				let rhs = self.execute_expression(&rhs_expression, line)?;
				let (lhs, rhs) = lhs.upcast(rhs, line, *column)?;
				match (lhs, rhs) {
					(Value::Bool(lhs), Value::Bool(rhs)) => Value::Bool(lhs <= rhs),
					(Value::Int(lhs), Value::Int(rhs)) => Value::Bool(lhs <= rhs),
					(Value::Float(lhs), Value::Float(rhs)) => Value::Bool(lhs <= rhs),
					(Value::Complex(lhs), Value::Complex(rhs)) => match lhs.im != 0. || rhs.im != 0. {
						false => Value::Bool(lhs.re <= rhs.re),
						true => return Err(Error { variant: ErrorVariant::NonRealComparison(lhs, rhs), line_number: line.cloned(), column_number: Some(*column) }),
					}
					(Value::String(_), Value::String(_))=>
						return Err(Error { variant: ErrorVariant::NotYetImplemented("String <, <=, >, >=".into()), line_number: line.cloned(), column_number: Some(*column) }),
					_ => unreachable!(),
				}
			}
			ExpressionVariant::GreaterThan(lhs_expression, rhs_expression) => {
				let lhs = self.execute_expression(&lhs_expression, line)?;
				let rhs = self.execute_expression(&rhs_expression, line)?;
				let (lhs, rhs) = lhs.upcast(rhs, line, *column)?;
				match (lhs, rhs) {
					(Value::Bool(lhs), Value::Bool(rhs)) => Value::Bool(lhs > rhs),
					(Value::Int(lhs), Value::Int(rhs)) => Value::Bool(lhs > rhs),
					(Value::Float(lhs), Value::Float(rhs)) => Value::Bool(lhs > rhs),
					(Value::Complex(lhs), Value::Complex(rhs)) => match lhs.im != 0. || rhs.im != 0. {
						false => Value::Bool(lhs.re > rhs.re),
						true => return Err(Error { variant: ErrorVariant::NonRealComparison(lhs, rhs), line_number: line.cloned(), column_number: Some(*column) }),
					}
					(Value::String(_), Value::String(_))=>
						return Err(Error { variant: ErrorVariant::NotYetImplemented("String <, <=, >, >=".into()), line_number: line.cloned(), column_number: Some(*column) }),
					_ => unreachable!(),
				}
			}
			ExpressionVariant::GreaterThanOrEqualTo(lhs_expression, rhs_expression) => {
				let lhs = self.execute_expression(&lhs_expression, line)?;
				let rhs = self.execute_expression(&rhs_expression, line)?;
				let (lhs, rhs) = lhs.upcast(rhs, line, *column)?;
				match (lhs, rhs) {
					(Value::Bool(lhs), Value::Bool(rhs)) => Value::Bool(lhs >= rhs),
					(Value::Int(lhs), Value::Int(rhs)) => Value::Bool(lhs >= rhs),
					(Value::Float(lhs), Value::Float(rhs)) => Value::Bool(lhs >= rhs),
					(Value::Complex(lhs), Value::Complex(rhs)) => match lhs.im != 0. || rhs.im != 0. {
						false => Value::Bool(lhs.re >= rhs.re),
						true => return Err(Error { variant: ErrorVariant::NonRealComparison(lhs, rhs), line_number: line.cloned(), column_number: Some(*column) }),
					}
					(Value::String(_), Value::String(_))=>
						return Err(Error { variant: ErrorVariant::NotYetImplemented("String <, <=, >, >=".into()), line_number: line.cloned(), column_number: Some(*column) }),
					_ => unreachable!(),
				}
			}
			ExpressionVariant::And(lhs_expression, rhs_expression) => {
				let lhs = self.execute_expression(&lhs_expression, line)?;
				let rhs = self.execute_expression(&rhs_expression, line)?;
				let (lhs, rhs) = lhs.upcast(rhs, line, *column)?;
				match (lhs, rhs) {
					(Value::Bool(lhs), Value::Bool(rhs)) => Value::Bool(lhs && rhs),
					(Value::Int(mut lhs), Value::Int(rhs)) => {
						let int = Rc::<BigInt>::make_mut(&mut lhs);
						(*int) &= &*rhs;
						Value::Int(lhs)
					}
					(Value::Float(lhs), Value::Float(rhs)) =>
						Value::Int(Rc::new((&*Value::Float(lhs).cast_to_int(line, *column).unwrap().unwrap_int()) & (&*Value::Float(rhs).cast_to_int(line, *column).unwrap().unwrap_int()))),
					(Value::Complex(lhs), Value::Complex(rhs)) =>
						Value::Int(Rc::new((&*Value::Complex(lhs).cast_to_int(line, *column)?.unwrap_int()) & (&*Value::Complex(rhs).cast_to_int(line, *column)?.unwrap_int()))),
					(Value::String(_), Value::String(_)) => return Err(Error { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line.cloned(), column_number: Some(*column) }),
					_ => unreachable!(),
				}
			}
			ExpressionVariant::Or(lhs_expression, rhs_expression) => {
				let lhs = self.execute_expression(&lhs_expression, line)?;
				let rhs = self.execute_expression(&rhs_expression, line)?;
				let (lhs, rhs) = lhs.upcast(rhs, line, *column)?;
				match (lhs, rhs) {
					(Value::Bool(lhs), Value::Bool(rhs)) => Value::Bool(lhs || rhs),
					(Value::Int(mut lhs), Value::Int(rhs)) => {
						let int = Rc::<BigInt>::make_mut(&mut lhs);
						(*int) |= &*rhs;
						Value::Int(lhs)
					}
					(Value::Float(lhs), Value::Float(rhs)) =>
						Value::Int(Rc::new((&*Value::Float(lhs).cast_to_int(line, *column).unwrap().unwrap_int()) | (&*Value::Float(rhs).cast_to_int(line, *column).unwrap().unwrap_int()))),
					(Value::Complex(lhs), Value::Complex(rhs)) =>
						Value::Int(Rc::new((&*Value::Complex(lhs).cast_to_int(line, *column)?.unwrap_int()) | (&*Value::Complex(rhs).cast_to_int(line, *column)?.unwrap_int()))),
					(Value::String(_), Value::String(_)) => return Err(Error { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line.cloned(), column_number: Some(*column) }),
					_ => unreachable!(),
				}
			}
			ExpressionVariant::Negation(sub_expression) => {
				let sub_expression = self.execute_expression(&sub_expression, line)?;
				match sub_expression {
					Value::Bool(sub_expression) => match sub_expression {
						true => Value::Int(Rc::new(BigInt::from_i8(-1).unwrap())),
						false => Value::Bool(false),
					}
					Value::Int(mut sub_expression) => {
						let int = Rc::<BigInt>::make_mut(&mut sub_expression);
						let int = -(int.clone());
						Value::Int(Rc::new(int))
					}
					Value::Float(sub_expression) => Value::Float(-sub_expression),
					Value::Complex(sub_expression) => Value::Complex(-sub_expression),
					Value::String(_) => return Err(Error { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line.cloned(), column_number: Some(*column) }),
				}
			}
			ExpressionVariant::UnaryPlus(sub_expression) => self.execute_expression(&sub_expression, line)?,
			ExpressionVariant::Not(sub_expression) => {
				let sub_expression = self.execute_expression(&sub_expression, line)?;
				match sub_expression {
					Value::Bool(sub_expression) => match sub_expression {
						true => Value::Int(Rc::new(BigInt::from_i8(-2).unwrap())),
						false => Value::Int(Rc::new(BigInt::from_i8(-1).unwrap())),
					}
					Value::Int(mut sub_expression) => {
						let int = Rc::<BigInt>::make_mut(&mut sub_expression);
						let int = !(int.clone());
						Value::Int(Rc::new(int))
					}
					Value::Float(sub_expression) => Value::Int(Rc::new(!(&*Value::Float(sub_expression).cast_to_int(line, *column).unwrap().unwrap_int()))),
					Value::Complex(sub_expression) => Value::Int(Rc::new(!(&*Value::Complex(sub_expression).cast_to_int(line, *column)?.unwrap_int()))),
					Value::String(_) => return Err(Error { variant: ErrorVariant::CannotUseThisOperatorOnAString, line_number: line.cloned(), column_number: Some(*column) }),
				}
			}
			ExpressionVariant::IdentifierOrFunction { name, identifier_type, is_optional, arguments: _, uses_fn_keyword, has_parentheses } => {
				if *has_parentheses || *uses_fn_keyword {
					return Err(Error { variant: ErrorVariant::NotYetImplemented("Arrays and functions".into()), line_number: line.cloned(), column_number: Some(*column) });
				}
				if *is_optional {
					return Err(Error { variant: ErrorVariant::NotYetImplemented("Optionals".into()), line_number: line.cloned(), column_number: Some(*column) });
				}
				match identifier_type {
					IdentifierType::Integer => match self.int_variables.get(name) {
						Some(int_variable) => Value::Int(int_variable.clone()),
						None => return Err(Error { variant: ErrorVariant::VariableNotFound, line_number: line.cloned(), column_number: Some(*column) }),
					}
					IdentifierType::UnmarkedNumber => match self.float_variables.get(name) {
						Some(float_variable) => Value::Float(*float_variable),
						None => return Err(Error { variant: ErrorVariant::VariableNotFound, line_number: line.cloned(), column_number: Some(*column) }),
					}
					IdentifierType::ComplexNumber => match self.complex_variables.get(name) {
						Some(complex_variable) => Value::Complex(*complex_variable),
						None => return Err(Error { variant: ErrorVariant::VariableNotFound, line_number: line.cloned(), column_number: Some(*column) }),
					}
					IdentifierType::String => match self.string_variables.get(name) {
						Some(string_variable) => Value::String(string_variable.clone()),
						None => return Err(Error { variant: ErrorVariant::VariableNotFound, line_number: line.cloned(), column_number: Some(*column) }),
					}
				}
			}
			ExpressionVariant::PrintComma | ExpressionVariant::PrintSemicolon => unreachable!(),
			//_ => return Err(Error { variant: ErrorVariant::NotYetImplemented("Other expressions".into()), line_number: line.cloned(), column_number: Some(*column) })
		})
		
	}*/
}

/*#[derive(Clone, Debug)]
pub enum Value {
	Int(Rc<BigInt>),
	Float(f64),
	Bool(bool),
	Complex(Complex64),
	String(Rc<String>),
}*/

/*impl Value {
	fn upcast(self, rhs: Self, line_number: Option<&BigInt>, column_number: NonZeroUsize) -> Result<(Self, Self), Error> {
		Ok(match (&self, &rhs) {
			// Two of the same type
			(Self::Float(_), Self::Float(_)) | (Self::Int(_), Self::Int(_)) | (Self::Complex(_), Self::Complex(_)) | (Self::Bool(_), Self::Bool(_)) | (Self::String(_), Self::String(_)) => (self, rhs),
			// To int
			(Self::Int(_), Self::Bool(_)) => (self, rhs.cast_to_int(line_number, column_number)?),
			(Self::Bool(_), Self::Int(_)) => (self.cast_to_int(line_number, column_number)?, rhs),
			// To float
			(Self::Float(_), Self::Bool(_) | Self::Int(_)) => (self, rhs.cast_to_float(line_number, column_number)?),
			(Self::Bool(_) | Self::Int(_), Self::Float(_)) => (self.cast_to_float(line_number, column_number)?, rhs),
			// To complex
			(Self::Complex(_), Self::Bool(_) | Self::Int(_) | Self::Float(_)) => (self, rhs.cast_to_complex(line_number, column_number)?),
			(Self::Bool(_) | Self::Int(_) | Self::Float(_), Self::Complex(_)) => (self.cast_to_complex(line_number, column_number)?, rhs),
			_ => return Err(Error { variant: ErrorVariant::StringCastToNumber, line_number: line_number.cloned(), column_number: Some(column_number) }),
		})
	}

	fn cast_to_int(self, line_number: Option<&BigInt>, column_number: NonZeroUsize) -> Result<Self, Error> {
		Ok(match self {
			Self::Bool(value) => Self::Int(Rc::new(BigInt::from_u8(value as u8).unwrap())),
			Self::Int(_) => self,
			Self::Float(value) => Self::Int(Rc::new(match BigInt::from_f64(value) {
				Some(value) => value,
				None => return Err(Error { variant: ErrorVariant::NonNumberValueCastToInt(self), line_number: line_number.cloned(), column_number: Some(column_number) }),
			})),
			Self::Complex(Complex { re, im }) => match im == 0. {
				true => Self::Int(Rc::new(match BigInt::from_f64(re) {
					Some(value) => value,
					None => return Err(Error { variant: ErrorVariant::NonNumberValueCastToInt(self), line_number: line_number.cloned(), column_number: Some(column_number) }),
				})),
				false => return Err(Error { variant: ErrorVariant::NonRealComplexValueCastToReal(self), line_number: line_number.cloned(), column_number: Some(column_number) }),
			}
			Self::String(_) => return Err(Error { variant: ErrorVariant::StringCastToNumber, line_number: line_number.cloned(), column_number: Some(column_number) }),
		})
	}

	fn cast_to_string(self, line_number: Option<&BigInt>, column_number: NonZeroUsize) -> Result<Self, Error> {
		Ok(match self {
			Self::String(_) => self,
			_ => return Err(Error { variant: ErrorVariant::StringCastToNumber, line_number: line_number.cloned(), column_number: Some(column_number) }),
		})
	}

	fn cast_to_float(self, line_number: Option<&BigInt>, column_number: NonZeroUsize) -> Result<Self, Error> {
		Ok(match self {
			Self::Bool(value) => Self::Float(value as u8 as f64),
			Self::Int(value) => Self::Float(match (&*value).to_i128() {
				Some(int_value) => int_value as f64,
				None => match &*value > &BigInt::ZERO {
					true => INFINITY,
					false => NEG_INFINITY,
				}
			}),
			Self::Float(_) => self,
			Self::Complex(Complex { re, im }) => match im == 0. {
				true => Self::Float(re),
				false => return Err(Error { variant: ErrorVariant::NonRealComplexValueCastToReal(self), line_number: line_number.cloned(), column_number: Some(column_number) }),
			}
			Self::String(_) => return Err(Error { variant: ErrorVariant::StringCastToNumber, line_number: line_number.cloned(), column_number: Some(column_number) }),
		})
	}

	fn cast_to_complex(self, line_number: Option<&BigInt>, column_number: NonZeroUsize) -> Result<Self, Error> {
		Ok(match self {
			Self::Bool(value) => Self::Complex(Complex { re: value as u8 as f64, im: 0. }),
			Self::Int(value) => Self::Complex(Complex { re: match (&*value).to_i128() {
				Some(int_value) => int_value as f64,
				None => match &*value > &BigInt::ZERO {
					true => INFINITY,
					false => NEG_INFINITY,
				}
			}, im: 0. }),
			Self::Float(value) => Self::Complex(Complex { re: value, im: 0. }),
			Self::Complex(_) => self,
			Self::String(_) => return Err(Error { variant: ErrorVariant::StringCastToNumber, line_number: line_number.cloned(), column_number: Some(column_number) }),
		})
	}

	pub fn get_type_name(&self) -> &'static str {
		match self {
			Self::Bool(_) => "boolean",
			Self::Float(_) => "float",
			Self::Int(_) => "integer",
			Self::String(_) => "string",
			Self::Complex(_) => "complex",
		}
	}

	pub fn unwrap_bool(self) -> bool {
		match self {
			Value::Bool(value) => value,
			_ => panic!(),
		}
	}

	pub fn unwrap_int(self) -> Rc<BigInt> {
		match self {
			Value::Int(value) => value,
			_ => panic!(),
		}
	}

	pub fn unwrap_float(self) -> f64 {
		match self {
			Value::Float(value) => value,
			_ => panic!(),
		}
	}

	pub fn unwrap_complex(self) -> Complex64 {
		match self {
			Value::Complex(value) => value,
			_ => panic!(),
		}
	}

	pub fn unwrap_string(self) -> Rc<String> {
		match self {
			Value::String(value) => value,
			_ => panic!(),
		}
	}
}

impl Display for Value {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			Self::String(value) => write!(f, "{value}"),
			Self::Int(value) => write!(f, "{value}"),
			Self::Float(value) => write!(f, "{value}"),
			Self::Complex(value) => write!(f, "{value}"),
			Self::Bool(value) => write!(f, "{}", *value as u8),
		}
	}
}*/