use std::{f64::{INFINITY, NAN, NEG_INFINITY}, fmt::{self, Display, Formatter}, num::NonZeroUsize, rc::Rc};

use num::{complex::Complex64, BigInt, Complex, FromPrimitive, ToPrimitive};

use crate::mavagk_basic::{abstract_syntax_tree::{parse_line, Expression, ExpressionVariant, Statement, StatementVariant}, error::{Error, ErrorVariant}, program::Program, token::Token};

pub struct Machine {
	is_executing_unnumbered_line: bool,
	line_executing: BigInt,
}

impl Machine {
	pub fn new() -> Self {
		Self {
			is_executing_unnumbered_line: false,
			line_executing: 0.into(),
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
	}

	pub fn line_of_text_entered(&mut self, line: Box<str>, program: &mut Program) -> Result<(), Error> {
		// Parse line
		let (line_number, tokens) = Token::tokenize_line(&*line)?;
		let statements = parse_line(&*tokens)?;
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
							let value = self.execute_expression(sub_expression, line_number)?;
							print!("{value}");
						}
						println!();
					}
					StatementVariant::Goto(sub_expression) | StatementVariant::Run(sub_expression) => {
						// Set the line to be executed next
						match sub_expression {
							Some(sub_expression) => {
								let line_number = self.execute_expression(sub_expression, line_number)?.cast_to_int(line_number, *column)?;
								let line_number = line_number.unwrap_int();
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
					StatementVariant::Assign(_, _) => todo!(),
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

	fn execute_expression(&self, expression: &Expression, line: Option<&BigInt>) -> Result<Value, Error> {
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
			_ => return Err(Error { variant: ErrorVariant::NotYetImplemented("Other expressions".into()), line_number: line.cloned(), column_number: Some(*column) })
		})
	}
}

#[derive(Clone)]
pub enum Value {
	Int(Rc<BigInt>),
	Float(f64),
	Bool(bool),
	Complex(Complex64),
	String(Rc<String>),
}

impl Value {
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
}