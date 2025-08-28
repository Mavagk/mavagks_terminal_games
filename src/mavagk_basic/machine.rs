use std::{f64::{INFINITY, NEG_INFINITY}, fmt::{self, Display, Formatter}, rc::Rc};

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
		loop {
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
			for Statement { variant, column: _ } in statements {
				match variant {
					StatementVariant::Print(sub_expressions) => {
						for sub_expression in sub_expressions {
							let expression = self.execute_expression(sub_expression, line_number)?;
							print!("{expression}");
						}
						println!();
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

	fn execute_expression(&self, expression: &Expression, line_number: Option<&BigInt>) -> Result<Value, Error> {
		let Expression { variant, column } = expression;
		Ok(match variant {
			ExpressionVariant::StringLiteral(string) => Value::String(string.clone()),
			ExpressionVariant::IntegerLiteral(value) => Value::Int(value.clone()),
			ExpressionVariant::FloatLiteral { value, is_imaginary } => match is_imaginary {
				false => Value::Float(*value),
				true => Value::Complex(Complex64::new(0., *value)),
			}
			_ => return Err(Error { variant: ErrorVariant::NotYetImplemented("Other expressions".into()), line_number: line_number.cloned(), column_number: Some(*column) })
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
	fn upcast(self, rhs: Self) -> Result<(Self, Self), Error> {
		Ok(match (&self, &rhs) {
			// Two of the same type
			(Self::Float(_), Self::Float(_)) | (Self::Int(_), Self::Int(_)) | (Self::Complex(_), Self::Complex(_)) | (Self::Bool(_), Self::Bool(_)) | (Self::String(_), Self::String(_)) => (self, rhs),
			// To int
			(Self::Int(_), Self::Bool(_)) => (self, rhs.cast_to_int()?),
			(Self::Bool(_), Self::Int(_)) => (self.cast_to_int()?, rhs),
			// To float
			(Self::Float(_), Self::Bool(_) | Self::Int(_)) => (self, rhs.cast_to_float()?),
			(Self::Bool(_) | Self::Int(_), Self::Float(_)) => (self.cast_to_float()?, rhs),
			// To complex
			(Self::Complex(_), Self::Bool(_) | Self::Int(_) | Self::Float(_)) => (self, rhs.cast_to_complex()?),
			(Self::Bool(_) | Self::Int(_) | Self::Float(_), Self::Complex(_)) => (self.cast_to_complex()?, rhs),
			_ => return Err(Error { variant: ErrorVariant::StringCastToNumber, line_number: None, column_number: None }),
		})
	}

	fn cast_to_int(self) -> Result<Self, Error> {
		Ok(match self {
			Self::Bool(value) => Self::Int(Rc::new(BigInt::from_u8(value as u8).unwrap())),
			Self::Int(_) => self,
			Self::Float(value) => Self::Int(Rc::new(match BigInt::from_f64(value) {
				Some(value) => value,
				None => return Err(Error { variant: ErrorVariant::NonNumberValueCastToInt(self), line_number: None, column_number: None }),
			})),
			Self::Complex(Complex { re, im }) => match im == 0. {
				true => Self::Int(Rc::new(match BigInt::from_f64(re) {
					Some(value) => value,
					None => return Err(Error { variant: ErrorVariant::NonNumberValueCastToInt(self), line_number: None, column_number: None }),
				})),
				false => return Err(Error { variant: ErrorVariant::NonRealComplexValueCastToReal(self), line_number: None, column_number: None }),
			}
			Self::String(_) => return Err(Error { variant: ErrorVariant::StringCastToNumber, line_number: None, column_number: None }),
		})
	}

	fn cast_to_float(self) -> Result<Self, Error> {
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
				false => return Err(Error { variant: ErrorVariant::NonRealComplexValueCastToReal(self), line_number: None, column_number: None }),
			}
			Self::String(_) => return Err(Error { variant: ErrorVariant::StringCastToNumber, line_number: None, column_number: None }),
		})
	}

	fn cast_to_complex(self) -> Result<Self, Error> {
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
			Self::String(_) => return Err(Error { variant: ErrorVariant::StringCastToNumber, line_number: None, column_number: None }),
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