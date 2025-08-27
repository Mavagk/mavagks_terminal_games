use std::{fmt::{self, Display, Formatter}, rc::Rc};

use num::{complex::Complex64, BigInt};

use crate::mavagk_basic::{abstract_syntax_tree::{parse_line, Expression, ExpressionVariant, Statement, StatementVariant}, error::Error, program::Program, token::Token};

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
					None => return Err(Error::InvalidLineNumber(self.line_executing.clone())),
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
			ExpressionVariant::StringLiteral(string) => Value::String((&**string).into()),
			_ => return Err(Error::NotYetImplemented(line_number.cloned(), *column, "".into()))
		})
	}
}

#[derive(Clone)]
enum Value {
	Int(Rc<BigInt>),
	Float(f64),
	Bool(bool),
	Complex(Complex64),
	String(Rc<str>),
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