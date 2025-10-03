use std::{collections::BTreeMap, num::NonZeroUsize, ops::{RangeFrom, RangeTo}, rc::Rc};

use num::BigInt;

use crate::mavagk_basic::{abstract_syntax_tree::{AngleOption, Datum, MachineOption, MathOption, OptionVariableAndValue, Statement, StatementVariant}, error::Error, machine::Machine};

/// A MavagkBasic program containing all its lines, does not include a direct mode line.
pub struct Program {
	/// Maps line numbers to lines
	lines: BTreeMap<Rc<BigInt>, Line>,

	angle_options: BTreeMap<(Rc<BigInt>, usize), Option<AngleOption>>,
	math_options: BTreeMap<(Rc<BigInt>, usize), Option<MathOption>>,
	machine_options: BTreeMap<(Rc<BigInt>, usize), Option<MachineOption>>,

	data: BTreeMap<Rc<BigInt>, Box<[(Datum, NonZeroUsize)]>>,
}

impl Program {
	pub fn new() -> Self {
		Self {
			lines: BTreeMap::new(),
			angle_options: BTreeMap::new(),
			math_options: BTreeMap::new(),
			machine_options: BTreeMap::new(),
			data: BTreeMap::new(),
		}
	}

	pub fn get_options(&self, machine: &mut Machine, line_number: Rc<BigInt>, sub_line: usize) {
		machine.angle_option = match self.angle_options.range::<(Rc<BigInt>, usize), RangeTo<(Rc<BigInt>, usize)>>(..(line_number.clone(), sub_line)).last() {
			Some(option) => *option.1,
			None => None,
		};
		machine.math_option = match self.math_options.range::<(Rc<BigInt>, usize), RangeTo<(Rc<BigInt>, usize)>>(..(line_number.clone(), sub_line)).last() {
			Some(option) => *option.1,
			None => None,
		};
		machine.machine_option = match self.machine_options.range::<(Rc<BigInt>, usize), RangeTo<(Rc<BigInt>, usize)>>(..(line_number, sub_line)).last() {
			Some(option) => *option.1,
			None => None,
		};
	}

	pub fn contains_line(&self, line_number: &BigInt) -> bool {
		return self.lines.contains_key(line_number);
	}

	pub fn insert_line(&mut self, line_number: Rc<BigInt>, line: Line) {
		self.lines.remove(&line_number);
		for (sub_line_number, statement) in line.optimized_statements.iter().enumerate() {
			match statement.variant {
				StatementVariant::Option(OptionVariableAndValue::Angle(option)) => {
					self.angle_options.insert((line_number.clone(), sub_line_number), option);
				}
				StatementVariant::Option(OptionVariableAndValue::Math(option)) => {
					self.math_options.insert((line_number.clone(), sub_line_number), option);
				}
				StatementVariant::Option(OptionVariableAndValue::Machine(option)) => {
					self.machine_options.insert((line_number.clone(), sub_line_number), option);
				}
				_ => {},
			}
		}
		let mut data = Vec::new();
		for sub_line in line.optimized_statements.iter() {
			match sub_line {
				Statement { variant: StatementVariant::Data(statement_data), .. } => {
					for datum in statement_data {
						data.push(datum.clone());
					}
				}
				_ => {}
			}
		}
		self.data.insert(line_number.clone(), data.into_boxed_slice());
		self.lines.insert(line_number, line);
	}

	pub fn remove_line(&mut self, line_number: &Rc<BigInt>) {
		let line = self.lines.remove(line_number);
		self.data.remove(line_number);
		let line = match line {
			Some(line) => line,
			None => return,
		};
		for (sub_line_number, statement) in line.optimized_statements.iter().enumerate() {
			match statement.variant {
				StatementVariant::Option(OptionVariableAndValue::Angle(_)) => {
					self.angle_options.remove(&(line_number.clone(), sub_line_number));
				}
				StatementVariant::Option(OptionVariableAndValue::Math(_)) => {
					self.math_options.remove(&(line_number.clone(), sub_line_number));
				}
				StatementVariant::Option(OptionVariableAndValue::Machine(_)) => {
					self.machine_options.remove(&(line_number.clone(), sub_line_number));
				}
				_ => {},
			}
		}
	}

	pub fn get_line(&self, line_number: &BigInt) -> Option<&Line> {
		self.lines.get(line_number)
	}

	pub fn get_first_line_after(&self, line_number: &BigInt) -> Option<&Rc<BigInt>> {
		match self.lines.range::<BigInt, RangeFrom<&BigInt>>(line_number..).nth(1) {
			Some(value) => Some(value.0),
			None => None,
		}
	}

	pub fn get_first_data_line_after(&self, line_number: &BigInt) -> Option<&Rc<BigInt>> {
		match self.data.range::<BigInt, RangeFrom<&BigInt>>(line_number..).nth(1) {
			Some(value) => Some(value.0),
			None => None,
		}
	}

	pub fn get_first_line(&self) -> Option<&Rc<BigInt>> {
		match self.lines.first_key_value() {
			Some(value) => Some(value.0),
			None => None,
		}
	}

	pub fn get_first_data_line(&self) -> Option<&Rc<BigInt>> {
		match self.data.first_key_value() {
			Some(value) => Some(value.0),
			None => None,
		}
	}

	pub fn get_lines(&self) -> &BTreeMap<Rc<BigInt>, Line> {
		&self.lines
	}

	pub fn clear_program(&mut self) {
		*self = Self::new();
	}
}

pub struct Line {
	pub unoptimized_statements: Box<[Statement]>,
	pub optimized_statements: Box<[Statement]>,
	pub error: Option<Error>,
	pub source_code: Box<str>,
}