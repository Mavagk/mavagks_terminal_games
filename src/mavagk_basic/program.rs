use std::{collections::BTreeMap, ops::RangeFrom, rc::Rc};

use num::BigInt;

use crate::mavagk_basic::{abstract_syntax_tree::{AngleOption, MachineOption, MathOption, Statement}, error::Error};

/// A MavagkBasic program containing all its lines, does not include a direct mode line.
pub struct Program {
	/// Maps line numbers to lines
	lines: BTreeMap<Rc<BigInt>, Line>,

	_angle_options: BTreeMap<Rc<BigInt>, AngleOption>,
	_math_options: BTreeMap<Rc<BigInt>, MathOption>,
	_machine_options: BTreeMap<Rc<BigInt>, MachineOption>,
}

impl Program {
	pub fn new() -> Self {
		Self {
			lines: BTreeMap::new(),
			_angle_options: BTreeMap::new(),
			_math_options: BTreeMap::new(),
			_machine_options: BTreeMap::new(),
		}
	}

	pub fn contains_line(&self, line_number: &BigInt) -> bool {
		return self.lines.contains_key(line_number);
	}

	pub fn insert_line(&mut self, line_number: Rc<BigInt>, line: Line) {
		self.lines.insert(line_number, line);
	}

	pub fn remove_line(&mut self, line_number: &BigInt) {
		self.lines.remove(line_number);
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

	pub fn get_first_line(&self) -> Option<&Rc<BigInt>> {
		match self.lines.first_key_value() {
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