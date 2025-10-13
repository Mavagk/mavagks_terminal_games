use std::{collections::{BTreeMap, HashSet}, num::NonZeroUsize, ops::{RangeFrom, RangeTo}, rc::Rc};

use num::BigInt;

use crate::mavagk_basic::{abstract_syntax_tree::{AngleOption, BaseOption, Datum, MachineOption, MathOption, OptionVariableAndValue, Statement, StatementVariant}, error::Error, machine::Machine, token::IdentifierType};

/// A MavagkBasic program containing all its lines, does not include a direct mode line.
pub struct Program {
	/// Maps line numbers to lines
	lines: BTreeMap<Rc<BigInt>, Line>,

	angle_options: BTreeMap<(Rc<BigInt>, usize), Option<AngleOption>>,
	math_options: BTreeMap<(Rc<BigInt>, usize), Option<MathOption>>,
	machine_options: BTreeMap<(Rc<BigInt>, usize), Option<MachineOption>>,
	base_options: BTreeMap<(Rc<BigInt>, usize), Option<BaseOption>>,

	pub float_array_declarations: BTreeMap<Box<str>, HashSet<(Rc<BigInt>, usize)>>,
	pub int_array_declarations: BTreeMap<Box<str>, HashSet<(Rc<BigInt>, usize)>>,
	pub complex_array_declarations: BTreeMap<Box<str>, HashSet<(Rc<BigInt>, usize)>>,
	pub string_array_declarations: BTreeMap<Box<str>, HashSet<(Rc<BigInt>, usize)>>,

	data: BTreeMap<Rc<BigInt>, Box<[(Datum, NonZeroUsize)]>>,
}

impl Program {
	pub fn new() -> Self {
		Self {
			lines: BTreeMap::new(),
			angle_options: BTreeMap::new(),
			math_options: BTreeMap::new(),
			machine_options: BTreeMap::new(),
			base_options: BTreeMap::new(),
			float_array_declarations: BTreeMap::new(),
			int_array_declarations: BTreeMap::new(),
			complex_array_declarations: BTreeMap::new(),
			string_array_declarations: BTreeMap::new(),
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
		machine.machine_option = match self.machine_options.range::<(Rc<BigInt>, usize), RangeTo<(Rc<BigInt>, usize)>>(..(line_number.clone(), sub_line)).last() {
			Some(option) => *option.1,
			None => None,
		};
		machine.base_option = match self.base_options.range::<(Rc<BigInt>, usize), RangeTo<(Rc<BigInt>, usize)>>(..(line_number, sub_line)).last() {
			Some(option) => *option.1,
			None => None,
		};
	}

	pub fn contains_line(&self, line_number: &BigInt) -> bool {
		return self.lines.contains_key(line_number);
	}

	pub fn contains_data_line(&self, line_number: &BigInt) -> bool {
		return self.data.contains_key(line_number);
	}

	pub fn insert_line(&mut self, line_number: Rc<BigInt>, line: Line) {
		self.remove_line(&line_number);
		for (sub_line_number, statement) in line.optimized_statements.iter().enumerate() {
			match &statement.variant {
				StatementVariant::Option(OptionVariableAndValue::Angle(option)) => {
					self.angle_options.insert((line_number.clone(), sub_line_number), *option);
				}
				StatementVariant::Option(OptionVariableAndValue::Math(option)) => {
					self.math_options.insert((line_number.clone(), sub_line_number), *option);
				}
				StatementVariant::Option(OptionVariableAndValue::Machine(option)) => {
					self.machine_options.insert((line_number.clone(), sub_line_number), *option);
				}
				StatementVariant::Dimension(arrays) => {
					for array in arrays {
						match array.array_type {
							IdentifierType::UnmarkedOrFloat => {
								let declarations_of_array = match self.float_array_declarations.get_mut(&array.name) {
									Some(declarations_of_array) => declarations_of_array,
									None => {
										self.float_array_declarations.insert(array.name.clone(), HashSet::new());
										self.float_array_declarations.get_mut(&array.name).unwrap()
									}
								};
								declarations_of_array.insert((line_number.clone(), sub_line_number));
							}
							IdentifierType::Integer => {
								let declarations_of_array = match self.int_array_declarations.get_mut(&array.name) {
									Some(declarations_of_array) => declarations_of_array,
									None => {
										self.int_array_declarations.insert(array.name.clone(), HashSet::new());
										self.int_array_declarations.get_mut(&array.name).unwrap()
									}
								};
								declarations_of_array.insert((line_number.clone(), sub_line_number));
							}
							IdentifierType::ComplexNumber => {
								let declarations_of_array = match self.complex_array_declarations.get_mut(&array.name) {
									Some(declarations_of_array) => declarations_of_array,
									None => {
										self.complex_array_declarations.insert(array.name.clone(), HashSet::new());
										self.complex_array_declarations.get_mut(&array.name).unwrap()
									}
								};
								declarations_of_array.insert((line_number.clone(), sub_line_number));
							}
							IdentifierType::String => {
								let declarations_of_array = match self.string_array_declarations.get_mut(&array.name) {
									Some(declarations_of_array) => declarations_of_array,
									None => {
										self.string_array_declarations.insert(array.name.clone(), HashSet::new());
										self.string_array_declarations.get_mut(&array.name).unwrap()
									}
								};
								declarations_of_array.insert((line_number.clone(), sub_line_number));
							}
						}
					}
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
		if !data.is_empty() {
			self.data.insert(line_number.clone(), data.into_boxed_slice());
		}
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
			match &statement.variant {
				StatementVariant::Option(OptionVariableAndValue::Angle(_)) => {
					self.angle_options.remove(&(line_number.clone(), sub_line_number));
				}
				StatementVariant::Option(OptionVariableAndValue::Math(_)) => {
					self.math_options.remove(&(line_number.clone(), sub_line_number));
				}
				StatementVariant::Option(OptionVariableAndValue::Machine(_)) => {
					self.machine_options.remove(&(line_number.clone(), sub_line_number));
				}
				StatementVariant::Option(OptionVariableAndValue::Base(_)) => {
					self.base_options.remove(&(line_number.clone(), sub_line_number));
				}
				StatementVariant::Dimension(arrays) => {
					for array in arrays {
						match array.array_type {
							IdentifierType::UnmarkedOrFloat => self.float_array_declarations.get_mut(&array.name).unwrap().remove(&(line_number.clone(), sub_line_number)),
							IdentifierType::Integer => self.int_array_declarations.get_mut(&array.name).unwrap().remove(&(line_number.clone(), sub_line_number)),
							IdentifierType::ComplexNumber => self.complex_array_declarations.get_mut(&array.name).unwrap().remove(&(line_number.clone(), sub_line_number)),
							IdentifierType::String => self.string_array_declarations.get_mut(&array.name).unwrap().remove(&(line_number.clone(), sub_line_number)),
						};
					}
				}
				_ => {},
			}
		}
	}

	pub fn get_line(&self, line_number: &BigInt) -> Option<&Line> {
		self.lines.get(line_number)
	}

	pub fn get_data_line(&self, line_number: &BigInt) -> Option<&Box<[(Datum, NonZeroUsize)]>> {
		self.data.get(line_number)
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

	pub fn base_option_at(&self, line: (Rc<BigInt>, usize)) -> Option<BaseOption> {
		match self.base_options.range::<(Rc<BigInt>, usize), RangeTo<(Rc<BigInt>, usize)>>(..line).last() {
			Some(option) => *option.1,
			None => None,
		}
	}
}

pub struct Line {
	pub unoptimized_statements: Box<[Statement]>,
	pub optimized_statements: Box<[Statement]>,
	pub error: Option<Error>,
	pub source_code: Box<str>,
}