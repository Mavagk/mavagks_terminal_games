use std::{collections::{BTreeMap, BTreeSet, HashMap, HashSet}, num::NonZeroUsize, ops::{RangeFrom, RangeTo}, rc::Rc};

use num::BigInt;

use crate::mavagk_basic::{abstract_syntax_tree::{AnyTypeLValue, Datum, OptionVariableAndValue, Statement, StatementVariant}, error::Error, options::{AngleOption, BaseOption, CollateOption, MachineOption, MathOption, Options}, token::IdentifierType};

/// A MavagkBasic program containing all its lines, does not include a direct mode line.
pub struct Program {
	/// Maps line numbers to lines
	lines: BTreeMap<Rc<BigInt>, Line>,

	// Maps line and sub-line numbers to options
	angle_options: BTreeMap<(Rc<BigInt>, usize), Option<AngleOption>>,
	math_options: BTreeMap<(Rc<BigInt>, usize), Option<MathOption>>,
	machine_options: BTreeMap<(Rc<BigInt>, usize), Option<MachineOption>>,
	base_options: BTreeMap<(Rc<BigInt>, usize), Option<BaseOption>>,
	collate_options: BTreeMap<(Rc<BigInt>, usize), Option<CollateOption>>,

	// Maps identifier names to array declarations lines and sub-lines
	pub float_array_declarations: HashMap<Box<str>, HashSet<(Rc<BigInt>, usize)>>,
	pub int_array_declarations: HashMap<Box<str>, HashSet<(Rc<BigInt>, usize)>>,
	pub complex_array_declarations: HashMap<Box<str>, HashSet<(Rc<BigInt>, usize)>>,
	pub string_array_declarations: HashMap<Box<str>, HashSet<(Rc<BigInt>, usize)>>,

	// Maps identifier names to locations of NEXT statements that are not nested.
	pub next_float_statements: HashMap<Box<str>, BTreeSet<(Rc<BigInt>, usize)>>,
	pub next_int_statements: HashMap<Box<str>, BTreeSet<(Rc<BigInt>, usize)>>,

	// Maps identifier names and argument counts to function declaration lines and sub-lines
	pub float_functions: HashMap<(Box<str>, usize), HashSet<(Rc<BigInt>, usize)>>,
	pub int_functions: HashMap<(Box<str>, usize), HashSet<(Rc<BigInt>, usize)>>,
	pub complex_functions: HashMap<(Box<str>, usize), HashSet<(Rc<BigInt>, usize)>>,
	pub string_functions: HashMap<(Box<str>, usize), HashSet<(Rc<BigInt>, usize)>>,
	/// Maps line numbers to all DATA values on said line if it has data statements.
	data: BTreeMap<Rc<BigInt>, Box<[(Datum, NonZeroUsize)]>>,
}

impl Program {
	/// Creates a new blank program containing no lines.
	pub fn new() -> Self {
		Self {
			lines: BTreeMap::new(),
			angle_options: BTreeMap::new(),
			math_options: BTreeMap::new(),
			machine_options: BTreeMap::new(),
			base_options: BTreeMap::new(),
			collate_options: BTreeMap::new(),
			float_array_declarations: HashMap::new(),
			int_array_declarations: HashMap::new(),
			complex_array_declarations: HashMap::new(),
			string_array_declarations: HashMap::new(),
			complex_functions: HashMap::new(),
			float_functions: HashMap::new(),
			int_functions: HashMap::new(),
			string_functions: HashMap::new(),
			data: BTreeMap::new(),
			next_float_statements: HashMap::new(),
			next_int_statements: HashMap::new(),
		}
	}

	/// Get which OPTIONs are set at a given line and sub-line
	#[must_use]
	pub fn get_options(&self, line_number: &Rc<BigInt>, sub_line: usize) -> Options {
		Options {
			angle: match self.angle_options.range::<(Rc<BigInt>, usize), RangeTo<(Rc<BigInt>, usize)>>(..(line_number.clone(), sub_line)).last() {
				Some(option) => *option.1,
				None => None,
			},
			math: match self.math_options.range::<(Rc<BigInt>, usize), RangeTo<(Rc<BigInt>, usize)>>(..(line_number.clone(), sub_line)).last() {
				Some(option) => *option.1,
				None => None,
			},
			machine: match self.machine_options.range::<(Rc<BigInt>, usize), RangeTo<(Rc<BigInt>, usize)>>(..(line_number.clone(), sub_line)).last() {
				Some(option) => *option.1,
				None => None,
			},
			base: match self.base_options.range::<(Rc<BigInt>, usize), RangeTo<(Rc<BigInt>, usize)>>(..(line_number.clone(), sub_line)).last() {
				Some(option) => *option.1,
				None => None,
			},
			collate: match self.collate_options.range::<(Rc<BigInt>, usize), RangeTo<(Rc<BigInt>, usize)>>(..(line_number.clone(), sub_line)).last() {
				Some(option) => *option.1,
				None => None,
			},
		}
	}

	/// Returns if the program contains a line with the given line number
	pub fn contains_line(&self, line_number: &BigInt) -> bool {
		return self.lines.contains_key(line_number);
	}

	/// Returns if the program contains a line with the given line number that has at least one DATA statement on it.
	pub fn contains_data_line(&self, line_number: &BigInt) -> bool {
		return self.data.contains_key(line_number);
	}

	/// Inserts a line into the program, removes any line that already has the given number.
	pub fn insert_line(&mut self, line_number: Rc<BigInt>, line: Line) {
		self.remove_line(&line_number);
		for (sub_line_number, statement) in line.optimized_statements.iter().enumerate() {
			match &statement.variant {
				StatementVariant::Option(options) => {
					for (option, _) in options {
						match option {
							OptionVariableAndValue::Angle(option) => {
								self.angle_options.insert((line_number.clone(), sub_line_number), *option);
							}
							OptionVariableAndValue::Math(option) => {
								self.math_options.insert((line_number.clone(), sub_line_number), *option);
							}
							OptionVariableAndValue::Machine(option) => {
								self.machine_options.insert((line_number.clone(), sub_line_number), *option);
							}
							OptionVariableAndValue::Base(option) => {
								self.base_options.insert((line_number.clone(), sub_line_number), *option);
							}
							OptionVariableAndValue::Collate(option) => {
								self.collate_options.insert((line_number.clone(), sub_line_number), *option);
							}
							OptionVariableAndValue::ArithmeticDecimal | OptionVariableAndValue::ArithmeticDefault | OptionVariableAndValue::ArithmeticNative => {}
						}
					}
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
				StatementVariant::DefFloat(l_value, _) => {
					let declarations_of_function = match self.float_functions.get_mut(&(l_value.name.clone(), l_value.arguments.len())) {
						Some(declarations_of_function) => declarations_of_function,
						None => {
							self.float_functions.insert((l_value.name.clone(), l_value.arguments.len()), HashSet::new());
							self.float_functions.get_mut(&(l_value.name.clone(), l_value.arguments.len())).unwrap()
						}
					};
					declarations_of_function.insert((line_number.clone(), sub_line_number));
				}
				StatementVariant::DefInt(l_value, _) => {
					let declarations_of_function = match self.int_functions.get_mut(&(l_value.name.clone(), l_value.arguments.len())) {
						Some(declarations_of_function) => declarations_of_function,
						None => {
							self.int_functions.insert((l_value.name.clone(), l_value.arguments.len()), HashSet::new());
							self.int_functions.get_mut(&(l_value.name.clone(), l_value.arguments.len())).unwrap()
						}
					};
					declarations_of_function.insert((line_number.clone(), sub_line_number));
				}
				StatementVariant::DefComplex(l_value, _) => {
					let declarations_of_function = match self.complex_functions.get_mut(&(l_value.name.clone(), l_value.arguments.len())) {
						Some(declarations_of_function) => declarations_of_function,
						None => {
							self.complex_functions.insert((l_value.name.clone(), l_value.arguments.len()), HashSet::new());
							self.complex_functions.get_mut(&(l_value.name.clone(), l_value.arguments.len())).unwrap()
						}
					};
					declarations_of_function.insert((line_number.clone(), sub_line_number));
				}
				StatementVariant::DefString(l_value, _) => {
					let declarations_of_function = match self.string_functions.get_mut(&(l_value.name.clone(), l_value.arguments.len())) {
						Some(declarations_of_function) => declarations_of_function,
						None => {
							self.string_functions.insert((l_value.name.clone(), l_value.arguments.len()), HashSet::new());
							self.string_functions.get_mut(&(l_value.name.clone(), l_value.arguments.len())).unwrap()
						}
					};
					declarations_of_function.insert((line_number.clone(), sub_line_number));
				}
				StatementVariant::Next(l_values) => {
					for l_value in l_values {
						match l_value {
							AnyTypeLValue::Float(l_value) => {
								let next_statement_with_name = match self.next_float_statements.get_mut(&l_value.name) {
									Some(next_statement_with_name) => next_statement_with_name,
									None => {
										self.next_float_statements.insert(l_value.name.clone(), BTreeSet::new());
										self.next_float_statements.get_mut(&l_value.name).unwrap()
									}
								};
								next_statement_with_name.insert((line_number.clone(), sub_line_number));
							}
							AnyTypeLValue::Int(l_value) => {
								let next_statement_with_name = match self.next_int_statements.get_mut(&l_value.name) {
									Some(next_statement_with_name) => next_statement_with_name,
									None => {
										self.next_int_statements.insert(l_value.name.clone(), BTreeSet::new());
										self.next_int_statements.get_mut(&l_value.name).unwrap()
									}
								};
								next_statement_with_name.insert((line_number.clone(), sub_line_number));
							}
							_ => {}
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

	/// Removes a line from the program.
	pub fn remove_line(&mut self, line_number: &Rc<BigInt>) {
		let line = self.lines.remove(line_number);
		self.data.remove(line_number);
		let line = match line {
			Some(line) => line,
			None => return,
		};
		for (sub_line_number, statement) in line.optimized_statements.iter().enumerate() {
			match &statement.variant {
				StatementVariant::Option(options) => {
					for (option, _) in options {
						match option {
							OptionVariableAndValue::Angle(..) => {
								self.angle_options.remove(&(line_number.clone(), sub_line_number));
							}
							OptionVariableAndValue::Math(..) => {
								self.math_options.remove(&(line_number.clone(), sub_line_number));
							}
							OptionVariableAndValue::Machine(..) => {
								self.machine_options.remove(&(line_number.clone(), sub_line_number));
							}
							OptionVariableAndValue::Base(..) => {
								self.base_options.remove(&(line_number.clone(), sub_line_number));
							}
							OptionVariableAndValue::Collate(..) => {
								self.collate_options.remove(&(line_number.clone(), sub_line_number));
							}
							OptionVariableAndValue::ArithmeticDecimal | OptionVariableAndValue::ArithmeticDefault | OptionVariableAndValue::ArithmeticNative => {}
						}
					}
				}
				StatementVariant::Dimension(arrays) => {
					for array in arrays {
						match array.array_type {
							IdentifierType::UnmarkedOrFloat => {
								self.float_array_declarations.get_mut(&array.name).unwrap().remove(&(line_number.clone(), sub_line_number));
								if self.float_array_declarations.get_mut(&array.name).unwrap().is_empty() {
									self.float_array_declarations.remove(&array.name);
								}
							}
							IdentifierType::Integer => {
								self.int_array_declarations.get_mut(&array.name).unwrap().remove(&(line_number.clone(), sub_line_number));
								if self.int_array_declarations.get_mut(&array.name).unwrap().is_empty() {
									self.int_array_declarations.remove(&array.name);
								}
							}
							IdentifierType::ComplexNumber => {
								self.complex_array_declarations.get_mut(&array.name).unwrap().remove(&(line_number.clone(), sub_line_number));
								if self.complex_array_declarations.get_mut(&array.name).unwrap().is_empty() {
									self.complex_array_declarations.remove(&array.name);
								}
							}
							IdentifierType::String => {
								self.string_array_declarations.get_mut(&array.name).unwrap().remove(&(line_number.clone(), sub_line_number));
								if self.string_array_declarations.get_mut(&array.name).unwrap().is_empty() {
									self.string_array_declarations.remove(&array.name);
								}
							}
						};
					}
				}
				StatementVariant::Next(l_values) => {
					for l_value in l_values {
						match l_value {
							AnyTypeLValue::Float(l_value) => {
								self.next_float_statements.get_mut(&l_value.name).unwrap().remove(&(line_number.clone(), sub_line_number));
								if self.next_float_statements.get_mut(&l_value.name).unwrap().is_empty() {
									self.next_float_statements.remove(&l_value.name);
								}
							}
							AnyTypeLValue::Int(l_value) => {
								self.next_int_statements.get_mut(&l_value.name).unwrap().remove(&(line_number.clone(), sub_line_number));
								if self.next_int_statements.get_mut(&l_value.name).unwrap().is_empty() {
									self.next_int_statements.remove(&l_value.name);
								}
							}
							_ => {}
						}
					}
				}
				StatementVariant::DefFloat(l_value, _) => {
					self.float_functions.get_mut(&(l_value.name.clone(), l_value.arguments.len())).unwrap().remove(&(line_number.clone(), sub_line_number));
					if self.float_functions.get_mut(&(l_value.name.clone(), l_value.arguments.len())).unwrap().is_empty() {
						self.float_functions.remove(&(l_value.name.clone(), l_value.arguments.len()));
					}
				}
				StatementVariant::DefInt(l_value, _) => {
					self.int_functions.get_mut(&(l_value.name.clone(), l_value.arguments.len())).unwrap().remove(&(line_number.clone(), sub_line_number));
					if self.int_functions.get_mut(&(l_value.name.clone(), l_value.arguments.len())).unwrap().is_empty() {
						self.int_functions.remove(&(l_value.name.clone(), l_value.arguments.len()));
					}
				}
				StatementVariant::DefComplex(l_value, _) => {
					self.complex_functions.get_mut(&(l_value.name.clone(), l_value.arguments.len())).unwrap().remove(&(line_number.clone(), sub_line_number));
					if self.complex_functions.get_mut(&(l_value.name.clone(), l_value.arguments.len())).unwrap().is_empty() {
						self.complex_functions.remove(&(l_value.name.clone(), l_value.arguments.len()));
					}
				}
				StatementVariant::DefString(l_value, _) => {
					self.string_functions.get_mut(&(l_value.name.clone(), l_value.arguments.len())).unwrap().remove(&(line_number.clone(), sub_line_number));
					if self.string_functions.get_mut(&(l_value.name.clone(), l_value.arguments.len())).unwrap().is_empty() {
						self.string_functions.remove(&(l_value.name.clone(), l_value.arguments.len()));
					}
				}
				_ => {},
			}
		}
	}

	/// Gets the line at the given line number or `None` if there is no line with said line number.
	pub fn get_line(&self, line_number: &BigInt) -> Option<&Line> {
		self.lines.get(line_number)
	}

	/// Gets a list of data values at the given line number or `None` if there is no line with said line number with DATA statements.
	pub fn get_data_line(&self, line_number: &BigInt) -> Option<&Box<[(Datum, NonZeroUsize)]>> {
		self.data.get(line_number)
	}

	/// Gets the line number of the next line after a line that is in the program or returns `None` if the entered line is the last line.
	pub fn get_first_line_after(&self, line_number: &BigInt) -> Option<&Rc<BigInt>> {
		match self.lines.range::<BigInt, RangeFrom<&BigInt>>(line_number..).nth(1) {
			Some(value) => Some(value.0),
			None => None,
		}
	}

	/// Gets the line number of the next line after a line that is in the program that contains at least one DATA statement or returns `None` if the entered line is the last DATA line.
	pub fn get_first_data_line_after(&self, line_number: &BigInt) -> Option<&Rc<BigInt>> {
		match self.data.range::<BigInt, RangeFrom<&BigInt>>(line_number..).nth(1) {
			Some(value) => Some(value.0),
			None => None,
		}
	}

	/// Get the line number of the lowest numbered line in the program or returns `None` if there are no lines in the program.
	pub fn get_first_line(&self) -> Option<&Rc<BigInt>> {
		match self.lines.first_key_value() {
			Some(value) => Some(value.0),
			None => None,
		}
	}

	/// Get the line number of the lowest numbered line in the program that contains at least one DATA statement or returns `None` if there are no lines in the program.
	pub fn get_first_data_line(&self) -> Option<&Rc<BigInt>> {
		match self.data.first_key_value() {
			Some(value) => Some(value.0),
			None => None,
		}
	}

	/// Gets a line number to line mapping.
	pub fn get_lines(&self) -> &BTreeMap<Rc<BigInt>, Line> {
		&self.lines
	}

	/// Deletes all lines in the program.
	pub fn clear_program(&mut self) {
		*self = Self::new();
	}

	/// Gets the line and sub-line number of the next unnested NEXT statement that increments the next float variable with the given name.
	pub fn get_next_float_after(&self, name: &Box<str>, line_number: &Rc<BigInt>, sub_line: usize) -> Option<&(Rc<BigInt>, usize)> {
		let next_statements = self.next_float_statements.get(name)?;
		next_statements.range::<(Rc<BigInt>, usize), RangeFrom<(Rc<BigInt>, usize)>>((line_number.clone(), sub_line)..).nth(0)
	}

	/// Gets the line and sub-line number of the next unnested NEXT statement that increments the next int variable with the given name.
	pub fn get_next_int_after(&self, name: &Box<str>, line_number: &Rc<BigInt>, sub_line: usize) -> Option<&(Rc<BigInt>, usize)> {
		let next_statements = self.next_int_statements.get(name)?;
		next_statements.range::<(Rc<BigInt>, usize), RangeFrom<(Rc<BigInt>, usize)>>((line_number.clone(), sub_line)..).nth(0)
	}
}

/// A parsed and optimized line in the program.
pub struct Line {
	///The parsed line before it has been optimized. Contains a list of statement sub-lines that where colon separated in the source code.
	pub unoptimized_statements: Box<[Statement]>,
	///The parsed line after it has been optimized. Contains a list of statement sub-lines that where colon separated in the source code.
	pub optimized_statements: Box<[Statement]>,
	/// Contains a parse error for if an error was encountered while parsing the line.
	/// Colon separated sub-lines before the sub-line with the error will still be contained in `unoptimized_statements` and `optimized_statements`
	/// but sub-lines for and after said error will not be.
	pub error: Option<Error>,
	/// The raw text that was entered into the terminal.
	pub source_code: Box<str>,
}