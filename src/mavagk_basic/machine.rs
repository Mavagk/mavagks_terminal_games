use std::{collections::HashMap, fs::{create_dir_all, File}, io::{stdin, stdout, BufRead, Read, Write}, mem::take, num::NonZeroUsize, ops::{RangeFrom, RangeFull, RangeInclusive, RangeToInclusive}, path::{Path, PathBuf}, rc::Rc, str::FromStr};

use crossterm::{cursor::position, execute, style::{Color, ContentStyle, PrintStyledContent, StyledContent}};
use num::{BigInt, FromPrimitive, Signed, Zero};
use rand::{random_range, rngs::SmallRng, Rng, SeedableRng};

use crate::mavagk_basic::{abstract_syntax_tree::{AngleOption, AnyTypeExpression, AnyTypeLValue, BoolExpression, ComplexExpression, ComplexLValue, FloatExpression, FloatLValue, IntExpression, IntLValue, MachineOption, MathOption, OptionVariableAndValue, PrintOperand, Statement, StatementVariant, StringExpression, StringLValue}, error::{handle_error, Error, ErrorVariant, FullError}, optimize::optimize_statement, parse::{parse_line, Tokens}, program::Program, token::{SuppliedFunction, Token}, value::{AnyTypeValue, BoolValue, ComplexValue, FloatValue, IntValue, StringValue}};

/// A MavagkBasic virtual machine with its execution state, variables, options. Does not contain the program being executed.
pub struct Machine {
	// Program counter
	/// The current line being executed, `None` if executing the real mode line or if the first line should be executed.
	line_executing: Option<Rc<BigInt>>,
	/// The current colon separated sub-line being executed. `None` to execute the first line.
	sub_line_executing: Option<usize>,
	execution_source: ExecutionSource,
	// Variables
	float_variables: HashMap<Box<str>, FloatValue>,
	complex_variables: HashMap<Box<str>, ComplexValue>,
	int_variables: HashMap<Box<str>, IntValue>,
	string_variables: HashMap<Box<str>, StringValue>,

	/// A list of active loops and multi-line program structures such as the not yet implemented if blocks.
	block_stack: Vec<BlockOnStack>,
	/// Maps a for loop variable name and if it is a float variable to an index into the block stack.
	for_loop_variable_to_block_stack_index: HashMap<(Box<str>, bool), usize>,
	// Options set via an OPTION statement
	angle_option: Option<AngleOption>,
	math_option: Option<MathOption>,
	machine_option: Option<MachineOption>,

	basic_home_path: Option<Box<Path>>,

	rng: SmallRng,
}

impl Machine {
	pub fn new() -> Self {
		Self {
			line_executing: None,
			sub_line_executing: None,
			execution_source: ExecutionSource::ProgramEnded,
			int_variables: HashMap::new(),
			float_variables: HashMap::new(),
			complex_variables: HashMap::new(),
			string_variables: HashMap::new(),
			block_stack: Vec::new(),
			for_loop_variable_to_block_stack_index: HashMap::new(),
			angle_option: None,
			math_option: None,
			machine_option: None,
			basic_home_path: None,
			rng: SmallRng::seed_from_u64(0),
		}
	}

	/// Set the MavagkBasic home path.
	pub fn set_basic_home_path(&mut self, path: Box<Path> ) {
		_ = create_dir_all(&path);
		self.basic_home_path = Some(path);
	}

	/// Takes in a string slice and converts it into a filepath relative to the MavagkBasic home path.
	pub fn string_to_full_filepath(&self, string: &str) -> Result<PathBuf, ErrorVariant> {
		match &self.basic_home_path {
			None => {
				let string_path = match PathBuf::from_str(string) {
					Ok(string_path) => string_path,
					Err(_) => return Err(ErrorVariant::InvalidFilepath(string.into())),
				};
				if !string_path.is_absolute() {
					return Err(ErrorVariant::FilesystemError);
				}
				Ok(string_path)
			}
			Some(basic_home_path) => {
				let mut filepath = basic_home_path.to_path_buf();
				filepath.push(string);
				Ok(filepath)
			}
		}
	}

	/// Used by GOTO, RUN and GOSUB to set the program line being executing, returns an error if the line being jumped to is not in the program.
	/// Use a line number of `None` to jump to the lowest numbered line of the program.
	fn set_line_executing_by_jumping(&mut self, program: &Program, line_number_to_jump_to: Option<Rc<BigInt>>, column_number_jumping_from: NonZeroUsize) -> Result<(), Error> {
		self.line_executing = match line_number_to_jump_to {
			Some(goto_line_number) => {
				if !program.lines.contains_key(&goto_line_number) {
					return Err(ErrorVariant::InvalidLineNumber((*goto_line_number).clone()).at_column(column_number_jumping_from));
				}
				Some(goto_line_number)
			}
			None => None,
		};
		self.sub_line_executing = None;
		self.execution_source = ExecutionSource::Program;
		Ok(())
	}

	/// Removes a block from the block stack and any blocks added since it was added (blocks inside it).
	fn truncate_block_stack(&mut self, truncate_to: usize) {
		for block in self.block_stack.drain(truncate_to..) {
			match block {
				BlockOnStack::IntForLoop { name, .. } => {
					self.for_loop_variable_to_block_stack_index.remove(&(name, false));
				}
				BlockOnStack::FloatForLoop { name, .. } => {
					self.for_loop_variable_to_block_stack_index.remove(&(name, true));
				}
			}
		}
	}

	// Clears everything about the machine state except where the machine is currently executing in the program.
	fn clear_machine_state(&mut self) {
		*self = Self {
			// Stuff to keep
			line_executing: take(&mut self.line_executing),
			sub_line_executing: self.sub_line_executing,
			execution_source: self.execution_source,
			basic_home_path: take(&mut self.basic_home_path),
			// Stuff to discard
			int_variables: HashMap::new(),
			float_variables: HashMap::new(),
			complex_variables: HashMap::new(),
			string_variables: HashMap::new(),

			block_stack: Vec::new(),
			for_loop_variable_to_block_stack_index: HashMap::new(),

			angle_option: None,
			math_option: None,
			machine_option: None,

			rng: SmallRng::seed_from_u64(0),
		}
	}

	/// Called when a line of text is entered into the terminal.
	pub fn line_of_text_entered(&mut self, line_text: Box<str>, program: &mut Program) -> Result<(), FullError> {
		// Parse line
		let (line_number, tokens, error) = match Token::tokenize_line(&*line_text) {
			(line_number, Ok(tokens)) => (line_number, tokens, None),
			(line_number, Err(error)) => (line_number, Box::default(), Some(error)),
		};
		let (unoptimized_statements, error) = match error {
			None => parse_line(&mut Tokens::new(&tokens)),
			Some(error) => (Box::default(), Some(error)),
		};
		let mut optimized_statements = unoptimized_statements.clone();
		for statement in optimized_statements.iter_mut() {
			optimize_statement(statement);
		}
		// Enter line number into program and run if it does not have a line number
		match line_number {
			// If the line has a line number
			Some(line_number) => {
				if let Some(error) = &error {
					let error = error.clone().to_full_error(Some(line_number.clone()), Some(line_text.clone().into()));
					handle_error::<()>(Err(error));
				}
				if unoptimized_statements.is_empty() && error.is_none() {
					program.lines.remove(&line_number);
				}
				else {
					program.lines.insert(Rc::new(line_number), (optimized_statements, unoptimized_statements, error, line_text));
				}
			}
			// Run the line in direct mode if it does not have a line number
			None => {
				if let Some(error) = error {
					return Err(error.clone().to_full_error(None, Some(line_text.clone().into())));
				}
				self.execution_source = ExecutionSource::DirectModeLine;
				self.execute(program, &unoptimized_statements, &line_text)?;
			}
		}
		Ok(())
	}

	const fn get_math_option(&self) -> MathOption {
		match self.math_option {
			None => MathOption::Ansi,
			Some(math_option) => math_option,
		}
	}

	const fn get_angle_option(&self) -> AngleOption {
		match self.angle_option {
			None => AngleOption::Radians,
			Some(math_option) => math_option,
		}
	}

	const fn get_machine_option(&self) -> MachineOption {
		match self.machine_option {
			None => MachineOption::Ansi,
			Some(math_option) => math_option,
		}
	}

	/// Returns false if taking the real square root of a negative number should throw an error, returns true if it should return NaN.
	const fn allow_real_square_root_of_negative(&self) -> bool {
		match self.get_math_option() {
			MathOption::Ieee => true,
			MathOption::Ansi => false,
		}
	}

	/// Returns false if numeric overflow should throw an error, returns true if it should return a non finite value.
	const fn allow_overflow(&self) -> bool {
		match self.get_math_option() {
			MathOption::Ieee => true,
			MathOption::Ansi => false,
		}
	}

	/// Returns false if division by zero should throw an error, returns true if it should return a non finite value.
	const fn allow_divide_by_zero(&self) -> bool {
		match self.get_math_option() {
			MathOption::Ieee => true,
			MathOption::Ansi => false,
		}
	}

	/// Start executing until the program terminates
	fn execute(&mut self, program: &mut Program, direct_mode_statements: &Box<[Statement]>, direct_mode_line_text: &str) -> Result<(), FullError> {
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
					// Get which sub-line to start executing from
					let start_sub_line = match self.sub_line_executing {
						Some(start_sub_line) => start_sub_line,
						None => 0,
					};
					// Execute each sub-line to be executed
					let (optimized_statements, _, line_error, line_text) = program.lines.get(&line_number).unwrap();
					for (sub_line, statement) in optimized_statements.iter().enumerate().skip(start_sub_line) {
						// Set the sub line so that the statement executer can access it
						self.sub_line_executing = Some(sub_line);
						// Execute the sub-line
						let flow_control_used = match self.execute_statement(statement, program) {
							Ok(flow_control_used) => flow_control_used,
							Err(error) => return Err(error.to_full_error(Some((*line_number).clone()), Some(line_text.clone().into_string()))),
						};
						// We should execute from the start of the next if flow control was not used
						if !flow_control_used {
							self.sub_line_executing = None;
						}
						// Break out of sub-line loop if flow control was used
						if flow_control_used || self.execution_source != ExecutionSource::Program {
							continue 'lines_loop;
						}
					}
					self.sub_line_executing = None;
					// If there is an error at the end of the line, throw the error
					if let Some(line_error) = line_error {
						return Err(line_error.clone().to_full_error(Some((&*line_number).clone()), Some(line_text.clone().into_string())));
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
					// Get which sub-line to start executing from
					let start_sub_line = match self.sub_line_executing {
						Some(start_sub_line) => start_sub_line,
						None => 0,
					};
					// Execute each sub-line in the direct mode line to be executed
					for (sub_line, direct_mode_statement) in direct_mode_statements.iter().enumerate().skip(start_sub_line) {
						// Set the sub line so that the statement executer can access it
						self.sub_line_executing = Some(sub_line);
						// Execute the sub-line
						let flow_control_used = match self.execute_direct_mode_statement(&direct_mode_statement, program) {
							Ok(flow_control_used) => flow_control_used,
							Err(error) => return Err(error.to_full_error(None, Some(direct_mode_line_text.into()))),
						};
						// We should execute from the start of the next if flow control was not used
						if !flow_control_used {
							self.sub_line_executing = None;
						}
						// Break out of sub-line loop if flow control was used
						if flow_control_used || self.execution_source != ExecutionSource::DirectModeLine {
							continue 'lines_loop;
						}
					}
					self.sub_line_executing = None;
					// If we did not jump out of the direct mode line, such as with a RUN, end execution
					self.execution_source = ExecutionSource::ProgramEnded;
				}
				// Return if the program has ended
				ExecutionSource::ProgramEnded => return Ok(()),
			}
		}
	}

	/// Called when executing a statement in direct mode. Can modify the program since it is not executing it.
	fn execute_direct_mode_statement(&mut self, statement: &Statement, program: &mut Program) -> Result<bool, Error> {
		let Statement { variant, column } = &statement;
		match variant {
			StatementVariant::Load(filepath_expression) => {
				// Execute expression
				let filename_value = match filepath_expression {
					Some(filepath_expression) => self.execute_string_expression(filepath_expression)?,
					None => return Err(ErrorVariant::Unimplemented("LOAD without arguments".into()).at_column(*column)),
				};
				// Convert string value to filepath
				let filepath = self.string_to_full_filepath(&filename_value.value)
					.map_err(|error| error.at_column(filepath_expression.as_ref().unwrap().get_start_column()))?;
				// File is a basic batch file if it has a ".basbat" extension
				let mut is_basic_batch_file = filepath.extension().is_some_and(|extension| extension.eq_ignore_ascii_case("basbat"));
				// Open file
				let mut file = File::open(filepath).map_err(|_| ErrorVariant::UnableToOpenFile.at_column(filepath_expression.as_ref().unwrap().get_start_column()))?;
				let mut file_buffer = Vec::new();
				file.read_to_end(&mut file_buffer).map_err(|_| ErrorVariant::UnableToReadFile.at_column(filepath_expression.as_ref().unwrap().get_start_column()))?;
				let mut lines = file_buffer.lines().peekable();
				// File is also a batch file if the first char of the first line is a digit or negative sign
				if !is_basic_batch_file {
					match lines.peek() {
						Some(Ok(line)) if line.as_str().trim_ascii().starts_with(|chr| matches!(chr, '0'..='9' | '-')) => is_basic_batch_file = true,
						_ => {},
					}
				}
				// Read lines
				match is_basic_batch_file {
					// If this is a batch basic file, enter each line into the machine as if it where typed in
					true => {
						program.clear_program();
						for line in lines {
							let line = match line {
								Ok(line) => line,
								Err(_) => return Err(ErrorVariant::UnableToReadFile.at_column(filepath_expression.as_ref().unwrap().get_start_column())),
							};
							handle_error(self.line_of_text_entered(line.into_boxed_str(), program));
						}
					}
					// If this is not a batch basic file, enter each line into the machine with its line number prefixed
					false => {
						program.clear_program();
						let mut line_number = 1usize;
						for line in lines {
							let line = match line {
								Ok(line) => line,
								Err(_) => return Err(ErrorVariant::UnableToReadFile.at_column(filepath_expression.as_ref().unwrap().get_start_column())),
							};
							handle_error(self.line_of_text_entered(format!("{line_number} {line}").into_boxed_str(), program));
							line_number += 1;
						}
					}
				}
				Ok(false)
			}
			_ => self.execute_statement(statement, program),
		}
	}

	/// Called when executing a statement that is not a direct mode only statement. Cannot modify the program since it could be executing it.
	fn execute_statement(&mut self, statement: &Statement, program: &Program) -> Result<bool, Error> {
		let Statement { variant, column } = &statement;
		match variant {
			StatementVariant::Print(sub_expressions) => {
				let mut do_print_trailing_newline = true;
				// For each sub-expression
				for (index, sub_expression) in sub_expressions.iter().enumerate() {
					// Print the sub-expression
					match sub_expression {
						PrintOperand::Expression(expression) => match expression {
							// TODO: Make sure TAB calls can be overwritten when user defined functions are added.
							// TAB calls
							AnyTypeExpression::Float(FloatExpression::LValue(FloatLValue { arguments, supplied_function: Some(SuppliedFunction::Tab), .. })) if (&**arguments).len() == 1 => {
								let argument_expression = &arguments[0];
								let argument_value = self.execute_any_type_expression(argument_expression)?
									.to_int().map_err(|error| error.at_column(argument_expression.get_start_column()))?;
								stdout().flush().unwrap();
								let y_position = position().unwrap().0;
								let spaces_to_insert = &*argument_value.value - y_position;
								let spaces_to_insert: usize = match (&spaces_to_insert).try_into() {
									Ok(spaces_to_insert) => spaces_to_insert,
									Err(_) => match spaces_to_insert.is_negative() {
										true => 0,
										false => usize::MAX,
									}
								};
								for _ in 0..spaces_to_insert {
									print!(" ")
								}
							}
							// Expressions
							_ => self.execute_any_type_expression(expression)?.print(&mut stdout(), true, true).unwrap()
						}
						// Semicolons do nothing
						PrintOperand::Semicolon(_) => {}
						// TODO
						PrintOperand::Comma(sub_expression_column) =>
							return Err(ErrorVariant::NotYetImplemented("comma in PRINT statement".into()).at_column(*sub_expression_column))
					}
					// Decide weather or not to suppress a trailing auto printed newlines
					if matches!(sub_expression, PrintOperand::Comma(_) | PrintOperand::Semicolon(_)) && index == sub_expressions.len() - 1 {
						do_print_trailing_newline = false;
					}
				}
				// Print the newline unless it has been suppressed
				if do_print_trailing_newline {
					println!();
				}
			}
			StatementVariant::Input { prompt, timeout, elapsed, inputs } => {
				// TODO
				if let Some(timeout) = timeout {
					return Err(ErrorVariant::NotYetImplemented("TIMEOUT".into()).at_column(timeout.get_start_column()))
				}
				if let Some(elapsed) = elapsed {
					return Err(ErrorVariant::NotYetImplemented("ELAPSED".into()).at_column(elapsed.get_start_column()))
				}
				// Loop until all inputs have been entered
				let mut is_redoing = false;
				'a: loop {
					// Print if text needs to be re-entered
					if is_redoing {
						println!("Input error, please re-enter.")
					}
					is_redoing = true;
					// Print the prompt
					// TODO: OPTION for setting if "? " should be auto printed
					if let Some(prompt_expression) = prompt {
						self.execute_any_type_expression(prompt_expression)?.print(&mut stdout(), true, true).unwrap();
						if self.get_machine_option() == MachineOption::C64 {
							print!("? ");
						}
					}
					else {
						print!("? ");
					}
					// Get inputs
					stdout().flush().unwrap();
					let mut input_buffer = String::new();
					if stdin().read_line(&mut input_buffer).is_err() {
						continue;
					}
					// Break if a blank string was entered
					if !input_buffer.contains(|chr: char| !chr.is_ascii_whitespace()) {
						break;
					}
					// For each input
					let mut inputs_left = &**inputs;
					let mut input_buffer_left = input_buffer.as_str();
					while !inputs_left.is_empty() {
						let next_input = &inputs_left[0];
						inputs_left = &inputs_left[1..];
						let next_input_text_byte_length = input_buffer_left.find(',');
						let next_input_text = match next_input_text_byte_length {
							Some(next_input_text_byte_length) => {
								let (next_input_text, text_after_next_input_text) = input_buffer_left.split_at(next_input_text_byte_length);
								input_buffer_left = &text_after_next_input_text[1..];
								next_input_text
							}
							None => {
								if !inputs_left.is_empty() {
									continue 'a;
								}
								take(&mut input_buffer_left)
							}
						}.trim_ascii();
						match next_input {
							AnyTypeLValue::Int(l_value) => {
								let parsed_value = match next_input_text.parse() {
									Ok(parsed_value) => IntValue::new(Rc::new(parsed_value)),
									Err(_) => continue 'a,
								};
								self.execute_int_l_value_write(l_value, parsed_value)?;
							}
							AnyTypeLValue::Float(l_value) => {
								let parsed_value = match next_input_text.parse() {
									Ok(parsed_value) => FloatValue::new(parsed_value),
									Err(_) => continue 'a,
								};
								self.execute_float_l_value_write(l_value, parsed_value)?;
							}
							AnyTypeLValue::Complex(l_value) => {
								let parsed_value = match next_input_text.parse() {
									Ok(parsed_value) => ComplexValue::new(parsed_value),
									Err(_) => continue 'a,
								};
								self.execute_complex_l_value_write(l_value, parsed_value)?;
							}
							AnyTypeLValue::String(l_value) => {
								if next_input_text.contains('"') {
									// TODO: Quoted text input
									continue 'a;
								}
								self.execute_string_l_value_write(l_value, StringValue::new(Rc::new(next_input_text.into())))?;
							}
						}
					}
					if input_buffer_left.contains(|chr: char| !chr.is_ascii_whitespace()) {
						println!("Extra inputs ignored.");
					}
					break;
				}
			}
			StatementVariant::ForInt { loop_variable, initial, limit, step } => {
				// Execute expressions
				let initial_value = self.execute_int_expression(initial)?;
				let final_value = self.execute_int_expression(limit)?;
				let step_value = match step {
					Some(step) => self.execute_int_expression(step)?,
					None => IntValue::one(),
				};
				// TODO
				if (step_value.is_negative() && (&*initial_value.value < &*final_value.value)) || (!step_value.is_negative() && (&*initial_value.value > &*final_value.value)) {
					return Err(ErrorVariant::NotYetImplemented("FOR looping zero times".into()).at_column(*column));
				}
				self.execute_int_l_value_write(loop_variable, initial_value)?;
				// Construct loop
				let stack_loop = BlockOnStack::IntForLoop { name: loop_variable.name.clone(), final_value, step_value, for_line: self.line_executing.clone(), for_sub_line: self.sub_line_executing.unwrap() };
				// If a for loop using the same variable exists, pop the loop and all blocks inside it
				match self.for_loop_variable_to_block_stack_index.get(&(loop_variable.name.clone(), false)) {
					None => {},
					Some(block_stack_index) => self.truncate_block_stack(*block_stack_index),
				}
				// Push loop
				self.block_stack.push(stack_loop);
				self.for_loop_variable_to_block_stack_index.insert((loop_variable.name.clone(), false), self.block_stack.len() - 1);
			}
			StatementVariant::ForFloat { loop_variable, initial, limit, step } => {
				// Execute expressions
				let initial_value = self.execute_float_expression(initial)?;
				let final_value = self.execute_float_expression(limit)?;
				let step_value = match step {
					Some(step) => self.execute_float_expression(step)?,
					None => FloatValue::ONE,
				};
				// TODO
				if (step_value.is_negative() && (initial_value.value < final_value.value)) || (!step_value.is_negative() && (initial_value.value > final_value.value)) {
					return Err(ErrorVariant::NotYetImplemented("FOR looping zero times".into()).at_column(*column));
				}
				self.execute_float_l_value_write(loop_variable, initial_value)?;
				// Construct loop
				let stack_loop = BlockOnStack::FloatForLoop { name: loop_variable.name.clone(), final_value, step_value, for_line: self.line_executing.clone(), for_sub_line: self.sub_line_executing.unwrap() };
				// If a for loop using the same variable exists, pop the loop and all blocks inside it
				match self.for_loop_variable_to_block_stack_index.get(&(loop_variable.name.clone(), true)) {
					None => {},
					Some(block_stack_index) => self.truncate_block_stack(*block_stack_index),
				}
				// Push loop
				self.block_stack.push(stack_loop);
				self.for_loop_variable_to_block_stack_index.insert((loop_variable.name.clone(), true), self.block_stack.len() - 1);
			}
			StatementVariant::Next(loop_variables) => {
				let allow_overflow = self.allow_overflow();
				// If a NEXT without arguments is executed
				if loop_variables.is_empty() {
					for (index, loop_block) in self.block_stack.iter().enumerate().rev() {
						match loop_block {
							BlockOnStack::IntForLoop { name, final_value, step_value, for_line, for_sub_line } => {
								// Get current value
								let loop_variable_value = self.int_variables.get_mut(name).unwrap();
								// Increment
								*loop_variable_value = loop_variable_value.clone().add(step_value);
								// Remove the loop and blocks inside if it has finished
								if (step_value.is_negative() && (&*loop_variable_value.value) < (&*final_value.value)) || (!step_value.is_negative() && (&*loop_variable_value.value) > (&*final_value.value)) {
									self.truncate_block_stack(index);
									return Ok(false);
								}
								self.line_executing = for_line.clone();
								self.sub_line_executing = Some(for_sub_line + 1);
								return Ok(true);
							}
							BlockOnStack::FloatForLoop { name, final_value, step_value, for_line, for_sub_line } => {
								// Get current value
								let loop_variable_value = self.float_variables.get_mut(name).unwrap();
								// Increment
								*loop_variable_value = loop_variable_value.add(*step_value, allow_overflow).map_err(|error| error.at_column(*column))?;
								// Remove the loop and blocks inside if it has finished
								if (step_value.is_negative() && (loop_variable_value.value) < (final_value.value)) || (!step_value.is_negative() && (loop_variable_value.value) > (final_value.value)) {
									self.truncate_block_stack(index);
									return Ok(false);
								}
								self.line_executing = for_line.clone();
								self.sub_line_executing = Some(for_sub_line + 1);
								return Ok(true);
							}
						}
					}
					return Err(ErrorVariant::NoLoops.at_column(*column));
				}
				// If the NEXT statement does have arguments
				for loop_variable in loop_variables {
					match loop_variable {
						AnyTypeLValue::Int(IntLValue { name, start_column, .. }) => {
							let block_stack_index = match self.for_loop_variable_to_block_stack_index.get(&(name.clone(), false)) {
								Some(block_stack_index) => block_stack_index,
								None => return Err(ErrorVariant::ForLoopVariableNotFound.at_column(*start_column)),
							};
							let (final_value, step_value, for_line, for_sub_line) = match self.block_stack.get(*block_stack_index).unwrap() {
								BlockOnStack::IntForLoop { final_value, step_value, for_line, for_sub_line, .. } => (final_value, step_value, for_line, for_sub_line),
								_ => unreachable!(),
							};
							// Get current value
							let loop_variable_value = self.int_variables.get_mut(name).unwrap();
							// Increment
							*loop_variable_value = loop_variable_value.clone().add(step_value);
							// Remove the loop and blocks inside if it has finished and continue to the next for loop variable
							if (step_value.is_negative() && (&*loop_variable_value.value) < (&*final_value.value)) || (!step_value.is_negative() && (&*loop_variable_value.value) > (&*final_value.value)) {
								self.truncate_block_stack(*block_stack_index);
								continue;
							}
							// Else go to the sub-line after the for loop of this variable if we are not done with the loop
							self.line_executing = for_line.clone();
							self.sub_line_executing = Some(for_sub_line + 1);
							return Ok(true);
						}
						AnyTypeLValue::Float(FloatLValue { name, start_column, .. }) => {
							let block_stack_index = match self.for_loop_variable_to_block_stack_index.get(&(name.clone(), true)) {
								Some(block_stack_index) => block_stack_index,
								None => return Err(ErrorVariant::ForLoopVariableNotFound.at_column(*start_column)),
							};
							let (final_value, step_value, for_line, for_sub_line) = match self.block_stack.get(*block_stack_index).unwrap() {
								BlockOnStack::FloatForLoop { final_value, step_value, for_line, for_sub_line, .. } => (final_value, step_value, for_line, for_sub_line),
								_ => unreachable!(),
							};
							// Get current value
							let loop_variable_value = self.float_variables.get_mut(name).unwrap();
							// Increment
							*loop_variable_value = loop_variable_value.clone().add(*step_value, allow_overflow).map_err(|error| error.at_column(*start_column))?;
							// Remove the loop and blocks inside if it has finished and continue to the next for loop variable
							if (step_value.is_negative() && (loop_variable_value.value) < (final_value.value)) || (!step_value.is_negative() && (loop_variable_value.value) > (final_value.value)) {
								self.truncate_block_stack(*block_stack_index);
								continue;
							}
							// Else go to the sub-line after the for loop of this variable if we are not done with the loop
							self.line_executing = for_line.clone();
							self.sub_line_executing = Some(for_sub_line + 1);
							return Ok(true);
						}
						_ => todo!()
					}
				}
			}
			StatementVariant::Goto(sub_expression) | StatementVariant::Run(sub_expression) => {
				// Set the line to be executed next
				match sub_expression {
					Some(sub_expression) => {
						let line_number_to_jump_to = self.execute_int_expression(sub_expression)?.value;
						self.set_line_executing_by_jumping(program, Some(line_number_to_jump_to), sub_expression.get_start_column())?;
					}
					None => self.set_line_executing_by_jumping(program, None, *column)?,
				}
				// Clear if this is a RUN statement
				if matches!(variant, StatementVariant::Run(..)) {
					self.clear_machine_state();
				}
				// Next statement
				return Ok(true);
			}
			StatementVariant::Gosub(_) => return Err(ErrorVariant::NotYetImplemented("GOSUB statement".into()).at_column(*column)),
			StatementVariant::AssignInt(l_value, r_value_expression) => {
				let value = self.execute_int_expression(r_value_expression)?;
				self.execute_int_l_value_write(l_value, value)?
			}
			StatementVariant::AssignFloat(l_value, r_value_expression) => {
				let value = self.execute_float_expression(r_value_expression)?;
				self.execute_float_l_value_write(l_value, value)?
			}
			StatementVariant::AssignComplex(l_value, r_value_expression) => {
				let value = self.execute_complex_expression(r_value_expression)?;
				self.execute_complex_l_value_write(l_value, value)?
			}
			StatementVariant::AssignString(l_value, r_value_expression) =>
				self.execute_string_l_value_write(l_value, self.execute_string_expression(r_value_expression)?)?,
			StatementVariant::List(range_start, range_end) => {
				let range_start_value = match range_start {
					Some(range_start) => Some(&*self.execute_int_expression(range_start)?.value),
					None => None,
				};
				let range_end_value = match range_end {
					Some(range_end) => Some(&*self.execute_int_expression(range_end)?.value),
					None => None,
				};
				let range = match (range_start_value, range_end_value) {
					(None, None) => program.lines.range::<BigInt, RangeFull>(..),
					(Some(range_start_value), None) => program.lines.range::<BigInt, RangeFrom<&BigInt>>(range_start_value..),
					(None, Some(range_end_value)) => program.lines.range::<BigInt, RangeToInclusive<&BigInt>>(..=range_end_value),
					(Some(range_start_value), Some(range_end_value)) => program.lines.range::<BigInt, RangeInclusive<&BigInt>>(range_start_value..=range_end_value),
				};
				for (_line, (_optimized_statements, _, error, code_text)) in range {
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
				let condition_value = self.execute_bool_expression(condition)?;
				// Execute statement
				if condition_value.value {
					return self.execute_statement(then_statement, program);
				}
				else {
					if let Some(else_statement) = else_statement {
						return self.execute_statement(else_statement, program);
					}
				}
			}
			StatementVariant::Option(option_variable_and_value) => {
				match option_variable_and_value {
					OptionVariableAndValue::ArithmeticDecimal | OptionVariableAndValue::ArithmeticNative | OptionVariableAndValue::ArithmeticDefault => {},
					OptionVariableAndValue::Angle(angle_option) => self.angle_option = *angle_option,
					OptionVariableAndValue::Math(math_option) => self.math_option = *math_option,
					OptionVariableAndValue::Machine(machine_option) => self.machine_option = *machine_option,
				}
			}
			StatementVariant::Load(_filename_expression) => {
				return Err(ErrorVariant::CanOnlyExecuteInDirectMode.at_column(*column));
			}
			StatementVariant::End => {
				// TODO: Should store stop location to allow a CONT
				self.execution_source = ExecutionSource::ProgramEnded;
				return Ok(true)
			}
			StatementVariant::Stop => {
				self.execution_source = ExecutionSource::ProgramEnded;
				return Ok(true)
			}
		}
		Ok(false)
	}

	/// Execute an expression that returns an integer value.
	fn execute_int_expression(&mut self, expression: &IntExpression) -> Result<IntValue, Error> {
		Ok(match expression {
			IntExpression::ConstantValue { value, .. } => value.clone(),
			IntExpression::CastFromBool(sub_expression) => self.execute_bool_expression(sub_expression)?.to_int(),
			IntExpression::CastFromFloat(sub_expression) => {
				self.execute_float_expression(sub_expression)?.to_int().map_err(|error| error.at_column(sub_expression.get_start_column()))?
			}
			IntExpression::BitwiseAnd { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression)?.and(self.execute_int_expression(rhs_expression)?),
			IntExpression::BitwiseOr { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression)?.or(self.execute_int_expression(rhs_expression)?),
			IntExpression::Addition { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression)?.add(&self.execute_int_expression(rhs_expression)?),
			IntExpression::Subtraction { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression)?.sub(self.execute_int_expression(rhs_expression)?),
			IntExpression::Multiplication { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression)?.mul(self.execute_int_expression(rhs_expression)?),
			IntExpression::FlooredDivision { lhs_expression, rhs_expression, start_column } =>
				self.execute_int_expression(lhs_expression)?.floored_div(self.execute_int_expression(rhs_expression)?)
					.map_err(|error| error.at_column(*start_column))?,
			IntExpression::BitwiseNot { sub_expression, .. } => self.execute_int_expression(sub_expression)?.not(),
			IntExpression::Negation { sub_expression, .. } => self.execute_int_expression(sub_expression)?.neg(),
			IntExpression::LValue(l_value) => self.execute_int_l_value_read(l_value)?,
		})
	}

	/// Execute an expression that returns an float value.
	fn execute_float_expression(&mut self, expression: &FloatExpression) -> Result<FloatValue, Error> {
		Ok(match expression {
			FloatExpression::ConstantValue { value, .. } => value.clone(),
			FloatExpression::CastFromInt(sub_expression) => self.execute_int_expression(sub_expression)?.to_float(),
			FloatExpression::CastFromComplex(sub_expression) =>
				self.execute_complex_expression(sub_expression)?.to_float().map_err(|error| error.at_column(sub_expression.get_start_column()))?,
			FloatExpression::Addition { lhs_expression, rhs_expression, start_column } =>
				self.execute_float_expression(lhs_expression)?.add(self.execute_float_expression(rhs_expression)?, self.allow_overflow())
					.map_err(|error| error.at_column(*start_column))?,
			FloatExpression::Subtraction { lhs_expression, rhs_expression, start_column } =>
				self.execute_float_expression(lhs_expression)?.sub(self.execute_float_expression(rhs_expression)?, self.allow_overflow())
					.map_err(|error| error.at_column(*start_column))?,
			FloatExpression::Multiplication { lhs_expression, rhs_expression, start_column } =>
				self.execute_float_expression(lhs_expression)?.mul(self.execute_float_expression(rhs_expression)?, self.allow_overflow())
					.map_err(|error| error.at_column(*start_column))?,
			FloatExpression::Division { lhs_expression, rhs_expression, start_column } =>
				self.execute_float_expression(lhs_expression)?
					.div(self.execute_float_expression(rhs_expression)?, self.allow_overflow(), self.allow_divide_by_zero())
					.map_err(|error| error.at_column(*start_column))?,
			FloatExpression::Exponentiation { lhs_expression, rhs_expression, start_column } =>
				self.execute_float_expression(lhs_expression)?
					.pow(self.execute_float_expression(rhs_expression)?, self.allow_overflow(), self.allow_divide_by_zero())
					.map_err(|error| error.at_column(*start_column))?,
			FloatExpression::Negation { sub_expression, .. } => self.execute_float_expression(&sub_expression)?.neg(),
			FloatExpression::LValue(l_value) => self.execute_float_l_value_read(l_value)?,
		})
	}

	/// Execute an expression that returns an complex value.
	fn execute_complex_expression(&mut self, expression: &ComplexExpression) -> Result<ComplexValue, Error> {
		Ok(match expression {
			ComplexExpression::ConstantValue { value, .. } => *value,
			ComplexExpression::CastFromFloat(sub_expression) => self.execute_float_expression(sub_expression)?.to_complex(),
			ComplexExpression::Addition { lhs_expression, rhs_expression, start_column } =>
				self.execute_complex_expression(lhs_expression)?.add(self.execute_complex_expression(rhs_expression)?, self.allow_overflow())
					.map_err(|error| error.at_column(*start_column))?,
			ComplexExpression::Subtraction { lhs_expression, rhs_expression, start_column } =>
				self.execute_complex_expression(lhs_expression)?.sub(self.execute_complex_expression(rhs_expression)?, self.allow_overflow())
					.map_err(|error| error.at_column(*start_column))?,
			ComplexExpression::Multiplication { lhs_expression, rhs_expression, start_column } =>
				self.execute_complex_expression(lhs_expression)?.mul(self.execute_complex_expression(rhs_expression)?, self.allow_overflow())
					.map_err(|error| error.at_column(*start_column))?,
			ComplexExpression::Division { lhs_expression, rhs_expression, start_column } =>
				self.execute_complex_expression(lhs_expression)?
					.div(self.execute_complex_expression(rhs_expression)?, self.allow_overflow(), self.allow_divide_by_zero())
					.map_err(|error| error.at_column(*start_column))?,
			ComplexExpression::Exponentiation { lhs_expression, rhs_expression, start_column } =>
				self.execute_complex_expression(lhs_expression)?.pow(self.execute_complex_expression(rhs_expression)?, self.allow_overflow())
					.map_err(|error| error.at_column(*start_column))?,
			ComplexExpression::Negation { sub_expression, .. } => self.execute_complex_expression(sub_expression)?.neg(),
			ComplexExpression::LValue(l_value) => self.execute_complex_l_value_read(l_value)?,
		})
	}

	/// Execute an expression that returns an boolean value.
	fn execute_bool_expression(&mut self, expression: &BoolExpression) -> Result<BoolValue, Error> {
		Ok(match expression {
			BoolExpression::ConstantValue { value, .. } => *value,
			BoolExpression::IntIsNonZero(int_expression) => BoolValue::new(!self.execute_int_expression(&int_expression)?.value.is_zero()),
			BoolExpression::FloatIsNonZero(float_expression) => BoolValue::new(!self.execute_float_expression(&float_expression)?.is_zero()),
			BoolExpression::ComplexIsNonZero(complex_expression) => BoolValue::new(!self.execute_complex_expression(&complex_expression)?.value.is_zero()),
			BoolExpression::StringIsNotEmpty(string_expression) => BoolValue::new(!self.execute_string_expression(&string_expression)?.value.is_empty()),

			BoolExpression::And { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression)?.and(self.execute_bool_expression(rhs_expression)?),
			BoolExpression::Or { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression)?.or(self.execute_bool_expression(rhs_expression)?),
			BoolExpression::Not { sub_expression, .. } => self.execute_bool_expression(sub_expression)?.not(),

			BoolExpression::BoolEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression)?.equal_to(self.execute_bool_expression(rhs_expression)?),
			BoolExpression::BoolNotEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression)?.not_equal_to(self.execute_bool_expression(rhs_expression)?),
			BoolExpression::BoolLessThan { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression)?.less_than(self.execute_bool_expression(rhs_expression)?),
			BoolExpression::BoolLessThanOrEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression)?.less_than_or_equal_to(self.execute_bool_expression(rhs_expression)?),
			BoolExpression::BoolGreaterThan { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression)?.greater_than(self.execute_bool_expression(rhs_expression)?),
			BoolExpression::BoolGreaterThanOrEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression)?.greater_than_or_equal_to(self.execute_bool_expression(rhs_expression)?),
			
			BoolExpression::IntEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression)?.equal_to(&self.execute_int_expression(rhs_expression)?),
			BoolExpression::IntNotEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression)?.not_equal_to(&self.execute_int_expression(rhs_expression)?),
			BoolExpression::IntLessThan { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression)?.less_than(&self.execute_int_expression(rhs_expression)?),
			BoolExpression::IntLessThanOrEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression)?.less_than_or_equal_to(&self.execute_int_expression(rhs_expression)?),
			BoolExpression::IntGreaterThan { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression)?.greater_than(&self.execute_int_expression(rhs_expression)?),
			BoolExpression::IntGreaterThanOrEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression)?.greater_than_or_equal_to(&self.execute_int_expression(rhs_expression)?),

			BoolExpression::FloatEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_float_expression(lhs_expression)?.equal_to(&self.execute_float_expression(rhs_expression)?),
			BoolExpression::FloatNotEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_float_expression(lhs_expression)?.not_equal_to(&self.execute_float_expression(rhs_expression)?),
			BoolExpression::FloatLessThan { lhs_expression, rhs_expression, .. } =>
				self.execute_float_expression(lhs_expression)?.less_than(&self.execute_float_expression(rhs_expression)?),
			BoolExpression::FloatLessThanOrEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_float_expression(lhs_expression)?.less_than_or_equal_to(&self.execute_float_expression(rhs_expression)?),
			BoolExpression::FloatGreaterThan { lhs_expression, rhs_expression, .. } =>
				self.execute_float_expression(lhs_expression)?.greater_than(&self.execute_float_expression(rhs_expression)?),
			BoolExpression::FloatGreaterThanOrEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_float_expression(lhs_expression)?.greater_than_or_equal_to(&self.execute_float_expression(rhs_expression)?),

			BoolExpression::ComplexEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_complex_expression(lhs_expression)?.equal_to(self.execute_complex_expression(rhs_expression)?),
			BoolExpression::ComplexNotEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_complex_expression(lhs_expression)?.not_equal_to(self.execute_complex_expression(rhs_expression)?),

			BoolExpression::StringEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_string_expression(lhs_expression)?.equal_to(&self.execute_string_expression(rhs_expression)?),
			BoolExpression::StringNotEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_string_expression(lhs_expression)?.not_equal_to(&self.execute_string_expression(rhs_expression)?),
			BoolExpression::StringLessThan { lhs_expression: _, rhs_expression: _, start_column } =>
				return Err(ErrorVariant::NotYetImplemented("String <, <=, >, >= operators".into()).at_column(*start_column)),
			BoolExpression::StringLessThanOrEqualTo { lhs_expression: _, rhs_expression: _, start_column } =>
				return Err(ErrorVariant::NotYetImplemented("String <, <=, >, >= operators".into()).at_column(*start_column)),
			BoolExpression::StringGreaterThan { lhs_expression: _, rhs_expression: _, start_column } =>
				return Err(ErrorVariant::NotYetImplemented("String <, <=, >, >= operators".into()).at_column(*start_column)),
			BoolExpression::StringGreaterThanOrEqualTo { lhs_expression: _, rhs_expression: _, start_column } =>
				return Err(ErrorVariant::NotYetImplemented("String <, <=, >, >= operators".into()).at_column(*start_column)),
		})
	}

	/// Execute an expression that returns an string value.
	fn execute_string_expression(&self, expression: &StringExpression) -> Result<StringValue, Error> {
		Ok(match expression {
			StringExpression::ConstantValue { value, .. } => value.clone(),
			StringExpression::Concatenation { lhs_expression, rhs_expression, .. } =>
				self.execute_string_expression(lhs_expression)?.concat(self.execute_string_expression(rhs_expression)?),
			StringExpression::LValue(l_value) => self.execute_string_l_value_read(l_value)?,
		})
	}

	/// Execute an expression that could return a value of any type.
	fn execute_any_type_expression(&mut self, expression: &AnyTypeExpression) -> Result<AnyTypeValue, Error> {
		Ok(match expression {
			AnyTypeExpression::Bool(expression) => AnyTypeValue::Bool(self.execute_bool_expression(expression)?),
			AnyTypeExpression::Int(expression) => AnyTypeValue::Int(self.execute_int_expression(expression)?),
			AnyTypeExpression::Float(expression) => AnyTypeValue::Float(self.execute_float_expression(expression)?),
			AnyTypeExpression::Complex(expression) => AnyTypeValue::Complex(self.execute_complex_expression(expression)?),
			AnyTypeExpression::String(expression) => AnyTypeValue::String(self.execute_string_expression(expression)?),
		})
	}

	/// Reads a integer variable or from an integer array or executes a function that returns an integer.
	fn execute_int_l_value_read(&mut self, l_value: &IntLValue) -> Result<IntValue, Error> {
		// Unpack
		let IntLValue { name, arguments, /*uses_fn_keyword,*/ has_parentheses, start_column, supplied_function } = l_value;
		// If it is a user defined variable that has been defined, get it
		if !*has_parentheses /*&& !*uses_fn_keyword*/ && let Some(variable) = self.int_variables.get(name) {
			return Ok(variable.clone());
		}
		// Else try to execute a supplied (built-in) function
		if let Some(supplied_function) = supplied_function {
			match (supplied_function, arguments) {
				// SQR%(X)
				(SuppliedFunction::Sqr, arguments) if arguments.len() == 1 => {
					let argument = &arguments[0];
					return match self.execute_any_type_expression(&arguments[0])?.to_int().map_err(|error| error.at_column(argument.get_start_column()))? {
						result if result.is_negative() => Err(ErrorVariant::IntSquareRootOfNegativeNumber.at_column(*start_column)),
						result => Ok(IntValue::new(Rc::new(result.value.sqrt())))
					};
				}
				// ABS%(X)
				(SuppliedFunction::Abs, arguments) if arguments.len() == 1 => {
					let argument = &arguments[0];
					return Ok(IntValue::new(Rc::new(
						self.execute_any_type_expression(argument)?.to_int().map_err(|error| error.at_column(argument.get_start_column()))?.value.abs()
					)))
				}
				// TRUE%
				(SuppliedFunction::Abs, _) if !has_parentheses => return Ok(IntValue::new(Rc::new((-1i8).into()))),
				// FALSE%
				(SuppliedFunction::False, _) if !has_parentheses => return Ok(IntValue::zero()),
				// INT%(X)
				(SuppliedFunction::Int, arguments) if arguments.len() == 1 =>
					return Ok({
						let argument = &arguments[0];
						let value = self.execute_any_type_expression(argument)?;
						match value {
							AnyTypeValue::Bool(_) | AnyTypeValue::Int(_) => value.to_int().map_err(|error| error.at_column(argument.get_start_column()))?,
							_ => {
								let float_value = value.to_float().map_err(|error| error.at_column(argument.get_start_column()))?.value.floor();
								match BigInt::from_f64(float_value) {
									Some(result) => IntValue::new(Rc::new(result)),
									None => return Err(ErrorVariant::NonNumberValueCastToInt(float_value).at_column(*start_column)),
								}
							}
						}
					}),
				// LEN%(X$)
				(SuppliedFunction::Len, arguments) if arguments.len() == 1 => {
					let argument = &arguments[0];
					return Ok(IntValue::from_usize(self.execute_any_type_expression(argument)?
						.to_string().map_err(|error| error.at_column(argument.get_start_column()))?.count_chars()))
				}
				// SGN%(X)
				(SuppliedFunction::Sgn, arguments) if arguments.len() == 1 =>
					return Ok(IntValue::new(Rc::new({
						let argument = &arguments[0];
						let value = self.execute_any_type_expression(argument)?;
						match value {
							AnyTypeValue::Bool(_) | AnyTypeValue::Int(_) => return Ok(value.to_int().map_err(|error| error.at_column(argument.get_start_column()))?.signum()),
							_ => {
								match value.to_float().map_err(|error| error.at_column(argument.get_start_column()))? {
									value if value.is_zero() => 0.into(),
									value if value.is_negative() => (-1).into(),
									value if value.is_positive() => 1.into(),
									value => return Err(ErrorVariant::NonNumberValueCastToInt(value.value).at_column(*start_column)),
								}
							}
						}
					}))),
				_ => {}
			}
		}
		// TODO
		if *has_parentheses {
			return Err(ErrorVariant::NotYetImplemented("Arrays and user defined functions".into()).at_column(*start_column));
		}
		// Else return zero
		Ok(IntValue::zero())
	}

	/// Reads a float variable or from a float array or executes a function that returns a float.
	fn execute_float_l_value_read(&mut self, l_value: &FloatLValue) -> Result<FloatValue, Error> {
		// Unpack
		let FloatLValue { name, arguments, has_parentheses, start_column, supplied_function } = l_value;
		// If it is a user defined variable that has been defined, get it
		if !*has_parentheses && let Some(variable) = self.float_variables.get(name) {
			return Ok(variable.clone());
		}
		// Else try to execute a supplied (built-in) function
		if let Some(supplied_function) = supplied_function {
			match supplied_function {
				// Constants
				_ if !*has_parentheses => match supplied_function {
					SuppliedFunction::Pi => return Ok(FloatValue::PI),
					SuppliedFunction::E => return Ok(FloatValue::E),
					SuppliedFunction::Tau => return Ok(FloatValue::TAU),
					SuppliedFunction::Phi => return Ok(FloatValue::PHI),
					SuppliedFunction::EGamma => return Ok(FloatValue::EGAMMA),
					SuppliedFunction::MaxNum => return Ok(FloatValue::MAX),
					SuppliedFunction::NaN => return Ok(FloatValue::NAN),
					SuppliedFunction::Inf => return Ok(FloatValue::INFINITY),
					SuppliedFunction::NInf => return Ok(FloatValue::NEG_INFINITY),
					SuppliedFunction::True => return Ok(FloatValue::TRUE),
					SuppliedFunction::False => return Ok(FloatValue::FALSE),
					_ => {}
				}
				// ABS(X#)
				SuppliedFunction::Abs if arguments.len() == 1 && arguments[0].is_complex() => {
					let argument = &arguments[0];
					return self.execute_any_type_expression(argument)?.to_complex().map_err(|error| error.at_column(argument.get_start_column()))?
						.abs(self.allow_overflow()).map_err(|error| error.at_column(argument.get_start_column()));
				}
				// Functions that have one float argument
				SuppliedFunction::Sqr | SuppliedFunction::Abs | SuppliedFunction::Int | SuppliedFunction::Sgn | SuppliedFunction::Sin | SuppliedFunction::Cos | SuppliedFunction::Tan if arguments.len() == 1 => {
					let argument_expression = &arguments[0];
					let argument_value = self.execute_any_type_expression(argument_expression)?
						.to_float().map_err(|error| error.at_column(argument_expression.get_start_column()))?;
					return Ok(match supplied_function {
						SuppliedFunction::Sqr =>
							argument_value.sqrt(self.allow_real_square_root_of_negative()).map_err(|error| error.at_column(argument_expression.get_start_column()))?,
						SuppliedFunction::Abs => argument_value.abs(),
						SuppliedFunction::Int => argument_value.floor(),
						SuppliedFunction::Sgn => argument_value.signum(),
						SuppliedFunction::Sin =>
							argument_value.sin(self.get_angle_option(), self.allow_overflow()).map_err(|error| error.at_column(argument_expression.get_start_column()))?,
						SuppliedFunction::Cos =>
							argument_value.cos(self.get_angle_option(), self.allow_overflow()).map_err(|error| error.at_column(argument_expression.get_start_column()))?,
						SuppliedFunction::Tan =>
							argument_value.tan(self.get_angle_option(), self.allow_overflow()).map_err(|error| error.at_column(argument_expression.get_start_column()))?,
						_ => unreachable!()
					})
				}
				// Functions that have one complex argument
				SuppliedFunction::Real | SuppliedFunction::Imag if arguments.len() == 1 => {
					let argument_expression = &arguments[0];
					let argument_value = self.execute_any_type_expression(argument_expression)?
						.to_complex().map_err(|error| error.at_column(argument_expression.get_start_column()))?;
					return Ok(match supplied_function {
						SuppliedFunction::Real => argument_value.re(),
						SuppliedFunction::Imag => argument_value.im(),
						_ => unreachable!()
					})
				}
				// LEN(X$)
				SuppliedFunction::Len if arguments.len() == 1 => {
					let argument = &arguments[0];
					return Ok(FloatValue::from_usize(self.execute_any_type_expression(argument)?
						.to_string().map_err(|error| error.at_column(argument.get_start_column()))?.count_chars()))
				}
				// RND(X)
				SuppliedFunction::Rnd if arguments.len() < 2 => {
					// If the function has an argument
					if arguments.len() == 1 {
						// Execute the argument and cast to a float
						let argument_expression = &arguments[0];
						let argument_value = self.execute_any_type_expression(argument_expression)?.to_float()
							.map_err(|err| err.at_column(argument_expression.get_start_column()))?;
						// A value of zero means to generate a value that is not from the machine RNG
						if argument_value.is_zero() {
							return Ok(FloatValue::new(random_range(0.0..1.)));
						}
						// If positive, generate one form the machine RNG
						if argument_value.is_positive() {
							return Ok(FloatValue::new(self.rng.random_range(0.0..1.)));
						}
						// If negative, seed first using the value, then generate from the machine RNG
						self.rng = SmallRng::seed_from_u64(argument_value.value.to_bits());
						return Ok(FloatValue::new(self.rng.random_range(0.0..1.)));
					}
					// If it does not then generate one form the machine RNG
					return Ok(FloatValue::new(self.rng.random_range(0.0..1.)));
				}
				_ => {}
			}
		}
		// TODO
		if *has_parentheses {
			return Err(ErrorVariant::NotYetImplemented("Arrays and user defined functions".into()).at_column(*start_column));
		}
		// Else return zero
		Ok(FloatValue::ZERO)
	}

	/// Reads a complex variable or from a complex array or executes a function that returns a complex.
	fn execute_complex_l_value_read(&mut self, l_value: &ComplexLValue) -> Result<ComplexValue, Error> {
		// Unpack
		let ComplexLValue { name, arguments/*, uses_fn_keyword*/, has_parentheses, start_column, supplied_function } = l_value;
		// If it is a user defined variable that has been defined, get it
		if !*has_parentheses && let Some(variable) = self.complex_variables.get(name) {
			return Ok(variable.clone());
		}
		// Else try to execute a supplied (built-in) function
		if let Some(supplied_function) = supplied_function {
			match supplied_function {
				// Constants
				SuppliedFunction::I if !*has_parentheses => return Ok(ComplexValue::I),
				// Functions that have one complex number as an argument
				SuppliedFunction::Sqr if arguments.len() == 1 => {
					let argument_expression = &arguments[0];
					let argument_value = self.execute_any_type_expression(argument_expression)?.to_complex().map_err(|error| error.at_column(argument_expression.get_start_column()))?;
					return Ok(match supplied_function {
						SuppliedFunction::Sqr => argument_value.sqrt(self.allow_overflow()).map_err(|error| error.at_column(argument_expression.get_start_column()))?,
						_ => unreachable!(),
					})
				}
				_ => {}
			}
		}
		// TODO
		if *has_parentheses {
			return Err(ErrorVariant::NotYetImplemented("Arrays and user defined functions".into()).at_column(*start_column));
		}
		// Else return zero
		Ok(ComplexValue::ZERO)
	}

	/// Reads a string variable or from a string array or executes a function that returns a string.
	fn execute_string_l_value_read(&self, l_value: &StringLValue) -> Result<StringValue, Error> {
		// Unpack
		let StringLValue { name, arguments/*, uses_fn_keyword*/, has_parentheses, start_column, supplied_function } = l_value;
		// If it is a user defined variable that has been defined, get it
		if !*has_parentheses /* && !*uses_fn_keyword*/ && let Some(variable) = self.string_variables.get(name) {
			return Ok(variable.clone());
		}
		// Else try to execute a supplied (built-in) function
		if let Some(supplied_function) = supplied_function {
			match (supplied_function, arguments) {
				_ => {}
			}
		}
		// TODO
		if *has_parentheses {
			return Err(ErrorVariant::NotYetImplemented("Arrays and user defined functions".into()).at_column(*start_column));
		}
		// Else return zero
		Ok(StringValue::empty())
	}

	/// Writes to a integer variable or to an integer array.
	fn execute_int_l_value_write(&mut self, l_value: &IntLValue, value: IntValue) -> Result<(), Error> {
		// Unpack
		let IntLValue { name, arguments, has_parentheses, start_column, .. } = l_value;
		// TODO
		if !arguments.is_empty() || *has_parentheses {
			return Err(ErrorVariant::NotYetImplemented("Arrays and user defined functions".into()).at_column(*start_column));
		}
		// Assign to global variable
		self.int_variables.insert(name.clone(), value);
		// Return
		Ok(())
	}

	/// Writes to a float variable or to an float array.
	fn execute_float_l_value_write(&mut self, l_value: &FloatLValue, value: FloatValue) -> Result<(), Error> {
		// Unpack
		let FloatLValue { name, arguments, has_parentheses, start_column, .. } = l_value;
		// TODO
		if !arguments.is_empty() || *has_parentheses {
			return Err(ErrorVariant::NotYetImplemented("Arrays and user defined functions".into()).at_column(*start_column));
		}
		// Assign to global variable
		self.float_variables.insert(name.clone(), value);
		// Return
		Ok(())
	}

	/// Writes to a complex variable or to an complex array.
	fn execute_complex_l_value_write(&mut self, l_value: &ComplexLValue, value: ComplexValue) -> Result<(), Error> {
		// Unpack
		let ComplexLValue { name, arguments, has_parentheses, start_column, .. } = l_value;
		// TODO
		if !arguments.is_empty() || *has_parentheses {
			return Err(ErrorVariant::NotYetImplemented("Arrays and user defined functions".into()).at_column(*start_column));
		}
		// Assign to global variable
		self.complex_variables.insert(name.clone(), value);
		// Return
		Ok(())
	}

	/// Writes to a string variable or to an string array.
	fn execute_string_l_value_write(&mut self, l_value: &StringLValue, value: StringValue) -> Result<(), Error> {
		// Unpack
		let StringLValue { name, arguments, has_parentheses, start_column, .. } = l_value;
		// TODO
		if !arguments.is_empty() || *has_parentheses {
			return Err(ErrorVariant::NotYetImplemented("Arrays and user defined functions".into()).at_column(*start_column));
		}
		// Assign to global variable
		self.string_variables.insert(name.clone(), value);
		// Return
		Ok(())
	}
}

#[derive(PartialEq, Eq, Clone, Copy)]
/// A location for the machine to get the lines to execute from.
enum ExecutionSource {
	/// Get the lines from the lined program.
	Program,
	/// Get the unlined direct mode line.
	DirectModeLine,
	/// Stop executing.
	ProgramEnded,
}

/// A for loop or other block that is on the stack.
enum BlockOnStack {
	IntForLoop { name: Box<str>, final_value: IntValue, step_value: IntValue, for_line: Option<Rc<BigInt>>, for_sub_line: usize },
	FloatForLoop { name: Box<str>, final_value: FloatValue, step_value: FloatValue, for_line: Option<Rc<BigInt>>, for_sub_line: usize },
}