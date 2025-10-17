use std::{collections::HashMap, fs::{create_dir_all, File}, io::{stdin, stdout, BufRead, Read, Write}, iter::repeat_n, mem::take, num::NonZeroUsize, ops::{RangeFrom, RangeFull, RangeInclusive, RangeToInclusive}, path::{Path, PathBuf}, rc::Rc, str::FromStr};

use crossterm::{cursor::position, execute, style::{Color, ContentStyle, PrintStyledContent, StyledContent}};
use num::{BigInt, FromPrimitive, Signed, Zero};
use rand::{random_range, rngs::SmallRng, Rng, SeedableRng};

use crate::mavagk_basic::{abstract_syntax_tree::{AnyTypeExpression, AnyTypeLValue, BoolExpression, ComplexExpression, ComplexLValue, FloatExpression, FloatLValue, IntExpression, IntLValue, OptionVariableAndValue, PrintOperand, Statement, StatementVariant, StringExpression, StringLValue}, error::{handle_error, Error, ErrorVariant, FullError}, optimize::optimize_statement, options::{BaseOption, MachineOption, Options}, parse::{parse_line, Tokens}, program::{Line, Program}, token::{parse_datum_complex, parse_datum_float, parse_datum_int, parse_datum_string, IdentifierType, SuppliedFunction, Token}, value::{AnyTypeValue, BoolValue, ComplexValue, FloatValue, IntValue, StringValue, Value}};

/// A MavagkBasic virtual machine with its execution state, variables, options. Does not contain the program being executed.
pub struct Machine {
	// Program counter
	/// The current line being executed, `None` if executing the real mode line or if the first line should be executed.
	line_executing: Option<Rc<BigInt>>,
	/// The line that the next READ statement should read from.
	data_line_number_to_read: Option<Rc<BigInt>>,
	/// The datum index in the line that the next READ statement should read from.
	datum_index_in_data_line_to_read: usize,
	/// The current colon separated sub-line being executed. `None` to execute the first line.
	sub_line_executing: Option<usize>,
	execution_source: ExecutionSource,
	// Variables
	float_variables: HashMap<Box<str>, FloatValue>,
	complex_variables: HashMap<Box<str>, ComplexValue>,
	int_variables: HashMap<Box<str>, IntValue>,
	string_variables: HashMap<Box<str>, StringValue>,
	// Arrays
	float_arrays: HashMap<Box<str>, Array<FloatValue>>,
	int_arrays: HashMap<Box<str>, Array<IntValue>>,
	complex_arrays: HashMap<Box<str>, Array<ComplexValue>>,
	string_arrays: HashMap<Box<str>, Array<StringValue>>,
	// Functions
	float_functions: HashMap<(Box<str>, usize), (Box<[AnyTypeLValue]>, FloatExpression, Option<(Rc<BigInt>, usize)>)>,
	int_functions: HashMap<(Box<str>, usize), (Box<[AnyTypeLValue]>, IntExpression, Option<(Rc<BigInt>, usize)>)>,
	complex_functions: HashMap<(Box<str>, usize), (Box<[AnyTypeLValue]>, ComplexExpression, Option<(Rc<BigInt>, usize)>)>,
	string_functions: HashMap<(Box<str>, usize), (Box<[AnyTypeLValue]>, StringExpression, Option<(Rc<BigInt>, usize)>)>,
	/// A stack of GOSUB levels containing return addresses and program structure blocks such as active FOR loops. Must always contain at least one level.
	gosub_stack: Vec<GosubLevel>,
	/// The state of all the currently set OPTIONs.
	options: Options,

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
			int_arrays: HashMap::new(),
			float_arrays: HashMap::new(),
			complex_arrays: HashMap::new(),
			string_arrays: HashMap::new(),
			complex_functions: HashMap::new(),
			float_functions: HashMap::new(),
			int_functions: HashMap::new(),
			string_functions: HashMap::new(),
			//block_stack: Vec::new(),
			//for_loop_variable_to_block_stack_index: HashMap::new(),
			gosub_stack: vec![GosubLevel::new()],
			// angle_option: None,
			// math_option: None,
			// machine_option: None,
			// base_option: None,
			options: Options::new(),
			basic_home_path: None,
			rng: SmallRng::seed_from_u64(0),
			data_line_number_to_read: None,
			datum_index_in_data_line_to_read: 0,
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
				if !program.contains_line(&goto_line_number) {
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
		self.gosub_stack.last_mut().unwrap().truncate_block_stack(truncate_to);
	}

	// Clears everything about the machine state except where the machine is currently executing in the program.
	fn clear_machine_state(&mut self, program: &Program) {
		*self = Self {
			// Stuff to keep
			line_executing: take(&mut self.line_executing),
			sub_line_executing: self.sub_line_executing,
			execution_source: self.execution_source,
			basic_home_path: take(&mut self.basic_home_path),
			options: self.options.clone(),
			// Stuff to discard
			gosub_stack: vec![GosubLevel::new()],
			int_variables: HashMap::new(),
			float_variables: HashMap::new(),
			complex_variables: HashMap::new(),
			string_variables: HashMap::new(),

			int_arrays: HashMap::new(),
			float_arrays: HashMap::new(),
			complex_arrays: HashMap::new(),
			string_arrays: HashMap::new(),

			complex_functions: HashMap::new(),
			float_functions: HashMap::new(),
			int_functions: HashMap::new(),
			string_functions: HashMap::new(),

			//angle_option: None,
			//math_option: None,
			//machine_option: None,
			//base_option: None,

			rng: SmallRng::seed_from_u64(0),

			data_line_number_to_read: program.get_first_data_line().cloned(),
			datum_index_in_data_line_to_read: 0,
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
				let line_number = Rc::new(line_number);
				if let Some(error) = &error {
					let error = error.clone().to_full_error(Some((*line_number).clone()), Some(line_text.clone().into()));
					handle_error::<()>(Err(error));
				}
				if unoptimized_statements.is_empty() && error.is_none() {
					program.remove_line(&line_number);
				}
				else {
					let line = Line {
						unoptimized_statements,
						optimized_statements,
						error,
						source_code: line_text,
					};
					program.insert_line(line_number, line);
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

	/// Start executing until the program terminates
	fn execute(&mut self, program: &mut Program, direct_mode_statements: &Box<[Statement]>, direct_mode_line_text: &str) -> Result<(), FullError> {
		// For each line
		'lines_loop: loop {
			match self.execution_source {
				// If we are executing a line in the program
				ExecutionSource::Program => {
					// No line executing means that we should execute the first line
					if self.line_executing.is_none() {
						self.line_executing = match program.get_first_line() {
							Some(first_entry) => Some(first_entry.clone()),
							None => {
								self.execution_source = ExecutionSource::ProgramEnded;
								continue 'lines_loop;
							}
						};
					}
					let line_number = self.line_executing.as_ref().unwrap().clone();
					self.options = program.get_options(&line_number, 0);
					//program.get_options(self, line_number.clone(), 0);
					// Get which sub-line to start executing from
					let start_sub_line = match self.sub_line_executing {
						Some(start_sub_line) => start_sub_line,
						None => 0,
					};
					// Execute each sub-line to be executed
					let line = program.get_line(&line_number).unwrap();
					for (sub_line, statement) in line.optimized_statements.iter().enumerate().skip(start_sub_line) {
						// Set the sub line so that the statement executer can access it
						self.sub_line_executing = Some(sub_line);
						// Execute the sub-line
						let flow_control_used = match self.execute_statement(statement, program) {
							Ok(flow_control_used) => flow_control_used,
							Err(error) => return Err(error.to_full_error(Some((*line_number).clone()), Some(line.source_code.clone().into_string()))),
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
					if let Some(line_error) = &line.error {
						return Err(line_error.clone().to_full_error(Some((&*line_number).clone()), Some(line.source_code.clone().into_string())));
					}
					// Jump to the next line
					self.line_executing = match program.get_first_line_after(&line_number) {
						Some(line_number) => Some(line_number.clone()),
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
					Some(filepath_expression) => self.execute_string_expression(filepath_expression, Some(program))?,
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
			StatementVariant::Save(filepath_expression) => {
				// Execute expression
				let filename_value = match filepath_expression {
					Some(filepath_expression) => self.execute_string_expression(filepath_expression, Some(program))?,
					None => return Err(ErrorVariant::Unimplemented("SAVE without arguments".into()).at_column(*column)),
				};
				// Convert string value to filepath
				let filepath = self.string_to_full_filepath(&filename_value.value)
					.map_err(|error| error.at_column(filepath_expression.as_ref().unwrap().get_start_column()))?;
				// Open file
				let mut file = File::create(filepath).map_err(|_| ErrorVariant::UnableToOpenFile.at_column(filepath_expression.as_ref().unwrap().get_start_column()))?;
				// Save lines
				let mut is_first_line = true;
				for (_, line) in program.get_lines() {
					if !is_first_line {
						file.write_all(b"\n").map_err(|_| ErrorVariant::UnableToWriteFile.at_column(filepath_expression.as_ref().unwrap().get_start_column()))?;
					}
					file.write_all(line.source_code.as_bytes()).map_err(|_| ErrorVariant::UnableToWriteFile.at_column(filepath_expression.as_ref().unwrap().get_start_column()))?;
					is_first_line = false;
				}
				file.flush().map_err(|_| ErrorVariant::UnableToWriteFile.at_column(filepath_expression.as_ref().unwrap().get_start_column()))?;
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
								let argument_value = self.execute_any_type_expression(argument_expression, Some(program))?
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
							_ => self.execute_any_type_expression(expression, Some(program))?.print(&mut stdout(), true, true).unwrap()
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
						self.execute_any_type_expression(prompt_expression, Some(program))?.print(&mut stdout(), true, true).unwrap();
						if self.options.get_machine_option() == MachineOption::C64 {
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
						match next_input {
							AnyTypeLValue::Int(l_value) => {
								let parsed_value;
								(parsed_value, input_buffer_left) = match parse_datum_int(input_buffer_left) {
									Ok((Some(parsed_value), input_buffer_left)) => (parsed_value, input_buffer_left),
									Err(_) | Ok((None, _)) => continue 'a,
								};
								self.execute_int_l_value_write(l_value, parsed_value, Some(program))?;
							}
							AnyTypeLValue::Float(l_value) => {
								let parsed_value;
								(parsed_value, input_buffer_left) = match parse_datum_float(input_buffer_left) {
									Ok((Some(parsed_value), input_buffer_left)) => (parsed_value, input_buffer_left),
									Err(_) | Ok((None, _)) => continue 'a,
								};
								self.execute_float_l_value_write(l_value, parsed_value, Some(program))?;
							}
							AnyTypeLValue::Complex(l_value) => {
								let parsed_value;
								(parsed_value, input_buffer_left) = match parse_datum_complex(input_buffer_left) {
									Ok((Some(parsed_value), input_buffer_left)) => (parsed_value, input_buffer_left),
									Err(_) | Ok((None, _)) => continue 'a,
								};
								self.execute_complex_l_value_write(l_value, parsed_value, Some(program))?;
							}
							AnyTypeLValue::String(l_value) => {
								let parsed_value;
								(parsed_value, input_buffer_left) = match parse_datum_string(input_buffer_left, false) {
									Ok((parsed_value, input_buffer_left)) => (parsed_value, input_buffer_left),
									Err(_) => continue 'a,
								};
								self.execute_string_l_value_write(l_value, parsed_value, Some(program))?;
							}
						}
						input_buffer_left = input_buffer_left.trim_ascii_start();
						match input_buffer_left.chars().next() {
							Some(',') => input_buffer_left = &input_buffer_left[1..],
							Some(_) => continue 'a,
							None => {}
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
				let initial_value = self.execute_int_expression(initial, Some(program))?;
				let final_value = self.execute_int_expression(limit, Some(program))?;
				let step_value = match step {
					Some(step) => self.execute_int_expression(step, Some(program))?,
					None => IntValue::one(),
				};
				// TODO
				if (step_value.is_negative() && (&*initial_value.value < &*final_value.value)) || (!step_value.is_negative() && (&*initial_value.value > &*final_value.value)) {
					return Err(ErrorVariant::NotYetImplemented("FOR looping zero times".into()).at_column(*column));
				}
				self.execute_int_l_value_write(loop_variable, initial_value, Some(program))?;
				// Construct loop
				let stack_loop = BlockOnStack::IntForLoop { name: loop_variable.name.clone(), final_value, step_value, for_line: self.line_executing.clone(), for_sub_line: self.sub_line_executing.unwrap() };
				// If a for loop using the same variable exists, pop the loop and all blocks inside it
				match self.gosub_stack.last().unwrap().for_loop_variable_to_block_stack_index.get(&(loop_variable.name.clone(), false)) {
					None => {},
					Some(block_stack_index) => self.truncate_block_stack(*block_stack_index),
				}
				// Push loop
				self.gosub_stack.last_mut().unwrap().block_stack.push(stack_loop);
				let index = (self.gosub_stack.last().unwrap().block_stack.len() - 1).clone();
				self.gosub_stack.last_mut().unwrap().for_loop_variable_to_block_stack_index.insert((loop_variable.name.clone(), false), index);
			}
			StatementVariant::ForFloat { loop_variable, initial, limit, step } => {
				// Execute expressions
				let initial_value = self.execute_float_expression(initial, Some(program))?;
				let final_value = self.execute_float_expression(limit, Some(program))?;
				let step_value = match step {
					Some(step) => self.execute_float_expression(step, Some(program))?,
					None => FloatValue::ONE,
				};
				// TODO
				if (step_value.is_negative() && (initial_value.value < final_value.value)) || (!step_value.is_negative() && (initial_value.value > final_value.value)) {
					return Err(ErrorVariant::NotYetImplemented("FOR looping zero times".into()).at_column(*column));
				}
				self.execute_float_l_value_write(loop_variable, initial_value, Some(program))?;
				// Construct loop
				let stack_loop = BlockOnStack::FloatForLoop { name: loop_variable.name.clone(), final_value, step_value, for_line: self.line_executing.clone(), for_sub_line: self.sub_line_executing.unwrap() };
				// If a for loop using the same variable exists, pop the loop and all blocks inside it
				match self.gosub_stack.last().unwrap().for_loop_variable_to_block_stack_index.get(&(loop_variable.name.clone(), true)) {
					None => {},
					Some(block_stack_index) => self.truncate_block_stack(*block_stack_index),
				}
				// Push loop
				self.gosub_stack.last_mut().unwrap().block_stack.push(stack_loop);
				let index = (self.gosub_stack.last().unwrap().block_stack.len() - 1).clone();
				self.gosub_stack.last_mut().unwrap().for_loop_variable_to_block_stack_index.insert((loop_variable.name.clone(), true), index);
			}
			StatementVariant::Next(loop_variables) => {
				let allow_overflow = self.options.allow_overflow();
				// If a NEXT without arguments is executed
				if loop_variables.is_empty() {
					for (index, loop_block) in self.gosub_stack.last().unwrap().block_stack.iter().enumerate().rev() {
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
							let block_stack_index = match self.gosub_stack.last().unwrap().for_loop_variable_to_block_stack_index.get(&(name.clone(), false)) {
								Some(block_stack_index) => block_stack_index,
								None => return Err(ErrorVariant::ForLoopVariableNotFound.at_column(*start_column)),
							};
							let (final_value, step_value, for_line, for_sub_line) = match self.gosub_stack.last().unwrap().block_stack.get(*block_stack_index).unwrap() {
								BlockOnStack::IntForLoop { final_value, step_value, for_line, for_sub_line, .. } => (final_value, step_value, for_line, for_sub_line),
								_ => unreachable!(),
							};
							// Get current value
							let loop_variable_value = self.int_variables.get_mut(name).unwrap();
							// Increment
							*loop_variable_value = loop_variable_value.clone().add(&step_value);
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
							let block_stack_index = match self.gosub_stack.last().unwrap().for_loop_variable_to_block_stack_index.get(&(name.clone(), true)) {
								Some(block_stack_index) => block_stack_index,
								None => return Err(ErrorVariant::ForLoopVariableNotFound.at_column(*start_column)),
							};
							let (final_value, step_value, for_line, for_sub_line) = match self.gosub_stack.last().unwrap().block_stack.get(*block_stack_index).unwrap() {
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
						_ => unreachable!(),
					}
				}
			}
			StatementVariant::Goto(sub_expression) | StatementVariant::Run(sub_expression) | StatementVariant::Gosub(sub_expression) => {
				// Push the return address and create a new GOSUB level if this is a GOSUB statement
				if matches!(variant, StatementVariant::Gosub(..)) {
					if self.execution_source == ExecutionSource::DirectModeLine {
						return Err(ErrorVariant::GosubInDirectMode.at_column(*column));
					}
					self.gosub_stack.push(GosubLevel::from_return_location(self.line_executing.clone(), self.sub_line_executing.unwrap()));
				}
				// Set the line to be executed next
				match sub_expression {
					Some(sub_expression) => {
						let line_number_to_jump_to = self.execute_int_expression(sub_expression, Some(program))?.value;
						self.set_line_executing_by_jumping(program, Some(line_number_to_jump_to), sub_expression.get_start_column())?;
					}
					None => self.set_line_executing_by_jumping(program, None, *column)?,
				}
				// Clear if this is a RUN statement
				if matches!(variant, StatementVariant::Run(..)) {
					self.clear_machine_state(program);
				}
				// Next statement
				return Ok(true);
			}
			StatementVariant::AssignInt(l_value, r_value_expression) => {
				let value = self.execute_int_expression(r_value_expression, Some(program))?;
				self.execute_int_l_value_write(l_value, value, Some(program))?
			}
			StatementVariant::AssignFloat(l_value, r_value_expression) => {
				let value = self.execute_float_expression(r_value_expression, Some(program))?;
				self.execute_float_l_value_write(l_value, value, Some(program))?
			}
			StatementVariant::AssignComplex(l_value, r_value_expression) => {
				let value = self.execute_complex_expression(r_value_expression, Some(program))?;
				self.execute_complex_l_value_write(l_value, value, Some(program))?
			}
			StatementVariant::AssignString(l_value, r_value_expression) => {
				let value = self.execute_string_expression(r_value_expression, Some(program))?;
				self.execute_string_l_value_write(l_value, value, Some(program))?
			}
			StatementVariant::List(range_start, range_end) => {
				let range_start_value = match range_start {
					Some(range_start) => Some(&*self.execute_int_expression(range_start, Some(program))?.value),
					None => None,
				};
				let range_end_value = match range_end {
					Some(range_end) => Some(&*self.execute_int_expression(range_end, Some(program))?.value),
					None => None,
				};
				let range = match (range_start_value, range_end_value) {
					(None, None) => program.get_lines().range::<BigInt, RangeFull>(..),
					(Some(range_start_value), None) => program.get_lines().range::<BigInt, RangeFrom<&BigInt>>(range_start_value..),
					(None, Some(range_end_value)) => program.get_lines().range::<BigInt, RangeToInclusive<&BigInt>>(..=range_end_value),
					(Some(range_start_value), Some(range_end_value)) => program.get_lines().range::<BigInt, RangeInclusive<&BigInt>>(range_start_value..=range_end_value),
				};
				for (_line, line) in range {
					if let Some(_) = line.error {
						execute!(stdout(), PrintStyledContent(StyledContent::new(ContentStyle { foreground_color: Some(Color::Red), ..Default::default() }, format!("{}\n", line.source_code)))).unwrap()
					}
					else {
						println!("{}", line.source_code);
					}
				}
			}
			StatementVariant::OneLineIf { condition_expression: condition, then_statement, else_statement } => {
				// Execute condition
				let condition_value = self.execute_bool_expression(condition, Some(program))?;
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
					OptionVariableAndValue::Angle(angle_option) => self.options.angle = *angle_option,
					OptionVariableAndValue::Math(math_option) => self.options.math = *math_option,
					OptionVariableAndValue::Machine(machine_option) => self.options.machine = *machine_option,
					OptionVariableAndValue::Base(base_option) => self.options.base = *base_option,
				}
			}
			StatementVariant::Load(_filename_expression) | StatementVariant::Save(_filename_expression) => {
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
			StatementVariant::Data(_data) => {}
			StatementVariant::Read { to_do_when_data_missing_statement, variables: l_values_to_assign_to } => {
				// For each l-value that we should read to
				for l_value in l_values_to_assign_to {
					// If we have run out of data to read
					let data_line_number_to_read = match self.data_line_number_to_read.clone() {
						Some(data_line_to_read) => data_line_to_read,
						None => match to_do_when_data_missing_statement {
							Some(to_do_when_data_missing_statement) => return self.execute_statement(to_do_when_data_missing_statement, program),
							None => return Err(ErrorVariant::ReadOutOfData.at_column(l_value.get_start_column())),
						},
					};
					// Else get the next datum
					let data_line_values = program.get_data_line(&data_line_number_to_read).unwrap();
					let (datum, datum_start_column) = data_line_values[self.datum_index_in_data_line_to_read].clone();
					// Read datum
					match l_value {
						AnyTypeLValue::Int(l_value) => {
							let datum = match &datum.as_integer {
								Some(datum) => datum,
								None => return Err(ErrorVariant::NonNumericReadToNumeric((*data_line_number_to_read).clone(), datum_start_column).at_column(l_value.start_column)),
							};
							self.execute_int_l_value_write(l_value, datum.clone(), Some(program))?;
						}
						AnyTypeLValue::Float(l_value) => {
							let datum = match &datum.as_float {
								Some(datum) => datum,
								None => return Err(ErrorVariant::NonNumericReadToNumeric((*data_line_number_to_read).clone(), datum_start_column).at_column(l_value.start_column)),
							};
							self.execute_float_l_value_write(l_value, datum.clone(), Some(program))?;
						}
						AnyTypeLValue::Complex(l_value) => {
							let datum = match &datum.as_complex {
								Some(datum) => datum,
								None => return Err(ErrorVariant::NonNumericReadToNumeric((*data_line_number_to_read).clone(), datum_start_column).at_column(l_value.start_column)),
							};
							self.execute_complex_l_value_write(l_value, datum.clone(), Some(program))?;
						}
						AnyTypeLValue::String(l_value) => self.execute_string_l_value_write(l_value, datum.as_string.clone(), Some(program))?,
					}
					// Calculate new line number
					self.datum_index_in_data_line_to_read += 1;
					if self.datum_index_in_data_line_to_read >= data_line_values.len() {
						self.data_line_number_to_read = program.get_first_data_line_after(&*data_line_number_to_read).cloned();
						self.datum_index_in_data_line_to_read = 0;
					}
				}
			}
			StatementVariant::Restore(restore_to_line_number_expression) => {
				let restore_to_line_number_expression = match restore_to_line_number_expression {
					Some(restore_to_line_number_expression) => restore_to_line_number_expression,
					None => {
						self.data_line_number_to_read = program.get_first_data_line().cloned();
						self.datum_index_in_data_line_to_read = 0;
						return Ok(false);
					}
				};
				let restore_to_line_number = self.execute_int_expression(restore_to_line_number_expression, Some(program))?.value;
				if !program.contains_data_line(&*restore_to_line_number) {
					return Err(ErrorVariant::RestoreToLineWithoutData((*restore_to_line_number).clone()).at_column(restore_to_line_number_expression.get_start_column()));
				}
				self.data_line_number_to_read = Some(restore_to_line_number);
			}
			StatementVariant::Return => {
				// A matching GOSUB statement must have been executed
				if self.gosub_stack.len() < 2 {
					return Err(ErrorVariant::ReturnWithoutGosub.at_column(*column));
				}
				// Pop GOSUB level off the stack and jump to the sub-line after the one the GOSUB was located on
				let popped_gosub_level = self.gosub_stack.pop().unwrap();
				self.line_executing = popped_gosub_level.return_line;
				self.sub_line_executing = Some(popped_gosub_level.return_sub_line + 1);
				// Flow control used
				return Ok(true);
			}
			StatementVariant::Dimension(arrays) => {
				if self.options.arrays_created_on_dim_execution() {
					for array in arrays {
						self.execute_array_declaration(statement, program, &array.name, array.array_type)?;
					}
				}
			}
			StatementVariant::DefFloat(_l_value, _expression) => {
				if self.options.functions_defined_on_fn_execution() {
					self.execute_function_declaration(statement)?;
				}
			}
			StatementVariant::DefInt(_l_value, _expression) => {
				if self.options.functions_defined_on_fn_execution() {
					self.execute_function_declaration(statement)?;
				}
			}
			StatementVariant::DefComplex(_l_value, _expression) => {
				if self.options.functions_defined_on_fn_execution() {
					self.execute_function_declaration(statement)?;
				}
			}
			StatementVariant::DefString(_l_value, _expression) => {
				if self.options.functions_defined_on_fn_execution() {
					self.execute_function_declaration(statement)?;
				}
			}
		}
		Ok(false)
	}

	fn execute_array_declaration(&mut self, statement: &Statement, program: &Program, name: &str, declaration_type: IdentifierType) -> Result<(), Error> {
		let Statement { variant, column: _ } = &statement;
		match variant {
			StatementVariant::Dimension(arrays) => {
				for array in arrays {
					if &*array.name == name && array.array_type == declaration_type {
						match declaration_type {
							IdentifierType::UnmarkedOrFloat => {
								let mut dimensions = Vec::new();
								for dimension_expression in &array.dimensions {
									let lower_bound = match &dimension_expression.0 {
										Some(lower_bound) => self.execute_int_expression(lower_bound, Some(program))?,
										None => match self.options.get_base_option() {
											BaseOption::Zero => IntValue::zero(),
											BaseOption::One => IntValue::one(),
										}
									};
									let upper_bound = self.execute_int_expression(&dimension_expression.1, Some(program))?;
									let dimension_length = upper_bound.sub(lower_bound.clone()).add(&IntValue::one());
									let dimension_length = match dimension_length.to_usize() {
										Some(dimension_length) => dimension_length,
										None => return Err(ErrorVariant::InvalidArrayDimensionLength.at_column(array.start_column)),
									};
									dimensions.push((dimension_length, lower_bound));
								}
								let array = Array::new(dimensions.into_boxed_slice()).map_err(|err| err.at_column(array.start_column))?;
								self.float_arrays.insert(name.into(), array);
							}
							IdentifierType::Integer => {
								let mut dimensions = Vec::new();
								for dimension_expression in &array.dimensions {
									let lower_bound = match &dimension_expression.0 {
										Some(lower_bound) => self.execute_int_expression(lower_bound, Some(program))?,
										None => match self.options.get_base_option() {
											BaseOption::Zero => IntValue::zero(),
											BaseOption::One => IntValue::one(),
										}
									};
									let upper_bound = self.execute_int_expression(&dimension_expression.1, Some(program))?;
									let dimension_length = upper_bound.sub(lower_bound.clone()).add(&IntValue::one());
									let dimension_length = match dimension_length.to_usize() {
										Some(dimension_length) => dimension_length,
										None => return Err(ErrorVariant::InvalidArrayDimensionLength.at_column(array.start_column)),
									};
									dimensions.push((dimension_length, lower_bound));
								}
								let array = Array::new(dimensions.into_boxed_slice()).map_err(|err| err.at_column(array.start_column))?;
								self.int_arrays.insert(name.into(), array);
							}
							IdentifierType::ComplexNumber => {
								let mut dimensions = Vec::new();
								for dimension_expression in &array.dimensions {
									let lower_bound = match &dimension_expression.0 {
										Some(lower_bound) => self.execute_int_expression(lower_bound, Some(program))?,
										None => match self.options.get_base_option() {
											BaseOption::Zero => IntValue::zero(),
											BaseOption::One => IntValue::one(),
										}
									};
									let upper_bound = self.execute_int_expression(&dimension_expression.1, Some(program))?;
									let dimension_length = upper_bound.sub(lower_bound.clone()).add(&IntValue::one());
									let dimension_length = match dimension_length.to_usize() {
										Some(dimension_length) => dimension_length,
										None => return Err(ErrorVariant::InvalidArrayDimensionLength.at_column(array.start_column)),
									};
									dimensions.push((dimension_length, lower_bound));
								}
								let array = Array::new(dimensions.into_boxed_slice()).map_err(|err| err.at_column(array.start_column))?;
								self.complex_arrays.insert(name.into(), array);
							}
							IdentifierType::String => {
								let mut dimensions = Vec::new();
								for dimension_expression in &array.dimensions {
									let lower_bound = match &dimension_expression.0 {
										Some(lower_bound) => self.execute_int_expression(lower_bound, Some(program))?,
										None => match self.options.get_base_option() {
											BaseOption::Zero => IntValue::zero(),
											BaseOption::One => IntValue::one(),
										}
									};
									let upper_bound = self.execute_int_expression(&dimension_expression.1, Some(program))?;
									let dimension_length = upper_bound.sub(lower_bound.clone()).add(&IntValue::one());
									let dimension_length = match dimension_length.to_usize() {
										Some(dimension_length) => dimension_length,
										None => return Err(ErrorVariant::InvalidArrayDimensionLength.at_column(array.start_column)),
									};
									dimensions.push((dimension_length, lower_bound));
								}
								let array = Array::new(dimensions.into_boxed_slice()).map_err(|err| err.at_column(array.start_column))?;
								self.string_arrays.insert(name.into(), array);
							}
						}
					}
				}
				Ok(())
			},
			_ => unreachable!(),
		}
	}

	fn execute_function_declaration(&mut self, statement: &Statement) -> Result<(), Error> {
		let Statement { variant, column: _ } = &statement;
		match variant {
			StatementVariant::DefFloat(l_value, expression) => {
				let mut parameters = Vec::new();
				let def_location = match &self.line_executing {
					None => None,
					Some(line_executing) => Some((line_executing.clone(), self.sub_line_executing.unwrap())),
				};
				for function_parameter in l_value.arguments.iter() {
					if !function_parameter.is_valid_function_argument() {
						return Err(ErrorVariant::InvalidFunctionParameter.at_column(l_value.start_column));
					}
					match function_parameter {
						AnyTypeExpression::Float(FloatExpression::LValue(parameter)) => parameters.push(AnyTypeLValue::Float(parameter.clone())),
						AnyTypeExpression::Int(IntExpression::LValue(parameter)) => parameters.push(AnyTypeLValue::Int(parameter.clone())),
						AnyTypeExpression::Complex(ComplexExpression::LValue(parameter)) => parameters.push(AnyTypeLValue::Complex(parameter.clone())),
						AnyTypeExpression::String(StringExpression::LValue(parameter)) => parameters.push(AnyTypeLValue::String(parameter.clone())),
						_ => unreachable!(),
					}
				}
				self.float_functions.insert((l_value.name.clone(), l_value.arguments.len()), (parameters.into_boxed_slice(), expression.clone(), def_location));
				match l_value.has_parentheses {
					true => {
						self.float_arrays.remove(&l_value.name);
					}
					false => {
						self.float_variables.remove(&l_value.name);
					}
				}
				Ok(())
			}
			StatementVariant::DefInt(l_value, expression) => {
				let mut parameters = Vec::new();
				let def_location = match &self.line_executing {
					None => None,
					Some(line_executing) => Some((line_executing.clone(), self.sub_line_executing.unwrap())),
				};
				for function_parameter in l_value.arguments.iter() {
					if !function_parameter.is_valid_function_argument() {
						return Err(ErrorVariant::InvalidFunctionParameter.at_column(l_value.start_column));
					}
					match function_parameter {
						AnyTypeExpression::Float(FloatExpression::LValue(parameter)) => parameters.push(AnyTypeLValue::Float(parameter.clone())),
						AnyTypeExpression::Int(IntExpression::LValue(parameter)) => parameters.push(AnyTypeLValue::Int(parameter.clone())),
						AnyTypeExpression::Complex(ComplexExpression::LValue(parameter)) => parameters.push(AnyTypeLValue::Complex(parameter.clone())),
						AnyTypeExpression::String(StringExpression::LValue(parameter)) => parameters.push(AnyTypeLValue::String(parameter.clone())),
						_ => unreachable!(),
					}
				}
				self.int_functions.insert((l_value.name.clone(), l_value.arguments.len()), (parameters.into_boxed_slice(), expression.clone(), def_location));
				match l_value.has_parentheses {
					true => {
						self.int_arrays.remove(&l_value.name);
					}
					false => {
						self.int_variables.remove(&l_value.name);
					}
				}
				Ok(())
			}
			StatementVariant::DefComplex(l_value, expression) => {
				let mut parameters = Vec::new();
				let def_location = match &self.line_executing {
					None => None,
					Some(line_executing) => Some((line_executing.clone(), self.sub_line_executing.unwrap())),
				};
				for function_parameter in l_value.arguments.iter() {
					if !function_parameter.is_valid_function_argument() {
						return Err(ErrorVariant::InvalidFunctionParameter.at_column(l_value.start_column));
					}
					match function_parameter {
						AnyTypeExpression::Float(FloatExpression::LValue(parameter)) => parameters.push(AnyTypeLValue::Float(parameter.clone())),
						AnyTypeExpression::Int(IntExpression::LValue(parameter)) => parameters.push(AnyTypeLValue::Int(parameter.clone())),
						AnyTypeExpression::Complex(ComplexExpression::LValue(parameter)) => parameters.push(AnyTypeLValue::Complex(parameter.clone())),
						AnyTypeExpression::String(StringExpression::LValue(parameter)) => parameters.push(AnyTypeLValue::String(parameter.clone())),
						_ => unreachable!(),
					}
				}
				self.complex_functions.insert((l_value.name.clone(), l_value.arguments.len()), (parameters.into_boxed_slice(), expression.clone(), def_location));
				match l_value.has_parentheses {
					true => {
						self.complex_arrays.remove(&l_value.name);
					}
					false => {
						self.complex_variables.remove(&l_value.name);
					}
				}
				Ok(())
			}
			StatementVariant::DefString(l_value, expression) => {
				let mut parameters = Vec::new();
				let def_location = match &self.line_executing {
					None => None,
					Some(line_executing) => Some((line_executing.clone(), self.sub_line_executing.unwrap())),
				};
				for function_parameter in l_value.arguments.iter() {
					if !function_parameter.is_valid_function_argument() {
						return Err(ErrorVariant::InvalidFunctionParameter.at_column(l_value.start_column));
					}
					match function_parameter {
						AnyTypeExpression::Float(FloatExpression::LValue(parameter)) => parameters.push(AnyTypeLValue::Float(parameter.clone())),
						AnyTypeExpression::Int(IntExpression::LValue(parameter)) => parameters.push(AnyTypeLValue::Int(parameter.clone())),
						AnyTypeExpression::Complex(ComplexExpression::LValue(parameter)) => parameters.push(AnyTypeLValue::Complex(parameter.clone())),
						AnyTypeExpression::String(StringExpression::LValue(parameter)) => parameters.push(AnyTypeLValue::String(parameter.clone())),
						_ => unreachable!(),
					}
				}
				self.string_functions.insert((l_value.name.clone(), l_value.arguments.len()), (parameters.into_boxed_slice(), expression.clone(), def_location));
				match l_value.has_parentheses {
					true => {
						self.string_arrays.remove(&l_value.name);
					}
					false => {
						self.string_variables.remove(&l_value.name);
					}
				}
				Ok(())
			}
			_ => unreachable!(),
		}
	}

	/// Execute an expression that returns an integer value.
	fn execute_int_expression(&mut self, expression: &IntExpression, program: Option<&Program>) -> Result<IntValue, Error> {
		Ok(match expression {
			IntExpression::ConstantValue { value, .. } => value.clone(),
			IntExpression::CastFromBool(sub_expression) => self.execute_bool_expression(sub_expression, program)?.to_int(),
			IntExpression::CastFromFloat(sub_expression) => {
				self.execute_float_expression(sub_expression, program)?.to_int().map_err(|error| error.at_column(sub_expression.get_start_column()))?
			}
			IntExpression::BitwiseAnd { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression, program)?.and(self.execute_int_expression(rhs_expression, program)?),
			IntExpression::BitwiseOr { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression, program)?.or(self.execute_int_expression(rhs_expression, program)?),
			IntExpression::Addition { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression, program)?.add(&self.execute_int_expression(rhs_expression, program)?),
			IntExpression::Subtraction { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression, program)?.sub(self.execute_int_expression(rhs_expression, program)?),
			IntExpression::Multiplication { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression, program)?.mul(self.execute_int_expression(rhs_expression, program)?),
			IntExpression::FlooredDivision { lhs_expression, rhs_expression, start_column } =>
				self.execute_int_expression(lhs_expression, program)?.floored_div(self.execute_int_expression(rhs_expression, program)?)
					.map_err(|error| error.at_column(*start_column))?,
			IntExpression::BitwiseNot { sub_expression, .. } => self.execute_int_expression(sub_expression, program)?.not(),
			IntExpression::Negation { sub_expression, .. } => self.execute_int_expression(sub_expression, program)?.neg(),
			IntExpression::LValue(l_value) => self.execute_int_l_value_read(l_value, program)?,
		})
	}

	/// Execute an expression that returns an float value.
	fn execute_float_expression(&mut self, expression: &FloatExpression, program: Option<&Program>) -> Result<FloatValue, Error> {
		Ok(match expression {
			FloatExpression::ConstantValue { value, .. } => value.clone(),
			FloatExpression::CastFromInt(sub_expression) => self.execute_int_expression(sub_expression, program)?.to_float(),
			FloatExpression::CastFromComplex(sub_expression) =>
				self.execute_complex_expression(sub_expression, program)?.to_float().map_err(|error| error.at_column(sub_expression.get_start_column()))?,
			FloatExpression::Addition { lhs_expression, rhs_expression, start_column } =>
				self.execute_float_expression(lhs_expression, program)?.add(self.execute_float_expression(rhs_expression, program)?, self.options.allow_overflow())
					.map_err(|error| error.at_column(*start_column))?,
			FloatExpression::Subtraction { lhs_expression, rhs_expression, start_column } =>
				self.execute_float_expression(lhs_expression, program)?.sub(self.execute_float_expression(rhs_expression, program)?, self.options.allow_overflow())
					.map_err(|error| error.at_column(*start_column))?,
			FloatExpression::Multiplication { lhs_expression, rhs_expression, start_column } =>
				self.execute_float_expression(lhs_expression, program)?.mul(self.execute_float_expression(rhs_expression, program)?, self.options.allow_overflow())
					.map_err(|error| error.at_column(*start_column))?,
			FloatExpression::Division { lhs_expression, rhs_expression, start_column } =>
				self.execute_float_expression(lhs_expression, program)?
					.div(self.execute_float_expression(rhs_expression, program)?, self.options.allow_overflow(), self.options.allow_divide_by_zero())
					.map_err(|error| error.at_column(*start_column))?,
			FloatExpression::Exponentiation { lhs_expression, rhs_expression, start_column } =>
				self.execute_float_expression(lhs_expression, program)?
					.pow(self.execute_float_expression(rhs_expression, program)?, self.options.allow_overflow(), self.options.allow_divide_by_zero())
					.map_err(|error| error.at_column(*start_column))?,
			FloatExpression::Negation { sub_expression, .. } => self.execute_float_expression(&sub_expression, program)?.neg(),
			FloatExpression::LValue(l_value) => self.execute_float_l_value_read(l_value, program)?,
		})
	}

	/// Execute an expression that returns an complex value.
	fn execute_complex_expression(&mut self, expression: &ComplexExpression, program: Option<&Program>) -> Result<ComplexValue, Error> {
		Ok(match expression {
			ComplexExpression::ConstantValue { value, .. } => *value,
			ComplexExpression::CastFromFloat(sub_expression) => self.execute_float_expression(sub_expression, program)?.to_complex(),
			ComplexExpression::Addition { lhs_expression, rhs_expression, start_column } =>
				self.execute_complex_expression(lhs_expression, program)?.add(self.execute_complex_expression(rhs_expression, program)?, self.options.allow_overflow())
					.map_err(|error| error.at_column(*start_column))?,
			ComplexExpression::Subtraction { lhs_expression, rhs_expression, start_column } =>
				self.execute_complex_expression(lhs_expression, program)?.sub(self.execute_complex_expression(rhs_expression, program)?, self.options.allow_overflow())
					.map_err(|error| error.at_column(*start_column))?,
			ComplexExpression::Multiplication { lhs_expression, rhs_expression, start_column } =>
				self.execute_complex_expression(lhs_expression, program)?.mul(self.execute_complex_expression(rhs_expression, program)?, self.options.allow_overflow())
					.map_err(|error| error.at_column(*start_column))?,
			ComplexExpression::Division { lhs_expression, rhs_expression, start_column } =>
				self.execute_complex_expression(lhs_expression, program)?
					.div(self.execute_complex_expression(rhs_expression, program)?, self.options.allow_overflow(), self.options.allow_divide_by_zero())
					.map_err(|error| error.at_column(*start_column))?,
			ComplexExpression::Exponentiation { lhs_expression, rhs_expression, start_column } =>
				self.execute_complex_expression(lhs_expression, program)?.pow(self.execute_complex_expression(rhs_expression, program)?, self.options.allow_overflow())
					.map_err(|error| error.at_column(*start_column))?,
			ComplexExpression::Negation { sub_expression, .. } => self.execute_complex_expression(sub_expression, program)?.neg(),
			ComplexExpression::LValue(l_value) => self.execute_complex_l_value_read(l_value, program)?,
		})
	}

	/// Execute an expression that returns an boolean value.
	fn execute_bool_expression(&mut self, expression: &BoolExpression, program: Option<&Program>) -> Result<BoolValue, Error> {
		Ok(match expression {
			BoolExpression::ConstantValue { value, .. } => *value,
			BoolExpression::IntIsNonZero(int_expression) => BoolValue::new(!self.execute_int_expression(&int_expression, program)?.value.is_zero()),
			BoolExpression::FloatIsNonZero(float_expression) => BoolValue::new(!self.execute_float_expression(&float_expression, program)?.is_zero()),
			BoolExpression::ComplexIsNonZero(complex_expression) => BoolValue::new(!self.execute_complex_expression(&complex_expression, program)?.value.is_zero()),
			BoolExpression::StringIsNotEmpty(string_expression) => BoolValue::new(!self.execute_string_expression(&string_expression, program)?.value.is_empty()),

			BoolExpression::And { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression, program)?.and(self.execute_bool_expression(rhs_expression, program)?),
			BoolExpression::Or { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression, program)?.or(self.execute_bool_expression(rhs_expression, program)?),
			BoolExpression::Not { sub_expression, .. } => self.execute_bool_expression(sub_expression, program)?.not(),

			BoolExpression::BoolEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression, program)?.equal_to(self.execute_bool_expression(rhs_expression, program)?),
			BoolExpression::BoolNotEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression, program)?.not_equal_to(self.execute_bool_expression(rhs_expression, program)?),
			BoolExpression::BoolLessThan { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression, program)?.less_than(self.execute_bool_expression(rhs_expression, program)?),
			BoolExpression::BoolLessThanOrEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression, program)?.less_than_or_equal_to(self.execute_bool_expression(rhs_expression, program)?),
			BoolExpression::BoolGreaterThan { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression, program)?.greater_than(self.execute_bool_expression(rhs_expression, program)?),
			BoolExpression::BoolGreaterThanOrEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_bool_expression(lhs_expression, program)?.greater_than_or_equal_to(self.execute_bool_expression(rhs_expression, program)?),
			
			BoolExpression::IntEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression, program)?.equal_to(&self.execute_int_expression(rhs_expression, program)?),
			BoolExpression::IntNotEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression, program)?.not_equal_to(&self.execute_int_expression(rhs_expression, program)?),
			BoolExpression::IntLessThan { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression, program)?.less_than(&self.execute_int_expression(rhs_expression, program)?),
			BoolExpression::IntLessThanOrEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression, program)?.less_than_or_equal_to(&self.execute_int_expression(rhs_expression, program)?),
			BoolExpression::IntGreaterThan { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression, program)?.greater_than(&self.execute_int_expression(rhs_expression, program)?),
			BoolExpression::IntGreaterThanOrEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_int_expression(lhs_expression, program)?.greater_than_or_equal_to(&self.execute_int_expression(rhs_expression, program)?),

			BoolExpression::FloatEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_float_expression(lhs_expression, program)?.equal_to(&self.execute_float_expression(rhs_expression, program)?),
			BoolExpression::FloatNotEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_float_expression(lhs_expression, program)?.not_equal_to(&self.execute_float_expression(rhs_expression, program)?),
			BoolExpression::FloatLessThan { lhs_expression, rhs_expression, .. } =>
				self.execute_float_expression(lhs_expression, program)?.less_than(&self.execute_float_expression(rhs_expression, program)?),
			BoolExpression::FloatLessThanOrEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_float_expression(lhs_expression, program)?.less_than_or_equal_to(&self.execute_float_expression(rhs_expression, program)?),
			BoolExpression::FloatGreaterThan { lhs_expression, rhs_expression, .. } =>
				self.execute_float_expression(lhs_expression, program)?.greater_than(&self.execute_float_expression(rhs_expression, program)?),
			BoolExpression::FloatGreaterThanOrEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_float_expression(lhs_expression, program)?.greater_than_or_equal_to(&self.execute_float_expression(rhs_expression, program)?),

			BoolExpression::ComplexEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_complex_expression(lhs_expression, program)?.equal_to(self.execute_complex_expression(rhs_expression, program)?),
			BoolExpression::ComplexNotEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_complex_expression(lhs_expression, program)?.not_equal_to(self.execute_complex_expression(rhs_expression, program)?),

			BoolExpression::StringEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_string_expression(lhs_expression, program)?.equal_to(&self.execute_string_expression(rhs_expression, program)?),
			BoolExpression::StringNotEqualTo { lhs_expression, rhs_expression, .. } =>
				self.execute_string_expression(lhs_expression, program)?.not_equal_to(&self.execute_string_expression(rhs_expression, program)?),
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
	fn execute_string_expression(&mut self, expression: &StringExpression, program: Option<&Program>) -> Result<StringValue, Error> {
		Ok(match expression {
			StringExpression::ConstantValue { value, .. } => value.clone(),
			StringExpression::Concatenation { lhs_expression, rhs_expression, .. } =>
				self.execute_string_expression(lhs_expression, program)?.concat(self.execute_string_expression(rhs_expression, program)?),
			StringExpression::LValue(l_value) => self.execute_string_l_value_read(l_value, program)?,
		})
	}

	/// Execute an expression that could return a value of any type.
	fn execute_any_type_expression(&mut self, expression: &AnyTypeExpression, program: Option<&Program>) -> Result<AnyTypeValue, Error> {
		Ok(match expression {
			AnyTypeExpression::Bool(expression) => AnyTypeValue::Bool(self.execute_bool_expression(expression, program)?),
			AnyTypeExpression::Int(expression) => AnyTypeValue::Int(self.execute_int_expression(expression, program)?),
			AnyTypeExpression::Float(expression) => AnyTypeValue::Float(self.execute_float_expression(expression, program)?),
			AnyTypeExpression::Complex(expression) => AnyTypeValue::Complex(self.execute_complex_expression(expression, program)?),
			AnyTypeExpression::String(expression) => AnyTypeValue::String(self.execute_string_expression(expression, program)?),
		})
	}

	/// Reads a integer variable or from an integer array or executes a function that returns an integer.
	fn execute_int_l_value_read(&mut self, l_value: &IntLValue, program: Option<&Program>) -> Result<IntValue, Error> {
		// Unpack
		let IntLValue { name, arguments, /*uses_fn_keyword,*/ has_parentheses, start_column, supplied_function } = l_value;
		// Create the array if it is not yet created
		if *has_parentheses && !self.int_arrays.contains_key(name) && !self.options.arrays_created_on_dim_execution() &&
			program.is_some() && let Some(int_array_declarations) = program.unwrap().int_array_declarations.get(name)
		{
			if int_array_declarations.len() > 1 {
				return Err(ErrorVariant::MultipleDeclarationsOfArray.at_column(l_value.start_column));
			}
			let array_declarations_location = int_array_declarations.iter().next().unwrap();
			let statement = &program.unwrap().get_line(&array_declarations_location.0).unwrap().optimized_statements[array_declarations_location.1];
			let options_before_array_creation = self.options.clone();
			self.options = program.unwrap().get_options(&array_declarations_location.0.clone(), array_declarations_location.1);
			self.execute_array_declaration(statement, program.unwrap(), &name, IdentifierType::Integer)?;
			self.options = options_before_array_creation;
		}
		// Define function if it is not yet defined
		if !self.float_functions.contains_key(&(name.clone(), arguments.len())) && !self.options.functions_defined_on_fn_execution() &&
			program.is_some() && let Some(float_functions) = program.unwrap().float_functions.get(&(name.clone(), arguments.len()))
		{
			if float_functions.len() > 1 {
				return Err(ErrorVariant::MultipleDeclarationsOfFunction.at_column(l_value.start_column));
			}
			if !arguments.is_empty() && self.float_arrays.contains_key(name) {
				return Err(ErrorVariant::MultipleDeclarationsOfFunctionAndArray.at_column(l_value.start_column));
			}
			let float_function = float_functions.iter().next().unwrap();
			let statement = &program.unwrap().get_line(&float_function.0).unwrap().optimized_statements[float_function.1];
			//let math_option = self.math_option;
			//let base_option = self.base_option;
			//let angle_option = self.angle_option;
			//let machine_option = self.machine_option;
			//program.unwrap().get_options(self, float_function.0.clone(), float_function.1);
			self.execute_function_declaration(statement)?;
			//self.math_option = math_option;
			//self.base_option = base_option;
			//self.angle_option = angle_option;
			//self.machine_option = machine_option;
		}
		// If the user has defined an array, read from it.
		if *has_parentheses && self.int_arrays.contains_key(name) {
			let mut indices = Vec::new();
			for argument in arguments {
				indices.push(self.execute_any_type_expression(argument, program)?.to_int().map_err(|err| err.at_column(argument.get_start_column()))?);
			}
			let element = self.int_arrays.get(name).unwrap()
				.read_element(&indices, self.options.allow_uninitialized_read())
				.map_err(|err| err.at_column(*start_column))?;
			return Ok(element);
		}
		// If a function with the argument count is defined, get it
		if let Some((function_parameters, function_expression, function_location)) = self.int_functions.get(&(name.clone(), arguments.len())).cloned() {
			// Push GOSUB level for function execution
			let mut gosub_level_to_push = GosubLevel::new();
			for (argument_index, function_parameter) in function_parameters.iter().enumerate() {
				let argument = &arguments[argument_index];
				match function_parameter {
					AnyTypeLValue::Float(l_value) => {
						let argument_value = self.execute_any_type_expression(argument, program)?.to_float().map_err(|err| err.at_column(l_value.start_column))?;
						gosub_level_to_push.local_float_variables.insert(l_value.name.clone(), argument_value);
					}
					AnyTypeLValue::Int(l_value) => {
						let argument_value = self.execute_any_type_expression(argument, program)?.to_int().map_err(|err| err.at_column(l_value.start_column))?;
						gosub_level_to_push.local_int_variables.insert(l_value.name.clone(), argument_value);
					}
					AnyTypeLValue::Complex(l_value) => {
						let argument_value = self.execute_any_type_expression(argument, program)?.to_complex().map_err(|err| err.at_column(l_value.start_column))?;
						gosub_level_to_push.local_complex_variables.insert(l_value.name.clone(), argument_value);
					}
					AnyTypeLValue::String(l_value) => {
						let argument_value = self.execute_any_type_expression(argument, program)?.to_string().map_err(|err| err.at_column(l_value.start_column))?;
						gosub_level_to_push.local_string_variables.insert(l_value.name.clone(), argument_value);
					}
				}
			}
			self.gosub_stack.push(gosub_level_to_push);
			let line_executing = self.line_executing.clone();
			let sub_line_executing = self.sub_line_executing;
			let options_before_function_execution = self.options.clone();
			if let Some(function_location) = function_location {
				self.line_executing = Some(function_location.0);
				self.sub_line_executing = Some(function_location.1);
			}
			if let Some(program) = program {
				self.options = program.get_options(&self.line_executing.clone().unwrap(), self.sub_line_executing.unwrap());
			}
			// Execute
			let result = self.execute_int_expression(&function_expression, program)?;
			// Pop
			self.gosub_stack.pop();
			self.line_executing = line_executing;
			self.sub_line_executing = sub_line_executing;
			self.options = options_before_function_execution;
			return Ok(result);
		}
		// If it is a local user defined variable that has been defined, get it
		if !*has_parentheses && let Some(variable) = self.gosub_stack.last().unwrap().local_int_variables.get(name) {
			return Ok(variable.clone());
		}
		// If it is a user defined variable that has been defined, get it
		if !*has_parentheses && let Some(variable) = self.int_variables.get(name) {
			return Ok(variable.clone());
		}
		// Else try to execute a supplied (built-in) function
		if let Some(supplied_function) = supplied_function {
			match (supplied_function, arguments) {
				// SQR%(X)
				(SuppliedFunction::Sqr, arguments) if arguments.len() == 1 => {
					let argument = &arguments[0];
					return match self.execute_any_type_expression(&arguments[0], program)?.to_int().map_err(|error| error.at_column(argument.get_start_column()))? {
						result if result.is_negative() => Err(ErrorVariant::IntSquareRootOfNegativeNumber.at_column(*start_column)),
						result => Ok(IntValue::new(Rc::new(result.value.sqrt())))
					};
				}
				// ABS%(X)
				(SuppliedFunction::Abs, arguments) if arguments.len() == 1 => {
					let argument = &arguments[0];
					return Ok(IntValue::new(Rc::new(
						self.execute_any_type_expression(argument, program)?.to_int().map_err(|error| error.at_column(argument.get_start_column()))?.value.abs()
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
						let value = self.execute_any_type_expression(argument, program)?;
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
					return Ok(IntValue::from_usize(self.execute_any_type_expression(argument, program)?
						.to_string().map_err(|error| error.at_column(argument.get_start_column()))?.count_chars()))
				}
				// SGN%(X)
				(SuppliedFunction::Sgn, arguments) if arguments.len() == 1 =>
					return Ok(IntValue::new(Rc::new({
						let argument = &arguments[0];
						let value = self.execute_any_type_expression(argument, program)?;
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
		//
		if *has_parentheses {
			return Err(ErrorVariant::ArrayOrFunctionNotDefined.at_column(*start_column));
		}
		// Else
		match self.options.allow_uninitialized_read() {
			true => Ok(IntValue::zero()),
			false => Err(ErrorVariant::VariableReadUninitialized.at_column(*start_column)),
		}
	}

	/// Reads a float variable or from a float array or executes a function that returns a float.
	fn execute_float_l_value_read(&mut self, l_value: &FloatLValue, program: Option<&Program>) -> Result<FloatValue, Error> {
		// Unpack
		let FloatLValue { name, arguments, has_parentheses, start_column, supplied_function } = l_value;
		// Create the array if it is not yet created
		if *has_parentheses && !self.float_arrays.contains_key(name) && !self.options.arrays_created_on_dim_execution() &&
			program.is_some() && let Some(float_array_declarations) = program.unwrap().float_array_declarations.get(name)
		{
			if float_array_declarations.len() > 1 {
				return Err(ErrorVariant::MultipleDeclarationsOfArray.at_column(l_value.start_column));
			}
			let array_declarations_location = float_array_declarations.iter().next().unwrap();
			let statement = &program.unwrap().get_line(&array_declarations_location.0).unwrap().optimized_statements[array_declarations_location.1];
			let options_before_function_execution = self.options.clone();
			self.options = program.unwrap().get_options(&array_declarations_location.0.clone(), array_declarations_location.1);
			self.execute_array_declaration(statement, program.unwrap(), &name, IdentifierType::UnmarkedOrFloat)?;
			self.options = options_before_function_execution;
		}
		// Define function if it is not yet defined
		if !self.float_functions.contains_key(&(name.clone(), arguments.len())) && !self.options.functions_defined_on_fn_execution() &&
			program.is_some() && let Some(float_functions) = program.unwrap().float_functions.get(&(name.clone(), arguments.len()))
		{
			if float_functions.len() > 1 {
				return Err(ErrorVariant::MultipleDeclarationsOfFunction.at_column(l_value.start_column));
			}
			if !arguments.is_empty() && self.float_arrays.contains_key(name) {
				return Err(ErrorVariant::MultipleDeclarationsOfFunctionAndArray.at_column(l_value.start_column));
			}
			let float_function = float_functions.iter().next().unwrap();
			let statement = &program.unwrap().get_line(&float_function.0).unwrap().optimized_statements[float_function.1];
			//let math_option = self.math_option;
			//let base_option = self.base_option;
			//let angle_option = self.angle_option;
			//let machine_option = self.machine_option;
			//program.unwrap().get_options(self, float_function.0.clone(), float_function.1);
			self.execute_function_declaration(statement)?;
			//self.math_option = math_option;
			//self.base_option = base_option;
			//self.angle_option = angle_option;
			//self.machine_option = machine_option;
		}
		// If the user has defined an array, read from it.
		if *has_parentheses && self.float_arrays.contains_key(name) {
			let mut indices = Vec::new();
			for argument in arguments {
				indices.push(self.execute_any_type_expression(argument, program)?.to_int().map_err(|err| err.at_column(argument.get_start_column()))?);
			}
			let element = self.float_arrays.get(name).unwrap()
				.read_element(&indices, self.options.allow_uninitialized_read())
				.map_err(|err| err.at_column(*start_column))?;
			return Ok(element);
		}
		// If a function with the argument count is defined, get it
		if let Some((function_parameters, function_expression, function_location)) = self.float_functions.get(&(name.clone(), arguments.len())).cloned() {
			// Push GOSUB level for function execution
			let mut gosub_level_to_push = GosubLevel::new();
			for (argument_index, function_parameter) in function_parameters.iter().enumerate() {
				let argument = &arguments[argument_index];
				match function_parameter {
					AnyTypeLValue::Float(l_value) => {
						let argument_value = self.execute_any_type_expression(argument, program)?.to_float().map_err(|err| err.at_column(l_value.start_column))?;
						gosub_level_to_push.local_float_variables.insert(l_value.name.clone(), argument_value);
					}
					AnyTypeLValue::Int(l_value) => {
						let argument_value = self.execute_any_type_expression(argument, program)?.to_int().map_err(|err| err.at_column(l_value.start_column))?;
						gosub_level_to_push.local_int_variables.insert(l_value.name.clone(), argument_value);
					}
					AnyTypeLValue::Complex(l_value) => {
						let argument_value = self.execute_any_type_expression(argument, program)?.to_complex().map_err(|err| err.at_column(l_value.start_column))?;
						gosub_level_to_push.local_complex_variables.insert(l_value.name.clone(), argument_value);
					}
					AnyTypeLValue::String(l_value) => {
						let argument_value = self.execute_any_type_expression(argument, program)?.to_string().map_err(|err| err.at_column(l_value.start_column))?;
						gosub_level_to_push.local_string_variables.insert(l_value.name.clone(), argument_value);
					}
				}
			}
			self.gosub_stack.push(gosub_level_to_push);
			let line_executing = self.line_executing.clone();
			let sub_line_executing = self.sub_line_executing;
			let options_before_function_execution = self.options.clone();
			if let Some(function_location) = function_location {
				self.line_executing = Some(function_location.0);
				self.sub_line_executing = Some(function_location.1);
			}
			if let Some(program) = program {
				self.options = program.get_options(&self.line_executing.clone().unwrap(), self.sub_line_executing.unwrap());
			}
			// Execute
			let result = self.execute_float_expression(&function_expression, program)?;
			// Pop
			self.gosub_stack.pop();
			self.line_executing = line_executing;
			self.sub_line_executing = sub_line_executing;
			self.options = options_before_function_execution;
			return Ok(result);
		}
		// If it is a local user defined variable that has been defined, get it
		if !*has_parentheses && let Some(variable) = self.gosub_stack.last().unwrap().local_float_variables.get(name) {
			return Ok(variable.clone());
		}
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
					return self.execute_any_type_expression(argument, program)?.to_complex().map_err(|error| error.at_column(argument.get_start_column()))?
						.abs(self.options.allow_overflow()).map_err(|error| error.at_column(argument.get_start_column()));
				}
				// Functions that have one float argument
				SuppliedFunction::Sqr | SuppliedFunction::Abs | SuppliedFunction::Int | SuppliedFunction::Sgn | SuppliedFunction::Log | SuppliedFunction::Exp |
				SuppliedFunction::Sin | SuppliedFunction::Cos | SuppliedFunction::Tan | SuppliedFunction::Cot | SuppliedFunction::Sec | SuppliedFunction::Csc |
				SuppliedFunction::Asin | SuppliedFunction::Acos | SuppliedFunction::Atan | SuppliedFunction::Acot | SuppliedFunction::Asec | SuppliedFunction::Acsc if arguments.len() == 1 => {
					let argument_expression = &arguments[0];
					let argument_value = self.execute_any_type_expression(argument_expression, program)?
						.to_float().map_err(|error| error.at_column(argument_expression.get_start_column()))?;
					return Ok(match supplied_function {
						SuppliedFunction::Sqr =>
							argument_value.sqrt(self.options.allow_real_square_root_of_negative()).map_err(|error| error.at_column(argument_expression.get_start_column()))?,
						SuppliedFunction::Abs => argument_value.abs(),
						SuppliedFunction::Int => argument_value.floor(),
						SuppliedFunction::Sgn => argument_value.signum(),
						SuppliedFunction::Sin =>
							argument_value.sin(self.options.get_angle_option(), self.options.allow_overflow()).map_err(|error| error.at_column(argument_expression.get_start_column()))?,
						SuppliedFunction::Cos =>
							argument_value.cos(self.options.get_angle_option(), self.options.allow_overflow()).map_err(|error| error.at_column(argument_expression.get_start_column()))?,
						SuppliedFunction::Tan =>
							argument_value.tan(self.options.get_angle_option(), self.options.allow_overflow()).map_err(|error| error.at_column(argument_expression.get_start_column()))?,
						SuppliedFunction::Cot =>
							argument_value.cot(self.options.get_angle_option(), self.options.allow_overflow()).map_err(|error| error.at_column(argument_expression.get_start_column()))?,
						SuppliedFunction::Sec =>
							argument_value.sec(self.options.get_angle_option(), self.options.allow_overflow()).map_err(|error| error.at_column(argument_expression.get_start_column()))?,
						SuppliedFunction::Csc =>
							argument_value.csc(self.options.get_angle_option(), self.options.allow_overflow()).map_err(|error| error.at_column(argument_expression.get_start_column()))?,
						SuppliedFunction::Asin => argument_value.asin(self.options.get_angle_option(), self.options.allow_real_trig_out_of_range())
							.map_err(|error| error.at_column(argument_expression.get_start_column()))?,
						SuppliedFunction::Acos => argument_value.acos(self.options.get_angle_option(), self.options.allow_real_trig_out_of_range())
							.map_err(|error| error.at_column(argument_expression.get_start_column()))?,
						SuppliedFunction::Atan =>
							argument_value.atan(self.options.get_angle_option(), self.options.allow_overflow()).map_err(|error| error.at_column(argument_expression.get_start_column()))?,
						SuppliedFunction::Acot =>
							argument_value.acot(self.options.get_angle_option(), self.options.allow_overflow()).map_err(|error| error.at_column(argument_expression.get_start_column()))?,
						SuppliedFunction::Asec => argument_value.asec(self.options.get_angle_option(), self.options.allow_real_trig_out_of_range())
							.map_err(|error| error.at_column(argument_expression.get_start_column()))?,
						SuppliedFunction::Acsc => argument_value.acsc(self.options.get_angle_option(), self.options.allow_real_trig_out_of_range())
							.map_err(|error| error.at_column(argument_expression.get_start_column()))?,
						SuppliedFunction::Log =>
							argument_value.ln(self.options.allow_real_log_of_non_positive()).map_err(|error| error.at_column(argument_expression.get_start_column()))?,
						SuppliedFunction::Exp => argument_value.exp(self.options.allow_overflow()).map_err(|error| error.at_column(argument_expression.get_start_column()))?,
						_ => unreachable!()
					})
				}
				// Functions that have one complex argument
				SuppliedFunction::Real | SuppliedFunction::Imag if arguments.len() == 1 => {
					let argument_expression = &arguments[0];
					let argument_value = self.execute_any_type_expression(argument_expression, program)?
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
					return Ok(FloatValue::from_usize(self.execute_any_type_expression(argument, program)?
						.to_string().map_err(|error| error.at_column(argument.get_start_column()))?.count_chars()))
				}
				// RND(X)
				SuppliedFunction::Rnd if arguments.len() < 2 => {
					// If the function has an argument
					if arguments.len() == 1 {
						// Execute the argument and cast to a float
						let argument_expression = &arguments[0];
						let argument_value = self.execute_any_type_expression(argument_expression, program)?.to_float()
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
		//
		if *has_parentheses {
			return Err(ErrorVariant::ArrayOrFunctionNotDefined.at_column(*start_column));
		}
		// Else
		match self.options.allow_uninitialized_read() {
			true => Ok(FloatValue::ZERO),
			false => Err(ErrorVariant::VariableReadUninitialized.at_column(*start_column)),
		}
	}

	/// Reads a complex variable or from a complex array or executes a function that returns a complex.
	fn execute_complex_l_value_read(&mut self, l_value: &ComplexLValue, program: Option<&Program>) -> Result<ComplexValue, Error> {
		// Unpack
		let ComplexLValue { name, arguments/*, uses_fn_keyword*/, has_parentheses, start_column, supplied_function } = l_value;
		// Create the array if it is not yet created
		if *has_parentheses && !self.complex_arrays.contains_key(name) && !self.options.arrays_created_on_dim_execution() &&
			program.is_some() && let Some(complex_array_declarations) = program.unwrap().complex_array_declarations.get(name)
		{
			if complex_array_declarations.len() > 1 {
				return Err(ErrorVariant::MultipleDeclarationsOfArray.at_column(l_value.start_column));
			}
			let array_declarations_location = complex_array_declarations.iter().next().unwrap();
			let statement = &program.unwrap().get_line(&array_declarations_location.0).unwrap().optimized_statements[array_declarations_location.1];
			let options_before_array_execution = self.options.clone();
			self.options = program.unwrap().get_options(&array_declarations_location.0.clone(), array_declarations_location.1);
			self.execute_array_declaration(statement, program.unwrap(), &name, IdentifierType::ComplexNumber)?;
			self.options = options_before_array_execution;
		}
		// Define function if it is not yet defined
		if !self.complex_functions.contains_key(&(name.clone(), arguments.len())) && !self.options.functions_defined_on_fn_execution() &&
			program.is_some() && let Some(complex_functions) = program.unwrap().complex_functions.get(&(name.clone(), arguments.len()))
		{
			if complex_functions.len() > 1 {
				return Err(ErrorVariant::MultipleDeclarationsOfFunction.at_column(l_value.start_column));
			}
			if !arguments.is_empty() && self.complex_arrays.contains_key(name) {
				return Err(ErrorVariant::MultipleDeclarationsOfFunctionAndArray.at_column(l_value.start_column));
			}
			let complex_function = complex_functions.iter().next().unwrap();
			let statement = &program.unwrap().get_line(&complex_function.0).unwrap().optimized_statements[complex_function.1];
			//let math_option = self.math_option;
			//let base_option = self.base_option;
			//let angle_option = self.angle_option;
			//let machine_option = self.machine_option;
			//program.unwrap().get_options(self, float_function.0.clone(), float_function.1);
			self.execute_function_declaration(statement)?;
			//self.math_option = math_option;
			//self.base_option = base_option;
			//self.angle_option = angle_option;
			//self.machine_option = machine_option;
		}
		// If the user has defined an array, read from it.
		if *has_parentheses && self.complex_arrays.contains_key(name) {
			let mut indices = Vec::new();
			for argument in arguments {
				indices.push(self.execute_any_type_expression(argument, program)?.to_int().map_err(|err| err.at_column(argument.get_start_column()))?);
			}
			let element = self.complex_arrays.get(name).unwrap()
				.read_element(&indices, self.options.allow_uninitialized_read())
				.map_err(|err| err.at_column(*start_column))?;
			return Ok(element);
		}
		// If a function with the argument count is defined, get it
		if let Some((function_parameters, function_expression, function_location)) = self.complex_functions.get(&(name.clone(), arguments.len())).cloned() {
			// Push GOSUB level for function execution
			let mut gosub_level_to_push = GosubLevel::new();
			for (argument_index, function_parameter) in function_parameters.iter().enumerate() {
				let argument = &arguments[argument_index];
				match function_parameter {
					AnyTypeLValue::Float(l_value) => {
						let argument_value = self.execute_any_type_expression(argument, program)?.to_float().map_err(|err| err.at_column(l_value.start_column))?;
						gosub_level_to_push.local_float_variables.insert(l_value.name.clone(), argument_value);
					}
					AnyTypeLValue::Int(l_value) => {
						let argument_value = self.execute_any_type_expression(argument, program)?.to_int().map_err(|err| err.at_column(l_value.start_column))?;
						gosub_level_to_push.local_int_variables.insert(l_value.name.clone(), argument_value);
					}
					AnyTypeLValue::Complex(l_value) => {
						let argument_value = self.execute_any_type_expression(argument, program)?.to_complex().map_err(|err| err.at_column(l_value.start_column))?;
						gosub_level_to_push.local_complex_variables.insert(l_value.name.clone(), argument_value);
					}
					AnyTypeLValue::String(l_value) => {
						let argument_value = self.execute_any_type_expression(argument, program)?.to_string().map_err(|err| err.at_column(l_value.start_column))?;
						gosub_level_to_push.local_string_variables.insert(l_value.name.clone(), argument_value);
					}
				}
			}
			self.gosub_stack.push(gosub_level_to_push);
			let line_executing = self.line_executing.clone();
			let sub_line_executing = self.sub_line_executing;
			let options_before_function_execution = self.options.clone();
			if let Some(function_location) = function_location {
				self.line_executing = Some(function_location.0);
				self.sub_line_executing = Some(function_location.1);
			}
			if let Some(program) = program {
				self.options = program.get_options(&self.line_executing.clone().unwrap(), self.sub_line_executing.unwrap());
			}
			// Execute
			let result = self.execute_complex_expression(&function_expression, program)?;
			// Pop
			self.gosub_stack.pop();
			self.line_executing = line_executing;
			self.sub_line_executing = sub_line_executing;
			self.options = options_before_function_execution;
			return Ok(result);
		}
		// If it is a local user defined variable that has been defined, get it
		if !*has_parentheses && let Some(variable) = self.gosub_stack.last().unwrap().local_complex_variables.get(name) {
			return Ok(variable.clone());
		}
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
					let argument_value = self.execute_any_type_expression(argument_expression, program)?.to_complex().map_err(|error| error.at_column(argument_expression.get_start_column()))?;
					return Ok(match supplied_function {
						SuppliedFunction::Sqr => argument_value.sqrt(self.options.allow_overflow()).map_err(|error| error.at_column(argument_expression.get_start_column()))?,
						_ => unreachable!(),
					})
				}
				_ => {}
			}
		}
		//
		if *has_parentheses {
			return Err(ErrorVariant::ArrayOrFunctionNotDefined.at_column(*start_column));
		}
		// Else
		match self.options.allow_uninitialized_read() {
			true => Ok(ComplexValue::ZERO),
			false => Err(ErrorVariant::VariableReadUninitialized.at_column(*start_column)),
		}
	}

	/// Reads a string variable or from a string array or executes a function that returns a string.
	fn execute_string_l_value_read(&mut self, l_value: &StringLValue, program: Option<&Program>) -> Result<StringValue, Error> {
		// Unpack
		let StringLValue { name, arguments/*, uses_fn_keyword*/, has_parentheses, start_column, supplied_function } = l_value;
		// Create the array if it is not yet created
		if *has_parentheses && !self.string_arrays.contains_key(name) && !self.options.arrays_created_on_dim_execution() &&
			program.is_some() && let Some(string_array_declarations) = program.unwrap().string_array_declarations.get(name)
		{
			if string_array_declarations.len() > 1 {
				return Err(ErrorVariant::MultipleDeclarationsOfArray.at_column(l_value.start_column));
			}
			let array_declarations_location = string_array_declarations.iter().next().unwrap();
			let statement = &program.unwrap().get_line(&array_declarations_location.0).unwrap().optimized_statements[array_declarations_location.1];
			let options_before_array_execution = self.options.clone();
			self.options = program.unwrap().get_options(&array_declarations_location.0, array_declarations_location.1);
			self.execute_array_declaration(statement, program.unwrap(), &name, IdentifierType::String)?;
			self.options = options_before_array_execution;
		}
		// Define function if it is not yet defined
		if !self.string_functions.contains_key(&(name.clone(), arguments.len())) && !self.options.functions_defined_on_fn_execution() &&
			program.is_some() && let Some(string_functions) = program.unwrap().string_functions.get(&(name.clone(), arguments.len()))
		{
			if string_functions.len() > 1 {
				return Err(ErrorVariant::MultipleDeclarationsOfFunction.at_column(l_value.start_column));
			}
			if !arguments.is_empty() && self.string_arrays.contains_key(name) {
				return Err(ErrorVariant::MultipleDeclarationsOfFunctionAndArray.at_column(l_value.start_column));
			}
			let string_function = string_functions.iter().next().unwrap();
			let statement = &program.unwrap().get_line(&string_function.0).unwrap().optimized_statements[string_function.1];
			//let math_option = self.math_option;
			//let base_option = self.base_option;
			//let angle_option = self.angle_option;
			//let machine_option = self.machine_option;
			//program.unwrap().get_options(self, float_function.0.clone(), float_function.1);
			self.execute_function_declaration(statement)?;
			//self.math_option = math_option;
			//self.base_option = base_option;
			//self.angle_option = angle_option;
			//self.machine_option = machine_option;
		}
		// If the user has defined an array, read from it.
		if *has_parentheses && self.string_arrays.contains_key(name) {
			let mut indices = Vec::new();
			for argument in arguments {
				indices.push(self.execute_any_type_expression(argument, program)?.to_int().map_err(|err| err.at_column(argument.get_start_column()))?);
			}
			let element = self.string_arrays.get(name).unwrap()
				.read_element(&indices, self.options.allow_uninitialized_read())
				.map_err(|err| err.at_column(*start_column))?;
			return Ok(element);
		}
		// If a function with the argument count is defined, get it
		if let Some((function_parameters, function_expression, function_location)) = self.string_functions.get(&(name.clone(), arguments.len())).cloned() {
			// Push GOSUB level for function execution
			let mut gosub_level_to_push = GosubLevel::new();
			for (argument_index, function_parameter) in function_parameters.iter().enumerate() {
				let argument = &arguments[argument_index];
				match function_parameter {
					AnyTypeLValue::Float(l_value) => {
						let argument_value = self.execute_any_type_expression(argument, program)?.to_float().map_err(|err| err.at_column(l_value.start_column))?;
						gosub_level_to_push.local_float_variables.insert(l_value.name.clone(), argument_value);
					}
					AnyTypeLValue::Int(l_value) => {
						let argument_value = self.execute_any_type_expression(argument, program)?.to_int().map_err(|err| err.at_column(l_value.start_column))?;
						gosub_level_to_push.local_int_variables.insert(l_value.name.clone(), argument_value);
					}
					AnyTypeLValue::Complex(l_value) => {
						let argument_value = self.execute_any_type_expression(argument, program)?.to_complex().map_err(|err| err.at_column(l_value.start_column))?;
						gosub_level_to_push.local_complex_variables.insert(l_value.name.clone(), argument_value);
					}
					AnyTypeLValue::String(l_value) => {
						let argument_value = self.execute_any_type_expression(argument, program)?.to_string().map_err(|err| err.at_column(l_value.start_column))?;
						gosub_level_to_push.local_string_variables.insert(l_value.name.clone(), argument_value);
					}
				}
			}
			self.gosub_stack.push(gosub_level_to_push);
			let line_executing = self.line_executing.clone();
			let sub_line_executing = self.sub_line_executing;
			let options_before_function_execution = self.options.clone();
			if let Some(function_location) = function_location {
				self.line_executing = Some(function_location.0);
				self.sub_line_executing = Some(function_location.1);
			}
			if let Some(program) = program {
				self.options = program.get_options(&self.line_executing.clone().unwrap(), self.sub_line_executing.unwrap());
			}
			// Execute
			let result = self.execute_string_expression(&function_expression, program)?;
			// Pop
			self.gosub_stack.pop();
			self.line_executing = line_executing;
			self.sub_line_executing = sub_line_executing;
			self.options = options_before_function_execution;
			return Ok(result);
		}
		// If it is a local user defined variable that has been defined, get it
		if !*has_parentheses && let Some(variable) = self.gosub_stack.last().unwrap().local_string_variables.get(name) {
			return Ok(variable.clone());
		}
		// If it is a user defined variable that has been defined, get it
		if !*has_parentheses && let Some(variable) = self.string_variables.get(name) {
			return Ok(variable.clone());
		}
		// Else try to execute a supplied (built-in) function
		if let Some(supplied_function) = supplied_function {
			match (supplied_function, arguments) {
				_ => {}
			}
		}
		//
		if *has_parentheses {
			return Err(ErrorVariant::ArrayOrFunctionNotDefined.at_column(*start_column));
		}
		// Else
		match self.options.allow_uninitialized_read() {
			true => Ok(StringValue::empty()),
			false => Err(ErrorVariant::VariableReadUninitialized.at_column(*start_column)),
		}
	}

	/// Writes to a integer variable or to an integer array.
	fn execute_int_l_value_write(&mut self, l_value: &IntLValue, value: IntValue, program: Option<&Program>) -> Result<(), Error> {
		// Unpack
		let IntLValue { name, arguments, has_parentheses, start_column, .. } = l_value;
		// Create the array if it is not yet created
		if *has_parentheses && !self.int_arrays.contains_key(name) && !self.options.arrays_created_on_dim_execution() &&
			program.is_some() && let Some(int_array_declarations) = program.unwrap().int_array_declarations.get(name)
		{
			if int_array_declarations.len() > 1 {
				return Err(ErrorVariant::MultipleDeclarationsOfArray.at_column(l_value.start_column));
			}
			let array_declarations_location = int_array_declarations.iter().next().unwrap();
			let statement = &program.unwrap().get_line(&array_declarations_location.0).unwrap().optimized_statements[array_declarations_location.1];
			let options_before_array_execution = self.options.clone();
			self.options = program.unwrap().get_options(&array_declarations_location.0.clone(), array_declarations_location.1);
			self.execute_array_declaration(statement, program.unwrap(), &name, IdentifierType::Integer)?;
			self.options = options_before_array_execution;
		}
		// If the user has defined an array, write to it.
		if *has_parentheses && self.int_arrays.contains_key(name) {
			let mut indices = Vec::new();
			for argument in arguments {
				indices.push(self.execute_any_type_expression(argument, program)?.to_int().map_err(|err| err.at_column(argument.get_start_column()))?);
			}
			self.int_arrays.get_mut(name).unwrap()
				.write_element(&indices, value)
				.map_err(|err| err.at_column(*start_column))?;
			return Ok(());
		}
		// TODO
		if !arguments.is_empty() || *has_parentheses {
			return Err(ErrorVariant::NotYetImplemented("User defined functions".into()).at_column(*start_column));
		}
		// Assign to global variable
		self.int_variables.insert(name.clone(), value);
		// Return
		Ok(())
	}

	/// Writes to a float variable or to an float array.
	fn execute_float_l_value_write(&mut self, l_value: &FloatLValue, value: FloatValue, program: Option<&Program>) -> Result<(), Error> {
		// Unpack
		let FloatLValue { name, arguments, has_parentheses, start_column, .. } = l_value;
		// Create the array if it is not yet created
		if *has_parentheses && !self.float_arrays.contains_key(name) && !self.options.arrays_created_on_dim_execution() &&
			program.is_some() && let Some(float_array_declarations) = program.unwrap().float_array_declarations.get(name)
		{
			if float_array_declarations.len() > 1 {
				return Err(ErrorVariant::MultipleDeclarationsOfArray.at_column(l_value.start_column));
			}
			let array_declarations_location = float_array_declarations.iter().next().unwrap();
			let statement = &program.unwrap().get_line(&array_declarations_location.0).unwrap().optimized_statements[array_declarations_location.1];
			let options_before_array_execution = self.options.clone();
			self.options = program.unwrap().get_options(&array_declarations_location.0.clone(), array_declarations_location.1);
			self.execute_array_declaration(statement, program.unwrap(), &name, IdentifierType::UnmarkedOrFloat)?;
			self.options = options_before_array_execution;
		}
		// If the user has defined an array, write to it.
		if *has_parentheses && self.float_arrays.contains_key(name) {
			let mut indices = Vec::new();
			for argument in arguments {
				indices.push(self.execute_any_type_expression(argument, program)?.to_int().map_err(|err| err.at_column(argument.get_start_column()))?);
			}
			self.float_arrays.get_mut(name).unwrap()
				.write_element(&indices, value)
				.map_err(|err| err.at_column(*start_column))?;
			return Ok(());
		}
		// TODO
		if !arguments.is_empty() || *has_parentheses {
			return Err(ErrorVariant::NotYetImplemented("User defined functions".into()).at_column(*start_column));
		}
		// Assign to global variable
		self.float_variables.insert(name.clone(), value);
		// Return
		Ok(())
	}

	/// Writes to a complex variable or to an complex array.
	fn execute_complex_l_value_write(&mut self, l_value: &ComplexLValue, value: ComplexValue, program: Option<&Program>) -> Result<(), Error> {
		// Unpack
		let ComplexLValue { name, arguments, has_parentheses, start_column, .. } = l_value;
		// Create the array if it is not yet created
		if *has_parentheses && !self.complex_arrays.contains_key(name) && !self.options.arrays_created_on_dim_execution() &&
			program.is_some() && let Some(complex_array_declarations) = program.unwrap().complex_array_declarations.get(name)
		{
			if complex_array_declarations.len() > 1 {
				return Err(ErrorVariant::MultipleDeclarationsOfArray.at_column(l_value.start_column));
			}
			let array_declarations_location = complex_array_declarations.iter().next().unwrap();
			let statement = &program.unwrap().get_line(&array_declarations_location.0).unwrap().optimized_statements[array_declarations_location.1];
			let options_before_function_execution = self.options.clone();
			self.options = program.unwrap().get_options(&array_declarations_location.0.clone(), array_declarations_location.1);
			self.execute_array_declaration(statement, program.unwrap(), &name, IdentifierType::ComplexNumber)?;
			self.options = options_before_function_execution;
		}
		// If the user has defined an array, write to it.
		if *has_parentheses && self.complex_arrays.contains_key(name) {
			let mut indices = Vec::new();
			for argument in arguments {
				indices.push(self.execute_any_type_expression(argument, program)?.to_int().map_err(|err| err.at_column(argument.get_start_column()))?);
			}
			self.complex_arrays.get_mut(name).unwrap()
				.write_element(&indices, value)
				.map_err(|err| err.at_column(*start_column))?;
			return Ok(());
		}
		// TODO
		if !arguments.is_empty() || *has_parentheses {
			return Err(ErrorVariant::NotYetImplemented("User defined functions".into()).at_column(*start_column));
		}
		// Assign to global variable
		self.complex_variables.insert(name.clone(), value);
		// Return
		Ok(())
	}

	/// Writes to a string variable or to an string array.
	fn execute_string_l_value_write(&mut self, l_value: &StringLValue, value: StringValue, program: Option<&Program>) -> Result<(), Error> {
		// Unpack
		let StringLValue { name, arguments, has_parentheses, start_column, .. } = l_value;
		// Create the array if it is not yet created
		if *has_parentheses && !self.string_arrays.contains_key(name) && !self.options.arrays_created_on_dim_execution() &&
			program.is_some() && let Some(string_array_declarations) = program.unwrap().string_array_declarations.get(name)
		{
			if string_array_declarations.len() > 1 {
				return Err(ErrorVariant::MultipleDeclarationsOfArray.at_column(l_value.start_column));
			}
			let array_declarations_location = string_array_declarations.iter().next().unwrap();
			let statement = &program.unwrap().get_line(&array_declarations_location.0).unwrap().optimized_statements[array_declarations_location.1];
			let options_before_array_execution = self.options.clone();
			self.options = program.unwrap().get_options(&array_declarations_location.0.clone(), array_declarations_location.1);
			self.execute_array_declaration(statement, program.unwrap(), &name, IdentifierType::String)?;
			self.options = options_before_array_execution;
		}
		// If the user has defined an array, write to it.
		if *has_parentheses && self.string_arrays.contains_key(name) {
			let mut indices = Vec::new();
			for argument in arguments {
				indices.push(self.execute_any_type_expression(argument, program)?.to_int().map_err(|err| err.at_column(argument.get_start_column()))?);
			}
			self.string_arrays.get_mut(name).unwrap()
				.write_element(&indices, value)
				.map_err(|err| err.at_column(*start_column))?;
			return Ok(());
		}
		// TODO
		if !arguments.is_empty() || *has_parentheses {
			return Err(ErrorVariant::NotYetImplemented("User defined functions".into()).at_column(*start_column));
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

struct GosubLevel {
	/// A list of active loops and multi-line program structures such as the not yet implemented if blocks.
	pub block_stack: Vec<BlockOnStack>,
	/// Maps a for loop variable name and if it is a float variable to an index into the block stack.
	pub for_loop_variable_to_block_stack_index: HashMap<(Box<str>, bool), usize>,
	/// The line of the GOSUB statement that put this GOSUB level on the stack.
	pub return_line: Option<Rc<BigInt>>,
	/// The sub-line of the GOSUB statement that put this GOSUB level on the stack.
	pub return_sub_line: usize,
	// Variables
	pub local_float_variables: HashMap<Box<str>, FloatValue>,
	pub local_complex_variables: HashMap<Box<str>, ComplexValue>,
	pub local_int_variables: HashMap<Box<str>, IntValue>,
	pub local_string_variables: HashMap<Box<str>, StringValue>,
}

impl GosubLevel {
	pub fn new() -> Self {
		Self {
			block_stack: Vec::new(),
			for_loop_variable_to_block_stack_index: HashMap::new(),
			return_line: None,
			return_sub_line: 0,
			local_complex_variables: HashMap::new(),
			local_float_variables: HashMap::new(),
			local_int_variables: HashMap::new(),
			local_string_variables: HashMap::new(),
		}
	}

	pub fn from_return_location(return_line: Option<Rc<BigInt>>, return_sub_line: usize) -> Self {
		Self {
			block_stack: Vec::new(),
			for_loop_variable_to_block_stack_index: HashMap::new(),
			return_line,
			return_sub_line,
			local_complex_variables: HashMap::new(),
			local_float_variables: HashMap::new(),
			local_int_variables: HashMap::new(),
			local_string_variables: HashMap::new(),
		}
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
}

/// A BASIC multidimensional array.
struct Array<T: Value> {
	/// A list of dimensions, each dimension has a (dimension length, dimension lower bound) tuple.
	dimension_lengths_starts: Box<[(usize, IntValue)]>,
	/// All the elements in the array, `None` is uninitialized.
	elements: Vec<Option<T>>,
}

impl<T: Value> Array<T> {
	/// Creates a new array with the given dimension lengths and lower bounds.
	pub fn new(dimensions: Box<[(usize, IntValue)]>) -> Result<Self, ErrorVariant> {
		// Get the total amount of elements the array should have
		let mut total_element_count = 1usize;
		for (dimension_length, _) in dimensions.iter() {
			total_element_count = match total_element_count.checked_mul(*dimension_length) {
				Some(total_element_count) => total_element_count,
				None => return Err(ErrorVariant::ArrayTooLarge),
			};
		}
		// Create element list of uninitialized values
		let elements = Vec::from_iter(repeat_n(None, total_element_count));
		// Assemble into object
		Ok(Self {
			dimension_lengths_starts: dimensions,
			elements,
		})
	}

	/// Takes a list of indices and converts them into an index into the `elements` list.
	fn indices_to_elements_index(&self, indices: &[IntValue]) -> Result<usize, ErrorVariant> {
		// The dimension counts must match
		if self.dimension_lengths_starts.len() != indices.len() {
			return Err(ErrorVariant::ArrayDimensionCountMismatch);
		}
		// For each dimension of the array
		let mut elements_index = 0;
		let mut dimension_stride = 1;
		for (dimension, index) in indices.iter().enumerate() {
			let (dimension_length, dimension_start) = &self.dimension_lengths_starts[dimension];
			// Subtract the dimension lower bound from the dimension index to the a 0-indexed index
			let index_zero_indexed = match index.clone().sub(dimension_start.clone()).to_usize() {
				Some(index_usize) => index_usize,
				None => return Err(ErrorVariant::ArrayIndexOutOfBounds),
			};
			// Make sure the dimension index is not out of bounds
			if index_zero_indexed > *dimension_length {
				return Err(ErrorVariant::ArrayIndexOutOfBounds);
			}
			// Adjust the index based of the index and dimension stride
			elements_index += index_zero_indexed * dimension_stride;
			// Calculate the dimension stride for the next dimension
			dimension_stride *= dimension_length;
		}
		Ok(elements_index)
	}

	/// Reads an element from the array at the given indices.
	pub fn read_element(&self, indices: &[IntValue], allow_uninitialized_read: bool) -> Result<T, ErrorVariant> {
		let index = self.indices_to_elements_index(indices)?;
		match (&self.elements[index], allow_uninitialized_read) {
			(Some(element), _) => Ok(element.clone()),
			(None, true) => Ok(T::default()),
			(None, false) => Err(ErrorVariant::ArrayReadUninitialized),
		}
	}

	/// Writes an element to the array at the given indices.
	pub fn write_element(&mut self, indices: &[IntValue], element: T) -> Result<(), ErrorVariant> {
		let index = self.indices_to_elements_index(indices)?;
		self.elements[index] = Some(element);
		Ok(())
	}
}