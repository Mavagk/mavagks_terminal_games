use std::{collections::HashMap, fs::{File, create_dir_all}, io::{BufRead, Cursor, Read, Write, stdin, stdout}, iter::repeat_n, mem::take, num::NonZeroUsize, ops::{RangeFrom, RangeFull, RangeInclusive, RangeToInclusive}, path::{Path, PathBuf}, rc::Rc, str::FromStr};

use chrono::{Datelike, Local, Timelike};
use crossterm::{cursor::{position, MoveTo}, execute, style::{Color, ContentStyle, PrintStyledContent, StyledContent}, terminal::{Clear, ClearType}};
use num::{BigInt, Signed, Zero};
use rand::{random_range, rngs::SmallRng, Rng, SeedableRng};

use crate::mavagk_basic::{abstract_syntax_tree::{AnyTypeExpression, AnyTypeLValue, BoolExpression, ComplexExpression, ComplexLValue, ComplexSuppliedFunction, FloatExpression, FloatLValue, FloatSuppliedFunction, IntExpression, IntLValue, IntSuppliedFunction, OptionVariableAndValue, PrintOperand, Statement, StatementVariant, StringExpression, StringLValue, StringSuppliedFunction}, error::{Error, ErrorVariant, FullError, error_at_column, handle_error}, optimize::optimize_statement, options::Options, parse::{Tokens, get_complex_supplied_functions, get_float_supplied_functions, get_int_supplied_functions, parse_line}, program::{Line, Program}, token::{IdentifierType, Token, TokenVariant, parse_datum_complex, parse_datum_float, parse_datum_int, parse_datum_string}, value::{AnyTypeValue, BoolValue, ComplexValue, FloatValue, IntValue, StringValue, Value}};

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
	// Functions and values stored in variables and arrays
	pub float_stored_values: StoredValues<FloatValue>,
	pub int_stored_values: StoredValues<IntValue>,
	pub complex_stored_values: StoredValues<ComplexValue>,
	pub string_stored_values: StoredValues<StringValue>,
	/// A stack of GOSUB levels containing return addresses and program structure blocks such as active FOR loops. Must always contain at least one level.
	pub gosub_stack: Vec<GosubLevel>,
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
			float_stored_values: StoredValues::new(),
			int_stored_values: StoredValues::new(),
			complex_stored_values: StoredValues::new(),
			string_stored_values: StoredValues::new(),
			gosub_stack: vec![GosubLevel::new()],
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
			float_stored_values: StoredValues::new(),
			int_stored_values: StoredValues::new(),
			complex_stored_values: StoredValues::new(),
			string_stored_values: StoredValues::new(),

			rng: SmallRng::seed_from_u64(0),

			data_line_number_to_read: program.get_first_data_line().cloned(),
			datum_index_in_data_line_to_read: 0,
		}
	}

	/// Called when a line of text is entered into the terminal.
	pub fn line_of_text_entered(&mut self, line_text: Box<str>, program: &mut Program) -> Result<(), FullError> {
		// Parse line
		let (line_number, tokens, error, has_comment) = match Token::tokenize_line(&*line_text) {
			(line_number, Ok(tokens), has_comment) => (line_number, tokens, None, has_comment),
			(line_number, Err(error), has_comment) => (line_number, Box::default(), Some(error), has_comment),
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
				if unoptimized_statements.is_empty() && error.is_none() &&!has_comment {
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
		let Statement { variant, column: statement_keyword_start_column } = &statement;
		match variant {
			StatementVariant::Load(filepath_expression) => {
				// Execute expression
				let filename_value = match filepath_expression {
					Some(filepath_expression) => self.execute_string_expression(filepath_expression, Some(program))?,
					None => return Err(ErrorVariant::Unimplemented("LOAD without arguments".into()).at_column(*statement_keyword_start_column)),
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
					None => return Err(ErrorVariant::Unimplemented("SAVE without arguments".into()).at_column(*statement_keyword_start_column)),
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
			StatementVariant::New => {
				self.clear_machine_state(program);
				program.clear_program();
				self.execution_source = ExecutionSource::ProgramEnded;
				Ok(true)
			}
			StatementVariant::Help(token) => {
				// If nothing was entered after the HELP keyword, print a list of sub-menus. Else get the token variant.
				let token_variant = match token {
					None => return Err(ErrorVariant::NotYetImplemented("HELP with no arguments".into()).at_column(*statement_keyword_start_column)),
					Some(Token { variant, .. }) => variant,
				};
				// Get features that this token refers to.
				let mut _binary_operator = None;
				let mut _unary_operator = None;
				let mut _keyword = None;
				let mut _float_supplied_functions = Default::default();
				let mut _int_supplied_functions = Default::default();
				let mut _complex_supplied_functions = Default::default();
				match token_variant {
					TokenVariant::Datum(..) => unreachable!(),
					TokenVariant::Identifier {
						binary_operator: identifier_binary_operator,
						unary_operator: identifier_unary_operator,
						keyword: identifier_keyword,
						supplied_function, ..
					} => {
						_binary_operator = *identifier_binary_operator;
						_unary_operator = *identifier_unary_operator;
						_keyword = *identifier_keyword;
						if let Some(supplied_function) = supplied_function {
							_float_supplied_functions = get_float_supplied_functions(*supplied_function);
							_int_supplied_functions = get_int_supplied_functions(*supplied_function);
							_complex_supplied_functions = get_complex_supplied_functions(*supplied_function);
						}
					}
					_ => return Err(ErrorVariant::NotYetImplemented("HELP".into()).at_column(*statement_keyword_start_column)),
				};
				// If the token is an identifier
				return Err(ErrorVariant::NotYetImplemented("HELP".into()).at_column(*statement_keyword_start_column));
				//Ok(false)
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
					stdout().flush().unwrap();
					let current_columnar_print_position = position().unwrap().0;
					// Print the sub-expression
					match sub_expression {
						PrintOperand::Expression(expression) => match expression {
							// TAB calls
							AnyTypeExpression::Float(FloatExpression::LValue(FloatLValue { arguments, supplied_function: Some(FloatSuppliedFunction::Tab), .. }))
								if (&**arguments).len() == 1 && !self.float_stored_values.functions.contains_key(&("TAB".into(), 1)) && !self.float_stored_values.arrays.contains_key("TAB") =>
							{
								let argument_expression = &arguments[0];
								let argument_value = self.execute_any_type_expression(argument_expression, Some(program))?
									.to_int().map_err(|error| error.at_column(argument_expression.get_start_column()))?;
								stdout().flush().unwrap();
								let new_columnar_position = argument_value.clone().sub(IntValue::from_usize(self.options.get_columnar_first_position() as usize));
								let y_position = position().unwrap().0;
								let spaces_to_insert = &*new_columnar_position.value - y_position;
								let spaces_to_insert: usize = match (&spaces_to_insert).try_into() {
									Ok(spaces_to_insert) => spaces_to_insert,
									Err(_) => match spaces_to_insert.is_negative() {
										true => 0,
										false => return Err(ErrorVariant::TabArgumentTooLow.at_column(argument_expression.get_start_column())),
									}
								};
								for _ in 0..spaces_to_insert {
									print!(" ")
								}
							}
							// Expressions
							_ => self.execute_any_type_expression(expression, Some(program))?.print(&mut stdout(), true, true, &self.options).unwrap()
						}
						// Semicolons do nothing
						PrintOperand::Semicolon(_sub_expression_column) => {}
						// Commas set the columnar position to the start position of the next print zone.
						PrintOperand::Comma(_sub_expression_column) => {
							let print_zone_width = self.options.get_print_zone_width();
							let new_columnar_print_position = (current_columnar_print_position / print_zone_width as u16 + 1) as u32 * print_zone_width as u32;
							let spaces_to_print = (new_columnar_print_position - current_columnar_print_position as u32) as u16;
							for _ in 0..spaces_to_print {
								print!(" ");
							}
						}
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
						self.execute_any_type_expression(prompt_expression, Some(program))?.print(&mut stdout(), true, true, &self.options).unwrap();
						if self.options.always_print_question_mark_after_input_prompt() {
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
								let (l_value_arguments, l_value_string_slice_bounds) = self.execute_l_value_arguments::<IntValue>(l_value, Some(program))?;
								self.execute_l_value_write(l_value, l_value_arguments, l_value_string_slice_bounds, parsed_value, Some(program))?;
							}
							AnyTypeLValue::Float(l_value) => {
								let parsed_value;
								(parsed_value, input_buffer_left) = match parse_datum_float(input_buffer_left, Some(&self.options)) {
									Ok((Some(parsed_value), input_buffer_left)) => (parsed_value, input_buffer_left),
									Err(_) | Ok((None, _)) => continue 'a,
								};
								let (l_value_arguments, l_value_string_slice_bounds) = self.execute_l_value_arguments::<FloatValue>(l_value, Some(program))?;
								self.execute_l_value_write(l_value, l_value_arguments, l_value_string_slice_bounds, parsed_value, Some(program))?;
							}
							AnyTypeLValue::Complex(l_value) => {
								let parsed_value;
								(parsed_value, input_buffer_left) = match parse_datum_complex(input_buffer_left, Some(&self.options)) {
									Ok((Some(parsed_value), input_buffer_left)) => (parsed_value, input_buffer_left),
									Err(_) | Ok((None, _)) => continue 'a,
								};
								let (l_value_arguments, l_value_string_slice_bounds) = self.execute_l_value_arguments::<ComplexValue>(l_value, Some(program))?;
								self.execute_l_value_write(l_value, l_value_arguments, l_value_string_slice_bounds, parsed_value, Some(program))?;
							}
							AnyTypeLValue::String(l_value) => {
								let parsed_value;
								(parsed_value, input_buffer_left) = match parse_datum_string(input_buffer_left, false) {
									Ok((parsed_value, input_buffer_left)) => (parsed_value, input_buffer_left),
									Err(_) => continue 'a,
								};
								let (l_value_arguments, l_value_string_slice_bounds) = self.execute_l_value_arguments::<StringValue>(l_value, Some(program))?;
								self.execute_l_value_write(l_value, l_value_arguments, l_value_string_slice_bounds, parsed_value, Some(program))?;
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
				// If the loop condition is initially false
				let do_skip_to_next = (step_value.is_negative() && (initial_value.value < final_value.value)) || (!step_value.is_negative() && (initial_value.value > final_value.value)) &&
					self.options.for_initially_false_jumps_to_next();
				let (l_value_arguments, l_value_string_slice_bounds) = self.execute_l_value_arguments::<IntValue>(loop_variable, Some(program))?;
				self.execute_l_value_write(loop_variable, l_value_arguments, l_value_string_slice_bounds, initial_value, Some(program))?;
				// Construct loop
				let stack_loop = BlockOnStack::IntForLoop {
					name: loop_variable.name.clone(), final_value, step_value, for_line: self.line_executing.clone(), for_sub_line: self.sub_line_executing.unwrap(), is_zero_cycles: do_skip_to_next,
				};
				// If a for loop using the same variable exists, pop the loop and all blocks inside it
				match self.gosub_stack.last().unwrap().for_loop_variable_to_block_stack_index.get(&(loop_variable.name.clone(), false)) {
					None => {},
					Some(block_stack_index) => self.truncate_block_stack(*block_stack_index),
				}
				// Push loop
				self.gosub_stack.last_mut().unwrap().block_stack.push(stack_loop);
				let index = (self.gosub_stack.last().unwrap().block_stack.len() - 1).clone();
				self.gosub_stack.last_mut().unwrap().for_loop_variable_to_block_stack_index.insert((loop_variable.name.clone(), false), index);
				// If the loop condition is initially false
				if do_skip_to_next {
					match &self.line_executing {
						Some(line_executing) => {
							match program.get_next_int_after(&loop_variable.name, &line_executing, self.sub_line_executing.unwrap()) {
								Some(jump_to) => {
									self.line_executing = Some(jump_to.0.clone());
									self.sub_line_executing = Some(jump_to.1);
									return Ok(true);
								}
								None => return Err(ErrorVariant::NoMatchingNext.at_column(*column)),
							}
						}
						None => return Err(ErrorVariant::ZeroForInDirectMode.at_column(*column)),
					}
				}
			}
			StatementVariant::ForFloat { loop_variable, initial, limit, step } => {
				// Execute expressions
				let initial_value = self.execute_float_expression(initial, Some(program))?;
				let final_value = self.execute_float_expression(limit, Some(program))?;
				let step_value = match step {
					Some(step) => self.execute_float_expression(step, Some(program))?,
					None => FloatValue::ONE,
				};
				let (l_value_arguments, l_value_string_slice_bounds) = self.execute_l_value_arguments::<FloatValue>(loop_variable, Some(program))?;
				self.execute_l_value_write(loop_variable, l_value_arguments, l_value_string_slice_bounds, initial_value, Some(program))?;
				// If the loop condition is initially false
				let do_skip_to_next = (step_value.is_negative() && (initial_value.value < final_value.value)) || (!step_value.is_negative() && (initial_value.value > final_value.value)) &&
					self.options.for_initially_false_jumps_to_next();
				// Construct loop
				let stack_loop = BlockOnStack::FloatForLoop {
					name: loop_variable.name.clone(), final_value, step_value, for_line: self.line_executing.clone(), for_sub_line: self.sub_line_executing.unwrap(),
					is_zero_cycles: do_skip_to_next,
				};
				// If a for loop using the same variable exists, pop the loop and all blocks inside it
				match self.gosub_stack.last().unwrap().for_loop_variable_to_block_stack_index.get(&(loop_variable.name.clone(), true)) {
					None => {},
					Some(block_stack_index) => self.truncate_block_stack(*block_stack_index),
				}
				// Push loop
				self.gosub_stack.last_mut().unwrap().block_stack.push(stack_loop);
				let index = (self.gosub_stack.last().unwrap().block_stack.len() - 1).clone();
				self.gosub_stack.last_mut().unwrap().for_loop_variable_to_block_stack_index.insert((loop_variable.name.clone(), true), index);
				// If the loop condition is initially false
				if do_skip_to_next {
					match &self.line_executing {
						Some(line_executing) => {
							match program.get_next_float_after(&loop_variable.name, &line_executing, self.sub_line_executing.unwrap()) {
								Some(jump_to) => {
									self.line_executing = Some(jump_to.0.clone());
									self.sub_line_executing = Some(jump_to.1);
									return Ok(true);
								}
								None => return Err(ErrorVariant::NoMatchingNext.at_column(*column)),
							}
						}
						None => return Err(ErrorVariant::ZeroForInDirectMode.at_column(*column)),
					}
				}
			}
			StatementVariant::Next(loop_variables) => {
				// If a NEXT without arguments is executed
				if loop_variables.is_empty() {
					for (index, loop_block) in self.gosub_stack.last().unwrap().block_stack.iter().enumerate().rev() {
						match loop_block {
							BlockOnStack::IntForLoop { name, final_value, step_value, for_line, for_sub_line, is_zero_cycles } => {
								// Get current value
								let loop_variable_value = self.int_stored_values.simple_variables.get_mut(name).unwrap();
								// Increment
								if !*is_zero_cycles {
									*loop_variable_value = loop_variable_value.clone().add(step_value);
								}
								// Remove the loop and blocks inside if it has finished
								if (step_value.is_negative() && (&*loop_variable_value.value) < (&*final_value.value)) || (!step_value.is_negative() && (&*loop_variable_value.value) > (&*final_value.value)) {
									self.truncate_block_stack(index);
									return Ok(false);
								}
								self.line_executing = for_line.clone();
								self.sub_line_executing = Some(for_sub_line + 1);
								return Ok(true);
							}
							BlockOnStack::FloatForLoop { name, final_value, step_value, for_line, for_sub_line, is_zero_cycles } => {
								// Get current value
								let loop_variable_value = self.float_stored_values.simple_variables.get_mut(name).unwrap();
								// Increment
								if !*is_zero_cycles {
									*loop_variable_value = loop_variable_value.add(*step_value, Some(&self.options)).map_err(|error| error.at_column(*column))?;
								}
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
							let (final_value, step_value, for_line, for_sub_line, is_zero_cycles) = match self.gosub_stack.last().unwrap().block_stack.get(*block_stack_index).unwrap() {
								BlockOnStack::IntForLoop { final_value, step_value, for_line, for_sub_line, is_zero_cycles, .. } => (final_value, step_value, for_line, for_sub_line, is_zero_cycles),
								_ => unreachable!(),
							};
							// Get current value
							let loop_variable_value = self.int_stored_values.simple_variables.get_mut(name).unwrap();
							// Increment
							if !is_zero_cycles {
								*loop_variable_value = loop_variable_value.clone().add(step_value);
							}
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
							let (final_value, step_value, for_line, for_sub_line, is_zero_cycles) = match self.gosub_stack.last().unwrap().block_stack.get(*block_stack_index).unwrap() {
								BlockOnStack::FloatForLoop { final_value, step_value, for_line, for_sub_line, is_zero_cycles, .. } => (final_value, step_value, for_line, for_sub_line, is_zero_cycles),
								_ => unreachable!(),
							};
							// Get current value
							let loop_variable_value = self.float_stored_values.simple_variables.get_mut(name).unwrap();
							// Increment
							if !is_zero_cycles {
								*loop_variable_value = loop_variable_value.clone().add(*step_value, Some(&self.options)).map_err(|error| error.at_column(*start_column))?;
							}
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
				//self.execute_l_value_write(l_value, value, program)
				// Flow control used
				return Ok(true);
			}
			StatementVariant::NumericAssignment(l_value_expressions, r_value_expression) => {
				// Evaluate l-value arguments
				let mut l_value_argument_values = Vec::new();
				let mut l_value_string_slice_bounds = Vec::new();
				for l_value_expression in l_value_expressions {
					let (argument_values_for_this_l_value, string_slice_index_values_for_this_l_value) = match l_value_expression {
						AnyTypeLValue::Float(l_value_expression) => self.execute_l_value_arguments::<FloatValue>(l_value_expression, Some(program))?,
						AnyTypeLValue::Int(l_value_expression) => self.execute_l_value_arguments::<IntValue>(l_value_expression, Some(program))?,
						AnyTypeLValue::Complex(l_value_expression) => self.execute_l_value_arguments::<ComplexValue>(l_value_expression, Some(program))?,
						_ => unreachable!()
					};
					l_value_argument_values.push(argument_values_for_this_l_value);
					l_value_string_slice_bounds.push(string_slice_index_values_for_this_l_value);
				}
				// Evaluate r-value
				let value_to_assign = self.execute_any_type_expression(r_value_expression, Some(program))?;
				// Assign
				for (l_value_index, l_value_expression) in l_value_expressions.iter().enumerate() {
					match l_value_expression {
						AnyTypeLValue::Float(l_value_expression) => self.execute_l_value_write::<FloatValue>(
							l_value_expression, take(&mut l_value_argument_values[l_value_index]), take(&mut l_value_string_slice_bounds[l_value_index]),
							value_to_assign.clone().to_float().map_err(|error| error.at_column(r_value_expression.get_start_column()))?, Some(program)
						)?,
						AnyTypeLValue::Int(l_value_expression) => self.execute_l_value_write::<IntValue>(
							l_value_expression, take(&mut l_value_argument_values[l_value_index]), take(&mut l_value_string_slice_bounds[l_value_index]),
							value_to_assign.clone().to_int().map_err(|error| error.at_column(r_value_expression.get_start_column()))?, Some(program)
						)?,
						AnyTypeLValue::Complex(l_value_expression) => self.execute_l_value_write::<ComplexValue>(
							l_value_expression, take(&mut l_value_argument_values[l_value_index]), take(&mut l_value_string_slice_bounds[l_value_index]),
							value_to_assign.clone().to_complex().map_err(|error| error.at_column(r_value_expression.get_start_column()))?, Some(program)
						)?,
						_ => unreachable!(),
					}
				}
			}
			StatementVariant::StringAssignment(l_value_expressions, r_value_expression) => {
				// Evaluate l-value arguments
				let mut l_value_argument_values = Vec::new();
				let mut l_value_string_slice_bounds = Vec::new();
				for l_value_expression in l_value_expressions {
					let (argument_values_for_this_l_value, string_slice_index_values_for_this_l_value) = 
						self.execute_l_value_arguments::<StringValue>(l_value_expression, Some(program))?;
					l_value_argument_values.push(argument_values_for_this_l_value);
					l_value_string_slice_bounds.push(string_slice_index_values_for_this_l_value);
				}
				// Evaluate r-value
				let value_to_assign = self.execute_string_expression(r_value_expression, Some(program))?;
				// Assign
				for (l_value_index, l_value_expression) in l_value_expressions.iter().enumerate() {
					self.execute_l_value_write::<StringValue>(
						l_value_expression, take(&mut l_value_argument_values[l_value_index]), take(&mut l_value_string_slice_bounds[l_value_index]),
						value_to_assign.clone(), Some(program)
					)?
				}
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
			StatementVariant::Option(options) => {
				for (option, _) in options {
					match option {
						OptionVariableAndValue::ArithmeticDecimal | OptionVariableAndValue::ArithmeticNative | OptionVariableAndValue::ArithmeticDefault => {},
						OptionVariableAndValue::Angle(angle_option) => self.options.angle = *angle_option,
						OptionVariableAndValue::Math(math_option) => self.options.math = *math_option,
						OptionVariableAndValue::Machine(machine_option) => self.options.machine = *machine_option,
						OptionVariableAndValue::Base(base_option) => self.options.base = *base_option,
						OptionVariableAndValue::Collate(collate_option) => self.options.collate = *collate_option,
					}
				}
			}
			StatementVariant::Load(..) | StatementVariant::Save(..) | StatementVariant::New | StatementVariant::Help(..) => return Err(ErrorVariant::CanOnlyExecuteInDirectMode.at_column(*column)),
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
							let (l_value_arguments, l_value_string_slice_bounds) = self.execute_l_value_arguments::<IntValue>(l_value, Some(program))?;
							self.execute_l_value_write(l_value, l_value_arguments, l_value_string_slice_bounds, datum.clone(), Some(program))?;
						}
						AnyTypeLValue::Float(l_value) => {
							let datum = match &datum.as_float {
								Some(datum) => datum,
								None => return Err(ErrorVariant::NonNumericReadToNumeric((*data_line_number_to_read).clone(), datum_start_column).at_column(l_value.start_column)),
							};
							let (l_value_arguments, l_value_string_slice_bounds) = self.execute_l_value_arguments::<FloatValue>(l_value, Some(program))?;
							self.execute_l_value_write(l_value, l_value_arguments, l_value_string_slice_bounds, datum.clone(), Some(program))?;
						}
						AnyTypeLValue::Complex(l_value) => {
							let datum = match &datum.as_complex {
								Some(datum) => datum,
								None => return Err(ErrorVariant::NonNumericReadToNumeric((*data_line_number_to_read).clone(), datum_start_column).at_column(l_value.start_column)),
							};
							let (l_value_arguments, l_value_string_slice_bounds) = self.execute_l_value_arguments::<ComplexValue>(l_value, Some(program))?;
							self.execute_l_value_write(l_value, l_value_arguments, l_value_string_slice_bounds, datum.clone(), Some(program))?;
						}
						AnyTypeLValue::String(l_value) => {
							let (l_value_arguments, l_value_string_slice_bounds) = self.execute_l_value_arguments::<StringValue>(l_value, Some(program))?;
							self.execute_l_value_write(l_value, l_value_arguments, l_value_string_slice_bounds, datum.as_string, Some(program))?;
						}
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
			StatementVariant::Randomize => {
				match SmallRng::try_from_os_rng() {
					Ok(rng) => self.rng = rng,
					Err(_) => return Err(ErrorVariant::UnableToRandomize.at_column(*column)),
				}
			}
			StatementVariant::OnGoto { index: index_expression, line_numbers: line_number_expressions, else_statement } |
			StatementVariant::OnGosub { index: index_expression, line_numbers: line_number_expressions, else_statement } => {
				// Execute the index expression
				let index_value = match self.execute_int_expression(index_expression, Some(program))?.from_ones_index_to_usize() {
					Some(index_value) => index_value,
					None => match else_statement {
						Some(else_statement) => return self.execute_statement(&else_statement, program),
						None => return Err(ErrorVariant::OnGotoGosubIndexOutOfRange.at_column(index_expression.get_start_column())),
					}
				};
				// Get the line number expression to execute
				let line_number_expression = match line_number_expressions.get(index_value) {
					Some(index_value) => index_value,
					None => match else_statement {
						Some(else_statement) => return self.execute_statement(&else_statement, program),
						None => return Err(ErrorVariant::OnGotoGosubIndexOutOfRange.at_column(index_expression.get_start_column())),
					}
				};
				// Push the return address and create a new GOSUB level if this is a ON GOSUB statement
				if matches!(variant, StatementVariant::OnGosub { .. }) {
					if self.execution_source == ExecutionSource::DirectModeLine {
						return Err(ErrorVariant::GosubInDirectMode.at_column(*column));
					}
					self.gosub_stack.push(GosubLevel::from_return_location(self.line_executing.clone(), self.sub_line_executing.unwrap()));
				}
				// Set the line to be executed next
				let line_number_to_jump_to = self.execute_int_expression(line_number_expression, Some(program))?.value;
				self.set_line_executing_by_jumping(program, Some(line_number_to_jump_to), line_number_expression.get_start_column())?;
				// Flow control used
				return Ok(true);
			}
			StatementVariant::Clr => self.clear_machine_state(program),
			StatementVariant::Clear => {
				// TODO: Test
				execute!(stdout(), Clear(ClearType::All), MoveTo(0, 0)).unwrap();
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
										None => self.options.get_minimum_array_value(),
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
								self.float_stored_values.arrays.insert(name.into(), array);
							}
							IdentifierType::Integer => {
								let mut dimensions = Vec::new();
								for dimension_expression in &array.dimensions {
									let lower_bound = match &dimension_expression.0 {
										Some(lower_bound) => self.execute_int_expression(lower_bound, Some(program))?,
										None => self.options.get_minimum_array_value(),
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
								self.int_stored_values.arrays.insert(name.into(), array);
							}
							IdentifierType::ComplexNumber => {
								let mut dimensions = Vec::new();
								for dimension_expression in &array.dimensions {
									let lower_bound = match &dimension_expression.0 {
										Some(lower_bound) => self.execute_int_expression(lower_bound, Some(program))?,
										None => self.options.get_minimum_array_value(),
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
								self.complex_stored_values.arrays.insert(name.into(), array);
							}
							IdentifierType::String => {
								let mut dimensions = Vec::new();
								for dimension_expression in &array.dimensions {
									let lower_bound = match &dimension_expression.0 {
										Some(lower_bound) => self.execute_int_expression(lower_bound, Some(program))?,
										None => self.options.get_minimum_array_value(),
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
								self.string_stored_values.arrays.insert(name.into(), array);
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
				self.float_stored_values.functions.insert((l_value.name.clone(), l_value.arguments.len()), (parameters.into_boxed_slice(), expression.clone(), def_location));
				match l_value.has_parentheses {
					true => {
						self.float_stored_values.arrays.remove(&l_value.name);
					}
					false => {
						self.float_stored_values.simple_variables.remove(&l_value.name);
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
				self.int_stored_values.functions.insert((l_value.name.clone(), l_value.arguments.len()), (parameters.into_boxed_slice(), expression.clone(), def_location));
				match l_value.has_parentheses {
					true => {
						self.int_stored_values.arrays.remove(&l_value.name);
					}
					false => {
						self.int_stored_values.simple_variables.remove(&l_value.name);
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
				self.complex_stored_values.functions.insert((l_value.name.clone(), l_value.arguments.len()), (parameters.into_boxed_slice(), expression.clone(), def_location));
				match l_value.has_parentheses {
					true => {
						self.complex_stored_values.arrays.remove(&l_value.name);
					}
					false => {
						self.complex_stored_values.simple_variables.remove(&l_value.name);
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
				self.string_stored_values.functions.insert((l_value.name.clone(), l_value.arguments.len()), (parameters.into_boxed_slice(), expression.clone(), def_location));
				match l_value.has_parentheses {
					true => {
						self.string_stored_values.arrays.remove(&l_value.name);
					}
					false => {
						self.string_stored_values.simple_variables.remove(&l_value.name);
					}
				}
				Ok(())
			}
			_ => unreachable!(),
		}
	}

	/// Execute an expression that returns an integer value.
	pub fn execute_int_expression(&mut self, expression: &IntExpression, program: Option<&Program>) -> Result<IntValue, Error> {
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
			IntExpression::LValue(l_value) => self.execute_l_value_read(l_value, program)?,
		})
	}

	/// Execute an expression that returns an float value.
	pub fn execute_float_expression(&mut self, expression: &FloatExpression, program: Option<&Program>) -> Result<FloatValue, Error> {
		Ok(match expression {
			FloatExpression::ConstantValue { value, .. } => value.clone(),
			FloatExpression::CastFromInt(sub_expression) => self.execute_int_expression(sub_expression, program)?.to_float(),
			FloatExpression::CastFromComplex(sub_expression) =>
				self.execute_complex_expression(sub_expression, program)?.to_float().map_err(|error| error.at_column(sub_expression.get_start_column()))?,
			FloatExpression::Addition { lhs_expression, rhs_expression, start_column } =>
				self.execute_float_expression(lhs_expression, program)?.add(self.execute_float_expression(rhs_expression, program)?, Some(&self.options))
					.map_err(|error| error.at_column(*start_column))?,
			FloatExpression::Subtraction { lhs_expression, rhs_expression, start_column } =>
				self.execute_float_expression(lhs_expression, program)?.sub(self.execute_float_expression(rhs_expression, program)?, Some(&self.options))
					.map_err(|error| error.at_column(*start_column))?,
			FloatExpression::Multiplication { lhs_expression, rhs_expression, start_column } =>
				self.execute_float_expression(lhs_expression, program)?.mul(self.execute_float_expression(rhs_expression, program)?, Some(&self.options))
					.map_err(|error| error.at_column(*start_column))?,
			FloatExpression::Division { lhs_expression, rhs_expression, start_column } =>
				self.execute_float_expression(lhs_expression, program)?
					.div(self.execute_float_expression(rhs_expression, program)?, Some(&self.options))
					.map_err(|error| error.at_column(*start_column))?,
			FloatExpression::Exponentiation { lhs_expression, rhs_expression, start_column } =>
				self.execute_float_expression(lhs_expression, program)?
					.pow(self.execute_float_expression(rhs_expression, program)?, Some(&self.options))
					.map_err(|error| error.at_column(*start_column))?,
			FloatExpression::Negation { sub_expression, .. } => self.execute_float_expression(&sub_expression, program)?.neg(),
			FloatExpression::LValue(l_value) => self.execute_l_value_read(l_value, program)?,
		})
	}

	/// Execute an expression that returns an complex value.
	pub fn execute_complex_expression(&mut self, expression: &ComplexExpression, program: Option<&Program>) -> Result<ComplexValue, Error> {
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
			ComplexExpression::LValue(l_value) => self.execute_l_value_read(l_value, program)?,
		})
	}

	/// Execute an expression that returns an boolean value.
	pub fn execute_bool_expression(&mut self, expression: &BoolExpression, program: Option<&Program>) -> Result<BoolValue, Error> {
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
	pub fn execute_string_expression(&mut self, expression: &StringExpression, program: Option<&Program>) -> Result<StringValue, Error> {
		Ok(match expression {
			StringExpression::ConstantValue { value, .. } => value.clone(),
			StringExpression::Concatenation { lhs_expression, rhs_expression, .. } =>
				self.execute_string_expression(lhs_expression, program)?.concat(self.execute_string_expression(rhs_expression, program)?),
			StringExpression::LValue(l_value) => self.execute_l_value_read(l_value, program)?,
			StringExpression::StringSlicing { to_slice_expression, range_start_expression, range_end_expression, .. } => {
				let to_slice_value = self.execute_string_expression(to_slice_expression, program)?;
				let range_start_value = self.execute_int_expression(range_start_expression, program)?;
				let range_end_value = self.execute_int_expression(range_end_expression, program)?;
				to_slice_value.slice_chars(range_start_value, range_end_value)
			}
		})
	}

	/// Execute an expression that could return a value of any type.
	pub fn execute_any_type_expression(&mut self, expression: &AnyTypeExpression, program: Option<&Program>) -> Result<AnyTypeValue, Error> {
		Ok(match expression {
			AnyTypeExpression::Bool(expression) => AnyTypeValue::Bool(self.execute_bool_expression(expression, program)?),
			AnyTypeExpression::Int(expression) => AnyTypeValue::Int(self.execute_int_expression(expression, program)?),
			AnyTypeExpression::Float(expression) => AnyTypeValue::Float(self.execute_float_expression(expression, program)?),
			AnyTypeExpression::Complex(expression) => AnyTypeValue::Complex(self.execute_complex_expression(expression, program)?),
			AnyTypeExpression::String(expression) => AnyTypeValue::String(self.execute_string_expression(expression, program)?),
		})
	}

	/// Creates any array or function that has been defined but not yet created.
	fn make_sure_defined_array_or_function_is_created<T: Value>(&mut self, l_value: &T::LValueType, program: Option<&Program>) -> Result<(), Error> {
		// Unpack
		let name = T::get_l_value_name(l_value);
		let arguments = T::get_l_value_arguments(l_value);
		let has_parentheses = T::get_l_value_has_parentheses(l_value);
		let l_value_start_column = T::get_l_value_start_column(l_value);
		// Create the array if it is defined and we are accessing it and it is not yet created and it is not created on executing a DIM
		if has_parentheses && !T::get_stored_values(self).arrays.contains_key(name) && !self.options.arrays_created_on_dim_execution() &&
			program.is_some() && let Some(array_declarations) = T::get_array_declarations(program.unwrap()).get(name)
		{
			// Throw an error is there are conflicting array DIM statements
			if array_declarations.len() > 1 {
				return Err(ErrorVariant::MultipleDeclarationsOfArray.at_column(l_value_start_column));
			}
			// Get the array DIM statement and its location in the program
			let array_declarations_location = array_declarations.iter().next().unwrap();
			let statement = &program.unwrap().get_line(&array_declarations_location.0).unwrap().optimized_statements[array_declarations_location.1];
			// Save the current OPTIONs set
			let options_before_array_creation = self.options.clone();
			// Set the OPTIONs set to the OPTIONs set at the location of the array DIM statement
			self.options = program.unwrap().get_options(&array_declarations_location.0.clone(), array_declarations_location.1);
			// Create the array
			self.execute_array_declaration(statement, program.unwrap(), &name, T::IDENTIFIER_TYPE)?;
			// Restore the OPTIONs we had before we created the array
			self.options = options_before_array_creation;
		}
		// Create the function if it is defined and we are accessing it and it is not yet created and it is not created on executing a DEF
		if !T::get_stored_values(self).functions.contains_key(&(name.into(), arguments.len())) && !self.options.functions_defined_on_fn_execution() &&
			program.is_some() && let Some(functions) = T::get_function_declarations(program.unwrap()).get(&(name.into(), arguments.len()))
		{
			// Throw an error if there are other functions or created arrays with the same name
			if functions.len() > 1 {
				return Err(ErrorVariant::MultipleDeclarationsOfFunction.at_column(l_value_start_column));
			}
			if !arguments.is_empty() && T::get_stored_values(self).arrays.contains_key(name) {
				return Err(ErrorVariant::MultipleDeclarationsOfFunctionAndArray.at_column(l_value_start_column));
			}
			// Create function
			let function = functions.iter().next().unwrap();
			let statement = &program.unwrap().get_line(&function.0).unwrap().optimized_statements[function.1];
			self.execute_function_declaration(statement)?;
		}
		// Create an implicit array with a upper bound of 10 if an array or function has still not been created and the array name consists of one char with a possible type char
		if has_parentheses && name.chars().count() == 1 && !T::get_stored_values(self).arrays.contains_key(name) &&
			!T::get_stored_values(self).functions.contains_key(&(name.into(), arguments.len()))
		{
			let mut dimensions = Vec::new();
			let lower_bound = self.options.get_minimum_array_value();
			let dimension_length = 11 - lower_bound.to_usize().unwrap();
			for _ in 0..arguments.len() {
				dimensions.push((dimension_length, lower_bound.clone()));
			}
			let array = Array::new(dimensions.into_boxed_slice()).map_err(|err| err.at_column(l_value_start_column))?;
			T::get_stored_values_mut(self).arrays.insert(name.into(), array);
		}
		// Return
		Ok(())
	}

	/// Reads a value from a value, array or executes a function.
	fn execute_l_value_read<T: Value>(&mut self, l_value: &T::LValueType, program: Option<&Program>) -> Result<T, Error> {
		// Unpack
		let name = T::get_l_value_name(l_value);
		let arguments = T::get_l_value_arguments(l_value);
		let has_parentheses = T::get_l_value_has_parentheses(l_value);
		let l_value_start_column = T::get_l_value_start_column(l_value);
		// Get value before slicing
		let mut value = 'a: {
			// Create the array if it is defined and we are reading from it and it is not yet created and it is not created on executing a DIM
			self.make_sure_defined_array_or_function_is_created::<T>(l_value, program)?;
			// If the user has defined an array with the name read from it.
			if has_parentheses && T::get_stored_values(self).arrays.contains_key(name) {
				// Execute array indices
				let mut indices = Vec::new();
				for argument in arguments {
					indices.push(self.execute_any_type_expression(argument, program)?.to_int().map_err(|err| err.at_column(argument.get_start_column()))?);
				}
				// Read array element
				let element = T::get_stored_values(self).arrays.get(name).unwrap()
					.read_element(&indices, self.options.allow_uninitialized_read())
					.map_err(|err| err.at_column(l_value_start_column))?;
				break 'a element;
			}
			// Else if a function with the argument count and name has been defined, execute it
			if let Some((function_parameters, function_expression, function_location)) = T::get_stored_values(self).functions.get(&(name.into(), arguments.len())).cloned() {
				// Execute function arguments
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
				// Push GOSUB level for function execution
				self.gosub_stack.push(gosub_level_to_push);
				// Save program execution location and OPTIONs set
				let line_executing_before_function_execution = self.line_executing.clone();
				let sub_line_executing_before_function_execution = self.sub_line_executing;
				let options_before_function_execution = self.options.clone();
				// Get the OPTIONs set and program location executing
				if let Some(function_location) = function_location {
					self.line_executing = Some(function_location.0);
					self.sub_line_executing = Some(function_location.1);
				}
				if let Some(program) = program {
					self.options = program.get_options(&self.line_executing.clone().unwrap(), self.sub_line_executing.unwrap());
				}
				// Execute function
				let result = T::execute_expression(self, &function_expression, program)?;
				// Pop the pushed GOSUB stack
				self.gosub_stack.pop();
				// Restore OPTIONs set and the location in program that is being executed
				self.line_executing = line_executing_before_function_execution;
				self.sub_line_executing = sub_line_executing_before_function_execution;
				self.options = options_before_function_execution;
				break 'a result;
			}
			// Else if a local variable with the name is defined, read it
			if !has_parentheses && let Some(variable) = T::get_local_variables_mut(self).get(name) {
				break 'a variable.clone();
			}
			// Else if a global variable with the name is defined, read it
			if !has_parentheses && let Some(variable) = T::get_stored_values(self).simple_variables.get(name) {
				break 'a variable.clone();
			}
			// Else try to execute a supplied (built-in) function
			match T::execute_supplied_function(self, l_value, program)? {
				Some(value) => break 'a value,
				None => {},
			}
			// Else
			if has_parentheses {
				return Err(ErrorVariant::ArrayOrFunctionNotDefined.at_column(l_value_start_column));
			}
			match self.options.allow_uninitialized_read() {
				true => break 'a Default::default(),
				false => return Err(ErrorVariant::VariableReadUninitialized.at_column(l_value_start_column)),
			}
		};
		// Slice value if it is a string
		if let Some(string_slicings) = T::get_string_slicings(l_value) {
			for (string_slicing_start_index_expression, string_slicing_end_index_expression, _) in string_slicings {
				let string_slicing_start_index_value = self.execute_int_expression(string_slicing_start_index_expression, program)?;
				let string_slicing_end_index_value = self.execute_int_expression(string_slicing_end_index_expression, program)?;
				value = T::slice_chars(&value, string_slicing_start_index_value, string_slicing_end_index_value);
			}
		}
		Ok(value)
	}

	/// Returns a list of evaluated l_value arguments and and string slice bounds.
	fn execute_l_value_arguments<T: Value>(&mut self, l_value: &T::LValueType, program: Option<&Program>) -> Result<(Box<[AnyTypeValue]>, Box<[(IntValue, IntValue)]>), Error> {
		// Unpack
		let arguments_expressions = T::get_l_value_arguments(l_value);
		let string_slicing_bound_expressions = T::get_string_slicings(l_value);
		// Evaluate array indices
		let mut array_index_values = Vec::new();
		for argument_expression in arguments_expressions {
			array_index_values.push(self.execute_any_type_expression(argument_expression, program)?);
		}
		// Evaluate string slice bounds
		let mut string_slice_bounds_values = Vec::new();
		if let Some(string_slicing_bound_expressions) = string_slicing_bound_expressions {
			for (lower_bound_value, upper_bound_value, _) in string_slicing_bound_expressions {
				string_slice_bounds_values.push((self.execute_int_expression(lower_bound_value, program)?, self.execute_int_expression(upper_bound_value, program)?));
			}
		}
		// Return
		Ok((array_index_values.into(), string_slice_bounds_values.into()))
	}

	/// Writes to a value to a variable or array.
	fn execute_l_value_write<T: Value>(&mut self, l_value: &T::LValueType, l_value_arguments: Box<[AnyTypeValue]>, _l_value_string_slice_bounds: Box<[(IntValue, IntValue)]>, value: T, program: Option<&Program>)
		-> Result<(), Error>
	{
		// Unpack
		let name = T::get_l_value_name(l_value);
		let arguments = T::get_l_value_arguments(l_value);
		let has_parentheses = T::get_l_value_has_parentheses(l_value);
		let l_value_start_column = T::get_l_value_start_column(l_value);
		let string_slicing_indices = T::get_string_slicings(l_value);
		// Create the array if it is defined and we are reading from it and it is not yet created and it is not created on executing a DIM
		self.make_sure_defined_array_or_function_is_created::<T>(l_value, program)?;
		//
		if let Some(string_slicing_indices) = string_slicing_indices && !string_slicing_indices.is_empty() {
			if string_slicing_indices.len() > 1 {
				return Err(ErrorVariant::Unimplemented("More than one string l-value slicing operator".into()).at_column(string_slicing_indices[1].2));
			}
			let char_first_ones_index = self.execute_int_expression(&(**string_slicing_indices)[0].0, program)?;
			let char_last_ones_index = self.execute_int_expression(&(**string_slicing_indices)[0].1, program)?;
			// If we are writing to an array and it has been created, write to it.
			if has_parentheses && T::get_stored_values(self).arrays.contains_key(name) {
				let mut indices = Vec::new();
				for (argument_index, argument_value) in l_value_arguments.into_iter().enumerate() {
					indices.push(argument_value.to_int().map_err(|err| err.at_column(arguments[argument_index].get_start_column()))?);
				}
				let to_write_to = T::get_stored_values_mut(self).arrays.get_mut(name).unwrap()
					.get_element_mut(&indices)
					.map_err(|err| err.at_column(l_value_start_column))?;
				match to_write_to {
					None => return Err(ErrorVariant::VariableReadUninitialized.at_column(l_value_start_column)),
					Some(to_write_to) => {
						T::insert_slice_chars(to_write_to, char_first_ones_index, char_last_ones_index, value);
					}
				}
				return Ok(());
			}
			// Else assign to global variable
			if has_parentheses {
				return Err(ErrorVariant::ArrayOrFunctionNotDefined.at_column(l_value_start_column));
			}
			match T::get_stored_values_mut(self).simple_variables.get_mut(name.into()) {
				None => return Err(ErrorVariant::VariableReadUninitialized.at_column(l_value_start_column)),
				Some(to_write_to) => {
					T::insert_slice_chars(to_write_to, char_first_ones_index, char_last_ones_index, value);
				}
			}
		}
		else {
			// If we are writing to an array and it has been created, write to it.
			if has_parentheses && T::get_stored_values(self).arrays.contains_key(name) {
				let mut indices = Vec::new();
				for (argument_index, argument_value) in l_value_arguments.into_iter().enumerate() {
					indices.push(argument_value.to_int().map_err(|err| err.at_column(arguments[argument_index].get_start_column()))?);
				}
				T::get_stored_values_mut(self).arrays.get_mut(name).unwrap()
					.write_element(&indices, value)
					.map_err(|err| err.at_column(l_value_start_column))?;
				return Ok(());
			}
			// Else assign to global variable
			if has_parentheses {
				return Err(ErrorVariant::ArrayOrFunctionNotDefined.at_column(l_value_start_column));
			}
			T::get_stored_values_mut(self).simple_variables.insert(name.into(), value);
		}
		// Return
		Ok(())
	}

	pub fn execute_float_supplied_function(&mut self, l_value: &FloatLValue, program: Option<&Program>) -> Result<Option<FloatValue>, Error> {
		// Unpack
		let FloatLValue { arguments: argument_expressions, has_parentheses: _, start_column, supplied_function, .. } = l_value;
		let supplied_function = match supplied_function {
			Some(supplied_function) => *supplied_function,
			None => return Ok(None),
		};
		//
		let mut argument_values = Vec::with_capacity(argument_expressions.len());
		for argument_expression in argument_expressions {
			argument_values.push(self.execute_any_type_expression(argument_expression, program)?);
		}
		// Else try to execute a supplied (built-in) function
		let result = 'a: { match supplied_function {
			FloatSuppliedFunction::Tab => return Err(ErrorVariant::TabNotAsPrintItem.at_column(*start_column)),

			FloatSuppliedFunction::Pi =>     Ok(FloatValue::PI),
			FloatSuppliedFunction::E =>      Ok(FloatValue::E),
			FloatSuppliedFunction::Tau =>    Ok(FloatValue::TAU),
			FloatSuppliedFunction::Phi =>    Ok(FloatValue::PHI),
			FloatSuppliedFunction::EGamma => Ok(FloatValue::EGAMMA),
			FloatSuppliedFunction::MaxNum => Ok(FloatValue::MAX),
			FloatSuppliedFunction::NaN =>    Ok(FloatValue::NAN),
			FloatSuppliedFunction::Inf =>    Ok(FloatValue::INFINITY),
			FloatSuppliedFunction::NInf =>   Ok(FloatValue::NEG_INFINITY),
			FloatSuppliedFunction::True =>   Ok(FloatValue::TRUE),
			FloatSuppliedFunction::False =>  Ok(FloatValue::FALSE),

			FloatSuppliedFunction::Time => Ok(FloatValue::from_u32(Local::now().num_seconds_from_midnight())),
			FloatSuppliedFunction::Date => Ok({
				let date = Local::now();
				let mut day_of_year = date.day();
				for month in 1..date.month() {
					day_of_year += date.with_day(1).unwrap().with_month(month).unwrap().num_days_in_month() as u32;
				}
				FloatValue::from_u32((date.year() % 100) as u32 * 1000 + day_of_year)
			}),
			FloatSuppliedFunction::Second => Ok(FloatValue::from_u32(Local::now().second())),
			FloatSuppliedFunction::Minute => Ok(FloatValue::from_u32(Local::now().minute())),
			FloatSuppliedFunction::Hour   => Ok(FloatValue::from_u32(Local::now().hour())),
			FloatSuppliedFunction::Day    => Ok(FloatValue::from_u32(Local::now().day())),
			FloatSuppliedFunction::Month  => Ok(FloatValue::from_u32(Local::now().month())),
			FloatSuppliedFunction::Year   => Ok(FloatValue::from_i32(Local::now().year())),
			// Other
			FloatSuppliedFunction::Random => Ok(FloatValue::new(self.rng.random_range(0.0..1.))),
			// Functions that have one float argument
			FloatSuppliedFunction::Sqrt | FloatSuppliedFunction::Abs | FloatSuppliedFunction::Signum | FloatSuppliedFunction::LogE | FloatSuppliedFunction::Exp |
			FloatSuppliedFunction::Sin | FloatSuppliedFunction::Cos | FloatSuppliedFunction::Tan | FloatSuppliedFunction::Cot | FloatSuppliedFunction::Sec | FloatSuppliedFunction::Csc |
			FloatSuppliedFunction::Asin | FloatSuppliedFunction::Acos | FloatSuppliedFunction::Atan | FloatSuppliedFunction::Acot | FloatSuppliedFunction::Asec | FloatSuppliedFunction::Acsc |
			FloatSuppliedFunction::Sinh | FloatSuppliedFunction::Cosh | FloatSuppliedFunction::Tanh | FloatSuppliedFunction::Coth | FloatSuppliedFunction::Sech | FloatSuppliedFunction::Csch |
			FloatSuppliedFunction::Asinh | FloatSuppliedFunction::Acosh | FloatSuppliedFunction::Atanh | FloatSuppliedFunction::Acoth | FloatSuppliedFunction::Asech | FloatSuppliedFunction::Acsch |
			FloatSuppliedFunction::Ip | FloatSuppliedFunction::Fp | FloatSuppliedFunction::Deg | FloatSuppliedFunction::Rad | FloatSuppliedFunction::Ceil | FloatSuppliedFunction::Floor |
			FloatSuppliedFunction::Log10 | FloatSuppliedFunction::Log2 | FloatSuppliedFunction::Eps | FloatSuppliedFunction::CommodoreRandom => {
				let argument_value = match argument_values[0].clone().to_float() {
					Ok(argument_value) => argument_value,
					Err(error) => break 'a Err(error),
				};
				match supplied_function {
					// Other
					FloatSuppliedFunction::Sqrt            => argument_value.sqrt(&self.options),
					FloatSuppliedFunction::Abs             => Ok(argument_value.abs()),
					FloatSuppliedFunction::Signum          => Ok(argument_value.signum()),
					FloatSuppliedFunction::Exp             => argument_value.exp(&self.options),
					FloatSuppliedFunction::Rad             => Ok(argument_value.degrees_to_radians()),
					FloatSuppliedFunction::Fp              => Ok(argument_value.fractional_part()),
					FloatSuppliedFunction::Eps             => Ok(argument_value.basic_eps()),
					FloatSuppliedFunction::Deg             => argument_value.radians_to_degrees(&self.options),
					FloatSuppliedFunction::CommodoreRandom => {
						// A value of zero means to generate a value that is not from the machine RNG
						if argument_value.is_zero() {
							return Ok(Some(FloatValue::new(random_range(0.0..1.))));
						}
						// If positive, generate one form the machine RNG
						if argument_value.is_positive() {
							return Ok(Some(FloatValue::new(self.rng.random_range(0.0..1.))));
						}
						// If negative, seed first using the value, then generate from the machine RNG
						self.rng = SmallRng::seed_from_u64(argument_value.value.to_bits());
						return Ok(Some(FloatValue::new(self.rng.random_range(0.0..1.))));
					}
					// Rounding
					FloatSuppliedFunction::Ceil => Ok(argument_value.ceil()),
					FloatSuppliedFunction::Floor  => Ok(argument_value.floor()),
					FloatSuppliedFunction::Ip   => Ok(argument_value.integer_part()),
					// Trigonometry
					FloatSuppliedFunction::Sin  => argument_value.sin(&self.options),
					FloatSuppliedFunction::Cos  => argument_value.cos(&self.options),
					FloatSuppliedFunction::Tan  => argument_value.tan(&self.options),
					FloatSuppliedFunction::Cot  => argument_value.cot(&self.options),
					FloatSuppliedFunction::Sec  => argument_value.sec(&self.options),
					FloatSuppliedFunction::Csc  => argument_value.csc(&self.options),
					FloatSuppliedFunction::Asin => argument_value.asin(&self.options),
					FloatSuppliedFunction::Acos => argument_value.acos(&self.options),
					FloatSuppliedFunction::Atan => argument_value.atan(&self.options),
					FloatSuppliedFunction::Acot => argument_value.acot(&self.options),
					FloatSuppliedFunction::Asec => argument_value.asec(&self.options),
					FloatSuppliedFunction::Acsc => argument_value.acsc(&self.options),
					// Hyperbolic Trigonometry
					FloatSuppliedFunction::Sinh  => argument_value.sinh(&self.options),
					FloatSuppliedFunction::Cosh  => argument_value.cosh(&self.options),
					FloatSuppliedFunction::Tanh  => Ok(argument_value.tanh()),
					FloatSuppliedFunction::Coth  => argument_value.coth(&self.options),
					FloatSuppliedFunction::Sech  => Ok(argument_value.sech()),
					FloatSuppliedFunction::Csch  => argument_value.csch(&self.options),
					FloatSuppliedFunction::Asinh => Ok(argument_value.asinh()),
					FloatSuppliedFunction::Acosh => argument_value.acosh(&self.options),
					FloatSuppliedFunction::Atanh => argument_value.atanh(&self.options),
					FloatSuppliedFunction::Acoth => argument_value.acoth(&self.options),
					FloatSuppliedFunction::Asech => argument_value.asech(&self.options),
					FloatSuppliedFunction::Acsch => argument_value.acsch(&self.options),
					// Logarithm
					FloatSuppliedFunction::LogE =>    argument_value.ln(&self.options),
					FloatSuppliedFunction::Log10 => argument_value.log10(&self.options),
					FloatSuppliedFunction::Log2 =>  argument_value.log2(&self.options),

					_ => unreachable!()
				}
			},
			// Functions that have two float arguments
			FloatSuppliedFunction::Angle | FloatSuppliedFunction::Atan2 | FloatSuppliedFunction::Modulo | FloatSuppliedFunction::Remainder | FloatSuppliedFunction::LogN => {
				let argument_value_0 = match argument_values[0].clone().to_float() {
					Ok(argument_value) => argument_value,
					Err(error) => break 'a Err(error),
				};
				let argument_value_1 = match argument_values[1].clone().to_float() {
					Ok(argument_value) => argument_value,
					Err(error) => break 'a Err(error),
				};
				match supplied_function {
					// Trigonometry
					FloatSuppliedFunction::Angle => argument_value_1.atan2(argument_value_0, &self.options),
					FloatSuppliedFunction::Atan2 => argument_value_0.atan2(argument_value_1, &self.options),
					// Other
					FloatSuppliedFunction::Modulo =>    argument_value_0.basic_modulo(argument_value_1, &self.options),
					FloatSuppliedFunction::Remainder => argument_value_0.remainder(argument_value_1, &self.options),
					// Logarithm
					FloatSuppliedFunction::LogN =>      argument_value_1.log(argument_value_0, &self.options),
					_ => unreachable!()
				}
			}
			// Fold functions
			FloatSuppliedFunction::Min | FloatSuppliedFunction::Max => {
				// Get first argument
				let mut result = match argument_values[0].clone().to_float() {
					Ok(argument_value) => argument_value,
					Err(error) => break 'a Err(error),
				};
				// Get other values and apply function
				for argument in argument_values.iter().skip(1) {
					let argument_value = match argument.clone().to_float() {
						Ok(argument_value) => argument_value,
						Err(error) => break 'a Err(error),
					};
					result = match supplied_function {
						FloatSuppliedFunction::Min => result.min(argument_value),
						FloatSuppliedFunction::Max => result.max(argument_value),
						_ => unreachable!(),
					};
				}
				return Ok(Some(result));
			}
			// Functions that have one complex argument
			FloatSuppliedFunction::Real | FloatSuppliedFunction::Imag | FloatSuppliedFunction::Arg | FloatSuppliedFunction::AbsComplex => {
				let argument_value = match argument_values[0].clone().to_complex() {
					Ok(argument_value) => argument_value,
					Err(error) => break 'a Err(error),
				};
				match supplied_function {
					FloatSuppliedFunction::Real =>       Ok(argument_value.re()),
					FloatSuppliedFunction::Imag =>       Ok(argument_value.im()),
					FloatSuppliedFunction::Arg =>        argument_value.arg(&self.options),
					FloatSuppliedFunction::AbsComplex => argument_value.abs(&self.options),
					_ => unreachable!()
				}
			}
			// Functions that have one string argument
			FloatSuppliedFunction::Len | FloatSuppliedFunction::Ord | FloatSuppliedFunction::Asc | FloatSuppliedFunction::Val | FloatSuppliedFunction::MaxLen => {
				let argument_value = match argument_values[0].clone().to_string() {
					Ok(argument_value) => argument_value,
					Err(error) => break 'a Err(error),
				};
				match supplied_function {
					FloatSuppliedFunction::Len => Ok(FloatValue::from_usize(argument_value.count_chars())),
					FloatSuppliedFunction::Ord => argument_value.value_of_char_or_mnemonic(&self.options).map(|value| FloatValue::from_u32(value)),
					FloatSuppliedFunction::Asc => argument_value.value_of_first_char(&self.options).map(|value| FloatValue::from_u32(value)),
					FloatSuppliedFunction::Val => match parse_datum_float(&argument_value.value, Some(&self.options)) {
						Ok((None, _)) => Err(ErrorVariant::MalformedNumber),
						Ok((Some(..), remaining)) if remaining.contains(|chr: char| !chr.is_ascii_whitespace()) => Err(ErrorVariant::MalformedNumber),
						Ok((Some(value), _)) => Ok(value),
						Err(error) => Err(error),
					}
					FloatSuppliedFunction::MaxLen => Ok(FloatValue::MAX),
					_ => unreachable!()
				}
			}
			// Functions that have two string arguments
			FloatSuppliedFunction::Pos => {
				let argument_value_0 = match argument_values[0].clone().to_string() {
					Ok(argument_value) => argument_value,
					Err(error) => break 'a Err(error),
				};
				let argument_value_1 = match argument_values[1].clone().to_string() {
					Ok(argument_value) => argument_value,
					Err(error) => break 'a Err(error),
				};
				match supplied_function {
					FloatSuppliedFunction::Pos => Ok(match argument_value_0.find_substring_char_index(&argument_value_1) {
						Some(index) => FloatValue::from_usize(index + 1),
						None => FloatValue::ZERO,
					}),
					_ => unreachable!()
				}
			}
			// Functions that have one float argument and one int argument
			FloatSuppliedFunction::Truncate | FloatSuppliedFunction::Round => {
				let argument_value_0 = match argument_values[0].clone().to_float() {
					Ok(argument_value) => argument_value,
					Err(error) => break 'a Err(error),
				};
				let argument_value_1 = match argument_values[1].clone().to_int() {
					Ok(argument_value) => argument_value,
					Err(error) => break 'a Err(error),
				};
				match supplied_function {
					FloatSuppliedFunction::Truncate => Ok(argument_value_0.truncate_to_digits(argument_value_1)),
					FloatSuppliedFunction::Round    => Ok(argument_value_0.round_to_digits(argument_value_1)),
					_ => unreachable!()
				}
			}
		}};
		// Return
		Ok(Some(error_at_column(result, *start_column)?))
	}

	pub fn execute_int_supplied_function(&mut self, l_value: &IntLValue, program: Option<&Program>) -> Result<Option<IntValue>, Error> {
		// Unpack
		let IntLValue { arguments: argument_expressions, start_column, supplied_function, .. } = l_value;
		let supplied_function = match supplied_function {
			Some(supplied_function) => *supplied_function,
			None => return Ok(None),
		};
		//
		let mut argument_values = Vec::with_capacity(argument_expressions.len());
		for argument_expression in argument_expressions {
			argument_values.push(self.execute_any_type_expression(argument_expression, program)?);
		}
		// Else try to execute a supplied (built-in) function
		let result = 'a: { match supplied_function {
			// Constant
			IntSuppliedFunction::True => Ok(IntValue::new(Rc::new((-1i8).into()))),
			IntSuppliedFunction::False => Ok(IntValue::zero()),
			// Time
			IntSuppliedFunction::Time => Ok(IntValue::from_u32(Local::now().num_seconds_from_midnight())),
			IntSuppliedFunction::Date => Ok({
				let date = Local::now();
				let mut day_of_year = date.day();
				for month in 1..date.month() {
					day_of_year += date.with_day(1).unwrap().with_month(month).unwrap().num_days_in_month() as u32;
				}
				IntValue::from_u32((date.year() % 100) as u32 * 1000 + day_of_year)
			}),
			IntSuppliedFunction::Second => Ok(IntValue::from_u32(Local::now().second())),
			IntSuppliedFunction::Minute => Ok(IntValue::from_u32(Local::now().minute())),
			IntSuppliedFunction::Hour => Ok(IntValue::from_u32(Local::now().hour())),
			IntSuppliedFunction::Day => Ok(IntValue::from_u32(Local::now().day())),
			IntSuppliedFunction::Month => Ok(IntValue::from_u32(Local::now().month())),
			IntSuppliedFunction::Year => Ok(IntValue::from_i32(Local::now().year())),
			// Functions that have one int argument
			IntSuppliedFunction::Sqr | IntSuppliedFunction::Abs | IntSuppliedFunction::Log2 | IntSuppliedFunction::Log10 => {
				let argument_value = match argument_values[0].clone().to_int() {
					Ok(argument_value) => argument_value,
					Err(error) => break 'a Err(error),
				};
				match supplied_function {
					IntSuppliedFunction::Sqr => argument_value.sqrt(),
					IntSuppliedFunction::Abs => Ok(argument_value.abs()),
					IntSuppliedFunction::Log2 => argument_value.ilog2(),
					IntSuppliedFunction::Log10 => argument_value.ilog10(),
					_ => unreachable!()
				}
			}
			// Fold functions
			IntSuppliedFunction::Xor | IntSuppliedFunction::Min | IntSuppliedFunction::Max => {
				// Get first argument
				let mut result = match argument_values[0].clone().to_int() {
					Ok(argument_value) => argument_value,
					Err(error) => break 'a Err(error),
				};
				// Get other values and apply function
				for argument_values in argument_values.iter().skip(1) {
					let argument_value = match argument_values.clone().to_int() {
						Ok(argument_value) => argument_value,
						Err(error) => break 'a Err(error),
					};
					result = match supplied_function {
						IntSuppliedFunction::Xor => result.xor(argument_value),
						IntSuppliedFunction::Min => result.min(argument_value),
						IntSuppliedFunction::Max => result.max(argument_value),
						_ => unreachable!(),
					};
				}
				Ok(result)
			}
			// Functions that have one argument that can be multiple types
			IntSuppliedFunction::Floor | IntSuppliedFunction::Ceil | IntSuppliedFunction::Ip | IntSuppliedFunction::Sgn => {
				let argument_value = argument_values[0].clone();
				match supplied_function {
					IntSuppliedFunction::Floor => argument_value.floor_to_int(),
					IntSuppliedFunction::Ceil => argument_value.ceil_to_int(),
					IntSuppliedFunction::Ip => argument_value.integer_part_to_int(),
					IntSuppliedFunction::Sgn => argument_value.signum_to_int(),
					_ => unreachable!()
				}
			}
			// Functions that have one string argument
			IntSuppliedFunction::Len => {
				let argument_value = match argument_values[0].clone().to_string() {
					Ok(argument_value) => argument_value,
					Err(error) => break 'a Err(error),
				};
				return Ok(Some(match supplied_function {
					IntSuppliedFunction::Len => IntValue::from_usize(argument_value.count_chars()),
					_ => unreachable!()
				}))
			}
		}};
		// Return
		Ok(Some(error_at_column(result, *start_column)?))
	}

	pub fn execute_complex_supplied_function(&mut self, l_value: &ComplexLValue, program: Option<&Program>) -> Result<Option<ComplexValue>, Error> {
		// Unpack
		let ComplexLValue { arguments: argument_expressions, start_column, supplied_function, .. } = l_value;
		let supplied_function = match supplied_function {
			Some(supplied_function) => *supplied_function,
			None => return Ok(None),
		};
		//
		let mut argument_values = Vec::with_capacity(argument_expressions.len());
		for argument_expression in argument_expressions {
			argument_values.push(self.execute_any_type_expression(argument_expression, program)?);
		}
		// Else try to execute a supplied (built-in) function
		let result = 'a: { match supplied_function {
			// Constants
			ComplexSuppliedFunction::I => Ok(ComplexValue::I),
			// Functions that have one complex number as an argument
			ComplexSuppliedFunction::Sqr | ComplexSuppliedFunction::Exp | ComplexSuppliedFunction::LogE | ComplexSuppliedFunction::Log2 | ComplexSuppliedFunction::Log10 | ComplexSuppliedFunction::Conj |
			ComplexSuppliedFunction::Sin | ComplexSuppliedFunction::Cos | ComplexSuppliedFunction::Tan | ComplexSuppliedFunction::Cot | ComplexSuppliedFunction::Sec | ComplexSuppliedFunction::Csc |
			ComplexSuppliedFunction::Asin | ComplexSuppliedFunction::Acos | ComplexSuppliedFunction::Atan | ComplexSuppliedFunction::Acot | ComplexSuppliedFunction::Asec | ComplexSuppliedFunction::Acsc |
			ComplexSuppliedFunction::Sinh | ComplexSuppliedFunction::Cosh | ComplexSuppliedFunction::Tanh | ComplexSuppliedFunction::Coth | ComplexSuppliedFunction::Sech | ComplexSuppliedFunction::Csch |
			ComplexSuppliedFunction::Asinh | ComplexSuppliedFunction::Acosh | ComplexSuppliedFunction::Atanh | ComplexSuppliedFunction::Acoth | ComplexSuppliedFunction::Asech | ComplexSuppliedFunction::Acsch => {
				let argument_value = match argument_values[0].clone().to_complex() {
					Ok(argument_value) => argument_value,
					Err(error) => break 'a Err(error),
				};
				match supplied_function {
					ComplexSuppliedFunction::Sqr   => argument_value.sqrt(&self.options),
					ComplexSuppliedFunction::Exp   => argument_value.exp(&self.options),
					ComplexSuppliedFunction::LogE  => argument_value.ln(&self.options),
					ComplexSuppliedFunction::Log2  => argument_value.log2(&self.options),
					ComplexSuppliedFunction::Log10 => argument_value.log10(&self.options),
					ComplexSuppliedFunction::Conj  => Ok(argument_value.conj()),

					ComplexSuppliedFunction::Sin   => argument_value.sin(&self.options),
					ComplexSuppliedFunction::Cos   => argument_value.cos(&self.options),
					ComplexSuppliedFunction::Tan   => argument_value.tan(&self.options),
					ComplexSuppliedFunction::Cot   => argument_value.cot(&self.options),
					ComplexSuppliedFunction::Sec   => argument_value.sec(&self.options),
					ComplexSuppliedFunction::Csc   => argument_value.csc(&self.options),
					ComplexSuppliedFunction::Asin  => argument_value.asin(&self.options),
					ComplexSuppliedFunction::Acos  => argument_value.acos(&self.options),
					ComplexSuppliedFunction::Atan  => argument_value.atan(&self.options),
					ComplexSuppliedFunction::Acot  => argument_value.acot(&self.options),
					ComplexSuppliedFunction::Asec  => argument_value.asec(&self.options),
					ComplexSuppliedFunction::Acsc  => argument_value.acsc(&self.options),

					ComplexSuppliedFunction::Sinh  => argument_value.sinh(&self.options),
					ComplexSuppliedFunction::Cosh  => argument_value.cosh(&self.options),
					ComplexSuppliedFunction::Tanh  => argument_value.tanh(&self.options),
					ComplexSuppliedFunction::Coth  => argument_value.coth(&self.options),
					ComplexSuppliedFunction::Sech  => argument_value.sech(&self.options),
					ComplexSuppliedFunction::Csch  => argument_value.csch(&self.options),
					ComplexSuppliedFunction::Asinh => argument_value.asinh(&self.options),
					ComplexSuppliedFunction::Acosh => argument_value.acosh(&self.options),
					ComplexSuppliedFunction::Atanh => argument_value.atanh(&self.options),
					ComplexSuppliedFunction::Acoth => argument_value.acoth(&self.options),
					ComplexSuppliedFunction::Asech => argument_value.asech(&self.options),
					ComplexSuppliedFunction::Acsch => argument_value.acsch(&self.options),

					_ => unreachable!(),
				}
			}
			// Functions that have two complex arguments
			ComplexSuppliedFunction::LogN => {
				let argument_value_0 = match argument_values[0].clone().to_complex() {
					Ok(argument_value) => argument_value,
					Err(error) => break 'a Err(error),
				};
				let argument_value_1 = match argument_values[1].clone().to_complex() {
					Ok(argument_value) => argument_value,
					Err(error) => break 'a Err(error),
				};
				match supplied_function {
					ComplexSuppliedFunction::LogN => argument_value_1.log(argument_value_0, &self.options),
					_ => unreachable!()
				}
			}
		}};
		// Return
		Ok(Some(error_at_column(result, *start_column)?))
	}

	pub fn execute_string_supplied_function(&mut self, l_value: &StringLValue, program: Option<&Program>) -> Result<Option<StringValue>, Error> {
		// Unpack
		let StringLValue { arguments: argument_expressions, start_column, supplied_function, .. } = l_value;
		let supplied_function = match supplied_function {
			Some(supplied_function) => *supplied_function,
			None => return Ok(None),
		};
		//
		let mut argument_values = Vec::with_capacity(argument_expressions.len());
		for argument_expression in argument_expressions {
			argument_values.push(self.execute_any_type_expression(argument_expression, program)?);
		}
		// Else try to execute a supplied (built-in) function
		let result = 'a: { match supplied_function {
			// Constants and pseudo-variables
			StringSuppliedFunction::Time => Ok({
				let time = Local::now();
				StringValue::new(Rc::new(format!("{:02}:{:02}:{:02}", time.hour(), time.minute(), time.second())))
			}),
			StringSuppliedFunction::Date => Ok({
				let time = Local::now();
				StringValue::new(Rc::new(format!("{:04}{:02}{:02}", time.year(), time.month(), time.day())))
			}),
			// Functions that have one string argument
			StringSuppliedFunction::UCase | StringSuppliedFunction::LCase | StringSuppliedFunction::LTrim | StringSuppliedFunction::RTrim | StringSuppliedFunction::Left1Arg | StringSuppliedFunction::Right1Arg => {
				let argument_value = match argument_values[0].clone().to_string() {
					Ok(argument_value) => argument_value,
					Err(error) => break 'a Err(error),
				};
				match supplied_function {
					StringSuppliedFunction::UCase     => Ok(argument_value.to_uppercase()),
					StringSuppliedFunction::LCase     => Ok(argument_value.to_lowercase()),
					StringSuppliedFunction::LTrim     => Ok(argument_value.trim_start_spaces()),
					StringSuppliedFunction::RTrim     => Ok(argument_value.trim_end_spaces()),
					StringSuppliedFunction::Left1Arg  => argument_value.pop_last_char(),
					StringSuppliedFunction::Right1Arg => argument_value.take_last_char(),
					_ => unreachable!(),
				}
			}
			// Functions that have one int argument
			StringSuppliedFunction::Chr => {
				let argument_value = match argument_values[0].clone().to_int() {
					Ok(argument_value) => argument_value,
					Err(error) => break 'a Err(error),
				};
				match supplied_function {
					StringSuppliedFunction::Chr => StringValue::from_char_value(argument_value, &self.options),
					_ => unreachable!(),
				}
			}
			// Functions that have one string argument and an int argument
			StringSuppliedFunction::Repeat | StringSuppliedFunction::Left2Args | StringSuppliedFunction::Right2Args | StringSuppliedFunction::Mid2Args => {
				let argument_value_0 = match argument_values[0].clone().to_string() {
					Ok(argument_value) => argument_value,
					Err(error) => break 'a Err(error),
				};
				let argument_value_1 = match argument_values[1].clone().to_int() {
					Ok(argument_value) => argument_value,
					Err(error) => break 'a Err(error),
				};
				match supplied_function {
					StringSuppliedFunction::Repeat => argument_value_0.repeat(argument_value_1),
					StringSuppliedFunction::Left2Args => argument_value_0.take_left_chars(argument_value_1),
					StringSuppliedFunction::Right2Args | StringSuppliedFunction::Mid2Args => argument_value_0.take_right_chars(argument_value_1),
					_ => unreachable!(),
				}
			}
			// Functions that have one string argument and two int arguments
			StringSuppliedFunction::Mid3Args => {
				let argument_value_0 = match argument_values[0].clone().to_string() {
					Ok(argument_value) => argument_value,
					Err(error) => break 'a Err(error),
				};
				let argument_value_1 = match argument_values[1].clone().to_int() {
					Ok(argument_value) => argument_value,
					Err(error) => break 'a Err(error),
				};
				let argument_value_2 = match argument_values[2].clone().to_int() {
					Ok(argument_value) => argument_value,
					Err(error) => break 'a Err(error),
				};
				match supplied_function {
					StringSuppliedFunction::Mid3Args => argument_value_0.take_middle_chars(argument_value_1, argument_value_2),
					_ => unreachable!(),
				}
			}
			// Functions that have one argument that can be multiple types
			StringSuppliedFunction::Str => {
				let argument_value = argument_values[0].clone();
				if matches!(argument_value, AnyTypeValue::String(..)) {
					break 'a Err(ErrorVariant::Unimplemented("STR$ function used on string".into()));
				}
				match supplied_function {
					StringSuppliedFunction::Str => {
						let mut string_bytes = Vec::new();
						let mut string_cursor = Cursor::new(&mut string_bytes);
						argument_value.print(&mut string_cursor, false, false, &self.options).unwrap();
						Ok(StringValue::new(Rc::new(String::from_utf8(string_bytes).unwrap())))
					}
					_ => unreachable!()
				}
			}
		}};
		// Return
		Ok(Some(error_at_column(result, *start_column)?))
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
pub enum BlockOnStack {
	IntForLoop { name: Box<str>, final_value: IntValue, step_value: IntValue, for_line: Option<Rc<BigInt>>, for_sub_line: usize, is_zero_cycles: bool },
	FloatForLoop { name: Box<str>, final_value: FloatValue, step_value: FloatValue, for_line: Option<Rc<BigInt>>, for_sub_line: usize, is_zero_cycles: bool },
}

pub struct GosubLevel {
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

	/// Writes an element of an array at the given indices.
	pub fn get_element_mut(&mut self, indices: &[IntValue]) -> Result<&mut Option<T>, ErrorVariant> {
		let index = self.indices_to_elements_index(indices)?;
		Ok(&mut self.elements[index])
	}
}

/// A list of variables and arrays of type `T` stored in the MavagkBasic virtual machine as well as functions that return values of type `T`.
pub struct StoredValues<T: Value> {
	/// Variables of type `T`.
	simple_variables: HashMap<Box<str>, T>,
	/// Arrays of values of type `T`.
	arrays: HashMap<Box<str>, Array<T>>,
	/// Functions that return values of type `T`.
	functions: HashMap<(Box<str>, usize), (Box<[AnyTypeLValue]>, T::ExpressionType, Option<(Rc<BigInt>, usize)>)>,
}

impl<T: Value> StoredValues<T> {
	/// Create a new stored value container containing no stored values.
	pub fn new() -> Self {
		Self {
			simple_variables: HashMap::new(),
			arrays: HashMap::new(),
			functions: HashMap::new(),
		}
	}
}