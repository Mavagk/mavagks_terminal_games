use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::{game::game_trait::Game, get_input, mavagk_basic::{error::handle_error, machine::Machine, optimize::optimize_statement, parse::{parse_line, Tokens}, program::Program, token::Token}};

pub struct MavagkBasicTerminal {
	should_exit: bool,
	machine: Machine,
	program: Program,
}

impl Game for MavagkBasicTerminal {
	fn before_poll(&mut self) -> Result<(), String> {
		// Get the line of text
		let input = get_input();
		// Handle command
		let (terminal_command, text_after_terminal_command) = TerminalCommands::get_command_and_arguments(&*input);
		match terminal_command {
			// EXIT to return to the main menu
			Some(TerminalCommands::Exit) => self.should_exit = true,
			// TOKENS prints the tokens received from tokenizing the text entered
			Some(TerminalCommands::Tokens) => {
				let (line_number, tokens, error) = match Token::tokenize_line(text_after_terminal_command) {
					(line_number, Ok(tokens)) => (line_number, tokens, None),
					(line_number, Err(error)) => (line_number, Box::default(), Some(error)),
				};
				if let Some(error) = error {
					handle_error::<()>(Err(error.to_full_error(line_number, Some(text_after_terminal_command.into()))));
					return Ok(());
				}
				println!("{tokens:?}");
			}
			// AST prints the abstract syntax trees received from tokenizing and then parsing the text entered
			Some(TerminalCommands::AST) => {
				let (line_number, tokens, error) = match Token::tokenize_line(text_after_terminal_command) {
					(line_number, Ok(tokens)) => (line_number, tokens, None),
					(line_number, Err(error)) => (line_number, Box::default(), Some(error)),
				};
				if let Some(error) = error {
					handle_error::<()>(Err(error.to_full_error(line_number, Some(text_after_terminal_command.into()))));
					return Ok(());
				}
				let (trees, error) = parse_line(&mut Tokens::new(&tokens));
				if let Some(line_number) = &line_number {
					println!("Line: {line_number}");
				}
				for tree in trees {
					tree.print(0);
				}
				if let Some(error) = error {
					println!("Error{}", error.to_full_error(line_number, Some(text_after_terminal_command.into())));
				}
			}
			// ASTO prints the abstract syntax trees received from tokenizing and then parsing the text entered and optimizing the result
			Some(TerminalCommands::ASTOptimized) => {
				let (line_number, tokens, error) = match Token::tokenize_line(text_after_terminal_command) {
					(line_number, Ok(tokens)) => (line_number, tokens, None),
					(line_number, Err(error)) => (line_number, Box::default(), Some(error)),
				};
				if let Some(error) = error {
					handle_error::<()>(Err(error.to_full_error(line_number, Some(text_after_terminal_command.into()))));
					return Ok(());
				}
				let (mut trees, error) = parse_line(&mut Tokens::new(&tokens));
				for tree in trees.iter_mut() {
					optimize_statement(tree);
				}
				if let Some(line_number) = &line_number {
					println!("Line: {line_number}");
				}
				for tree in trees {
					tree.print(0);
				}
				if let Some(error) = error {
					println!("Error{}", error.to_full_error(line_number, Some(text_after_terminal_command.into())));
				}
			}
			// If a terminal command was not entered, enter the line of text into the MavagkBasic virtual machine
			None => {
				handle_error(self.machine.line_of_text_entered(input, &mut self.program));
			}
		}
		Ok(())
	}

	fn should_close(&self) -> bool {
		self.should_exit
	}
}

impl MavagkBasicTerminal {
	pub fn new() -> Self {
		Self {
			should_exit: false,
			machine: Machine::new(),
			program: Program::new(),
		}
	}
}

#[derive(Clone, Copy, EnumIter)]
pub enum TerminalCommands {
	Exit,
	Tokens,
	AST,
	ASTOptimized,
}

impl TerminalCommands {
	const fn get_names(self) -> &'static [&'static str] {
		match self {
			Self::Exit => &["exit", "quit"],
			Self::Tokens => &["tokens", "token"],
			Self::AST => &["ast", "asts", "tree", "trees"],
			Self::ASTOptimized => &["asto"],
		}
	}

	/// Takes in some text and returns (Some(command), text after terminal command keyword) if the first word is a terminal command keyword. Else returns (None, input unaltered)
	fn get_command_and_arguments<'a>(input: &'a str) -> (Option<Self>, &'a str) {
		// Get the first word and the input text after the first word
		let first_space_byte_index = match input.find(|chr: char| chr.is_ascii_whitespace()) {
			Some(first_space_byte_index) => first_space_byte_index,
			None => return (None, input),
		};
		let first_word_of_input = &input[..first_space_byte_index];
		let input_after_first_word = input[first_space_byte_index..].trim_ascii();
		// Get the command that the first word matches with
		for command in Self::iter() {
			let names = command.get_names();
			for name in names {
				if name.eq_ignore_ascii_case(first_word_of_input) {
					return (Some(command), input_after_first_word);
				}
			}
		}
		// Else return None
		(None, input)
	}
}