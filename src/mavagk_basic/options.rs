use crate::mavagk_basic::{error::ErrorVariant, value::IntValue};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum AngleOption {
	Radians,
	Degrees,
	Gradians,
	Revolutions,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum MathOption {
	AnsiFull,
	EcmaMinimal,
	Ieee,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum MachineOption {
	AnsiFull,
	EcmaMinimal,
	C64,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BaseOption {
	Zero,
	One,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum CollateOption {
	/// Unicode
	Native,
	/// ASCII
	Standard,
	C64UnshiftedPETSCII,
}

impl CollateOption {
	pub fn from_u32(self, int_value: u32) -> Result<char, ErrorVariant> {
		match self {
			CollateOption::Standard => match int_value < 128 {
				true => Ok(int_value as u8 as char),
				false => Err(ErrorVariant::InvalidCharValue),
			}
			CollateOption::Native => match char::from_u32(int_value) {
				Some(chr) => Ok(chr),
				None => Err(ErrorVariant::InvalidCharValue),
			}
			CollateOption::C64UnshiftedPETSCII => match int_value {
				/*0x20..=0x40 => Ok(int_value as u8 as char),
				0x41..=0x5A => Ok(((int_value as u8) - 0x41 + b'A') as char),
				0x5B => Ok('['),
				0x5C => Ok('£'),
				0x5D => Ok(']'),
				0x5E => Ok('↑'),
				0x5F => Ok('←'),
				0x60 | 0xC0 => Ok('─'),
				0x61 | 0xC1 => Ok('♠'),

				0x69 | 0xC9 => Ok('╮'),
				0x6A | 0xCA => Ok('╰'),
				0x6B | 0xCB => Ok('╯'),
				
				0x6D | 0xCD => Ok('╲'),
				0x6E | 0xCE => Ok('╱'),

				0x71 | 0xD1 => Ok('•'),

				0x73 | 0xD3 => Ok('♥'),

				0x75 | 0xD5 => Ok('╭'),
				0x76 | 0xD6 => Ok('╳'),
				0x77 | 0xD7 => Ok('○'),
				0x78 | 0xD8 => Ok('♣'),

				0x7A | 0xDA => Ok('♦'),
				0x7B | 0xDB => Ok('┼'),

				0x7D | 0xDD => Ok('│'),
				0x7E | 0xDE => Ok('π'),
				0x7F | 0xDF => Ok('◥'),

				0xA1 | 0xE1 => Ok('▌'),
				0xA2 | 0xE2 => Ok('▄'),
				0xA3 | 0xE3 => Ok('▔'),
				0xA4 | 0xE4 => Ok('▁'),
				0xA5 | 0xE5 => Ok('▏'),
				0xA6 | 0xE6 => Ok('▒'),
				0xA7 | 0xE7 => Ok('▕'),

				0xA9 | 0xE9 => Ok('◤'),

				0xAB | 0xEB => Ok('├'),
				0xAC | 0xEC => Ok('▗'),
				0xAD | 0xED => Ok('└'),
				0xAE | 0xEE => Ok('┐'),
				0xAF | 0xEF => Ok('▂'),
				0xB0 | 0xF0 => Ok('┌'),
				0xB1 | 0xF1 => Ok('┴'),
				0xB2 | 0xF2 => Ok('┬'),
				0xB3 | 0xF3 => Ok('┤'),
				0xB4 | 0xF4 => Ok('▎'),
				0xB5 | 0xF5 => Ok('▍'),

				0xB9 | 0xF9 => Ok('▃'),

				0xBB | 0xFB => Ok('▖'),
				0xBC | 0xFC => Ok('▝'),
				0xBD | 0xFD => Ok('┘'),
				0xBE | 0xFE => Ok('▘'),
				0xBF => Ok('▚'),

				0xFF => Ok('π'),

				0x100.. => Err(ErrorVariant::InvalidCharValue),
				_ => Ok('?'),*/
				0x100.. => Err(ErrorVariant::InvalidCharValue),
				_ => return Err(ErrorVariant::NotYetImplemented("Code point values of chars with PETSCII char set.".into())),
			},
		}
	}

	pub fn to_u32(self, chr: char) -> Result<u32, ErrorVariant> {
		match self {
			CollateOption::Standard => {
				let value = chr as u32;
				if value > 127 {
					return Err(ErrorVariant::NotYetImplemented("Code point values of non-ASCII chars while OPTION COLLATE STANDARD is set.".into()));
				}
				Ok(value)
			}
			CollateOption::Native => Ok(self as u32),
			CollateOption::C64UnshiftedPETSCII => {
				return Err(ErrorVariant::NotYetImplemented("Code point values of chars with PETSCII char set.".into()));
			}
		}
	}
}

/// The set of OPTIONs set at a given time.
#[derive(Debug, Clone)]
pub struct Options {
	pub angle: Option<AngleOption>,
	pub math: Option<MathOption>,
	pub machine: Option<MachineOption>,
	pub base: Option<BaseOption>,
	pub collate: Option<CollateOption>,
}

impl Options {
	/// Get a default set of options for when a at the start of a program before all OPTION statements.
	pub fn new() -> Self {
		Self {
			angle: None,
			base: None,
			machine: None,
			math: None,
			collate: None,
		}
	}

	pub const fn get_math_option(&self) -> MathOption {
		match self.math {
			None => MathOption::EcmaMinimal,
			Some(math_option) => math_option,
		}
	}

	pub const fn get_angle_option(&self) -> AngleOption {
		match self.angle {
			None => AngleOption::Radians,
			Some(math_option) => math_option,
		}
	}

	pub const fn get_machine_option(&self) -> MachineOption {
		match self.machine {
			None => MachineOption::EcmaMinimal,
			Some(math_option) => math_option,
		}
	}

	pub const fn get_base_option(&self) -> BaseOption {
		match self.base {
			None => match self.get_machine_option() {
				MachineOption::EcmaMinimal | MachineOption::C64 => BaseOption::Zero,
				MachineOption::AnsiFull => BaseOption::One,
			}
			Some(base_option) => base_option,
		}
	}

	pub const fn get_collate_option(&self) -> CollateOption {
		match self.collate {
			None => match self.get_machine_option() {
				MachineOption::EcmaMinimal | MachineOption::AnsiFull => CollateOption::Standard,
				MachineOption::C64 => CollateOption::C64UnshiftedPETSCII,
			},
			Some(collate_option) => collate_option,
		}
	}

	/// Returns false if taking the real square root of a negative number should throw an error, returns true if it should return NaN.
	pub const fn allow_real_square_root_of_negative(&self) -> bool {
		match self.get_math_option() {
			MathOption::Ieee => true,
			MathOption::AnsiFull | MathOption::EcmaMinimal => false,
		}
	}

	/// Returns false if numeric overflow should throw an error, returns true if it should return a non finite value.
	pub const fn allow_overflow(&self) -> bool {
		match self.get_math_option() {
			MathOption::Ieee => true,
			MathOption::AnsiFull | MathOption::EcmaMinimal => false,
		}
	}

	/// Returns false if evaluating the tanh(0) or csch(0) should throw an error, returns true if it should return a non finite value.
	pub const fn allow_tanh_csch_of_zero(&self) -> bool {
		match self.get_math_option() {
			MathOption::Ieee => true,
			MathOption::AnsiFull | MathOption::EcmaMinimal => false,
		}
	}

	/// Returns false if evaluating inverse hyperbolic trigonometric functions outside of their range should throw an error, returns true if it should return a non finite value.
	pub const fn allow_inverse_hyperbolic_trig_out_of_range(&self) -> bool {
		match self.get_math_option() {
			MathOption::Ieee => true,
			MathOption::AnsiFull | MathOption::EcmaMinimal => false,
		}
	}

	/// Returns false if division by zero should throw an error, returns true if it should return a non finite value.
	pub const fn allow_divide_by_zero(&self) -> bool {
		match self.get_math_option() {
			MathOption::Ieee => true,
			MathOption::AnsiFull | MathOption::EcmaMinimal => false,
		}
	}

	/// Returns false if ANGLE(0, 0) or ATAN2(0, 0) should throw an error, returns true if it should return a non finite value.
	pub const fn allow_angle_zero_zero(&self) -> bool {
		match self.get_math_option() {
			MathOption::Ieee => true,
			MathOption::AnsiFull | MathOption::EcmaMinimal => false,
		}
	}

	/// Returns if arc real trigonometric functions given values outside their input range should trow an error.
	pub const fn allow_real_trig_out_of_range(&self) -> bool {
		match self.get_math_option() {
			MathOption::Ieee => true,
			MathOption::AnsiFull | MathOption::EcmaMinimal => false,
		}
	}

	/// Returns if real log functions given non-negative arguments should trow an error.
	pub const fn allow_real_log_of_non_positive(&self) -> bool {
		match self.get_math_option() {
			MathOption::Ieee => true,
			MathOption::AnsiFull | MathOption::EcmaMinimal => false,
		}
	}

	/// Returns if reading an uninitialized value should trow an error.
	pub const fn allow_uninitialized_read(&self) -> bool {
		match self.get_machine_option() {
			MachineOption::AnsiFull | MachineOption::EcmaMinimal => false,
			MachineOption::C64 => true,
		}
	}

	/// Returns if a DIM statement will create an array or if it will be created when first accessed.
	pub const fn arrays_created_on_dim_execution(&self) -> bool {
		match self.get_machine_option() {
			MachineOption::AnsiFull | MachineOption::EcmaMinimal => false,
			MachineOption::C64 => true,
		}
	}

	/// Returns if a FN statement will define a function or if it will be created when first accessed.
	pub const fn functions_defined_on_fn_execution(&self) -> bool {
		match self.get_machine_option() {
			MachineOption::AnsiFull | MachineOption::EcmaMinimal => false,
			MachineOption::C64 => true,
		}
	}

	/// Returns `true` if a INPUT statement should always print a question mark and then a space after printing the prompt.
	/// Returns `false` if a question mark and then a space should only be printed if there is no prompt.
	pub const fn always_print_question_mark_after_input_prompt(&self) -> bool {
		match self.get_machine_option() {
			MachineOption::AnsiFull | MachineOption::EcmaMinimal => false,
			MachineOption::C64 => true,
		}
	}

	/// Returns the minimum value for an array dimension if not explicitly specified using the `x TO y` syntax.
	pub fn get_minimum_array_value(&self) -> IntValue {
		match self.get_base_option() {
			BaseOption::Zero => IntValue::zero(),
			BaseOption::One => IntValue::one(),
		}
	}

	/// Returns if a FOR statement that has a condition that is initially false should jump to the next unnested NEXT statement with the same control variable.
	pub fn for_initially_false_jumps_to_next(&self) -> bool {
		match self.get_machine_option() {
			MachineOption::AnsiFull | MachineOption::EcmaMinimal => true,
			MachineOption::C64 => false,
		}
	}

	/// Returns false if non-complex negative number raised to a non-complex non-integer power should throw an error, returns true if it should return NaN.
	pub fn allow_negative_to_non_int_power(&self) -> bool {
		match self.get_math_option() {
			MathOption::Ieee => true,
			MathOption::AnsiFull | MathOption::EcmaMinimal => false,
		}
	}

	/// Returns the tab column width.
	pub fn get_print_zone_width(&self) -> u8 {
		match self.get_machine_option() {
			MachineOption::C64 => 10,
			MachineOption::AnsiFull | MachineOption::EcmaMinimal => 20,
		}
	}

	/// Returns the index of the leftmost columnar position.
	pub fn get_columnar_first_position(&self) -> u8 {
		match self.get_machine_option() {
			MachineOption::C64 => 0,
			MachineOption::AnsiFull | MachineOption::EcmaMinimal => 1,
		}
	}
}