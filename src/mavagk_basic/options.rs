use crate::mavagk_basic::value::IntValue;

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
	// Unicode
	Native,
	// ASCII
	Standard,
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
			None => CollateOption::Standard,
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