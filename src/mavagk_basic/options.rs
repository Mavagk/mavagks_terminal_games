#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum AngleOption {
	Radians,
	Degrees,
	Gradians,
	Revolutions,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum MathOption {
	Ansi,
	Ieee,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum MachineOption {
	Ansi,
	C64,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BaseOption {
	Zero,
	One,
}

/// The set of OPTIONs set at a given time.
#[derive(Debug, Clone)]
pub struct Options {
	pub angle: Option<AngleOption>,
	pub math: Option<MathOption>,
	pub machine: Option<MachineOption>,
	pub base: Option<BaseOption>,
}

impl Options {
	pub fn new() -> Self {
		Self {
			angle: None,
			base: None,
			machine: None,
			math: None,
		}
	}

	pub const fn get_math_option(&self) -> MathOption {
		match self.math {
			None => MathOption::Ansi,
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
			None => MachineOption::Ansi,
			Some(math_option) => math_option,
		}
	}

	pub const fn get_base_option(&self) -> BaseOption {
		match self.base {
			None => BaseOption::Zero,
			Some(base_option) => base_option,
		}
	}

	/// Returns false if taking the real square root of a negative number should throw an error, returns true if it should return NaN.
	pub const fn allow_real_square_root_of_negative(&self) -> bool {
		match self.get_math_option() {
			MathOption::Ieee => true,
			MathOption::Ansi => false,
		}
	}

	/// Returns false if numeric overflow should throw an error, returns true if it should return a non finite value.
	pub const fn allow_overflow(&self) -> bool {
		match self.get_math_option() {
			MathOption::Ieee => true,
			MathOption::Ansi => false,
		}
	}

	/// Returns false if division by zero should throw an error, returns true if it should return a non finite value.
	pub const fn allow_divide_by_zero(&self) -> bool {
		match self.get_math_option() {
			MathOption::Ieee => true,
			MathOption::Ansi => false,
		}
	}

	/// Returns if arc real trigonometric functions given values outside their input range should trow an error.
	pub const fn allow_real_trig_out_of_range(&self) -> bool {
		match self.get_math_option() {
			MathOption::Ieee => true,
			MathOption::Ansi => false,
		}
	}

	/// Returns if real log functions given non-negative arguments should trow an error.
	pub const fn allow_real_log_of_non_positive(&self) -> bool {
		match self.get_math_option() {
			MathOption::Ieee => true,
			MathOption::Ansi => false,
		}
	}

	/// Returns if reading an uninitialized value should trow an error.
	pub const fn allow_uninitialized_read(&self) -> bool {
		match self.get_machine_option() {
			MachineOption::Ansi => false,
			MachineOption::C64 => true,
		}
	}

	/// Returns if a DIM statement will create an array or if it will be created when first accessed.
	pub const fn arrays_created_on_dim_execution(&self) -> bool {
		match self.get_machine_option() {
			MachineOption::Ansi => false,
			MachineOption::C64 => true,
		}
	}

	/// Returns if a FN statement will define a function or if it will be created when first accessed.
	pub const fn functions_defined_on_fn_execution(&self) -> bool {
		match self.get_machine_option() {
			MachineOption::Ansi => false,
			MachineOption::C64 => true,
		}
	}
}