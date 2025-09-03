use std::{f64::{INFINITY, NEG_INFINITY}, fmt::{self, Display, Formatter}, rc::Rc};

use num::{complex::Complex64, BigInt, ToPrimitive, Zero};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IntValue {
	pub value: Rc<BigInt>,
}

impl IntValue {
	pub fn new(value: Rc<BigInt>) -> Self {
		Self {
			value,
		}
	}
}

impl Display for IntValue {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.value)
	}
}

#[derive(Debug, PartialEq, Clone)]
pub enum RealValue {
	IntValue(Rc<BigInt>),
	FloatValue(f64),
}

impl RealValue {
	pub fn get_float(&self) -> f64 {
		match self {
			Self::FloatValue(float_value) => *float_value,
			Self::IntValue(int_value) => match int_value.to_i128() {
				Some(int_value) => int_value as f64,
				None => match &**int_value > &BigInt::ZERO {
					true => INFINITY,
					false => NEG_INFINITY,
				}
			},
		}
	}

	pub fn is_zero(&self) -> bool {
		match self {
			Self::FloatValue(float_value) => *float_value == 0.,
			Self::IntValue(int_value) => int_value.is_zero(),
		}
	}
}

impl Display for RealValue {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			Self::FloatValue(value) => write!(f, "{}", value),
			Self::IntValue(value) => write!(f, "{}", value),
		}
	}
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct ComplexValue {
	pub value: Complex64,
}

impl Display for ComplexValue {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.value)
	}
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StringValue {
	pub value: Rc<String>,
}

impl StringValue {
	pub fn new(value: Rc<String>) -> Self {
		Self {
			value,
		}
	}
}

impl Display for StringValue {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.value)
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct BoolValue {
	pub value: bool,
}

impl Display for BoolValue {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "{}", match self.value {
			true => -1i8,
			false => 0,
		})
	}
}