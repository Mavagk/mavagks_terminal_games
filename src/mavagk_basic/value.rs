use std::{fmt::{self, Display, Formatter}, rc::Rc};

use num::{complex::Complex64, BigInt};

#[derive(Debug, PartialEq, Eq)]
pub struct IntValue {
	value: Rc<BigInt>,
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

#[derive(Debug, PartialEq)]
pub enum RealValue {
	IntValue(Rc<BigInt>),
	FloatValue(f64),
}

impl Display for RealValue {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			Self::FloatValue(value) => write!(f, "{}", value),
			Self::IntValue(value) => write!(f, "{}", value),
		}
	}
}

#[derive(Debug, PartialEq)]
pub struct ComplexValue {
	pub value: Complex64,
}

impl Display for ComplexValue {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.value)
	}
}

#[derive(Debug, PartialEq, Eq)]
pub struct StringValue {
	value: Rc<String>,
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

#[derive(Debug, PartialEq, Eq)]
pub struct BoolValue {
	value: bool,
}

impl Display for BoolValue {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "{}", match self.value {
			true => -1i8,
			false => 0,
		})
	}
}