use std::{f64::{INFINITY, NEG_INFINITY}, fmt::{self, Display, Formatter}, num::NonZeroUsize, rc::Rc};

use num::{complex::Complex64, BigInt, FromPrimitive, ToPrimitive, Zero, Signed};

use crate::mavagk_basic::error::{Error, ErrorVariant};

pub fn float_to_int(float_value: f64) -> Option<BigInt> {
	BigInt::from_f64(float_value)
}

pub fn int_to_float(int_value: &BigInt) -> f64 {
	match int_value.to_i128() {
		Some(int_value) => int_value as f64,
		None => match int_value.is_positive() {
			true => INFINITY,
			false => NEG_INFINITY,
		}
	}
}

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

	pub fn zero() -> Self {
		Self::new(Rc::new(BigInt::ZERO))
	}

	pub fn is_zero(&self) -> bool {
		self.value.is_zero()
	}
	
	pub fn to_bool(&self) -> BoolValue {
		BoolValue::new(!self.is_zero())
	}

	pub fn to_real(self) -> RealValue {
		RealValue::IntValue(self.value)
	}

	pub fn to_complex(&self) -> ComplexValue {
		ComplexValue::new(Complex64::new(int_to_float(&self.value), 0.))
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
			Self::IntValue(int_value) => int_to_float(int_value),
		}
	}

	pub fn is_zero(&self) -> bool {
		match self {
			Self::FloatValue(float_value) => float_value.is_zero(),
			Self::IntValue(int_value) => int_value.is_zero(),
		}
	}

	pub fn to_bool(&self) -> BoolValue {
		BoolValue::new(!self.is_zero())
	}

	pub fn to_int(self, line_number: Option<&BigInt>, start_column: NonZeroUsize) -> Result<IntValue, Error> {
		match self {
			Self::IntValue(value) => Ok(IntValue { value }),
			Self::FloatValue(value) => match float_to_int(value) {
				Some(result) => Ok(IntValue::new(Rc::new(result))),
				None => Err(Error{
					variant: ErrorVariant::NonNumberValueCastToInt(value), line_number: line_number.cloned(), column_number: Some(start_column), line_text: None
				}),
			}
		}
	}

	pub fn to_complex(&self) -> ComplexValue {
		match self {
			Self::FloatValue(value) => ComplexValue::new(Complex64::new(*value, 0.)),
			Self::IntValue(value) => ComplexValue::new(Complex64::new(int_to_float(&value), 0.)),
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

impl ComplexValue {
	pub fn new(value: Complex64) -> Self {
		Self {
			value,
		}
	}

	pub fn is_zero(&self) -> bool {
		self.value.is_zero()
	}

	pub fn to_bool(&self) -> BoolValue {
		BoolValue::new(!self.is_zero())
	}

	pub fn to_int(self, line_number: Option<&BigInt>, start_column: NonZeroUsize) -> Result<IntValue, Error> {
		if !self.value.re.is_zero() {
			return Err(Error { variant: ErrorVariant::NonRealComplexValueCastToReal(self.value), line_number: line_number.cloned(), column_number: Some(start_column), line_text: None })
		}
		match float_to_int(self.value.re) {
			Some(result) => Ok(IntValue::new(Rc::new(result))),
			None => Err(Error{
				variant: ErrorVariant::NonNumberValueCastToInt(self.value.re), line_number: line_number.cloned(), column_number: Some(start_column), line_text: None
			}),
		}
	}

	pub fn to_real(self, line_number: Option<&BigInt>, start_column: NonZeroUsize) -> Result<RealValue, Error> {
		if !self.value.re.is_zero() {
			return Err(Error { variant: ErrorVariant::NonRealComplexValueCastToReal(self.value), line_number: line_number.cloned(), column_number: Some(start_column), line_text: None })
		}
		Ok(RealValue::FloatValue(self.value.re))
	}
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

	pub fn to_bool(&self) -> BoolValue {
		BoolValue::new(!self.value.is_empty())
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

impl BoolValue {
	pub fn new(value: bool) -> Self {
		Self {
			value,
		}
	}

	pub fn is_zero(self) -> bool {
		!self.value
	}

	pub fn to_bool(self) -> BoolValue {
		BoolValue::new(!self.is_zero())
	}

	pub fn to_int(self) -> IntValue {
		IntValue::new(Rc::new(match self.value {
			true => BigInt::ZERO,
			false => BigInt::from_i8(-1).unwrap(),
		}))
	}

	pub fn to_real(self) -> RealValue {
		RealValue::IntValue(Rc::new(match self.value {
			true => BigInt::ZERO,
			false => BigInt::from_i8(-1).unwrap(),
		}))
	}

	pub fn to_complex(&self) -> ComplexValue {
		ComplexValue::new(Complex64::new(match self.value {
			true => 0.,
			false => -1.,
		}, 0.))
	}
}

impl Display for BoolValue {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "{}", match self.value {
			true => -1i8,
			false => 0,
		})
	}
}

#[derive(Debug, Clone)]
pub enum AnyTypeValue {
	Int(IntValue),
	Real(RealValue),
	Complex(ComplexValue),
	Bool(BoolValue),
	String(StringValue),
}

impl AnyTypeValue {
	pub fn to_bool(&self) -> BoolValue {
		match self {
			Self::Bool(value) => *value,
			Self::Int(value) => value.to_bool(),
			Self::Real(value) => value.to_bool(),
			Self::Complex(value) => value.to_bool(),
			Self::String(value) => value.to_bool(),
		}
	}
}