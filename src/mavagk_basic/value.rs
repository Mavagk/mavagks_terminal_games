use std::{f64::{INFINITY, NEG_INFINITY}, fmt::{self, Display, Formatter}, num::NonZeroUsize, rc::Rc};

use num::{complex::Complex64, BigInt, FromPrimitive, Signed, ToPrimitive, Zero};

use crate::mavagk_basic::error::{FullError, ErrorVariant};

pub fn float_to_int(float_value: f64) -> Option<BigInt> {
	BigInt::from_f64((float_value + 0.5).floor())
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

	pub fn is_negative(&self) -> bool {
		self.value.is_negative()
	}
	
	pub fn to_bool(&self) -> BoolValue {
		BoolValue::new(!self.is_zero())
	}

	pub fn to_float(self) -> FloatValue {
		FloatValue::new(int_to_float(&self.value))
	}

	pub fn to_complex(&self) -> ComplexValue {
		ComplexValue::new(Complex64::new(int_to_float(&self.value), 0.))
	}

	pub fn floored_div(mut self, rhs: Self) -> Option<IntValue> {
		match rhs.is_zero() {
			false => Some({
				let int = Rc::<BigInt>::make_mut(&mut self.value);
				(*int) /= &*rhs.value;
				Self::new(self.value)
			}),
			true => None,
		}
	}

	pub fn and(mut self, rhs: Self) -> Self {
		let int = Rc::<BigInt>::make_mut(&mut self.value);
		(*int) &= &*rhs.value;
		self
	}

	pub fn or(mut self, rhs: Self) -> Self {
		let int = Rc::<BigInt>::make_mut(&mut self.value);
		(*int) |= &*rhs.value;
		self
	}

	pub fn xor(mut self, rhs: Self) -> Self {
		let int = Rc::<BigInt>::make_mut(&mut self.value);
		(*int) ^= &*rhs.value;
		self
	}

	pub fn not(self) -> Self {
		Self::new(Rc::new(!&*self.value))
	}

	pub fn neg(self) -> Self {
		Self::new(Rc::new(-&*self.value))
	}

	pub fn equal_to(&self, rhs: &Self) -> BoolValue {
		BoolValue::new(self.value == rhs.value)
	}

	pub fn not_equal_to(&self, rhs: &Self) -> BoolValue {
		BoolValue::new(self.value != rhs.value)
	}

	pub fn less_than(&self, rhs: &Self) -> BoolValue {
		BoolValue::new(self.value < rhs.value)
	}

	pub fn less_than_or_equal_to(&self, rhs: &Self) -> BoolValue {
		BoolValue::new(self.value <= rhs.value)
	}

	pub fn greater_than(&self, rhs: &Self) -> BoolValue {
		BoolValue::new(self.value > rhs.value)
	}

	pub fn greater_than_or_equal_to(&self, rhs: &Self) -> BoolValue {
		BoolValue::new(self.value >= rhs.value)
	}

	pub fn add(mut self, rhs: Self) -> Self {
		let int = Rc::<BigInt>::make_mut(&mut self.value);
		(*int) += &*rhs.value;
		self
	}

	pub fn sub(mut self, rhs: Self) -> Self {
		let int = Rc::<BigInt>::make_mut(&mut self.value);
		(*int) -= &*rhs.value;
		self
	}

	pub fn mul(mut self, rhs: Self) -> Self {
		let int = Rc::<BigInt>::make_mut(&mut self.value);
		(*int) -= &*rhs.value;
		self
	}
}

impl Display for IntValue {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.value)
	}
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct FloatValue {
	pub value: f64,
}

impl FloatValue {
	pub fn new(value: f64) -> Self {
		Self {
			value,
		}
	}

	pub fn is_zero(self) -> bool {
		self.value.is_zero()
	}

	pub fn is_negative(self) -> bool {
		self.value < 0.
	}

	pub fn is_positive(self) -> bool {
		self.value > 0.
	}

	pub fn zero() -> Self {
		Self::new(0.)
	}

	pub fn is_int(self) -> bool {
		self.value.fract().is_zero()
	}

	pub fn to_bool(self) -> BoolValue {
		BoolValue::new(!self.is_zero())
	}

	pub fn to_int(self, line_number: Option<&BigInt>, start_column: NonZeroUsize) -> Result<IntValue, FullError> {
		match float_to_int(self.value) {
			Some(result) => Ok(IntValue::new(Rc::new(result))),
			None => Err(FullError{
				variant: ErrorVariant::NonNumberValueCastToInt(self.value), line_number: line_number.cloned(), column_number: Some(start_column), line_text: None
			}),
		}
	}

	pub fn to_complex(&self) -> ComplexValue {
		ComplexValue::new(Complex64::new(self.value, 0.))
	}

	pub fn add(self, rhs: Self, allow_overflow: bool) -> Option<Self> {
		let float_result = self.value + rhs.value;
		match float_result.is_finite() || allow_overflow {
			true => Some(Self::new(float_result)),
			false => None,
		}
	}

	pub fn sub(self, rhs: Self, allow_overflow: bool) -> Option<Self> {
		let float_result = self.value - rhs.value;
		match float_result.is_finite() || allow_overflow {
			true => Some(Self::new(float_result)),
			false => None,
		}
	}

	pub fn mul(self, rhs: Self, allow_overflow: bool) -> Option<Self> {
		let float_result = self.value * rhs.value;
		match float_result.is_finite() || allow_overflow {
			true => Some(Self::new(float_result)),
			false => None,
		}
	}

	pub fn div(self, rhs: Self, allow_overflow: bool, allow_div_by_zero: bool) -> Option<Self> {
		if rhs.is_zero() && !allow_div_by_zero {
			return None;
		}
		let float_result = self.value / rhs.value;
		match float_result.is_finite() || allow_overflow {
			true => Some(Self::new(float_result)),
			false => None,
		}
	}

	pub fn pow(self, rhs: Self, allow_overflow: bool, allow_neg_to_non_int_power: bool) -> Option<Self> {
		if self.is_negative() && !rhs.is_int() && !allow_neg_to_non_int_power {
			return None;
		}
		let float_result = self.value / rhs.value;
		match float_result.is_finite() || allow_overflow {
			true => Some(Self::new(float_result)),
			false => None,
		}
	}

	pub fn neg(self) -> Self {
		Self::new(-self.value)
	}

	pub fn equal_to(&self, rhs: &Self) -> BoolValue {
		BoolValue::new(self.value == rhs.value)
	}

	pub fn not_equal_to(&self, rhs: &Self) -> BoolValue {
		BoolValue::new(self.value != rhs.value)
	}

	pub fn less_than(&self, rhs: &Self) -> BoolValue {
		BoolValue::new(self.value < rhs.value)
	}
	
	pub fn less_than_or_equal_to(&self, rhs: &Self) -> BoolValue {
		BoolValue::new(self.value <= rhs.value)
	}
	
	pub fn greater_than(&self, rhs: &Self) -> BoolValue {
		BoolValue::new(self.value > rhs.value)
	}
	
	pub fn greater_than_or_equal_to(&self, rhs: &Self) -> BoolValue {
		BoolValue::new(self.value >= rhs.value)
	}

	pub fn abs(self) -> Self {
		Self::new(self.value.abs())
	}
}

impl Display for FloatValue {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.value)
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

	pub fn zero() -> Self {
		Self::new(Complex64::zero())
	}

	pub fn to_bool(&self) -> BoolValue {
		BoolValue::new(!self.is_zero())
	}

	pub fn to_int(self, line_number: Option<&BigInt>, start_column: NonZeroUsize) -> Result<IntValue, FullError> {
		if !self.value.im.is_zero() {
			return Err(FullError { variant: ErrorVariant::NonRealComplexValueCastToReal(self.value), line_number: line_number.cloned(), column_number: Some(start_column), line_text: None })
		}
		match float_to_int(self.value.re) {
			Some(result) => Ok(IntValue::new(Rc::new(result))),
			None => Err(FullError{
				variant: ErrorVariant::NonNumberValueCastToInt(self.value.re), line_number: line_number.cloned(), column_number: Some(start_column), line_text: None
			}),
		}
	}

	pub fn to_float(self, line_number: Option<&BigInt>, start_column: NonZeroUsize) -> Result<FloatValue, FullError> {
		if !self.value.im.is_zero() {
			return Err(FullError { variant: ErrorVariant::NonRealComplexValueCastToReal(self.value), line_number: line_number.cloned(), column_number: Some(start_column), line_text: None })
		}
		Ok(FloatValue::new(self.value.re))
	}

	pub fn add(self, rhs: Self, allow_overflow: bool) -> Option<Self> {
		let result = self.value + rhs.value;
		(allow_overflow || result.is_finite()).then_some(Self::new(result))
	}

	pub fn sub(self, rhs: Self, allow_overflow: bool) -> Option<Self> {
		let result = self.value - rhs.value;
		(allow_overflow || result.is_finite()).then_some(Self::new(result))
	}

	pub fn mul(self, rhs: Self, allow_overflow: bool) -> Option<Self> {
		let result = self.value * rhs.value;
		(allow_overflow || result.is_finite()).then_some(Self::new(result))
	}

	pub fn div(self, rhs: Self, allow_overflow: bool) -> Option<Self> {
		let result = self.value / rhs.value;
		(allow_overflow || result.is_finite()).then_some(Self::new(result))
	}

	pub fn pow(self, rhs: Self, allow_overflow: bool) -> Option<Self> {
		let result = self.value.powc(rhs.value);
		(allow_overflow || result.is_finite()).then_some(Self::new(result))
	}

	pub fn neg(self) -> Self {
		Self::new(-self.value)
	}

	pub fn equal_to(self, rhs: Self) -> BoolValue {
		BoolValue::new(self.value == rhs.value)
	}

	pub fn not_equal_to(self, rhs: Self) -> BoolValue {
		BoolValue::new(self.value != rhs.value)
	}

	pub fn abs(self, allow_overflow: bool) -> Option<FloatValue> {
		match self.value.norm() {
			value if value.is_infinite() && !allow_overflow => None,
			value => Some(FloatValue::new(value)),
		}
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

	pub fn is_empty(&self) -> bool {
		self.value.is_empty()
	}

	pub fn empty() -> Self {
		Self::new(Rc::default())
	}

	pub fn concat(mut self, rhs: Self) -> Self {
		let string = Rc::<String>::make_mut(&mut self.value);
		string.push_str(&rhs.value);
		self
	}

	pub fn equal_to(&self, rhs: &Self) -> BoolValue {
		BoolValue::new(self.value == rhs.value)
	}

	pub fn not_equal_to(&self, rhs: &Self) -> BoolValue {
		BoolValue::new(self.value != rhs.value)
	}

	pub fn count_chars(&self) -> usize {
		self.value.chars().count()
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

	pub fn zero() -> Self {
		Self::new(false)
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

	pub fn to_float(self) -> FloatValue {
		FloatValue::new(match self.value {
			true => -1.,
			false => 0.,
		})
	}

	pub fn to_complex(&self) -> ComplexValue {
		ComplexValue::new(Complex64::new(match self.value {
			true => -1.,
			false => 0.,
		}, 0.))
	}

	pub fn and(self, rhs: Self) -> Self {
		Self::new(self.value && rhs.value)
	}

	pub fn or(self, rhs: Self) -> Self {
		Self::new(self.value || rhs.value)
	}

	pub fn not(self) -> Self {
		Self::new(!self.value)
	}

	pub fn equal_to(self, rhs: Self) -> Self {
		Self::new(self.value == rhs.value)
	}

	pub fn not_equal_to(self, rhs: Self) -> Self {
		Self::new(self.value != rhs.value)
	}

	pub fn less_than(self, rhs: Self) -> Self {
		Self::new(self.value < rhs.value)
	}

	pub fn less_than_or_equal_to(self, rhs: Self) -> Self {
		Self::new(self.value <= rhs.value)
	}

	pub fn greater_than(self, rhs: Self) -> Self {
		Self::new(self.value > rhs.value)
	}

	pub fn greater_than_or_equal_to(self, rhs: Self) -> Self {
		Self::new(self.value >= rhs.value)
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
	Float(FloatValue),
	Complex(ComplexValue),
	Bool(BoolValue),
	String(StringValue),
}

impl AnyTypeValue {
	pub fn to_bool(&self) -> BoolValue {
		match self {
			Self::Bool(value) => *value,
			Self::Int(value) => value.to_bool(),
			Self::Float(value) => value.to_bool(),
			Self::Complex(value) => value.to_bool(),
			Self::String(value) => value.to_bool(),
		}
	}

	pub fn to_int(self, line_number: Option<&BigInt>, start_column: NonZeroUsize) -> Result<IntValue, FullError> {
		match self {
			Self::Bool(value) => Ok(value.to_int()),
			Self::Int(value) => Ok(value),
			Self::Float(value) => value.to_int(line_number, start_column),
			Self::Complex(value) => value.to_int(line_number, start_column),
			Self::String(_) => return Err(FullError {
				variant: ErrorVariant::StringCastToNumber, line_number: line_number.cloned(), column_number: Some(start_column), line_text: None
			}),
		}
	}

	pub fn to_float(self, line_number: Option<&BigInt>, start_column: NonZeroUsize) -> Result<FloatValue, FullError> {
		match self {
			Self::Bool(value) => Ok(value.to_float()),
			Self::Int(value) => Ok(value.to_float()),
			Self::Float(value) => Ok(value),
			Self::Complex(value) => value.to_float(line_number, start_column),
			Self::String(_) => return Err(FullError {
				variant: ErrorVariant::StringCastToNumber, line_number: line_number.cloned(), column_number: Some(start_column), line_text: None
			}),
		}
	}

	pub fn to_complex(self, line_number: Option<&BigInt>, start_column: NonZeroUsize) -> Result<ComplexValue, FullError> {
		match self {
			Self::Bool(value) => Ok(value.to_complex()),
			Self::Int(value) => Ok(value.to_complex()),
			Self::Float(value) => Ok(value.to_complex()),
			Self::Complex(value) => Ok(value),
			Self::String(_) => return Err(FullError {
				variant: ErrorVariant::StringCastToNumber, line_number: line_number.cloned(), column_number: Some(start_column), line_text: None
			}),
		}
	}
}