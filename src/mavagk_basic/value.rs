use std::{f64::{INFINITY, NEG_INFINITY}, fmt::{self, Display, Formatter}, num::NonZeroUsize, rc::Rc};

use num::{complex::Complex64, BigInt, BigUint, FromPrimitive, Integer, Signed, ToPrimitive, Zero};
use num_traits::Pow;

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

	pub fn floored_div(mut self, rhs: Self) -> Option<RealValue> {
		match rhs.is_zero() {
			false => Some({
				let int = Rc::<BigInt>::make_mut(&mut self.value);
				(*int) /= &*rhs.value;
				RealValue::IntValue(self.value)
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

	pub fn is_negative(&self) -> bool {
		match self {
			Self::FloatValue(float_value) => float_value.is_negative(),
			Self::IntValue(int_value) => int_value.is_negative(),
		}
	}

	pub fn is_integer(&self) -> bool {
		match self {
			Self::FloatValue(float_value) => float_value.fract().is_zero(),
			Self::IntValue(_) => true,
		}
	}

	pub fn zero() -> Self {
		Self::IntValue(Rc::new(BigInt::ZERO))
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

	pub fn add(self, rhs: Self, allow_overflow: bool) -> Option<Self> {
		match (self, rhs) {
			(Self::IntValue(mut lhs_value), Self::IntValue(rhs_value)) => Some(Self::IntValue({
				let int = Rc::<BigInt>::make_mut(&mut lhs_value);
				(*int) += &*rhs_value;
				lhs_value
			})),
			(lhs_value, rhs_value) => {
				let float_result = lhs_value.get_float() + rhs_value.get_float();
				match float_result.is_finite() || allow_overflow {
					true => Some(Self::FloatValue(float_result)),
					false => None,
				}
			}
		}
	}

	pub fn sub(self, rhs: Self, allow_overflow: bool) -> Option<Self> {
		match (self, rhs) {
			(Self::IntValue(mut lhs_value), Self::IntValue(rhs_value)) => Some(Self::IntValue({
				let int = Rc::<BigInt>::make_mut(&mut lhs_value);
				(*int) -= &*rhs_value;
				lhs_value
			})),
			(lhs_value, rhs_value) => {
				let float_result = lhs_value.get_float() - rhs_value.get_float();
				match float_result.is_finite() || allow_overflow {
					true => Some(Self::FloatValue(float_result)),
					false => None,
				}
			}
		}
	}

	pub fn mul(self, rhs: Self, allow_overflow: bool) -> Option<Self> {
		match (self, rhs) {
			(Self::IntValue(mut lhs_value), Self::IntValue(rhs_value)) => Some(Self::IntValue({
				let int = Rc::<BigInt>::make_mut(&mut lhs_value);
				(*int) *= &*rhs_value;
				lhs_value
			})),
			(lhs_value, rhs_value) => {
				let float_result = lhs_value.get_float() * rhs_value.get_float();
				match float_result.is_finite() || allow_overflow {
					true => Some(Self::FloatValue(float_result)),
					false => None,
				}
			}
		}
	}

	pub fn div(self, rhs: Self, allow_overflow_and_div_by_zero: bool) -> Option<Self> {
		match (self, rhs) {
			(lhs_value, rhs_value) if rhs_value.is_zero() => match allow_overflow_and_div_by_zero {
				true => Some(RealValue::FloatValue(lhs_value.get_float() / rhs_value.get_float())),
				false => None,
			},
			(lhs_value, rhs_value) if matches!((&lhs_value, &rhs_value), (RealValue::IntValue(lhs_int), RealValue::IntValue(rhs_int)) if !lhs_int.is_multiple_of(rhs_int)) => {
				let float_result = lhs_value.get_float() / rhs_value.get_float();
				match float_result.is_finite() || allow_overflow_and_div_by_zero {
					true => Some(Self::FloatValue(float_result)),
					false => None,
				}
			}
			(RealValue::IntValue(mut lhs_value), RealValue::IntValue(rhs_value)) => Some(RealValue::IntValue({
				let int = Rc::<BigInt>::make_mut(&mut lhs_value);
				(*int) /= &*rhs_value;
				lhs_value
			})),
			(lhs_value, rhs_value) => {
				let float_result = lhs_value.get_float() / rhs_value.get_float();
				match float_result.is_finite() || allow_overflow_and_div_by_zero {
					true => Some(Self::FloatValue(float_result)),
					false => None,
				}
			}
		}
	}

	pub fn pow(self, rhs: Self, allow_overflow: bool) -> Option<Self> {
		match (&self, &rhs) {
			(RealValue::IntValue(lhs_int), RealValue::IntValue(rhs_int)) if rhs_int.is_positive() =>
				Some(RealValue::IntValue(Rc::new(Pow::<BigUint>::pow(&**lhs_int, rhs_int.to_biguint().unwrap())))),
			(lhs_value, rhs_value) => {
				let float_result = lhs_value.get_float().powf(rhs_value.get_float());
				match float_result.is_finite() || allow_overflow {
					true => Some(RealValue::FloatValue(float_result)),
					false => None,
				}
			}
		}
	}

	pub fn neg(self) -> Self {
		match self {
			RealValue::IntValue(int_value) => RealValue::IntValue(Rc::new(-&*int_value)),
			RealValue::FloatValue(float_value) => RealValue::FloatValue(-float_value),
		}
	}

	pub fn equal_to(&self, rhs: &Self) -> BoolValue {
		BoolValue::new(match (self, rhs) {
			(RealValue::IntValue(lhs_int), RealValue::IntValue(rhs_int)) => **lhs_int == **rhs_int,
			(_, _) => self.get_float() == rhs.get_float(),
		})
	}

	pub fn not_equal_to(&self, rhs: &Self) -> BoolValue {
		BoolValue::new(match (self, rhs) {
			(RealValue::IntValue(lhs_int), RealValue::IntValue(rhs_int)) => **lhs_int != **rhs_int,
			(_, _) => self.get_float() != rhs.get_float(),
		})
	}

	pub fn less_than(&self, rhs: &Self) -> BoolValue {
		BoolValue::new(match (self, rhs) {
			(RealValue::IntValue(lhs_int), RealValue::IntValue(rhs_int)) => **lhs_int < **rhs_int,
			(_, _) => self.get_float() < rhs.get_float(),
		})
	}
	
	pub fn less_than_or_equal_to(&self, rhs: &Self) -> BoolValue {
		BoolValue::new(match (self, rhs) {
			(RealValue::IntValue(lhs_int), RealValue::IntValue(rhs_int)) => **lhs_int <= **rhs_int,
			(_, _) => self.get_float() <= rhs.get_float(),
		})
	}
	
	pub fn greater_than(&self, rhs: &Self) -> BoolValue {
		BoolValue::new(match (self, rhs) {
			(RealValue::IntValue(lhs_int), RealValue::IntValue(rhs_int)) => **lhs_int > **rhs_int,
			(_, _) => self.get_float() > rhs.get_float(),
		})
	}
	
	pub fn greater_than_or_equal_to(&self, rhs: &Self) -> BoolValue {
		BoolValue::new(match (self, rhs) {
			(RealValue::IntValue(lhs_int), RealValue::IntValue(rhs_int)) => **lhs_int >= **rhs_int,
			(_, _) => self.get_float() >= rhs.get_float(),
		})
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

	pub fn zero() -> Self {
		Self::new(Complex64::zero())
	}

	pub fn to_bool(&self) -> BoolValue {
		BoolValue::new(!self.is_zero())
	}

	pub fn to_int(self, line_number: Option<&BigInt>, start_column: NonZeroUsize) -> Result<IntValue, Error> {
		if !self.value.im.is_zero() {
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
		if !self.value.im.is_zero() {
			return Err(Error { variant: ErrorVariant::NonRealComplexValueCastToReal(self.value), line_number: line_number.cloned(), column_number: Some(start_column), line_text: None })
		}
		Ok(RealValue::FloatValue(self.value.re))
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

	pub fn to_int(self, line_number: Option<&BigInt>, start_column: NonZeroUsize) -> Result<IntValue, Error> {
		match self {
			Self::Bool(value) => Ok(value.to_int()),
			Self::Int(value) => Ok(value),
			Self::Real(value) => value.to_int(line_number, start_column),
			Self::Complex(value) => value.to_int(line_number, start_column),
			Self::String(_) => return Err(Error {
				variant: ErrorVariant::StringCastToNumber, line_number: line_number.cloned(), column_number: Some(start_column), line_text: None
			}),
		}
	}
}