use std::{f64::{INFINITY, NEG_INFINITY}, fmt::{self, Display, Formatter}, io::{self, Write}, rc::Rc};

use num::{complex::Complex64, BigInt, FromPrimitive, Signed, ToPrimitive, Zero, One};

use crate::mavagk_basic::error::ErrorVariant;

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

	pub fn one() -> Self {
		Self::new(Rc::new(BigInt::one()))
	}

	pub fn is_zero(&self) -> bool {
		self.value.is_zero()
	}

	pub fn is_negative(&self) -> bool {
		self.value.is_negative()
	}

	pub fn is_positive(&self) -> bool {
		self.value.is_positive()
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

	pub fn floored_div(mut self, rhs: Self) -> Result<IntValue, ErrorVariant> {
		match rhs.is_zero() {
			false => Ok({
				let int = Rc::<BigInt>::make_mut(&mut self.value);
				(*int) /= &*rhs.value;
				Self::new(self.value)
			}),
			true => Err(ErrorVariant::FlooredDivisionByZero),
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

	pub fn add(mut self, rhs: &Self) -> Self {
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

	pub fn print<T: Write>(&self, f: &mut T, print_leading_positive_space: bool, print_trailing_space: bool) -> io::Result<()> {
		match (self, print_leading_positive_space) {
			(x, _) if x.is_negative() => write!(f, "{}", x.value),
			(x, true) => write!(f, " {}", x.value),
			(x, false) => write!(f, "{}", x.value),
		}?;
		match print_trailing_space {
			true => write!(f, " "),
			false => write!(f, ""),
		}
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

	pub fn one() -> Self {
		Self::new(1.)
	}

	pub fn is_int(self) -> bool {
		self.value.fract().is_zero()
	}

	pub fn to_bool(self) -> BoolValue {
		BoolValue::new(!self.is_zero())
	}

	pub fn to_int(self) -> Result<IntValue, ErrorVariant> {
		match float_to_int(self.value) {
			Some(result) => Ok(IntValue::new(Rc::new(result))),
			None => Err(ErrorVariant::NonNumberValueCastToInt(self.value)),
		}
	}

	pub fn to_complex(&self) -> ComplexValue {
		ComplexValue::new(Complex64::new(self.value, 0.))
	}

	pub fn add(self, rhs: Self, allow_overflow: bool) -> Result<Self, ErrorVariant> {
		let float_result = self.value + rhs.value;
		match float_result.is_finite() || allow_overflow {
			true => Ok(Self::new(float_result)),
			false => Err(ErrorVariant::ValueOverflow),
		}
	}

	pub fn sub(self, rhs: Self, allow_overflow: bool) -> Result<Self, ErrorVariant> {
		let float_result = self.value - rhs.value;
		match float_result.is_finite() || allow_overflow {
			true => Ok(Self::new(float_result)),
			false => Err(ErrorVariant::ValueOverflow),
		}
	}

	pub fn mul(self, rhs: Self, allow_overflow: bool) -> Result<Self, ErrorVariant> {
		let float_result = self.value * rhs.value;
		match float_result.is_finite() || allow_overflow {
			true => Ok(Self::new(float_result)),
			false => Err(ErrorVariant::ValueOverflow),
		}
	}

	pub fn div(self, rhs: Self, allow_overflow: bool, allow_div_by_zero: bool) -> Result<Self, ErrorVariant> {
		if rhs.is_zero() && !allow_div_by_zero {
			return Err(ErrorVariant::DivisionByZero);
		}
		let float_result = self.value / rhs.value;
		match float_result.is_finite() || allow_overflow {
			true => Ok(Self::new(float_result)),
			false => Err(ErrorVariant::ValueOverflow),
		}
	}

	pub fn pow(self, rhs: Self, allow_overflow: bool, allow_neg_to_non_int_power: bool) -> Result<Self, ErrorVariant> {
		if self.is_negative() && !rhs.is_int() && !allow_neg_to_non_int_power {
			return Err(ErrorVariant::NegativeNumberRaisedToNonIntegerPower);
		}
		let float_result = self.value.powf(rhs.value);
		match float_result.is_finite() || allow_overflow {
			true => Ok(Self::new(float_result)),
			false => Err(ErrorVariant::ValueOverflow),
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

	pub fn floor(self) -> Self {
		Self::new(self.value.floor())
	}

	pub fn print<T: Write>(&self, f: &mut T, print_leading_positive_space: bool, print_trailing_space: bool) -> io::Result<()> {
		match (self, print_leading_positive_space) {
			(x, _) if x.is_negative() => write!(f, "{}", x.value),
			(x, true) => write!(f, " {}", x.value),
			(x, false) => write!(f, "{}", x.value),
		}?;
		match print_trailing_space {
			true => write!(f, " "),
			false => write!(f, ""),
		}
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

	pub fn to_int(self) -> Result<IntValue, ErrorVariant> {
		if !self.value.im.is_zero() {
			return Err(ErrorVariant::NonRealComplexValueCastToReal(self.value));
		}
		match float_to_int(self.value.re) {
			Some(result) => Ok(IntValue::new(Rc::new(result))),
			None => Err(ErrorVariant::NonNumberValueCastToInt(self.value.re)),
		}
	}

	pub fn to_float(self) -> Result<FloatValue, ErrorVariant> {
		if !self.value.im.is_zero() {
			return Err(ErrorVariant::NonRealComplexValueCastToReal(self.value))
		}
		Ok(FloatValue::new(self.value.re))
	}

	pub fn add(self, rhs: Self, allow_overflow: bool) -> Result<Self, ErrorVariant> {
		let result = self.value + rhs.value;
		match allow_overflow || result.is_finite() {
			true => Ok(Self::new(result)),
			false => Err(ErrorVariant::ValueOverflow),
		}
	}

	pub fn sub(self, rhs: Self, allow_overflow: bool) -> Result<Self, ErrorVariant> {
		let result = self.value - rhs.value;
		match allow_overflow || result.is_finite() {
			true => Ok(Self::new(result)),
			false => Err(ErrorVariant::ValueOverflow),
		}
	}

	pub fn mul(self, rhs: Self, allow_overflow: bool) -> Result<Self, ErrorVariant> {
		let result = self.value * rhs.value;
		match allow_overflow || result.is_finite() {
			true => Ok(Self::new(result)),
			false => Err(ErrorVariant::ValueOverflow),
		}
	}

	pub fn div(self, rhs: Self, allow_overflow: bool, allow_div_by_zero: bool) -> Result<Self, ErrorVariant> {
		if rhs.is_zero() && !allow_div_by_zero {
			return Err(ErrorVariant::DivisionByZero);
		}
		let result = self.value / rhs.value;
		match allow_overflow || result.is_finite() {
			true => Ok(Self::new(result)),
			false => Err(ErrorVariant::ValueOverflow),
		}
	}

	pub fn pow(self, rhs: Self, allow_overflow: bool) -> Result<Self, ErrorVariant> {
		let result = self.value.powc(rhs.value);
		match allow_overflow || result.is_finite() {
			true => Ok(Self::new(result)),
			false => Err(ErrorVariant::ValueOverflow),
		}
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

	pub fn abs(self, allow_overflow: bool) -> Result<FloatValue, ErrorVariant> {
		match self.value.norm() {
			value if value.is_infinite() && !allow_overflow => Err(ErrorVariant::ValueOverflow),
			value => Ok(FloatValue::new(value)),
		}
	}

	pub fn sqrt(self, allow_overflow: bool) -> Result<ComplexValue, ErrorVariant> {
		match self.value.sqrt() {
			value if value.is_infinite() && !allow_overflow => Err(ErrorVariant::ValueOverflow),
			value => Ok(ComplexValue::new(value)),
		}
	}

	pub fn print<T: Write>(&self, f: &mut T, print_leading_positive_space: bool, print_trailing_space: bool) -> io::Result<()> {
		match (self.value.re, self.value.im, print_leading_positive_space) {
			(re, im, true) if re > 0. && im > 0. => write!(f, " {re}+{im}i"),
			(re, im, false) if re > 0. && im > 0. => write!(f, "{re}+{im}i"),
			(re, im, true) if re > 0. && im == 0. => write!(f, " {re}"),
			(re, im, false) if re > 0. && im == 0. => write!(f, "{re}"),
			(re, im, true) if re > 0. && im < 0. => write!(f, " {re}{im}i"),
			(re, im, false) if re > 0. && im < 0. => write!(f, "{re}{im}i"),

			(re, im, true) if re == 0. && im > 0. => write!(f, " {im}i"),
			(re, im, false) if re == 0. && im > 0. => write!(f, "{im}i"),
			(re, im, true) if re == 0. && im == 0. => write!(f, " 0"),
			(re, im, false) if re == 0. && im == 0. => write!(f, "0"),
			(re, im, _) if re == 0. && im < 0. => write!(f, "{im}i"),

			(re, im, _) if re < 0. && im > 0. => write!(f, "{re}+{im}i"),
			(re, im, _) if re < 0. && im == 0. => write!(f, "{re}"),
			(re, im, _) if re < 0. && im < 0. => write!(f, "{re}{im}i"),
			_ => unreachable!()
		}?;
		match print_trailing_space {
			true => write!(f, " "),
			false => write!(f, ""),
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

	pub fn print<T: Write>(&self, f: &mut T) -> io::Result<()> {
		write!(f, "{}", self.value)
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

	pub fn print<T: Write>(&self, f: &mut T, print_leading_positive_space: bool, print_trailing_space: bool) -> io::Result<()> {
		write!(f, "{}", match (self.value, print_leading_positive_space) {
			(true, _) => "-1",
			(false, true) => " 0",
			(false, false) => "0",
		})?;
		match print_trailing_space {
			true => write!(f, " "),
			false => write!(f, ""),
		}
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

	pub fn to_int(self) -> Result<IntValue, ErrorVariant> {
		match self {
			Self::Bool(value) => Ok(value.to_int()),
			Self::Int(value) => Ok(value),
			Self::Float(value) => value.to_int(),
			Self::Complex(value) => value.to_int(),
			Self::String(_) => return Err(ErrorVariant::StringCastToNumber),
		}
	}

	pub fn to_float(self) -> Result<FloatValue, ErrorVariant> {
		match self {
			Self::Bool(value) => Ok(value.to_float()),
			Self::Int(value) => Ok(value.to_float()),
			Self::Float(value) => Ok(value),
			Self::Complex(value) => value.to_float(),
			Self::String(_) => return Err(ErrorVariant::StringCastToNumber),
		}
	}

	pub fn to_complex(self) -> Result<ComplexValue, ErrorVariant> {
		match self {
			Self::Bool(value) => Ok(value.to_complex()),
			Self::Int(value) => Ok(value.to_complex()),
			Self::Float(value) => Ok(value.to_complex()),
			Self::Complex(value) => Ok(value),
			Self::String(_) => return Err(ErrorVariant::StringCastToNumber),
		}
	}

	pub fn print<T: Write>(&self, f: &mut T, print_leading_positive_space: bool, print_trailing_numeric_space: bool) -> io::Result<()> {
		match self {
			AnyTypeValue::Bool(value) => value.print(f, print_leading_positive_space, print_trailing_numeric_space),
			AnyTypeValue::Int(value) => value.print(f, print_leading_positive_space, print_trailing_numeric_space),
			AnyTypeValue::Float(value) => value.print(f, print_leading_positive_space, print_trailing_numeric_space),
			AnyTypeValue::Complex(value) => value.print(f, print_leading_positive_space, print_trailing_numeric_space),
			AnyTypeValue::String(value) => value.print(f),
		}
	}
}

impl Display for AnyTypeValue {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			Self::Bool(value) => write!(f, "{}", value),
			Self::Int(value) => write!(f, "{}", value),
			Self::Float(value) => write!(f, "{}", value),
			Self::Complex(value) => write!(f, "{}", value),
			Self::String(value) => write!(f, "{}", value),
		}
	}
}