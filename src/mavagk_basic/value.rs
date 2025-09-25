use std::{f64::{consts::{E, PI, TAU}, INFINITY, NAN, NEG_INFINITY}, fmt::{self, Display, Formatter}, io::{self, Write}, rc::Rc};

use num::{complex::Complex64, BigInt, FromPrimitive, One, Signed, ToPrimitive, Zero};

use crate::mavagk_basic::{abstract_syntax_tree::AngleOption, error::ErrorVariant};

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
	pub const fn new(value: Rc<BigInt>) -> Self {
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

	pub fn from_usize(value: usize) -> Self {
		Self::new(Rc::new(BigInt::from_usize(value).unwrap()))
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

	pub fn signum(&self) -> Self {
		Self::new(Rc::new(self.value.signum()))
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
	pub const ZERO: Self = Self::new(0.);
	pub const ONE: Self = Self::new(1.);
	pub const FALSE: Self = Self::new(0.);
	pub const TRUE: Self = Self::new(-1.);
	pub const PI: Self = Self::new(PI);
	pub const E: Self = Self::new(E);
	pub const TAU: Self = Self::new(TAU);
	pub const PHI: Self = Self::new(1.618033988749894848204586834365638117720309179805762862135448622705260462818902);
	pub const EGAMMA: Self = Self::new(0.577215664901532860606512090082402431042159335939923598805767234884867726777664);
	pub const INFINITY: Self = Self::new(INFINITY);
	pub const NEG_INFINITY: Self = Self::new(NEG_INFINITY);
	pub const NAN: Self = Self::new(NAN);
	pub const MAX: Self = Self::new(f64::MAX);

	pub const fn new(value: f64) -> Self {
		Self {
			value,
		}
	}

	/// Constructs a `FloatValue` from a `f64`. Returns an error if overflow is not allowed and the input value is not a finite number.
	pub const fn try_new(value: f64, allow_overflow: bool) -> Result<Self, ErrorVariant> {
		match value.is_finite() || allow_overflow {
			true => Ok(Self::new(value)),
			false => Err(ErrorVariant::ValueOverflow),
		}
	}

	pub const fn from_usize(value: usize) -> Self {
		Self::new(value as f64)
	}

	pub const fn is_zero(self) -> bool {
		self.value == 0.
	}

	pub const fn is_negative(self) -> bool {
		self.value < 0.
	}

	pub const fn is_positive(self) -> bool {
		self.value > 0.
	}

	pub const fn signum(self) -> Self {
		Self::new(match self.value {
			value if value < 0. => -1.,
			value if value > 0. => 1.,
			value => value,
		})
	}

	pub fn is_int(self) -> bool {
		self.value.fract().is_zero()
	}

	pub const fn to_bool(self) -> BoolValue {
		BoolValue::new(!self.is_zero())
	}

	pub fn to_int(self) -> Result<IntValue, ErrorVariant> {
		match float_to_int(self.value) {
			Some(result) => Ok(IntValue::new(Rc::new(result))),
			None => Err(ErrorVariant::NonNumberValueCastToInt(self.value)),
		}
	}

	pub const fn to_complex(&self) -> ComplexValue {
		ComplexValue::new(Complex64::new(self.value, 0.))
	}

	pub const fn add(self, rhs: Self, allow_overflow: bool) -> Result<Self, ErrorVariant> {
		let float_result = self.value + rhs.value;
		match float_result.is_finite() || allow_overflow {
			true => Ok(Self::new(float_result)),
			false => Err(ErrorVariant::ValueOverflow),
		}
	}

	pub const fn sub(self, rhs: Self, allow_overflow: bool) -> Result<Self, ErrorVariant> {
		let float_result = self.value - rhs.value;
		match float_result.is_finite() || allow_overflow {
			true => Ok(Self::new(float_result)),
			false => Err(ErrorVariant::ValueOverflow),
		}
	}

	pub const fn mul(self, rhs: Self, allow_overflow: bool) -> Result<Self, ErrorVariant> {
		let float_result = self.value * rhs.value;
		match float_result.is_finite() || allow_overflow {
			true => Ok(Self::new(float_result)),
			false => Err(ErrorVariant::ValueOverflow),
		}
	}

	pub const fn div(self, rhs: Self, allow_overflow: bool, allow_div_by_zero: bool) -> Result<Self, ErrorVariant> {
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

	pub const fn neg(self) -> Self {
		Self::new(-self.value)
	}

	pub const fn equal_to(&self, rhs: &Self) -> BoolValue {
		BoolValue::new(self.value == rhs.value)
	}

	pub const fn not_equal_to(&self, rhs: &Self) -> BoolValue {
		BoolValue::new(self.value != rhs.value)
	}

	pub const fn less_than(&self, rhs: &Self) -> BoolValue {
		BoolValue::new(self.value < rhs.value)
	}
	
	pub const fn less_than_or_equal_to(&self, rhs: &Self) -> BoolValue {
		BoolValue::new(self.value <= rhs.value)
	}
	
	pub const fn greater_than(&self, rhs: &Self) -> BoolValue {
		BoolValue::new(self.value > rhs.value)
	}
	
	pub const fn greater_than_or_equal_to(&self, rhs: &Self) -> BoolValue {
		BoolValue::new(self.value >= rhs.value)
	}

	pub const fn abs(self) -> Self {
		Self::new(self.value.abs())
	}

	pub fn floor(self) -> Self {
		Self::new(self.value.floor())
	}

	pub fn sqrt(self, allow_real_square_root_of_negative: bool) -> Result<Self, ErrorVariant> {
		match self {
			value if value.is_negative() && !allow_real_square_root_of_negative => return Err(ErrorVariant::SquareRootOfNegative),
			value => Ok(Self::new(value.value.sqrt())),
		}
	}

	pub fn sin(self, units: AngleOption, allow_overflow: bool) -> Result<Self, ErrorVariant> {
		Ok(Self::new(self.to_radians(units, allow_overflow)?.value.sin()))
	}

	pub fn cos(self, units: AngleOption, allow_overflow: bool) -> Result<Self, ErrorVariant> {
		Ok(Self::new(self.to_radians(units, allow_overflow)?.value.cos()))
	}

	pub fn tan(self, units: AngleOption, allow_overflow: bool) -> Result<Self, ErrorVariant> {
		Self::try_new(self.to_radians(units, allow_overflow)?.value.tan(), allow_overflow)
	}

	pub fn cot(self, units: AngleOption, allow_overflow: bool) -> Result<Self, ErrorVariant> {
		let input_value_in_radians = self.to_radians(units, allow_overflow)?.value;
		Self::try_new(input_value_in_radians.cos() / input_value_in_radians.sin(), allow_overflow)
	}

	pub fn sec(self, units: AngleOption, allow_overflow: bool) -> Result<Self, ErrorVariant> {
		Self::try_new(1. / self.to_radians(units, allow_overflow)?.value.cos(), allow_overflow)
	}

	pub fn csc(self, units: AngleOption, allow_overflow: bool) -> Result<Self, ErrorVariant> {
		Self::try_new(1. / self.to_radians(units, allow_overflow)?.value.sin(), allow_overflow)
	}

	pub fn asin(self, units: AngleOption, allow_real_trig_out_of_range: bool) -> Result<Self, ErrorVariant> {
		let out = self.value.asin();
		if !out.is_finite() && !allow_real_trig_out_of_range {
			return Err(ErrorVariant::ATrigFunctionOutOfRange);
		}
		Self::new(out).from_radians(units, true)
	}

	pub fn acos(self, units: AngleOption, allow_real_trig_out_of_range: bool) -> Result<Self, ErrorVariant> {
		let out = self.value.acos();
		if !out.is_finite() && !allow_real_trig_out_of_range {
			return Err(ErrorVariant::ATrigFunctionOutOfRange);
		}
		Self::new(out).from_radians(units, true)
	}

	pub fn atan(self, units: AngleOption, allow_overflow: bool) -> Result<Self, ErrorVariant> {
		Self::new(self.value.atan()).from_radians(units, allow_overflow)
	}

	pub fn acot(self, units: AngleOption, allow_overflow: bool) -> Result<Self, ErrorVariant> {
		Self::new(1. / (self.value).atan()).from_radians(units, allow_overflow)
	}

	pub fn asec(self, units: AngleOption, allow_real_trig_out_of_range: bool) -> Result<Self, ErrorVariant> {
		let out = (1. /self.value).acos();
		if !out.is_finite() && !allow_real_trig_out_of_range {
			return Err(ErrorVariant::ATrigFunctionOutOfRange);
		}
		Self::new(out).from_radians(units, true)
	}

	pub fn acsc(self, units: AngleOption, allow_real_trig_out_of_range: bool) -> Result<Self, ErrorVariant> {
		let out = (1. /self.value).asin();
		if !out.is_finite() && !allow_real_trig_out_of_range {
			return Err(ErrorVariant::ATrigFunctionOutOfRange);
		}
		Self::new(out).from_radians(units, true)
	}

	pub fn exp(self, allow_overflow: bool) -> Result<Self, ErrorVariant> {
		Self::try_new(self.value.exp(), allow_overflow)
	}

	pub fn ln(self, allow_real_log_of_non_positive: bool) -> Result<Self, ErrorVariant> {
		let result = self.value.ln();
		match result.is_finite() || allow_real_log_of_non_positive {
			true => Ok(Self::new(result)),
			false => Err(ErrorVariant::LogOfNonPositive)
		}
	}

	pub const fn to_radians(self, units: AngleOption, allow_overflow: bool) -> Result<Self, ErrorVariant> {
		Ok(Self::new(match units {
			AngleOption::Radians => self.value,
			AngleOption::Degrees => self.value / 180. * PI,
			AngleOption::Gradians => self.value / 200. * PI,
			AngleOption::Revolutions => {
				let radians = self.value * 2. * PI;
				if !radians.is_finite() && !allow_overflow {
					return Err(ErrorVariant::ValueOverflow);
				}
				radians
			}
		}))
	}

	pub const fn from_radians(self, units: AngleOption, allow_overflow: bool) -> Result<Self, ErrorVariant> {
		let out = match units {
			AngleOption::Radians => self.value,
			AngleOption::Degrees => self.value / PI * 180.,
			AngleOption::Gradians => self.value / PI * 200.,
			AngleOption::Revolutions => self.value / 2. / PI,
		};
		if !out.is_finite() && !allow_overflow {
			return Err(ErrorVariant::ValueOverflow);
		}
		Ok(Self::new(out))
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
	pub const ZERO: Self = Self::new(Complex64::ZERO);
	pub const ONE: Self = Self::new(Complex64::ONE);
	pub const I: Self = Self::new(Complex64::I);

	pub const fn new(value: Complex64) -> Self {
		Self {
			value,
		}
	}

	/// Constructs a `ComplexValue` from a `Complex64`. Returns an error if overflow is not allowed and the input value is not a finite number.
	pub const fn try_new(value: Complex64, allow_overflow: bool) -> Result<Self, ErrorVariant> {
		match (value.re.is_finite() && value.im.is_finite()) || allow_overflow {
			true => Ok(Self::new(value)),
			false => Err(ErrorVariant::ValueOverflow),
		}
	}

	pub const fn is_zero(&self) -> bool {
		self.value.re == 0. && self.value.im == 0.
	}

	pub const fn to_bool(&self) -> BoolValue {
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

	pub const fn to_float(self) -> Result<FloatValue, ErrorVariant> {
		if self.value.im != 0. {
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

	pub const fn re(self) -> FloatValue {
		FloatValue::new(self.value.re)
	}

	pub const fn im(self) -> FloatValue {
		FloatValue::new(self.value.im)
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
	pub const fn new(value: Rc<String>) -> Self {
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
	pub const TRUE: Self = Self::new(true);
	pub const FALSE: Self = Self::new(false);
	pub const ZERO: Self = Self::new(false);

	pub const fn new(value: bool) -> Self {
		Self {
			value,
		}
	}

	pub const fn is_zero(self) -> bool {
		!self.value
	}

	pub fn to_int(self) -> IntValue {
		IntValue::new(Rc::new(match self.value {
			true => BigInt::ZERO,
			false => BigInt::from_i8(-1).unwrap(),
		}))
	}

	pub const fn to_float(self) -> FloatValue {
		FloatValue::new(match self.value {
			true => -1.,
			false => 0.,
		})
	}

	pub const fn to_complex(&self) -> ComplexValue {
		ComplexValue::new(Complex64::new(match self.value {
			true => -1.,
			false => 0.,
		}, 0.))
	}

	pub const fn and(self, rhs: Self) -> Self {
		Self::new(self.value && rhs.value)
	}

	pub const fn or(self, rhs: Self) -> Self {
		Self::new(self.value || rhs.value)
	}

	pub const fn not(self) -> Self {
		Self::new(!self.value)
	}

	pub const fn equal_to(self, rhs: Self) -> Self {
		Self::new(self.value == rhs.value)
	}

	pub const fn not_equal_to(self, rhs: Self) -> Self {
		Self::new(self.value != rhs.value)
	}

	pub const fn less_than(self, rhs: Self) -> Self {
		Self::new(self.value < rhs.value)
	}

	pub const fn less_than_or_equal_to(self, rhs: Self) -> Self {
		Self::new(self.value <= rhs.value)
	}

	pub const fn greater_than(self, rhs: Self) -> Self {
		Self::new(self.value > rhs.value)
	}

	pub const fn greater_than_or_equal_to(self, rhs: Self) -> Self {
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
			Self::String(_) => Err(ErrorVariant::StringCastToNumber),
		}
	}

	pub fn to_float(self) -> Result<FloatValue, ErrorVariant> {
		match self {
			Self::Bool(value) => Ok(value.to_float()),
			Self::Int(value) => Ok(value.to_float()),
			Self::Float(value) => Ok(value),
			Self::Complex(value) => value.to_float(),
			Self::String(_) => Err(ErrorVariant::StringCastToNumber),
		}
	}

	pub fn to_complex(self) -> Result<ComplexValue, ErrorVariant> {
		match self {
			Self::Bool(value) => Ok(value.to_complex()),
			Self::Int(value) => Ok(value.to_complex()),
			Self::Float(value) => Ok(value.to_complex()),
			Self::Complex(value) => Ok(value),
			Self::String(_) => Err(ErrorVariant::StringCastToNumber),
		}
	}

	pub fn to_string(self) -> Result<StringValue, ErrorVariant> {
		match self {
			Self::String(value) => return Ok(value),
			_ => return Err(ErrorVariant::NumberCastToString),
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