use std::{collections::{HashMap, HashSet}, f64::{INFINITY, NAN, NEG_INFINITY, consts::{E, PI, TAU}}, fmt::{self, Display, Formatter}, io::{self, Write}, num::NonZeroUsize, rc::Rc};

use num::{complex::Complex64, BigInt, FromPrimitive, One, Signed, ToPrimitive, Zero};

use crate::mavagk_basic::{abstract_syntax_tree::{AnyTypeExpression, AnyTypeLValue, BoolExpression, ComplexExpression, ComplexLValue, FloatExpression, FloatLValue, IntExpression, IntLValue, StringExpression, StringLValue}, error::{Error, ErrorVariant}, machine::{Machine, StoredValues}, options::{AngleOption, Options}, program::Program, token::{IdentifierType, SuppliedFunction}};

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

pub fn print_float<T: Write>(value: f64, f: &mut T, print_leading_positive_space: bool, print_trailing_space: bool, print_leading_plus: bool, max_width: u8) -> io::Result<()> {
	//write!(f, "{0:.1$}", self.value, 5)
	let value_abs = value.abs();
	let is_negative = value < 0.;
	match (is_negative, print_leading_positive_space, print_leading_plus) {
		(true, _, _) => write!(f, "-")?,
		(false, false, false) => {},
		(false, true, false) => write!(f, " ")?,
		(false, _, true) => write!(f, "+")?,
	}
	let common_width = (is_negative || print_leading_positive_space) as u8 + print_trailing_space as u8 + 1;
	let abs_max_length = max_width as u16 - common_width as u16;
	let value_abs_integer_part = value_abs.floor();
	if !value.is_finite() {
		write!(f, "{value}")?;
		return match print_trailing_space {
			true => write!(f, " "),
			false => write!(f, ""),
		};
	}
	if value_abs >= 1. {
		let value_abs_integer_part_digit_count = match value_abs_integer_part {
			_ if value_abs_integer_part == 0. => 1,
			_ => value_abs_integer_part.log10().floor() as u16 + 1,
		};
		if value_abs_integer_part_digit_count <= abs_max_length {
			write!(f, "{}", format!("{}", value_abs).chars().take(abs_max_length as usize).collect::<String>().trim_end_matches('.'))?;
		}
		else {
			write!(f, "{}", format!("{0:.1$E}", value_abs, (abs_max_length - 7) as usize))?;
		}
		return match print_trailing_space {
			true => write!(f, " "),
			false => write!(f, ""),
		};
	}
	let mut number_full: String = match value_abs {
		_ if value_abs == 0. => "0".into(),
		_ => format!("{value_abs}").trim_start_matches('0').into(),
	};
	if number_full.chars().nth(1) == Some('0') && number_full.chars().count() > abs_max_length as usize {
		write!(f, "{}", format!("{0:.1$E}", value_abs, (abs_max_length - 7) as usize))?;
	}
	else {
		number_full.truncate(abs_max_length as usize);
		write!(f, "{number_full}")?;
	}
	match print_trailing_space {
		true => write!(f, " "),
		false => write!(f, ""),
	}
}

pub trait Value: Default + Clone {
	type ExpressionType: Clone;
	type LValueType;

	fn get_l_value_name<'a>(l_value: &'a Self::LValueType) -> &'a str;
	fn get_l_value_arguments<'a>(l_value: &'a Self::LValueType) -> &'a [AnyTypeExpression];
	fn get_l_value_has_parentheses(l_value: &Self::LValueType) -> bool;
	fn get_l_value_start_column(l_value: &Self::LValueType) -> NonZeroUsize;
	fn get_l_value_supplied_function(l_value: &Self::LValueType) -> Option<SuppliedFunction>;

	fn get_stored_values<'a>(machine: &'a Machine) -> &'a StoredValues<Self>;
	fn get_stored_values_mut<'a>(machine: &'a mut Machine) -> &'a mut StoredValues<Self>;

	fn get_array_declarations<'a>(program: &'a Program) -> &'a HashMap<Box<str>, HashSet<(Rc<BigInt>, usize)>>;
	fn get_array_declarations_mut<'a>(program: &'a mut Program) -> &'a mut HashMap<Box<str>, HashSet<(Rc<BigInt>, usize)>>;
	fn get_function_declarations<'a>(program: &'a Program) -> &'a HashMap<(Box<str>, usize), HashSet<(Rc<BigInt>, usize)>>;
	fn get_function_declarations_mut<'a>(program: &'a mut Program) -> &'a mut HashMap<(Box<str>, usize), HashSet<(Rc<BigInt>, usize)>>;

	fn slice_chars(to_slice: &Self, char_first_ones_index: IntValue, char_last_ones_index: IntValue) -> Self;
	fn insert_slice_chars(to_slice: &mut Self, char_first_ones_index: IntValue, char_last_ones_index: IntValue, replace_with: Self);

	fn get_local_variables<'a>(machine: &'a Machine) -> &'a HashMap<Box<str>, Self>;
	fn get_local_variables_mut<'a>(machine: &'a mut Machine) -> &'a mut HashMap<Box<str>, Self>;

	fn get_string_slicings<'a>(l_value: &'a Self::LValueType) -> Option<&'a Box<[(Box<IntExpression>, Box<IntExpression>, NonZeroUsize)]>>;
	fn get_string_slicings_mut<'a>(l_value: &'a mut Self::LValueType) -> Option<&'a mut Box<[(Box<IntExpression>, Box<IntExpression>, NonZeroUsize)]>>;

	fn execute_expression(machine: &mut Machine, expression: &Self::ExpressionType, program: Option<&Program>) -> Result<Self, Error>;
	fn execute_supplied_function(machine: &mut Machine, l_value: &Self::LValueType, program: Option<&Program>) -> Result<Option<Self>, Error>;

	const IDENTIFIER_TYPE: IdentifierType;
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

	pub fn from_u32(value: u32) -> Self {
		Self::new(Rc::new(BigInt::from_u32(value).unwrap()))
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
		match self {
			_ if self.is_positive() => Self::one(),
			_ if self.is_zero()     => Self::zero(),
			_                       => Self::new(Rc::new(BigInt::from_i8(-1).unwrap())),
		}
	}

	pub fn sqrt(self) -> Result<Self, ErrorVariant> {
		match self {
			value if value.is_negative() => return Err(ErrorVariant::SquareRootOfNegative),
			value => Ok(Self::new(Rc::new(value.value.sqrt()))),
		}
	}

	pub fn abs(self) -> Self {
		Self::new(Rc::new(self.value.abs()))
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
		(*int) *= &*rhs.value;
		self
	}

	pub fn print<T: Write>(&self, f: &mut T, print_leading_positive_space: bool, print_trailing_space: bool, options: &Options) -> io::Result<()> {
		print_float(self.clone().to_float().value, f, print_leading_positive_space, print_trailing_space, false, options.get_print_zone_width())
	}

	pub fn to_usize(&self) -> Option<usize> {
		self.value.to_usize()
	}

	pub fn ones_index_to_zeros_index_usize_saturating(self) -> usize {
		let minus_one = self.sub(IntValue::one());
		match minus_one.to_usize() {
			Some(value) => value,
			None => match minus_one.is_negative() {
				true => 0,
				false => usize::MAX,
			}
		}
	}

	pub fn to_i32(&self) -> Option<i32> {
		self.value.to_i32()
	}

	pub fn to_u32(&self) -> Option<u32> {
		self.value.to_u32()
	}

	pub fn from_ones_index_to_usize(&self) -> Option<usize> {
		self.value.to_usize().and_then(|index| index.checked_sub(1))
	}
}

impl Value for IntValue {
	type ExpressionType = IntExpression;
	type LValueType = IntLValue;

	fn get_l_value_name<'a>(l_value: &'a Self::LValueType) -> &'a str {
		&l_value.name
	}

	fn get_l_value_arguments<'a>(l_value: &'a Self::LValueType) -> &'a [AnyTypeExpression] {
		&l_value.arguments
	}

	fn get_l_value_has_parentheses(l_value: &Self::LValueType) -> bool {
		l_value.has_parentheses
	}

	fn get_l_value_start_column(l_value: &Self::LValueType) -> NonZeroUsize {
		l_value.start_column
	}

	fn get_stored_values<'a>(machine: &'a Machine) -> &'a StoredValues<Self> {
		&machine.int_stored_values
	}

	fn get_stored_values_mut<'a>(machine: &'a mut Machine) -> &'a mut StoredValues<Self> {
		&mut machine.int_stored_values
	}

	fn get_array_declarations<'a>(program: &'a Program) -> &'a HashMap<Box<str>, HashSet<(Rc<BigInt>, usize)>> {
		&program.int_array_declarations
	}

	fn get_array_declarations_mut<'a>(program: &'a mut Program) -> &'a mut HashMap<Box<str>, HashSet<(Rc<BigInt>, usize)>> {
		&mut program.int_array_declarations
	}

	fn get_function_declarations<'a>(program: &'a Program) -> &'a HashMap<(Box<str>, usize), HashSet<(Rc<BigInt>, usize)>> {
		&program.int_functions
	}

	fn get_function_declarations_mut<'a>(program: &'a mut Program) -> &'a mut HashMap<(Box<str>, usize), HashSet<(Rc<BigInt>, usize)>> {
		&mut program.int_functions
	}

	fn execute_expression(machine: &mut Machine, expression: &Self::ExpressionType, program: Option<&Program>) -> Result<Self, Error> {
		machine.execute_int_expression(expression, program)
	}

	fn get_local_variables<'a>(machine: &'a Machine) -> &'a HashMap<Box<str>, Self> {
		&machine.gosub_stack.last().unwrap().local_int_variables
	}

	fn get_local_variables_mut<'a>(machine: &'a mut Machine) -> &'a mut HashMap<Box<str>, Self> {
		&mut machine.gosub_stack.last_mut().unwrap().local_int_variables
	}

	fn get_l_value_supplied_function(l_value: &Self::LValueType) -> Option<SuppliedFunction> {
		l_value.supplied_function
	}

	fn execute_supplied_function(machine: &mut Machine, l_value: &Self::LValueType, program: Option<&Program>) -> Result<Option<Self>, Error> {
		machine.execute_int_supplied_function(l_value, program)
	}

	fn get_string_slicings<'a>(_l_value: &'a Self::LValueType) -> Option<&'a Box<[(Box<IntExpression>, Box<IntExpression>, NonZeroUsize)]>> {
		None
	}

	fn get_string_slicings_mut<'a>(_l_value: &'a mut Self::LValueType) -> Option<&'a mut Box<[(Box<IntExpression>, Box<IntExpression>, NonZeroUsize)]>> {
		None
	}

	fn slice_chars(_to_slice: &Self, _char_first_ones_index: IntValue, _char_last_ones_index: IntValue) -> Self {
		unimplemented!()
	}

	fn insert_slice_chars(_to_slice: &mut Self, _char_first_ones_index: IntValue, _char_last_ones_index: IntValue, _replace_with: Self) {
		unimplemented!()
	}

	const IDENTIFIER_TYPE: IdentifierType = IdentifierType::Integer;
}

impl Default for IntValue {
	fn default() -> Self {
		IntValue::zero()
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
	pub fn try_new(value: f64, options: Option<&Options>) -> Result<Self, ErrorVariant> {
		match value.is_finite() || options.is_some_and(|options| options.allow_overflow()) {
			true => Ok(Self::new(value)),
			false => Err(ErrorVariant::ValueOverflow),
		}
	}

	pub const fn from_usize(value: usize) -> Self {
		Self::new(value as f64)
	}

	pub const fn from_u32(value: u32) -> Self {
		Self::new(value as f64)
	}

	pub const fn from_i32(value: i32) -> Self {
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

	pub fn add(self, rhs: Self, options: Option<&Options>) -> Result<Self, ErrorVariant> {
		let float_result = self.value + rhs.value;
		match float_result.is_finite() || options.is_some_and(|options| options.allow_overflow()) {
			true => Ok(Self::new(float_result)),
			false => Err(ErrorVariant::ValueOverflow),
		}
	}

	pub fn sub(self, rhs: Self, options: Option<&Options>) -> Result<Self, ErrorVariant> {
		let float_result = self.value - rhs.value;
		match float_result.is_finite() || options.is_some_and(|options| options.allow_overflow()) {
			true => Ok(Self::new(float_result)),
			false => Err(ErrorVariant::ValueOverflow),
		}
	}

	pub fn mul(self, rhs: Self, options: Option<&Options>) -> Result<Self, ErrorVariant> {
		let float_result = self.value * rhs.value;
		match float_result.is_finite() || options.is_some_and(|options| options.allow_overflow()) {
			true => Ok(Self::new(float_result)),
			false => Err(ErrorVariant::ValueOverflow),
		}
	}

	pub fn div(self, rhs: Self, options: Option<&Options>) -> Result<Self, ErrorVariant> {
		if rhs.is_zero() && !options.is_some_and(|options| options.allow_divide_by_zero()) {
			return Err(ErrorVariant::DivisionByZero);
		}
		let float_result = self.value / rhs.value;
		match float_result.is_finite() || options.is_some_and(|options| options.allow_overflow()) {
			true => Ok(Self::new(float_result)),
			false => Err(ErrorVariant::ValueOverflow),
		}
	}

	pub fn basic_modulo(self, rhs: Self, options: &Options) -> Result<Self, ErrorVariant> {
		if rhs.is_zero() && !options.allow_divide_by_zero() {
			return Err(ErrorVariant::ModOrRemainderByZero);
		}
		let float_result = self.value - rhs.value * (self.value / rhs.value).floor();
		match float_result.is_finite() || options.allow_divide_by_zero() {
			true => Ok(Self::new(float_result)),
			false => Err(ErrorVariant::ValueOverflow),
		}
	}

	pub fn remainder(self, rhs: Self, options: &Options) -> Result<Self, ErrorVariant> {
		if rhs.is_zero() && !options.allow_divide_by_zero() {
			return Err(ErrorVariant::ModOrRemainderByZero);
		}
		let float_result = self.value % rhs.value;
		match float_result.is_finite() || options.allow_divide_by_zero() {
			true => Ok(Self::new(float_result)),
			false => Err(ErrorVariant::ValueOverflow),
		}
	}

	pub fn pow(self, rhs: Self, options: Option<&Options>) -> Result<Self, ErrorVariant> {
		if self.is_negative() && !rhs.is_int() && !options.is_some_and(|options| options.allow_negative_to_non_int_power()) {
			return Err(ErrorVariant::NegativeNumberRaisedToNonIntegerPower);
		}
		let float_result = self.value.powf(rhs.value);
		match float_result.is_finite() || options.is_some_and(|options| options.allow_overflow()) {
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

	pub fn ceil(self) -> Self {
		Self::new(self.value.ceil())
	}

	pub fn sqrt(self, options: &Options) -> Result<Self, ErrorVariant> {
		match self {
			value if value.is_negative() && !options.allow_real_square_root_of_negative() => return Err(ErrorVariant::SquareRootOfNegative),
			value => Ok(Self::new(value.value.sqrt())),
		}
	}

	pub fn sin(self, options: &Options) -> Result<Self, ErrorVariant> {
		Ok(Self::new(self.to_radians(options)?.value.sin()))
	}

	pub fn cos(self, options: &Options) -> Result<Self, ErrorVariant> {
		Ok(Self::new(self.to_radians(options)?.value.cos()))
	}

	pub fn tan(self, options: &Options) -> Result<Self, ErrorVariant> {
		Self::try_new(self.to_radians(options)?.value.tan(), Some(options))
	}

	pub fn cot(self, options: &Options) -> Result<Self, ErrorVariant> {
		let input_value_in_radians = self.to_radians(options)?.value;
		Self::try_new(input_value_in_radians.cos() / input_value_in_radians.sin(), Some(options))
	}

	pub fn sec(self, options: &Options) -> Result<Self, ErrorVariant> {
		Self::try_new(1. / self.to_radians(options)?.value.cos(), Some(options))
	}

	pub fn csc(self, options: &Options) -> Result<Self, ErrorVariant> {
		Self::try_new(1. / self.to_radians(options)?.value.sin(), Some(options))
	}

	pub fn asin(self, options: &Options) -> Result<Self, ErrorVariant> {
		let out = self.value.asin();
		if !out.is_finite() && !options.allow_real_trig_out_of_range() {
			return Err(ErrorVariant::ATrigFunctionOutOfRange);
		}
		Self::new(out).from_radians(options)
	}

	pub fn acos(self, options: &Options) -> Result<Self, ErrorVariant> {
		let out = self.value.acos();
		if !out.is_finite() && !options.allow_real_trig_out_of_range() {
			return Err(ErrorVariant::ATrigFunctionOutOfRange);
		}
		Self::new(out).from_radians(options)
	}

	pub fn atan(self, options: &Options) -> Result<Self, ErrorVariant> {
		Self::new(self.value.atan()).from_radians(options)
	}

	pub fn atan2(self, x: Self, options: &Options) -> Result<Self, ErrorVariant> {
		if self.is_zero() && x.is_zero() && !options.allow_angle_zero_zero() {
			return Err(ErrorVariant::AngleOfZeroZero);
		}
		let result = self.value.atan2(x.value);
		if !result.is_finite() && !options.allow_angle_zero_zero() {
			return Err(ErrorVariant::AngleOfZeroZero);
		}
		Self::new(result).from_radians(options)
	}

	pub fn sinh(self, options: &Options) -> Result<Self, ErrorVariant> {
		Self::try_new(self.value.sinh(), Some(options))
	}
	
	pub fn cosh(self, options: &Options) -> Result<Self, ErrorVariant> {
		Self::try_new(self.value.cosh(), Some(options))
	}

	pub fn tanh(self) -> Self {
		Self::new(self.value.tanh())
	}

	pub fn coth(self, options: &Options) -> Result<Self, ErrorVariant> {
		if self.is_zero() && !options.allow_tanh_csch_of_zero() {
			return Err(ErrorVariant::HyperbolicCotangentOrCosecantOfZero);
		}
		match Self::try_new(self.value.sinh() / self.value.cosh(), Some(options)) {
			Ok(value) => Ok(value),
			Err(_) => Err(ErrorVariant::HyperbolicCotangentOrCosecantOfZero),
		}
	}

	pub fn sech(self) -> Self {
		Self::new(1. / self.value.cosh())
	}

	pub fn csch(self, options: &Options) -> Result<Self, ErrorVariant> {
		if self.is_zero() && !options.allow_tanh_csch_of_zero() {
			return Err(ErrorVariant::HyperbolicCotangentOrCosecantOfZero);
		}
		match Self::try_new(1. / self.value.sinh(), Some(options)) {
			Ok(value) => Ok(value),
			Err(_) => Err(ErrorVariant::HyperbolicCotangentOrCosecantOfZero),
		}
	}

	pub fn asinh(self) -> Self {
		Self::new(self.value.asinh())
	}

	pub fn acosh(self, options: &Options) -> Result<Self, ErrorVariant> {
		if self.value < 1. && !options.allow_inverse_hyperbolic_trig_out_of_range() {
			return Err(ErrorVariant::HyperbolicATrigFunctionOutOfRange);
		}
		match Self::try_new(self.value.acosh(), Some(options)) {
			Ok(value) => Ok(value),
			Err(_) => Err(ErrorVariant::HyperbolicATrigFunctionOutOfRange),
		}
	}

	pub fn atanh(self, options: &Options) -> Result<Self, ErrorVariant> {
		if (self.value >= 1. || self.value <= -1.) && !options.allow_inverse_hyperbolic_trig_out_of_range() {
			return Err(ErrorVariant::HyperbolicATrigFunctionOutOfRange);
		}
		match Self::try_new(self.value.atanh(), Some(options)) {
			Ok(value) => Ok(value),
			Err(_) => Err(ErrorVariant::HyperbolicATrigFunctionOutOfRange),
		}
	}

	pub fn acoth(self, options: &Options) -> Result<Self, ErrorVariant> {
		if (self.value <= 1. && self.value >= -1.) && !options.allow_inverse_hyperbolic_trig_out_of_range() {
			return Err(ErrorVariant::HyperbolicATrigFunctionOutOfRange);
		}
		match Self::try_new((1. / self.value).atanh(), Some(options)) {
			Ok(value) => Ok(value),
			Err(_) => Err(ErrorVariant::HyperbolicATrigFunctionOutOfRange),
		}
	}

	pub fn asech(self, options: &Options) -> Result<Self, ErrorVariant> {
		if (self.value > 1. || self.value <= 0.) && !options.allow_inverse_hyperbolic_trig_out_of_range() {
			return Err(ErrorVariant::HyperbolicATrigFunctionOutOfRange);
		}
		match Self::try_new((1. / self.value).acosh(), Some(options)) {
			Ok(value) => Ok(value),
			Err(_) => Err(ErrorVariant::HyperbolicATrigFunctionOutOfRange),
		}
	}

	pub fn acsch(self, options: &Options) -> Result<Self, ErrorVariant> {
		if self.value == 0. && !options.allow_inverse_hyperbolic_trig_out_of_range() {
			return Err(ErrorVariant::HyperbolicATrigFunctionOutOfRange);
		}
		match Self::try_new((1. / self.value).asinh(), Some(options)) {
			Ok(value) => Ok(value),
			Err(_) => Err(ErrorVariant::HyperbolicATrigFunctionOutOfRange),
		}
	}

	pub fn min(self, other: Self) -> Self {
		Self::new(self.value.min(other.value))
	}

	pub fn max(self, other: Self) -> Self {
		Self::new(self.value.max(other.value))
	}

	pub fn acot(self, options: &Options) -> Result<Self, ErrorVariant> {
		Self::new(1. / (self.value).atan()).from_radians(options)
	}

	pub fn asec(self, options: &Options) -> Result<Self, ErrorVariant> {
		let out = (1. /self.value).acos();
		if !out.is_finite() && !options.allow_real_trig_out_of_range() {
			return Err(ErrorVariant::ATrigFunctionOutOfRange);
		}
		Self::new(out).from_radians(options)
	}

	pub fn acsc(self, options: &Options) -> Result<Self, ErrorVariant> {
		let out = (1. /self.value).asin();
		if !out.is_finite() && !options.allow_real_trig_out_of_range() {
			return Err(ErrorVariant::ATrigFunctionOutOfRange);
		}
		Self::new(out).from_radians(options)
	}

	pub fn exp(self, options: &Options) -> Result<Self, ErrorVariant> {
		Self::try_new(self.value.exp(), Some(options))
	}

	pub fn ln(self, options: &Options) -> Result<Self, ErrorVariant> {
		let result = self.value.ln();
		match result.is_finite() || options.allow_real_log_of_non_positive() {
			true => Ok(Self::new(result)),
			false => Err(ErrorVariant::LogOfNonPositive)
		}
	}
	pub fn log2(self, options: &Options) -> Result<Self, ErrorVariant> {
		let result = self.value.log2();
		match result.is_finite() || options.allow_real_log_of_non_positive() {
			true => Ok(Self::new(result)),
			false => Err(ErrorVariant::LogOfNonPositive)
		}
	}
	pub fn log10(self, options: &Options) -> Result<Self, ErrorVariant> {
		let result = self.value.log10();
		match result.is_finite() || options.allow_real_log_of_non_positive() {
			true => Ok(Self::new(result)),
			false => Err(ErrorVariant::LogOfNonPositive)
		}
	}

	pub fn degrees_to_radians(self) -> Self {
		Self::new(self.value / 180. * PI)
	}

	pub fn radians_to_degrees(self, options: &Options) -> Result<Self, ErrorVariant> {
		Self::try_new(self.value / PI * 180., Some(options))
	}

	/// Rounds a number towards zero.
	pub fn integer_part(self) -> Self {
		Self::new(self.value.trunc())
	}

	pub fn fractional_part(self) -> Self {
		Self::new((self.value % 1.).abs())
	}

	pub fn truncate_to_digits(self, digits: IntValue) -> Self {
		let sign = self.value.signum();
		let value_abs = self.value.abs();
		let digits = match digits.clone().neg().to_i32() {
			Some(digits) => digits,
			None => match digits.is_negative() {
				true => return FloatValue::ZERO,
				false => return self,
			},
		};
		let truncate_to = f64::powi(10., digits);
		let out = (value_abs - value_abs % truncate_to) * sign;
		match out.is_finite() {
			true => FloatValue::new(out),
			false => match digits.is_negative() {
				false => return FloatValue::ZERO,
				true => return self,
			},
		}
	}

	pub fn round_to_digits(self, digits: IntValue) -> Self {
		let sign = self.value.signum();
		let digits = match digits.clone().neg().to_i32() {
			Some(digits) => digits,
			None => match digits.is_negative() {
				true => return FloatValue::ZERO,
				false => return self,
			},
		};
		let truncate_to = f64::powi(10., digits);
		let value_abs = match sign.is_sign_positive() {
			true => self.value.abs() + truncate_to / 2.,
			false => self.value.abs() - truncate_to / 2.
		};
		let out = (value_abs - value_abs % truncate_to) * sign;
		match out.is_finite() {
			true => FloatValue::new(out),
			false => match digits.is_negative() {
				false => return FloatValue::ZERO,
				true => return self,
			},
		}
	}

	pub fn basic_eps(self) -> Self {
		Self::new(match self.value.next_up().max(f64::MIN_POSITIVE) {
			value if !value.is_finite() => f64::MAX,
			value => value,
		})
	}

	pub const fn to_radians(self, options: &Options) -> Result<Self, ErrorVariant> {
		Ok(Self::new(match options.get_angle_option() {
			AngleOption::Radians => self.value,
			AngleOption::Degrees => self.value / 180. * PI,
			AngleOption::Gradians => self.value / 200. * PI,
			AngleOption::Revolutions => {
				let radians = self.value * 2. * PI;
				if !radians.is_finite() && !options.allow_overflow() {
					return Err(ErrorVariant::ValueOverflow);
				}
				radians
			}
		}))
	}

	pub const fn from_radians(self, options: &Options) -> Result<Self, ErrorVariant> {
		let out = match options.get_angle_option() {
			AngleOption::Radians => self.value,
			AngleOption::Degrees => self.value / PI * 180.,
			AngleOption::Gradians => self.value / PI * 200.,
			AngleOption::Revolutions => self.value / 2. / PI,
		};
		if !out.is_finite() && !options.allow_overflow() {
			return Err(ErrorVariant::ValueOverflow);
		}
		Ok(Self::new(out))
	}

	pub fn print<T: Write>(&self, f: &mut T, print_leading_positive_space: bool, print_trailing_space: bool, options: &Options) -> io::Result<()> {
		print_float(self.value, f, print_leading_positive_space, print_trailing_space, false, options.get_print_zone_width())
	}
}

impl Value for FloatValue {
	type ExpressionType = FloatExpression;
	type LValueType = FloatLValue;
	
	fn get_l_value_name<'a>(l_value: &'a Self::LValueType) -> &'a str {
		&l_value.name
	}

	fn get_l_value_arguments<'a>(l_value: &'a Self::LValueType) -> &'a [AnyTypeExpression] {
		&l_value.arguments
	}

	fn get_l_value_has_parentheses(l_value: &Self::LValueType) -> bool {
		l_value.has_parentheses
	}

	fn get_l_value_start_column(l_value: &Self::LValueType) -> NonZeroUsize {
		l_value.start_column
	}

	fn get_stored_values<'a>(machine: &'a Machine) -> &'a StoredValues<Self> {
		&machine.float_stored_values
	}

	fn get_stored_values_mut<'a>(machine: &'a mut Machine) -> &'a mut StoredValues<Self> {
		&mut machine.float_stored_values
	}

	fn get_array_declarations<'a>(program: &'a Program) -> &'a HashMap<Box<str>, HashSet<(Rc<BigInt>, usize)>> {
		&program.float_array_declarations
	}

	fn get_array_declarations_mut<'a>(program: &'a mut Program) -> &'a mut HashMap<Box<str>, HashSet<(Rc<BigInt>, usize)>> {
		&mut program.float_array_declarations
	}

	fn get_function_declarations<'a>(program: &'a Program) -> &'a HashMap<(Box<str>, usize), HashSet<(Rc<BigInt>, usize)>> {
		&program.float_functions
	}

	fn get_function_declarations_mut<'a>(program: &'a mut Program) -> &'a mut HashMap<(Box<str>, usize), HashSet<(Rc<BigInt>, usize)>> {
		&mut program.float_functions
	}

	fn execute_expression(machine: &mut Machine, expression: &Self::ExpressionType, program: Option<&Program>) -> Result<Self, Error> {
		machine.execute_float_expression(expression, program)
	}

	fn get_local_variables<'a>(machine: &'a Machine) -> &'a HashMap<Box<str>, Self> {
		&machine.gosub_stack.last().unwrap().local_float_variables
	}

	fn get_local_variables_mut<'a>(machine: &'a mut Machine) -> &'a mut HashMap<Box<str>, Self> {
		&mut machine.gosub_stack.last_mut().unwrap().local_float_variables
	}

	fn get_l_value_supplied_function(l_value: &Self::LValueType) -> Option<SuppliedFunction> {
		l_value.supplied_function
	}

	fn execute_supplied_function(machine: &mut Machine, l_value: &Self::LValueType, program: Option<&Program>) -> Result<Option<Self>, Error> {
		machine.execute_float_supplied_function(l_value, program)
	}

	fn get_string_slicings<'a>(_l_value: &'a Self::LValueType) -> Option<&'a Box<[(Box<IntExpression>, Box<IntExpression>, NonZeroUsize)]>> {
		None
	}

	fn get_string_slicings_mut<'a>(_l_value: &'a mut Self::LValueType) -> Option<&'a mut Box<[(Box<IntExpression>, Box<IntExpression>, NonZeroUsize)]>> {
		None
	}

	fn slice_chars(_to_slice: &Self, _char_first_ones_index: IntValue, _char_last_ones_index: IntValue) -> Self {
		unimplemented!()
	}

	fn insert_slice_chars(_to_slice: &mut Self, _char_first_ones_index: IntValue, _char_last_ones_index: IntValue, _replace_with: Self) {
		unimplemented!()
	}

	const IDENTIFIER_TYPE: IdentifierType = IdentifierType::UnmarkedOrFloat;
}

impl Default for FloatValue {
	fn default() -> Self {
		FloatValue::ZERO
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
	pub fn try_new(value: Complex64, options: Option<&Options>) -> Result<Self, ErrorVariant> {
		match (value.re.is_finite() && value.im.is_finite()) || options.is_some_and(|options| options.allow_overflow()) {
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

	pub fn print<T: Write>(&self, f: &mut T, print_leading_positive_space: bool, print_trailing_space: bool, options: &Options) -> io::Result<()> {
		if self.value.im == 0. {
			return print_float(self.value.re, f, print_leading_positive_space, print_trailing_space, false, options.get_print_zone_width());
		}
		let full_width = options.get_print_zone_width().max(20);
		if self.value.re == 0. {
			print_float(self.value.im, f, print_leading_positive_space, false, false, full_width)?;
			write!(f, "I")?;
			if print_trailing_space {
				write!(f, " ")?;
			}
			return Ok(());
		}
		print_float(self.value.re, f, print_leading_positive_space, false, false, full_width / 2 - 1)?;
		print_float(self.value.im, f, false, false, true, full_width / 2 - 1)?;
		write!(f, "I")?;
		if print_trailing_space {
			write!(f, " ")?;
		}
		Ok(())
		//match (self.value.re, self.value.im, print_leading_positive_space) {
		//	(re, im, true) if re > 0. && im > 0. => write!(f, " {re}+{im}i"),
		//	(re, im, false) if re > 0. && im > 0. => write!(f, "{re}+{im}i"),
		//	(re, im, true) if re > 0. && im == 0. => write!(f, " {re}"),
		//	(re, im, false) if re > 0. && im == 0. => write!(f, "{re}"),
		//	(re, im, true) if re > 0. && im < 0. => write!(f, " {re}{im}i"),
		//	(re, im, false) if re > 0. && im < 0. => write!(f, "{re}{im}i"),
//
		//	(re, im, true) if re == 0. && im > 0. => write!(f, " {im}i"),
		//	(re, im, false) if re == 0. && im > 0. => write!(f, "{im}i"),
		//	(re, im, true) if re == 0. && im == 0. => write!(f, " 0"),
		//	(re, im, false) if re == 0. && im == 0. => write!(f, "0"),
		//	(re, im, _) if re == 0. && im < 0. => write!(f, "{im}i"),
//
		//	(re, im, _) if re < 0. && im > 0. => write!(f, "{re}+{im}i"),
		//	(re, im, _) if re < 0. && im == 0. => write!(f, "{re}"),
		//	(re, im, _) if re < 0. && im < 0. => write!(f, "{re}{im}i"),
		//	_ => unreachable!()
		//}?;
		//match print_trailing_space {
		//	true => write!(f, " "),
		//	false => write!(f, ""),
		//}
	}
}

impl Value for ComplexValue {
	type ExpressionType = ComplexExpression;
	type LValueType = ComplexLValue;
	
	fn get_l_value_name<'a>(l_value: &'a Self::LValueType) -> &'a str {
		&l_value.name
	}

	fn get_l_value_arguments<'a>(l_value: &'a Self::LValueType) -> &'a [AnyTypeExpression] {
		&l_value.arguments
	}

	fn get_l_value_has_parentheses(l_value: &Self::LValueType) -> bool {
		l_value.has_parentheses
	}

	fn get_l_value_start_column(l_value: &Self::LValueType) -> NonZeroUsize {
		l_value.start_column
	}

	fn get_stored_values<'a>(machine: &'a Machine) -> &'a StoredValues<Self> {
		&machine.complex_stored_values
	}

	fn get_stored_values_mut<'a>(machine: &'a mut Machine) -> &'a mut StoredValues<Self> {
		&mut machine.complex_stored_values
	}

	fn get_array_declarations<'a>(program: &'a Program) -> &'a HashMap<Box<str>, HashSet<(Rc<BigInt>, usize)>> {
		&program.complex_array_declarations
	}

	fn get_array_declarations_mut<'a>(program: &'a mut Program) -> &'a mut HashMap<Box<str>, HashSet<(Rc<BigInt>, usize)>> {
		&mut program.complex_array_declarations
	}

	fn get_function_declarations<'a>(program: &'a Program) -> &'a HashMap<(Box<str>, usize), HashSet<(Rc<BigInt>, usize)>> {
		&program.complex_functions
	}

	fn get_function_declarations_mut<'a>(program: &'a mut Program) -> &'a mut HashMap<(Box<str>, usize), HashSet<(Rc<BigInt>, usize)>> {
		&mut program.complex_functions
	}

	fn execute_expression(machine: &mut Machine, expression: &Self::ExpressionType, program: Option<&Program>) -> Result<Self, Error> {
		machine.execute_complex_expression(expression, program)
	}

	fn get_local_variables<'a>(machine: &'a Machine) -> &'a HashMap<Box<str>, Self> {
		&machine.gosub_stack.last().unwrap().local_complex_variables
	}

	fn get_local_variables_mut<'a>(machine: &'a mut Machine) -> &'a mut HashMap<Box<str>, Self> {
		&mut machine.gosub_stack.last_mut().unwrap().local_complex_variables
	}

	fn get_l_value_supplied_function(l_value: &Self::LValueType) -> Option<SuppliedFunction> {
		l_value.supplied_function
	}

	fn execute_supplied_function(machine: &mut Machine, l_value: &Self::LValueType, program: Option<&Program>) -> Result<Option<Self>, Error> {
		machine.execute_complex_supplied_function(l_value, program)
	}

	fn get_string_slicings<'a>(_l_value: &'a Self::LValueType) -> Option<&'a Box<[(Box<IntExpression>, Box<IntExpression>, NonZeroUsize)]>> {
		None
	}

	fn get_string_slicings_mut<'a>(_l_value: &'a mut Self::LValueType) -> Option<&'a mut Box<[(Box<IntExpression>, Box<IntExpression>, NonZeroUsize)]>> {
		None
	}

	fn slice_chars(_to_slice: &Self, _char_first_ones_index: IntValue, _char_last_ones_index: IntValue) -> Self {
		unimplemented!()
	}

	fn insert_slice_chars(_to_slice: &mut Self, _char_first_ones_index: IntValue, _char_last_ones_index: IntValue, _replace_with: Self) {
		unimplemented!()
	}

	const IDENTIFIER_TYPE: IdentifierType = IdentifierType::ComplexNumber;
}

impl Default for ComplexValue {
	fn default() -> Self {
		ComplexValue::ZERO
	}
}

impl Display for ComplexValue {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.value)
	}
}

#[derive(Debug, PartialEq, Eq, Clone)]
/// A string value in MavagkBASIC, contains a reference counted string.
pub struct StringValue {
	pub value: Rc<String>,
}

impl StringValue {
	/// Creates a `StringValue` from a `Rc<String>`.
	pub const fn new(value: Rc<String>) -> Self {
		Self {
			value,
		}
	}

	/// Returns a `BoolValue` with a false value if this string is empty, returns true if this string contains any chars.
	pub fn to_bool(&self) -> BoolValue {
		BoolValue::new(!self.is_empty())
	}

	/// Returns `true` if the string contains no chars and has a length of zero.
	pub fn is_empty(&self) -> bool {
		self.value.is_empty()
	}

	/// Creates a new empty `StringValue` containing no chars.
	pub fn empty() -> Self {
		Self::new(Rc::default())
	}

	/// Returns a `StringValue` containing the chars of `rhs` concatenated onto the end of `self`.
	pub fn concat(mut self, rhs: Self) -> Self {
		let string = Rc::<String>::make_mut(&mut self.value);
		string.push_str(&rhs.value);
		self
	}

	/// Returns a `StringValue` containing the chars of `self` but any uppercase chars will be replaced with their corresponding lowercase variant.
	pub fn to_lowercase(self) -> Self {
		Self::new(Rc::new(self.value.to_lowercase()))
	}

	/// Returns a `StringValue` containing the chars of `self` but any lowercase chars will be replaced with their corresponding uppercase variant.
	pub fn to_uppercase(self) -> Self {
		Self::new(Rc::new(self.value.to_uppercase()))
	}

	/// Returns a `StringValue` containing the chars of `self` but with leading spaces removed.
	pub fn trim_start_spaces(self) -> Self {
		Self::new(Rc::new(self.value.trim_start_matches(' ').into()))
	}

	/// Returns a `StringValue` containing the chars of `self` but with trailing spaces removed.
	pub fn trim_end_spaces(mut self) -> Self {
		let string = Rc::<String>::make_mut(&mut self.value);
		match string.rfind(|chr| chr != ' ') {
			Some(index_before_trailing_spaces_start) => string.truncate(index_before_trailing_spaces_start + 1),
			None => *string = String::new(),
		}
		self
	}

	/// Returns a `StringValue` containing the chars of `self` repeated a given amount of times.
	pub fn repeat(self, times: IntValue) -> Result<Self, ErrorVariant> {
		match times.to_usize() {
			// If we can repeat the string
			Some(times) => Ok(Self::new(Rc::new(self.value.repeat(times)))),
			// If the repeat amount is negative
			None if times.is_negative() => Err(ErrorVariant::NegativeRepeat),
			// If the input string is empty and the repeat amount is not negative, return an empty string
			None if self.is_empty() => Ok(self),
			// If the repeat amount is larger that what a usize can contain and the input string is not empty
			_ => Err(ErrorVariant::OutOfMemoryEvaluatingString),
		}
	}

	/// Returns a `BoolValue` with a true value if the two input strings are the same size and contain the same sequence of chars in the same order, else returns false.
	pub fn equal_to(&self, rhs: &Self) -> BoolValue {
		BoolValue::new(self.value == rhs.value)
	}

	/// Returns a `BoolValue` with a false value if the two input strings are the same size and contain the same sequence of chars in the same order, else returns true.
	pub fn not_equal_to(&self, rhs: &Self) -> BoolValue {
		BoolValue::new(self.value != rhs.value)
	}

	/// Returns how many chars (not necessarily bytes) the input string contains.
	pub fn count_chars(&self) -> usize {
		self.value.chars().count()
	}

	/// Returns the code point value of the first char in the string or returns 0 if the string is empty.
	pub fn value_of_first_char(&self, options: &Options) -> Result<u32, ErrorVariant> {
		match self.value.chars().next() {
			Some(value) => options.get_collate_option().to_u32(value),
			None => Ok(0),
		}
	}

	/// If the input string contains a single char, returns the code point value of the char.
	/// If the string is case-insensitively equal to a multi-character mnemonic for a char, returns the char, else throw an error.
	pub fn value_of_char_or_mnemonic(&self, options: &Options) -> Result<u32, ErrorVariant> {
		match self.count_chars() {
			0 => Err(ErrorVariant::InvalidOrdArgument),
			1 => Ok(options.get_collate_option().to_u32(self.value.chars().next().unwrap())?),
			_ => Err(ErrorVariant::NotYetImplemented("ORD on mnemonics".into())),
		}
	}

	/// Takes in an integer value and converts it to a string containing one char with the code point that matches the input value.
	pub fn from_char_value(int_value: IntValue, options: &Options) -> Result<Self, ErrorVariant> {
		// Convert the input value to a u32
		let int_value_u32 = match int_value.to_u32() {
			Some(int_value_u32) => int_value_u32,
			None => return Err(ErrorVariant::InvalidCharValue),
		};
		// Convert to a char depending on the collating option
		let chr = options.get_collate_option().from_u32(int_value_u32)?;
		// Convert char to a one char string
		Ok(StringValue::new(Rc::new(chr.into())))
	}

	/// Returns a string with the first `count_to_take` chars of `self` or the entire string if the count is larger than the input string length.
	pub fn take_left_chars(mut self, count_to_take: IntValue) -> Result<StringValue, ErrorVariant> {
		// Get how many bytes to take
		let char_count_to_take = match count_to_take.to_usize() {
			Some(char_count_to_take) => char_count_to_take,
			None if count_to_take.is_negative() => return Err(ErrorVariant::StrSliceFnOutOfRange),
			None => usize::MAX,
		};
		let byte_count_to_take = match self.char_index_to_byte_index(char_count_to_take) {
			Ok(byte_count_to_take) | Err(byte_count_to_take) => byte_count_to_take,
		};
		// Take or clone string
		let string = Rc::<String>::make_mut(&mut self.value);
		// Truncate to byte length
		string.truncate(byte_count_to_take);
		// Return
		Ok(self)
	}

	/// Returns `self` string with the last char removed.
	pub fn pop_last_char(mut self) -> Result<StringValue, ErrorVariant> {
		if self.is_empty() {
			return Err(ErrorVariant::StrSliceFnOutOfRange);
		}
		// Take or clone string
		let string = Rc::<String>::make_mut(&mut self.value);
		// Truncate
		string.pop();
		// Return
		Ok(self)
	}

	/// Returns a string with the last `count_to_take` chars of `self` or the entire string if the count is larger than the input string length.
	pub fn take_right_chars(&self, char_index_from: IntValue) -> Result<StringValue, ErrorVariant> {
		// Get how many chars to take
		let char_index_from = match char_index_from.to_usize() {
			Some(char_index_from) => char_index_from,
			None if char_index_from.is_negative() => return Err(ErrorVariant::StrSliceFnOutOfRange),
			None => usize::MAX,
		};
		if char_index_from == 0 {
			return Ok(StringValue::empty());
		}
		// Get how many bytes to take
		let mut start_from_byte_index = 0;
		for (char_count, (byte_index, _)) in self.value.char_indices().rev().enumerate() {
			if char_count == char_index_from {
				start_from_byte_index = byte_index + 1;
			}
		}
		// Return
		Ok(StringValue::new(Rc::new((&self.value[start_from_byte_index..]).to_string())))
	}

	/// Returns a string with containing the chars of `take_from_char_ones_index` starting from ones char index `take_from_char_ones_index`.
	/// Takes `count_to_take` chars. Takes all chars if `take_from_char_ones_index` is zero. Takes no chars if all chars are sliced off and no input integers ane negative.
	pub fn take_middle_chars(self, take_from_char_ones_index: IntValue, count_to_take: IntValue) -> Result<StringValue, ErrorVariant> {
		// Get how many bytes to take
		let take_from_char_index = match take_from_char_ones_index.to_usize() {
			Some(take_from_char_ones_index) => take_from_char_ones_index.saturating_sub(1),
			None if take_from_char_ones_index.is_negative() => return Err(ErrorVariant::StrSliceFnOutOfRange),
			None => usize::MAX,
		};
		let take_from_byte_index = match self.char_index_to_byte_index(take_from_char_index) {
			Ok(byte_count_to_take) | Err(byte_count_to_take) => byte_count_to_take,
		};
		// Get how many bytes to take
		let char_count_to_take = match count_to_take.to_usize() {
			Some(char_count_to_take) => char_count_to_take,
			None if count_to_take.is_negative() => return Err(ErrorVariant::StrSliceFnOutOfRange),
			None => usize::MAX,
		};
		let byte_end_index = match self.char_index_to_byte_index(char_count_to_take.saturating_add(take_from_byte_index)) {
			Ok(byte_count_to_take) | Err(byte_count_to_take) => byte_count_to_take,
		}.min(self.value.len());
		// Return
		Ok(StringValue::new(Rc::new((&self.value[take_from_byte_index..byte_end_index]).to_string())))
	}

	/// Returns a string with containing the chars of `self` starting from ones char index `char_first_ones_index` and ending at the index `char_last_ones_index`.
	/// Takes from the start if `take_from_char_ones_index` is zero. Takes no chars if all chars are sliced off and no input integers ane negative.
	pub fn slice_chars(&self, char_first_ones_index: IntValue, char_last_ones_index: IntValue) -> StringValue {
		if char_last_ones_index.is_zero() || char_last_ones_index.is_negative() {
			return StringValue::empty();
		}
		// Get indices to slice
		let start_char_zeros_index = char_first_ones_index.ones_index_to_zeros_index_usize_saturating();
		let start_byte_zeros_index = self.char_index_to_byte_index(start_char_zeros_index).unwrap_or_else(|index| index);
		let end_char_zeros_index = char_last_ones_index.ones_index_to_zeros_index_usize_saturating().saturating_add(1);
		let end_byte_zeros_index = self.char_index_to_byte_index(end_char_zeros_index).unwrap_or_else(|index| index);
		// Bounds check
		if start_byte_zeros_index >= end_byte_zeros_index {
			return StringValue::empty();
		}
		// Slice and return
		StringValue::new(Rc::new((&self.value[start_byte_zeros_index..end_byte_zeros_index]).into()))
	}

	/// Replaces the chars of `self` starting from ones char index `char_first_ones_index` and ending at the index `char_last_ones_index` with the chars of replace_with.
	/// Replaces from the start if `take_from_char_ones_index` is zero. Replaces no chars if all chars are sliced off and no input integers ane negative.
	pub fn insert_slice_chars(&mut self, char_first_ones_index: IntValue, char_last_ones_index: IntValue, replace_with: StringValue) {
		//if char_last_ones_index.is_zero() || char_last_ones_index.is_negative() {
		//	return StringValue::empty();
		//}
		// Get indices to slice
		let start_char_zeros_index = char_first_ones_index.ones_index_to_zeros_index_usize_saturating();
		let start_byte_zeros_index = self.char_index_to_byte_index(start_char_zeros_index).unwrap_or_else(|index| index);
		let end_char_zeros_index = char_last_ones_index.ones_index_to_zeros_index_usize_saturating().saturating_add(1);
		let mut end_byte_zeros_index = self.char_index_to_byte_index(end_char_zeros_index).unwrap_or_else(|index| index);
		// Bounds check
		if start_byte_zeros_index >= end_byte_zeros_index {
			end_byte_zeros_index = start_byte_zeros_index;
		}
		// Slice and return
		let string = Rc::<String>::make_mut(&mut self.value);
		string.replace_range(start_byte_zeros_index..end_byte_zeros_index, &replace_with.value);
	}

	/// Returns a string containing only the last char of `self`.
	pub fn take_last_char(&self) -> Result<StringValue, ErrorVariant> {
		if self.is_empty() {
			return Err(ErrorVariant::StrSliceFnOutOfRange);
		}
		Ok(StringValue::new(Rc::new(self.value.chars().last().unwrap().into())))
	}

	/// Returns the byte index of the char with the input char index. Returns the byte length of the string wrapped in an `Err` variant if the char index does not exist.
	pub fn char_index_to_byte_index(&self, char_index: usize) -> Result<usize, usize> {
		if self.is_empty() {
			return Err(0);
		}
		let mut iter = self.value.char_indices().enumerate().peekable();
		loop {
			let (iter_char_index, (byte_index, _)) = iter.next().unwrap();
			if iter_char_index == char_index {
				return Ok(byte_index);
			}
			if iter.peek().is_none() {
				return Err(self.value.len());
			}
		}
	}

	/// Returns the char index of the first occurrence of `substring` in `self`, returns `None` if it is not a substring.
	pub fn find_substring_char_index(&self, substring: &StringValue) -> Option<usize> {
		let byte_index = self.value.find(&**substring.value)?;
		for (char_index, (x, _)) in self.value.char_indices().enumerate() {
			if x == byte_index {
				return Some(char_index);
			}
		}
		unreachable!()
	}

	/// Prints the string to the console.
	pub fn print<T: Write>(&self, f: &mut T) -> io::Result<()> {
		write!(f, "{}", self.value)
	}
}

impl Value for StringValue {
	type ExpressionType = StringExpression;
	type LValueType = StringLValue;
	
	fn get_l_value_name<'a>(l_value: &'a Self::LValueType) -> &'a str {
		&l_value.name
	}

	fn get_l_value_arguments<'a>(l_value: &'a Self::LValueType) -> &'a [AnyTypeExpression] {
		&l_value.arguments
	}

	fn get_l_value_has_parentheses(l_value: &Self::LValueType) -> bool {
		l_value.has_parentheses
	}

	fn get_l_value_start_column(l_value: &Self::LValueType) -> NonZeroUsize {
		l_value.start_column
	}

	fn get_stored_values<'a>(machine: &'a Machine) -> &'a StoredValues<Self> {
		&machine.string_stored_values
	}

	fn get_stored_values_mut<'a>(machine: &'a mut Machine) -> &'a mut StoredValues<Self> {
		&mut machine.string_stored_values
	}

	fn get_array_declarations<'a>(program: &'a Program) -> &'a HashMap<Box<str>, HashSet<(Rc<BigInt>, usize)>> {
		&program.string_array_declarations
	}

	fn get_array_declarations_mut<'a>(program: &'a mut Program) -> &'a mut HashMap<Box<str>, HashSet<(Rc<BigInt>, usize)>> {
		&mut program.string_array_declarations
	}

	fn get_function_declarations<'a>(program: &'a Program) -> &'a HashMap<(Box<str>, usize), HashSet<(Rc<BigInt>, usize)>> {
		&program.string_functions
	}

	fn get_function_declarations_mut<'a>(program: &'a mut Program) -> &'a mut HashMap<(Box<str>, usize), HashSet<(Rc<BigInt>, usize)>> {
		&mut program.string_functions
	}

	fn execute_expression(machine: &mut Machine, expression: &Self::ExpressionType, program: Option<&Program>) -> Result<Self, Error> {
		machine.execute_string_expression(expression, program)
	}

	fn get_local_variables<'a>(machine: &'a Machine) -> &'a HashMap<Box<str>, Self> {
		&machine.gosub_stack.last().unwrap().local_string_variables
	}

	fn get_local_variables_mut<'a>(machine: &'a mut Machine) -> &'a mut HashMap<Box<str>, Self> {
		&mut machine.gosub_stack.last_mut().unwrap().local_string_variables
	}

	fn get_l_value_supplied_function(l_value: &Self::LValueType) -> Option<SuppliedFunction> {
		l_value.supplied_function
	}

	fn execute_supplied_function(machine: &mut Machine, l_value: &Self::LValueType, program: Option<&Program>) -> Result<Option<Self>, Error> {
		machine.execute_string_supplied_function(l_value, program)
	}

	fn get_string_slicings<'a>(l_value: &'a Self::LValueType) -> Option<&'a Box<[(Box<IntExpression>, Box<IntExpression>, NonZeroUsize)]>> {
		Some(&l_value.string_slicings)
	}

	fn get_string_slicings_mut<'a>(l_value: &'a mut Self::LValueType) -> Option<&'a mut Box<[(Box<IntExpression>, Box<IntExpression>, NonZeroUsize)]>> {
		Some(&mut l_value.string_slicings)
	}

	fn slice_chars(to_slice: &Self, char_first_ones_index: IntValue, char_last_ones_index: IntValue) -> Self {
		to_slice.slice_chars(char_first_ones_index, char_last_ones_index)
	}

	fn insert_slice_chars(to_slice: &mut Self, char_first_ones_index: IntValue, char_last_ones_index: IntValue, replace_with: Self) {
		to_slice.insert_slice_chars(char_first_ones_index, char_last_ones_index, replace_with);
	}

	const IDENTIFIER_TYPE: IdentifierType = IdentifierType::String;
}

impl Default for StringValue {
	fn default() -> Self {
		StringValue::empty()
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

	pub fn print<T: Write>(&self, f: &mut T, print_leading_positive_space: bool, print_trailing_space: bool, _options: &Options) -> io::Result<()> {
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

impl Value for BoolValue {
	type ExpressionType = BoolExpression;
	type LValueType = ();

	fn get_l_value_name<'a>(_l_value: &'a Self::LValueType) -> &'a str {
		unimplemented!()
	}

	fn get_l_value_arguments<'a>(_l_value: &'a Self::LValueType) -> &'a [AnyTypeExpression] {
		unimplemented!()
	}

	fn get_l_value_has_parentheses(_l_value: &Self::LValueType) -> bool {
		unimplemented!()
	}

	fn get_l_value_start_column(_l_value: &Self::LValueType) -> NonZeroUsize {
		unimplemented!()
	}

	fn get_stored_values<'a>(_machine: &'a Machine) -> &'a StoredValues<Self> {
		unimplemented!()
	}

	fn get_stored_values_mut<'a>(_machine: &'a mut Machine) -> &'a mut StoredValues<Self> {
		unimplemented!()
	}

	fn get_array_declarations<'a>(_program: &'a Program) -> &'a HashMap<Box<str>, HashSet<(Rc<BigInt>, usize)>> {
		unimplemented!()
	}

	fn get_array_declarations_mut<'a>(_program: &'a mut Program) -> &'a mut HashMap<Box<str>, HashSet<(Rc<BigInt>, usize)>> {
		unimplemented!()
	}

	fn get_function_declarations<'a>(_program: &'a Program) -> &'a HashMap<(Box<str>, usize), HashSet<(Rc<BigInt>, usize)>> {
		unimplemented!()
	}

	fn get_function_declarations_mut<'a>(_program: &'a mut Program) -> &'a mut HashMap<(Box<str>, usize), HashSet<(Rc<BigInt>, usize)>> {
		unimplemented!()
	}

	fn execute_expression(machine: &mut Machine, expression: &Self::ExpressionType, program: Option<&Program>) -> Result<Self, Error> {
		machine.execute_bool_expression(expression, program)
	}

	fn get_local_variables<'a>(_machine: &'a Machine) -> &'a HashMap<Box<str>, Self> {
		unimplemented!()
	}

	fn get_local_variables_mut<'a>(_machine: &'a mut Machine) -> &'a mut HashMap<Box<str>, Self> {
		unimplemented!()
	}

	fn get_l_value_supplied_function(_l_value: &Self::LValueType) -> Option<SuppliedFunction> {
		unimplemented!()
	}

	fn execute_supplied_function(_machine: &mut Machine, _l_value: &Self::LValueType, _program: Option<&Program>) -> Result<Option<Self>, Error> {
		unimplemented!()
	}

	fn get_string_slicings<'a>(_l_value: &'a Self::LValueType) -> Option<&'a Box<[(Box<IntExpression>, Box<IntExpression>, NonZeroUsize)]>> {
		None
	}

	fn get_string_slicings_mut<'a>(_l_value: &'a mut Self::LValueType) -> Option<&'a mut Box<[(Box<IntExpression>, Box<IntExpression>, NonZeroUsize)]>> {
		None
	}

	fn slice_chars(_to_slice: &Self, _char_first_ones_index: IntValue, _char_last_ones_index: IntValue) -> Self {
		unimplemented!()
	}

	fn insert_slice_chars(_to_slice: &mut Self, _char_first_ones_index: IntValue, _char_last_ones_index: IntValue, _replace_with: Self) {
		unimplemented!()
	}

	const IDENTIFIER_TYPE: IdentifierType = IdentifierType::UnmarkedOrFloat;
}

impl Default for BoolValue {
	fn default() -> Self {
		BoolValue::FALSE
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

	pub fn floor_to_int(self) -> Result<IntValue, ErrorVariant> {
		match self {
			AnyTypeValue::Bool(_) | AnyTypeValue::Int(_) => self.to_int(),
			_ => {
				let float_value = self.to_float()?.value.floor();
				match BigInt::from_f64(float_value) {
					Some(result) => Ok(IntValue::new(Rc::new(result))),
					None => Err(ErrorVariant::NonNumberValueCastToInt(float_value)),
				}
			}
		}
	}

	pub fn signum_to_int(self) -> Result<IntValue, ErrorVariant> {
		Ok(match self {
			AnyTypeValue::Bool(_) | AnyTypeValue::Int(_) => self.to_int()?.signum(),
			_ => self.to_float()?.signum().to_int().unwrap(),
		})
	}

	pub fn print<T: Write>(&self, f: &mut T, print_leading_positive_space: bool, print_trailing_numeric_space: bool, options: &Options) -> io::Result<()> {
		match self {
			AnyTypeValue::Bool(value) => value.print(f, print_leading_positive_space, print_trailing_numeric_space, options),
			AnyTypeValue::Int(value) => value.print(f, print_leading_positive_space, print_trailing_numeric_space, options),
			AnyTypeValue::Float(value) => value.print(f, print_leading_positive_space, print_trailing_numeric_space, options),
			AnyTypeValue::Complex(value) => value.print(f, print_leading_positive_space, print_trailing_numeric_space, options),
			AnyTypeValue::String(value) => value.print(f),
		}
	}
}

impl Value for AnyTypeValue {
	type ExpressionType = AnyTypeExpression;
	type LValueType = AnyTypeLValue;
	
	fn get_l_value_name<'a>(_l_value: &'a Self::LValueType) -> &'a str {
		unimplemented!()
	}

	fn get_l_value_arguments<'a>(_l_value: &'a Self::LValueType) -> &'a [AnyTypeExpression] {
		unimplemented!()
	}

	fn get_l_value_has_parentheses(_l_value: &Self::LValueType) -> bool {
		unimplemented!()
	}

	fn get_l_value_start_column(_l_value: &Self::LValueType) -> NonZeroUsize {
		unimplemented!()
	}

	fn get_stored_values<'a>(_machine: &'a Machine) -> &'a StoredValues<Self> {
		unimplemented!()
	}

	fn get_stored_values_mut<'a>(_machine: &'a mut Machine) -> &'a mut StoredValues<Self> {
		unimplemented!()
	}

	fn get_array_declarations<'a>(_program: &'a Program) -> &'a HashMap<Box<str>, HashSet<(Rc<BigInt>, usize)>> {
		unimplemented!()
	}

	fn get_array_declarations_mut<'a>(_program: &'a mut Program) -> &'a mut HashMap<Box<str>, HashSet<(Rc<BigInt>, usize)>> {
		unimplemented!()
	}

	fn get_function_declarations<'a>(_program: &'a Program) -> &'a HashMap<(Box<str>, usize), HashSet<(Rc<BigInt>, usize)>> {
		unimplemented!()
	}

	fn get_function_declarations_mut<'a>(_program: &'a mut Program) -> &'a mut HashMap<(Box<str>, usize), HashSet<(Rc<BigInt>, usize)>> {
		unimplemented!()
	}

	fn execute_expression(machine: &mut Machine, expression: &Self::ExpressionType, program: Option<&Program>) -> Result<Self, Error> {
		machine.execute_any_type_expression(expression, program)
	}

	fn get_local_variables<'a>(_machine: &'a Machine) -> &'a HashMap<Box<str>, Self> {
		unimplemented!()
	}

	fn get_local_variables_mut<'a>(_machine: &'a mut Machine) -> &'a mut HashMap<Box<str>, Self> {
		unimplemented!()
	}

	fn get_l_value_supplied_function(_l_value: &Self::LValueType) -> Option<SuppliedFunction> {
		unimplemented!()
	}

	fn execute_supplied_function(_machine: &mut Machine, _l_value: &Self::LValueType, _program: Option<&Program>) -> Result<Option<Self>, Error> {
		unimplemented!()
	}

	fn get_string_slicings<'a>(_l_value: &'a Self::LValueType) -> Option<&'a Box<[(Box<IntExpression>, Box<IntExpression>, NonZeroUsize)]>> {
		None
	}

	fn get_string_slicings_mut<'a>(_l_value: &'a mut Self::LValueType) -> Option<&'a mut Box<[(Box<IntExpression>, Box<IntExpression>, NonZeroUsize)]>> {
		None
	}

	fn slice_chars(_to_slice: &Self, _char_first_ones_index: IntValue, _char_last_ones_index: IntValue) -> Self {
		unimplemented!()
	}

	fn insert_slice_chars(_to_slice: &mut Self, _char_first_ones_index: IntValue, _char_last_ones_index: IntValue, _replace_with: Self) {
		unimplemented!()
	}

	const IDENTIFIER_TYPE: IdentifierType = IdentifierType::UnmarkedOrFloat;
}

impl Default for AnyTypeValue {
	fn default() -> Self {
		AnyTypeValue::Bool(BoolValue::FALSE)
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