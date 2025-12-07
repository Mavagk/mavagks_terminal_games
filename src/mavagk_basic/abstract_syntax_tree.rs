use std::num::NonZeroUsize;

use strum_macros::{EnumDiscriminants, EnumIter};

use crate::mavagk_basic::{options::{AngleOption, BaseOption, CollateOption, MachineOption, MathOption}, token::{IdentifierType, Token}, value::{BoolValue, ComplexValue, FloatValue, IntValue, StringValue}};

#[derive(Debug, Clone)]
pub struct Statement {
	pub variant: StatementVariant,
	pub column: NonZeroUsize,
}

impl Statement {
	pub fn print(&self, depth: usize) {
		for _ in 0..depth {
			print!("-");
		}
		print!(" {:03}: ", self.column);
		match &self.variant {
			StatementVariant::Help(token) => {
				println!("HELP {token:?}");
			}
			StatementVariant::Print(arguments) => {
				println!("PRINT");
				for argument in arguments {
					argument.print(depth + 1);
				}
			}
			StatementVariant::Input { prompt, timeout, elapsed, inputs } => {
				print!("INPUT");
				if let Some(_) = prompt {
					print!(" Prompt");
				}
				if let Some(_) = timeout {
					print!(" Timeout");
				}
				if let Some(_) = elapsed {
					print!(" Elapsed");
				}
				println!();
				if let Some(prompt) = prompt {
					prompt.print(depth + 1);
				}
				if let Some(timeout) = timeout {
					timeout.print(depth + 1);
				}
				if let Some(elapsed) = elapsed {
					elapsed.print(depth + 1);
				}
				for input in inputs {
					input.print(depth + 1);
				}
			}
			StatementVariant::ForInt { loop_variable, initial, limit, step } => {
				print!("FOR (Integer)");
				if let Some(_) = step {
					print!(" Stepped")
				}
				println!();
				loop_variable.print(depth + 1);
				initial.print(depth + 1);
				limit.print(depth + 1);
				if let Some(step) = step {
					step.print(depth + 1);
				}
			}
			StatementVariant::ForFloat { loop_variable, initial, limit, step } => {
				print!("FOR (Float)");
				if let Some(_) = step {
					print!(" Stepped")
				}
				println!();
				loop_variable.print(depth + 1);
				initial.print(depth + 1);
				limit.print(depth + 1);
				if let Some(step) = step {
					step.print(depth + 1);
				}
			}
			StatementVariant::Next(loop_variables) => {
				println!("NEXT");
				for loop_variable in loop_variables {
					loop_variable.print(depth + 1);
				}
			}
			StatementVariant::Run(argument) => {
				println!("RUN");
				if let Some(argument) = argument {
					argument.print(depth + 1);
				}
			}
			StatementVariant::Goto(argument) => {
				println!("GOTO");
				if let Some(argument) = argument {
					argument.print(depth + 1);
				}
			}
			StatementVariant::Gosub(argument) => {
				println!("GOSUB");
				if let Some(argument) = argument {
					argument.print(depth + 1);
				}
			}
			StatementVariant::NumericAssignment(l_values, r_value) => {
				println!("LET (Numeric)");
				for l_value in l_values {
					l_value.print(depth + 1);
				}
				r_value.print(depth + 1);
			}
			StatementVariant::StringAssignment(l_values, r_value) => {
				println!("LET (String)");
				for l_value in l_values {
					l_value.print(depth + 1);
				}
				r_value.print(depth + 1);
			}
			StatementVariant::List(from, to) => {
				print!("LIST from ");
				match from {
					Some(_) => print!("Line"),
					None => print!("Program Start"),
				}
				print!(" to ");
				match to {
					Some(_) => print!("Line"),
					None => print!("Program End"),
				}
				println!();
				if let Some(from) = from {
					from.print(depth + 1);
				}
				if let Some(to) = to {
					to.print(depth + 1);
				}
			}
			StatementVariant::OneLineIf { condition_expression: condition, then_statement, else_statement } => {
				println!("IF (One Line)");
				condition.print(depth + 1);
				then_statement.print(depth + 1);
				if let Some(else_statement) = else_statement {
					else_statement.print(depth + 1);
				}
			}
			StatementVariant::Option(options) => {
				println!("OPTION ");
				for option in options {
					option.0.print(depth + 1, option.1);
				}
			}
			StatementVariant::Load(filename_expression) => {
				println!("LOAD");
				if let Some(filename_expression) = filename_expression {
					filename_expression.print(depth + 1);
				}
			}
			StatementVariant::Save(filename_expression) => {
				println!("SAVE");
				if let Some(filename_expression) = filename_expression {
					filename_expression.print(depth + 1);
				}
			}
			StatementVariant::End => {
				println!("END");
			}
			StatementVariant::Stop => {
				println!("STOP");
			}
			StatementVariant::Return => {
				println!("RETURN");
			}
			StatementVariant::Clear => {
				println!("CLEAR");
			}
			StatementVariant::Clr => {
				println!("CLR");
			}
			StatementVariant::New => {
				println!("NEW");
			}
			StatementVariant::Data(data) => {
				println!("DATA");
				for (datum, datum_start_column) in data {
					datum.print(depth + 1, *datum_start_column);
				}
			}
			StatementVariant::Read { to_do_when_data_missing_statement: to_do_when_data_missing, variables } => {
				println!("READ");
				if let Some(to_do_when_data_missing) = to_do_when_data_missing {
					to_do_when_data_missing.print(depth + 1);
				}
				for variable in variables {
					variable.print(depth + 1);
				}
			}
			StatementVariant::Restore(restore_to_line) => {
				println!("RESTORE");
				if let Some(restore_to_line) = restore_to_line {
					restore_to_line.print(depth + 1);
				}
			}
			StatementVariant::Dimension(arrays) => {
				println!("DIM");
				for array in arrays {
					array.print(depth + 1);
				}
			}
			StatementVariant::DefInt(l_value, expression) => {
				println!("DEF (Integer)");
				l_value.print(depth + 1);
				expression.print(depth + 1);
			}
			StatementVariant::DefFloat(l_value, expression) => {
				println!("DEF (Float)");
				l_value.print(depth + 1);
				expression.print(depth + 1);
			}
			StatementVariant::DefComplex(l_value, expression) => {
				println!("DEF (Complex)");
				l_value.print(depth + 1);
				expression.print(depth + 1);
			}
			StatementVariant::DefString(l_value, expression) => {
				println!("DEF (String)");
				l_value.print(depth + 1);
				expression.print(depth + 1);
			}
			StatementVariant::Randomize => {
				println!("RANDOMIZE");
			}
			StatementVariant::OnGoto { index, line_numbers, else_statement } => {
				print!("ON GOTO");
				if else_statement.is_some() {
					print!(" ELSE");
				}
				println!();
				index.print(depth + 1);
				for line_number in line_numbers {
					line_number.print(depth + 1);
				}
				if let Some(else_statement) = else_statement {
					else_statement.print(depth + 1);
				}
			}
			StatementVariant::OnGosub { index, line_numbers, else_statement } => {
				print!("ON GOSUB");
				if else_statement.is_some() {
					print!(" ELSE");
				}
				println!();
				index.print(depth + 1);
				for line_number in line_numbers {
					line_number.print(depth + 1);
				}
				if let Some(else_statement) = else_statement {
					else_statement.print(depth + 1);
				}
			}
		}
	}
}

#[derive(Debug, Clone)]
pub enum StatementVariant {
	Print(Box<[PrintOperand]>),
	Input { prompt: Option<AnyTypeExpression>, timeout: Option<AnyTypeExpression>, elapsed: Option<AnyTypeExpression>, inputs: Box<[AnyTypeLValue]> },
	Run(Option<IntExpression>),
	Goto(Option<IntExpression>),
	Gosub(Option<IntExpression>),
	StringAssignment(Box<[StringLValue]>, StringExpression),
	NumericAssignment(Box<[AnyTypeLValue]>, AnyTypeExpression),
	List(Option<IntExpression>, Option<IntExpression>),
	OneLineIf { condition_expression: BoolExpression, then_statement: Box<Statement>, else_statement: Option<Box<Statement>> },
	Option(Box<[(OptionVariableAndValue, NonZeroUsize)]>),
	ForInt { loop_variable: IntLValue, initial: IntExpression, limit: IntExpression, step: Option<IntExpression> },
	ForFloat { loop_variable: FloatLValue, initial: FloatExpression, limit: FloatExpression, step: Option<FloatExpression> },
	Next(Box<[AnyTypeLValue]>),
	Load(Option<StringExpression>),
	Save(Option<StringExpression>),
	End,
	Stop,
	Data(Box<[(Datum, NonZeroUsize)]>),
	Read { to_do_when_data_missing_statement: Option<Box<Statement>>, variables: Box<[AnyTypeLValue]> },
	Restore(Option<IntExpression>),
	Return,
	Dimension(Box<[ArrayDimension]>),
	DefInt(IntLValue, IntExpression),
	DefFloat(FloatLValue, FloatExpression),
	DefComplex(ComplexLValue, ComplexExpression),
	DefString(StringLValue, StringExpression),
	Randomize,
	OnGoto { index: IntExpression, line_numbers: Box<[IntExpression]>, else_statement: Option<Box<Statement>> },
	OnGosub { index: IntExpression, line_numbers: Box<[IntExpression]>, else_statement: Option<Box<Statement>> },
	Clear,
	Clr,
	New,
	Help(Option<Token>),
}

#[derive(Debug, Clone)]
pub enum PrintOperand {
	Expression(AnyTypeExpression),
	Comma(NonZeroUsize),
	Semicolon(NonZeroUsize),
}

impl PrintOperand {
	pub fn get_start_column(&self) -> NonZeroUsize {
		match self {
			Self::Expression(expression) => expression.get_start_column(),
			Self::Comma(start_column) => *start_column,
			Self::Semicolon(start_column) => *start_column,
		}
	}

	pub fn print(&self, depth: usize) {
		if !matches!(self, Self::Expression(_)) {
			for _ in 0..depth {
				print!("-");
			}
			print!(" {:03}: ", self.get_start_column());
		}
		match self {
			Self::Expression(expression) => expression.print(depth),
			Self::Comma(_) => println!("Comma"),
			Self::Semicolon(_) => println!("Semicolon"),
		}
	}
}

#[derive(Debug, Clone, EnumDiscriminants)]
#[strum_discriminants(derive(Hash))]
pub enum OptionVariableAndValue {
	Angle(Option<AngleOption>),
	Math(Option<MathOption>),
	Machine(Option<MachineOption>),
	Base(Option<BaseOption>),
	Collate(Option<CollateOption>),
	ArithmeticDecimal,
	ArithmeticNative,
	ArithmeticDefault,
}

impl OptionVariableAndValue {
	pub fn print(&self, depth: usize, column: NonZeroUsize) {
		for _ in 0..depth {
			print!("-");
		}
		print!(" {:03}: ", column);
		match self {
			OptionVariableAndValue::Angle(angle_option) => {
				print!("ANGLE ");
				match angle_option {
					Some(AngleOption::Radians) => print!("RADIANS"),
					Some(AngleOption::Degrees) => print!("DEGREES"),
					Some(AngleOption::Gradians) => print!("GRADIANS"),
					Some(AngleOption::Revolutions) => print!("REVOLUTIONS"),
					None => print!("DEFAULT"),
				}
			}
			OptionVariableAndValue::Math(math_option) => {
				print!("MATH ");
				match math_option {
					Some(MathOption::AnsiFull) => print!("ANSI FULL"),
					Some(MathOption::EcmaMinimal) => print!("ECMA MINIMAL"),
					Some(MathOption::Ieee) => print!("IEEE"),
					None => print!("DEFAULT"),
				}
			}
			OptionVariableAndValue::Machine(machine_option) => {
				print!("MATH ");
				match machine_option {
					Some(MachineOption::AnsiFull) => print!("ANSI FULL"),
					Some(MachineOption::EcmaMinimal) => print!("ECMA MINIMAL"),
					Some(MachineOption::C64) => print!("C64"),
					None => print!("DEFAULT"),
				}
			}
			OptionVariableAndValue::Base(machine_option) => {
				print!("BASE ");
				match machine_option {
					Some(BaseOption::Zero) => print!("0"),
					Some(BaseOption::One) => print!("1"),
					None => print!("DEFAULT"),
				}
			}
			OptionVariableAndValue::Collate(collate_option) => {
				print!("COLLATE ");
				match collate_option {
					Some(CollateOption::Native) => print!("NATIVE/UNICODE"),
					Some(CollateOption::Standard) => print!("STANDARD/ASCII"),
					Some(CollateOption::C64UnshiftedPETSCII) => print!("C64 ASCII"),
					None => print!("DEFAULT"),
				}
			}
			OptionVariableAndValue::ArithmeticDecimal => print!("ARITHMETIC DECIMAL"),
			OptionVariableAndValue::ArithmeticNative => print!("ARITHMETIC NATIVE"),
			OptionVariableAndValue::ArithmeticDefault => print!("ARITHMETIC DEFAULT"),
		}
		println!();
	}
}

#[derive(Debug, Clone)]
pub enum IntExpression {
	ConstantValue { value: IntValue, start_column: NonZeroUsize },
	LValue(IntLValue),
	BitwiseAnd { lhs_expression: Box<IntExpression>, rhs_expression: Box<IntExpression>, start_column: NonZeroUsize },
	BitwiseOr { lhs_expression: Box<IntExpression>, rhs_expression: Box<IntExpression>, start_column: NonZeroUsize },
	BitwiseNot { sub_expression: Box<IntExpression>, start_column: NonZeroUsize },
	FlooredDivision { lhs_expression: Box<IntExpression>, rhs_expression: Box<IntExpression>, start_column: NonZeroUsize },
	Multiplication { lhs_expression: Box<IntExpression>, rhs_expression: Box<IntExpression>, start_column: NonZeroUsize },
	Addition { lhs_expression: Box<IntExpression>, rhs_expression: Box<IntExpression>, start_column: NonZeroUsize },
	Subtraction { lhs_expression: Box<IntExpression>, rhs_expression: Box<IntExpression>, start_column: NonZeroUsize },
	Negation { sub_expression: Box<IntExpression>, start_column: NonZeroUsize },
	CastFromFloat(Box<FloatExpression>),
	CastFromBool(Box<BoolExpression>),
}

impl IntExpression {
	pub fn get_start_column(&self) -> NonZeroUsize {
		match self {
			Self::ConstantValue { start_column, .. } => *start_column,
			Self::BitwiseAnd { start_column, .. } => *start_column,
			Self::BitwiseOr { start_column, .. } => *start_column,
			Self::BitwiseNot { start_column, .. } => *start_column,
			Self::Addition { start_column, .. } => *start_column,
			Self::Subtraction { start_column, .. } => *start_column,
			Self::Multiplication { start_column, .. } => *start_column,
			Self::FlooredDivision { start_column, .. } => *start_column,
			Self::LValue(l_value) => l_value.start_column,
			Self::Negation { start_column, .. } => *start_column,
			Self::CastFromFloat(float_expression) => float_expression.get_start_column(),
			Self::CastFromBool(bool_expression) => bool_expression.get_start_column(),
		}
	}

	pub fn print(&self, depth: usize) {
		if !matches!(self, Self::LValue(_)) {
			for _ in 0..depth {
				print!("-");
			}
			print!(" {:03}: Int ", self.get_start_column());
		}
		match &self {
			Self::ConstantValue { value, .. } => println!("Constant Value {value}"),
			Self::BitwiseAnd { lhs_expression, rhs_expression, .. } => {
				println!("Bitwise AND");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::BitwiseOr { lhs_expression, rhs_expression, .. } => {
				println!("Bitwise OR");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::BitwiseNot { sub_expression, .. } => {
				println!("Bitwise NOT");
				sub_expression.print(depth + 1);
			},
			Self::CastFromBool(operand) => {
				println!("Cast from Bool");
				operand.print(depth + 1);
			},
			Self::CastFromFloat(operand) => {
				println!("Cast from Float");
				operand.print(depth + 1);
			},
			Self::LValue(l_value_expression) => {
				l_value_expression.print(depth);
			},
			Self::Addition { lhs_expression, rhs_expression, .. } => {
				println!("Addition");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::Subtraction { lhs_expression, rhs_expression, .. } => {
				println!("Subtraction");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::Multiplication { lhs_expression, rhs_expression, .. } => {
				println!("Multiplication");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::FlooredDivision { lhs_expression, rhs_expression, .. } => {
				println!("Floored Division");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::Negation { sub_expression, .. } => {
				println!("Negation");
				sub_expression.print(depth + 1);
			},
		}
	}
}

#[derive(Debug, Clone)]
pub struct IntLValue {
	pub name: Box<str>,
	pub arguments: Box<[AnyTypeExpression]>,
	//pub uses_fn_keyword: bool,
	pub has_parentheses: bool,
	pub start_column: NonZeroUsize,
	//pub supplied_function_identifier: Option<SuppliedFunctionIdentifier>,
	pub supplied_function: Option<IntSuppliedFunction>,
}

impl IntLValue {
	pub fn print(&self, depth: usize) {
		for _ in 0..depth {
			print!("-");
		}
		print!(" {:03}: Int L-Value \"{}\"", self.start_column, self.name);
		//if self.uses_fn_keyword {
		//	print!(", Fn");
		//}
		if self.has_parentheses {
			print!(", Parenthesised/()");
		}
		println!();
		for argument in &self.arguments {
			argument.print(depth + 1);
		}
	}
}

#[derive(Debug, Clone)]
pub enum FloatExpression {
	ConstantValue { value: FloatValue, start_column: NonZeroUsize },
	LValue(FloatLValue),
	Exponentiation { lhs_expression: Box<FloatExpression>, rhs_expression: Box<FloatExpression>, start_column: NonZeroUsize },
	Negation { sub_expression: Box<FloatExpression>, start_column: NonZeroUsize },
	Multiplication { lhs_expression: Box<FloatExpression>, rhs_expression: Box<FloatExpression>, start_column: NonZeroUsize },
	Division { lhs_expression: Box<FloatExpression>, rhs_expression: Box<FloatExpression>, start_column: NonZeroUsize },
	Addition { lhs_expression: Box<FloatExpression>, rhs_expression: Box<FloatExpression>, start_column: NonZeroUsize },
	Subtraction { lhs_expression: Box<FloatExpression>, rhs_expression: Box<FloatExpression>, start_column: NonZeroUsize },
	CastFromInt(Box<IntExpression>),
	CastFromComplex(Box<ComplexExpression>),
}

impl FloatExpression {
	pub fn get_start_column(&self) -> NonZeroUsize {
		match self {
			Self::ConstantValue { start_column, .. } => *start_column,
			Self::Negation { start_column, .. } => *start_column,
			Self::LValue(l_value) => l_value.start_column,
			Self::CastFromInt(int_expression) => int_expression.get_start_column(),
			Self::CastFromComplex(complex_expression) => complex_expression.get_start_column(),
			Self::Addition { start_column, .. } => *start_column,
			Self::Subtraction { start_column, .. } => *start_column,
			Self::Multiplication { start_column, .. } => *start_column,
			Self::Division { start_column, .. } => *start_column,
			Self::Exponentiation { start_column, .. } => *start_column,
		}
	}

	pub fn print(&self, depth: usize) {
		if !matches!(self, Self::LValue(_)) {
			for _ in 0..depth {
				print!("-");
			}
			print!(" {:03}: Float ", self.get_start_column());
		}
		match self {
			Self::ConstantValue { value, .. } => println!("Constant Value {value}"),
			Self::Addition { lhs_expression, rhs_expression, .. } => {
				println!("Addition");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::Subtraction { lhs_expression, rhs_expression, .. } => {
				println!("Subtraction");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::Multiplication { lhs_expression, rhs_expression, .. } => {
				println!("Multiplication");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::Division { lhs_expression, rhs_expression, .. } => {
				println!("Division");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::Exponentiation { lhs_expression, rhs_expression, .. } => {
				println!("Exponentiation");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::CastFromComplex(operand) => {
				println!("Cast from Complex");
				operand.print(depth + 1);
			},
			Self::CastFromInt(operand) => {
				println!("Cast from Int");
				operand.print(depth + 1);
			},
			Self::Negation { sub_expression, .. } => {
				println!("Negation");
				sub_expression.print(depth + 1);
			},
			Self::LValue(l_value) => {
				l_value.print(depth);
			},
		}
	}
}

#[derive(Debug, Clone)]
pub struct FloatLValue {
	pub name: Box<str>,
	pub arguments: Box<[AnyTypeExpression]>,
	//pub uses_fn_keyword: bool,
	pub has_parentheses: bool,
	pub start_column: NonZeroUsize,
	//pub supplied_function_identifier: Option<SuppliedFunctionIdentifier>,
	pub supplied_function: Option<FloatSuppliedFunction>,
}

impl FloatLValue {
	pub fn print(&self, depth: usize) {
		for _ in 0..depth {
			print!("-");
		}
		print!(" {:03}: Float L-Value \"{}\"", self.start_column, self.name);
		//if self.uses_fn_keyword {
		//	print!(", Fn");
		//}
		if self.has_parentheses {
			print!(", Parenthesised/()");
		}
		println!();
		for argument in &self.arguments {
			argument.print(depth + 1);
		}
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, EnumIter)]
pub enum FloatSuppliedFunction {
	// Constants
	Pi,
	E,
	Tau,
	Phi,
	EGamma,
	MaxNum,
	NaN,
	Inf,
	NInf,
	True,
	False,
	// Time
	Time,
	Date,
	Second,
	Minute,
	Hour,
	Day,
	Month,
	Year,
	// Rounding
	Floor,
	Ip,
	Ceil,
	Truncate,
	Round,
	// Other
	Abs,
	Signum,
	Fp,
	Deg,
	Rad,
	Eps,
	Sqrt,
	Exp,
	Modulo,
	Remainder,
	Min,
	Max,
	Random,
	CommodoreRandom,
	// Logarithm
	LogE,
	Log10,
	Log2,
	LogN,
	// Trigonometry
	Sin,
	Cos,
	Tan,
	Cot,
	Sec,
	Csc,
	Asin,
	Acos,
	Atan,
	Acot,
	Asec,
	Acsc,
	Angle,
	Atan2,
	// Hyperbolic Trigonometry
	Sinh,
	Cosh,
	Tanh,
	Coth,
	Sech,
	Csch,
	Asinh,
	Acosh,
	Atanh,
	Acoth,
	Asech,
	Acsch,
	// Complex to Float
	Real,
	Imag,
	Arg,
	AbsComplex,
	// String to Float
	Len,
	Ord,
	Asc,
	Val,
	MaxLen,
	Pos,
	// PRINT string function
	Tab,
}

impl FloatSuppliedFunction {
	/// Returns a (syntax, name, description brief, extended description, formulae) tuple for the given function.
	pub fn get_help_info(self) -> (&'static str, &'static str, &'static str, &'static str, &'static [&'static str]) {
		match self {
			// Constants
			Self::Pi     => ("PI",     "Archimedes' Constant / Pi", "3.14159..., the ratio of a circle's circumference to its diameter.", "Half a turn in radians.", &["PI = 3.141592..."]),
			Self::E      => ("E",      "Euler's Number / E", "2.71828..., the base of natural logarithms and the exponential function.",
				"Also the sum of the reciprocals of the factorials of all the non-negative integers.", &["E = 2.71828..."]),
			Self::Tau    => ("TAU",    "Archimedes' Constant Doubled / Tau", "6.28318..., the ratio of a circle's circumference to its radius.", "A full turn in radians.", &["TAU = 6.28318..."]),
			Self::Phi    => ("PHI",    "Golden Ratio Constant / Phi", "1.61803..., the number that is equal to itself plus one when squared.", "Also equal to (1 + SQR(5)) / 2.",
				&["PHI = 1.61803..."]),
			Self::EGamma => ("EGAMMA", "Euler-Mascheroni Constant / Euler's Constant", "0.57721..., negation of the digamma of 1.", "", &["EGAMMA = 0.57721..."]),
			Self::MaxNum => ("MAXNUM", "Maximum Float", "The largest floating point value that can be represented.", "", &[]),
			Self::NaN    => ("NAN",    "Not a Number", "The floating point not a number constant.", "", &[]),
			Self::Inf    => ("INF",    "Positive Infinity", "The floating point positive infinity constant.", "", &[]),
			Self::NInf   => ("NINF",   "Negative Infinity", "The floating point negative infinity constant.", "", &[]),
			Self::True   => ("TRUE",   "True", "The result of a true comparison or -1.", "", &["TRUE = -1"]),
			Self::False  => ("FALSE",  "False", "The result of a false comparison or 0.", "", &["FALSE = 0"]),
			// Time
			Self::Time   => ("TIME", "Time of Day (Seconds)", "Returns how many seconds have passed since the last midnight.",
				"Returns a value between 0 (midnight) and 86399 (second before midnight) using the local timezone.", &[]),
			Self::Date   => ("DATE", "Date", "Returns the current date in YYDDD format.",
				"YY is the last two digits of the current year number and DDD is the day number of the year starting at 1. The local timezone is used.", &[]),
			Self::Second => ("SECOND", "Second of Minute", "Returns the current second of the current minute.", "Returns a value between 0 and 59 using the local timezone.", &[]),
			Self::Minute => ("MINUTE", "Minute of Hour", "Returns the current minute of the current hour.", "Returns a value between 0 and 59 using the local timezone.", &[]),
			Self::Hour   => ("HOUR", "Hour of Day (24H)", "Returns the current hour of the current day in 24 hour format.", "Returns a value between 0 (midnight) and 23 using the local timezone.", &[]),
			Self::Day    => ("DAY", "Day of Month", "Returns the current day of the current month.", "Returns a value between 1 and 31 (longest months) using the local timezone.", &[]),
			Self::Month  => ("MONTH", "Month of Year", "Returns the current month of the current year.", "Returns a value between 1 and 12 using the local timezone.", &[]),
			Self::Year   => ("YEAR", "Year", "Returns the current year in the local timezone.", "", &[]),
			// Rounding
			Self::Floor    => ("FLOOR(X), INT(X)", "Floor / Integerize", "Rounds all non-integers down or towards negative infinity.",
				"Returns the largest integer less than or equal to X and returns X unchanged if it is an integer.", &[]),
			Self::Ceil     => ("CEIL(X)", "Ceiling", "Rounds all non-integers up or towards positive infinity.",
				"Returns the smallest integer greater than or equal to X and returns X unchanged if it is an integer.", &[]),
			Self::Ip       => ("IP(X)", "Integer Part", "Rounds all non-integers towards zero.",
				"Returns X with all digits to the right of the decimal point \"cut off\" and returns X unchanged if it is an integer.", &[]),
			Self::Truncate => ("TRUNCATE(X, N%)", "Truncate", "Truncates to N% decimal digits to the right of the decimal point.",
				"Will truncate to -N% digits to the left of the decimal point if N% is negative.", &["TRUNCATE(X, N%) = IP(X*10^N%)/10^N%"]),
			Self::Round    => ("ROUND(X, N%)", "Round", "Rounds to N% decimal digits to the right of the decimal point.",
				"Will round to -N% digits to the left of the decimal point if N% is negative.", &["EPS(X, N%) = INT(X*10^N%+.5)/10^N%"]),
			// Other
			Self::Abs             => ("ABS(X)", "Absolute Value", "Returns X with a positive or zero sign.", "Returns X as-is if it is non-negative.", &[]),
			Self::AbsComplex      => ("ABS(X#)", "Complex Absolute Value", "The absolute value of X# or |X#| or the distance between X# and zero.", "Returns X as-is if it is a non-negative real number.", &[]),
			Self::Signum          => ("SGN(X)", "Signum", "Returns -1 if X is negative, 0 if it is zero and 1 if it is positive.", "", &[]),
			Self::Fp              => ("FP(X)", "Fractional Part", "Returns X with the sign and digits to the left of the decimal point \"cut off\".", "", &["FP(X) = X - IP(X)"]),
			Self::Deg             => ("DEG(X)", "To Degrees", "Converts X radians to X degrees", "", &["DEG(X) = X / PI * 180"]),
			Self::Rad             => ("RAD(X)", "To Radians", "Converts X degrees to X radians.", "", &["RAG(X) = X * PI / 180"]),
			Self::Eps             => ("EPS(X)", "Epsilon", "Returns the next largest floating point value after X.",
				"Will return the smallest positive value representable for all non-positive values of X and the largest finite value if X is the largest finite value.", &[]),
			Self::Sqrt            => ("SQR(X), SQRT(X)", "Square Root", "The square root of X.",
				"Throws a fatal exception 3005 if X is negative and OPTION MATH ANSI is set (default) or NAN if OPTION MATH IEEE is set.",
				&["SQR(X) * SQR(X) = X if X >= 0", "SQR(X) = NAN if X < 0 and OPTION MATH IEEE is set"]),
			Self::Exp             => ("EXP(X)", "Exponential", "Returns E raised to the Xth power.", "", &["EXP(X) = E ^ X", "EXP(1) = E = 2.71828...", "EXP(0) = 1"]),
			Self::Modulo          => ("MOD(X, Y)", "Modulo", "X modulo Y.", "Throws fatal exception 3006 if Y is zero and OPTION MATH ANSI is set (default).", &["MOD(X, Y) = X-Y*INT(X/Y)"]),
			Self::Remainder       => ("REMAINDER(X, Y)", "Modulo", "Remainder of X / Y.", "Throws fatal exception 3006 if Y is zero and OPTION MATH ANSI is set (default).", &["REMAINDER(X, Y) = X-Y*IP(X/Y)"]),
			Self::Min             => ("MIN(X), MIN(X, Y), MIN(X, Y, Z), ...", "Minimum", "Returns the value of the argument that is the smallest.", "Can have one or more arguments.",
				&["MIN(X) = X", "MIN(X, Y) = X if X <= Y", "MIN(X, Y) = Y if Y <= X", "MIN(X, Y) = MIN(Y, X)",
				"MIN(X, Y, Z) = MIN(X, MIN(Y, Z)) = MIN(MIN(X, Y), Z)" , "MIN(X, Y, Z, A) = MIN(X, MIN(Y, MIN(Z, A))) ...", "MIN(X, Y) = (X < Y) * -1 * X + (X > Y) * -1 * Y"]),
			Self::Max             => ("MAX(X), MAX(X, Y), MAX(X, Y, Z), ...", "Maximum", "Returns the value of the argument that is the largest.", "Can have one or more arguments.",
				&["MAX(X) = X", "MAX(X, Y) = X if X >= Y", "MAX(X, Y) = Y if Y >= X", "MAX(X, Y) = MAX(Y, X)",
				"MAX(X, Y, Z) = MAX(X, MAX(Y, Z)) = MAX(MAX(X, Y), Z)" , "MAX(X, Y, Z, A) = MAX(X, MAX(Y, MAX(Z, A))) ...", "MAX(X, Y) = (X > Y) * -1 * X + (X < Y) * -1 * Y"]),
			Self::Random          => ("RND", "Random", "Generates a pseudorandom number that is less than one and non-negative.",
				"The initial seed for the random generator is always the same value but a RANDOMIZE statement can be used to randomize it an any point.", &["0 <= RND < 1"]),
			Self::CommodoreRandom => ("RND(X)", "Advanced Random", "Like RND but with an argument that can control the number generation.",
				"Generates an unseeded random number if X is zero or uses X as a seed if it is negative or functions the same as RND if it is positive.",
				&["0 <= RND(X) < 1", "RND(X) functions the same as RND if X > 0", "RND(X) = RND(X) if X < 0"]),
			// Logarithm
			Self::LogE => ("LOG(X), LN(X)", "Natural Logarithm", "Returns the base E logarithm or X.",
				"Throws fatal exception 3004 if X is non-positive and OPTION MATH ANSI is set (default) or returns NaN if OPTION MATH IEEE is set.",
				&["LOG(X) = LOG(E, X)", "LOG(X) = NAN if X <= 0 and OPTION MATH IEEE is set", "LOG(EXP(X)) = X", "LOG(E) = 1", "LOG(1) = 0"]),
			Self::LogN => ("LOG(N, X)", "Base N Logarithm", "Returns the base N logarithm or X.",
				"Throws fatal exception 3004 if the result would be non-real and OPTION MATH ANSI is set (default) or returns NaN if OPTION MATH IEEE is set.",
				&["LOG(E, X) = LOG(X)", "LOG(2, X) = LOG2(X)", "LOG(10, X) = LOG10(X)", "LOG(N, N ^ X) = X", "LOG(X, X) = 1 if X > 1"]),
			Self::Log2 => ("LOG2(X)", "Binary Logarithm", "Returns the base 2 logarithm or X.",
				"Throws fatal exception 3004 if X is non-positive and OPTION MATH ANSI is set (default) or returns NaN if OPTION MATH IEEE is set.",
				&["LOG2(X) = LOG(2, X)", "LOG2(X) = NAN if X <= 0 and OPTION MATH IEEE is set", "LOG2(2 ^ X) = X", "LOG2(2) = 1", "LOG2(1) = 0"]),
			Self::Log10 => ("LOG10(X)", "Common Logarithm", "Returns the base 10 logarithm or X.",
				"Throws fatal exception 3004 if X is non-positive and OPTION MATH ANSI is set (default) or returns NaN if OPTION MATH IEEE is set.",
				&["LOG10(X) = LOG(10, X)", "LOG10(X) = NAN if X <= 0 and OPTION MATH IEEE is set", "LOG10(10 ^ X) = X", "LOG10(10) = 1", "LOG10(1) = 0"]),
			_ => todo!()
		}
	}
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, EnumIter)]
pub enum IntSuppliedFunction {
	// Constant
	True,
	False,
	// Time
	Time,
	Date,
	Second,
	Minute,
	Hour,
	Day,
	Month,
	Year,
	// Other Math
	Sqr,
	Abs,
	Log2,
	Log10,
	Xor,
	Min,
	Max,
	Sgn,
	// Rounding
	Floor,
	Ceil,
	Ip,
	// String to Int
	Len,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, EnumIter)]
pub enum ComplexSuppliedFunction {
	// Constants
	I,
	// Other Math
	Sqr,
	Exp,
	Conj,
	// Logarithm
	LogE,
	Log2,
	Log10,
	LogN,
	// Trigonometry
	Sin,
	Cos,
	Tan,
	Cot,
	Sec,
	Csc,
	Asin,
	Acos,
	Atan,
	Acot,
	Asec,
	Acsc,
	// Hyperbolic Trigonometry
	Sinh,
	Cosh,
	Tanh,
	Coth,
	Sech,
	Csch,
	Asinh,
	Acosh,
	Atanh,
	Acoth,
	Asech,
	Acsch,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, EnumIter)]
pub enum StringSuppliedFunction {
	// Time
	Time,
	Date,
	// String Editing
	UCase,
	LCase,
	LTrim,
	RTrim,
	Left1Arg,
	Right1Arg,
	Repeat,
	Left2Args,
	Right2Args,
	Mid2Args,
	Mid3Args,
	// Int to String
	Chr,
	// Any value to String
	Str,
}

#[derive(Debug, Clone)]
pub enum ComplexExpression {
	ConstantValue { value: ComplexValue, start_column: NonZeroUsize },
	LValue(ComplexLValue),
	Exponentiation { lhs_expression: Box<ComplexExpression>, rhs_expression: Box<ComplexExpression>, start_column: NonZeroUsize },
	Negation { sub_expression: Box<ComplexExpression>, start_column: NonZeroUsize },
	Multiplication { lhs_expression: Box<ComplexExpression>, rhs_expression: Box<ComplexExpression>, start_column: NonZeroUsize },
	Division { lhs_expression: Box<ComplexExpression>, rhs_expression: Box<ComplexExpression>, start_column: NonZeroUsize },
	Addition { lhs_expression: Box<ComplexExpression>, rhs_expression: Box<ComplexExpression>, start_column: NonZeroUsize },
	Subtraction { lhs_expression: Box<ComplexExpression>, rhs_expression: Box<ComplexExpression>, start_column: NonZeroUsize },
	CastFromFloat(Box<FloatExpression>),
}

impl ComplexExpression {
	pub fn get_start_column(&self) -> NonZeroUsize {
		match self {
			Self::ConstantValue { start_column, .. } => *start_column,
			Self::Negation { start_column, .. } => *start_column,
			Self::LValue(l_value) => l_value.start_column,
			Self::CastFromFloat(float_expression) => float_expression.get_start_column(),
			Self::Addition { start_column, .. } => *start_column,
			Self::Subtraction { start_column, .. } => *start_column,
			Self::Multiplication { start_column, .. } => *start_column,
			Self::Division { start_column, .. } => *start_column,
			Self::Exponentiation { start_column, .. } => *start_column,
		}
	}

	pub fn print(&self, depth: usize) {
		if !matches!(self, Self::LValue(_)) {
			for _ in 0..depth {
				print!("-");
			}
			print!(" {:03}: Complex ", self.get_start_column());
		}
		match self {
			Self::ConstantValue { value, .. } => println!("Constant Value {value}"),
			Self::Addition { lhs_expression, rhs_expression, .. } => {
				println!("Addition");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::Subtraction { lhs_expression, rhs_expression, .. } => {
				println!("Subtraction");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::Multiplication { lhs_expression, rhs_expression, .. } => {
				println!("Multiplication");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::Division { lhs_expression, rhs_expression, .. } => {
				println!("Division");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::Exponentiation { lhs_expression, rhs_expression, .. } => {
				println!("Exponentiation");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::CastFromFloat(operand) => {
				println!("Cast from Float");
				operand.print(depth + 1);
			},
			Self::Negation { sub_expression, .. } => {
				println!("Negation");
				sub_expression.print(depth + 1);
			},
			Self::LValue(l_value) => {
				l_value.print(depth);
			},
		}
	}
}

#[derive(Debug, Clone)]
pub struct ComplexLValue {
	pub name: Box<str>,
	pub arguments: Box<[AnyTypeExpression]>,
	//pub uses_fn_keyword: bool,
	pub has_parentheses: bool,
	pub start_column: NonZeroUsize,
	//pub supplied_function_identifier: Option<SuppliedFunctionIdentifier>,
	pub supplied_function: Option<ComplexSuppliedFunction>,
}

impl ComplexLValue {
	pub fn print(&self, depth: usize) {
		for _ in 0..depth {
			print!("-");
		}
		print!(" {:03}: Complex L-Value \"{}\"", self.start_column, self.name);
		//if self.uses_fn_keyword {
		//	print!(", Fn");
		//}
		if self.has_parentheses {
			print!(", Parenthesised/()");
		}
		println!();
		for argument in &self.arguments {
			argument.print(depth + 1);
		}
	}
}

#[derive(Debug, Clone)]
pub enum StringExpression {
	ConstantValue { value: StringValue, start_column: NonZeroUsize },
	LValue(StringLValue),
	Concatenation { lhs_expression: Box<StringExpression>, rhs_expression: Box<StringExpression>, start_column: NonZeroUsize },
	StringSlicing { to_slice_expression: Box<StringExpression>, range_start_expression: Box<IntExpression>, range_end_expression: Box<IntExpression>, start_column: NonZeroUsize },
}

impl StringExpression {
	pub fn get_start_column(&self) -> NonZeroUsize {
		match self {
			Self::ConstantValue { start_column, .. } => *start_column,
			Self::LValue(l_value) => l_value.start_column,
			Self::Concatenation { start_column, .. } => *start_column,
			Self::StringSlicing { start_column, .. } => *start_column,
		}
	}

	pub fn print(&self, depth: usize) {
		if !matches!(self, Self::LValue(_)) {
			for _ in 0..depth {
				print!("-");
			}
			print!(" {:03}: String ", self.get_start_column());
		}
		match self {
			Self::ConstantValue { value, .. } => println!("Constant Value \"{value}\""),
			Self::Concatenation { lhs_expression, rhs_expression, .. } => {
				println!("Concatenation");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::LValue(l_value) => {
				l_value.print(depth);
			},
			Self::StringSlicing { to_slice_expression, range_start_expression, range_end_expression, .. } => {
				println!("String slicing");
				to_slice_expression.print(depth + 1);
				range_start_expression.print(depth + 1);
				range_end_expression.print(depth + 1);
			}
		}
	}
}

#[derive(Debug, Clone)]
pub struct StringLValue {
	pub name: Box<str>,
	pub arguments: Box<[AnyTypeExpression]>,
	pub has_parentheses: bool,
	pub start_column: NonZeroUsize,
	//pub supplied_function_identifier: Option<SuppliedFunctionIdentifier>,
	pub string_slicings: Box<[(Box<IntExpression>, Box<IntExpression>, NonZeroUsize)]>,
	pub supplied_function: Option<StringSuppliedFunction>,
}

impl StringLValue {
	pub fn print(&self, depth: usize) {
		for _ in 0..depth {
			print!("-");
		}
		print!(" {:03}: String L-Value \"{}\"", self.start_column, self.name);
		if self.has_parentheses {
			print!(", Parenthesised/()");
		}
		print!(", {} Slicings", self.string_slicings.len());
		println!();
		for argument in &self.arguments {
			argument.print(depth + 1);
		}
		for string_slicing in &self.string_slicings {
			string_slicing.0.print(depth + 1);
			string_slicing.1.print(depth + 1);
		}
	}
}

#[derive(Debug, Clone)]
pub enum BoolExpression {
	ConstantValue { value: BoolValue, start_column: NonZeroUsize },
	IntIsNonZero(Box<IntExpression>),
	FloatIsNonZero(Box<FloatExpression>),
	ComplexIsNonZero(Box<ComplexExpression>),
	StringIsNotEmpty(Box<StringExpression>),
	And { lhs_expression: Box<BoolExpression>, rhs_expression: Box<BoolExpression>, start_column: NonZeroUsize },
	Or { lhs_expression: Box<BoolExpression>, rhs_expression: Box<BoolExpression>, start_column: NonZeroUsize },
	Not { sub_expression: Box<BoolExpression>, start_column: NonZeroUsize },
	IntLessThan { lhs_expression: Box<IntExpression>, rhs_expression: Box<IntExpression>, start_column: NonZeroUsize },
	IntGreaterThan { lhs_expression: Box<IntExpression>, rhs_expression: Box<IntExpression>, start_column: NonZeroUsize },
	IntEqualTo { lhs_expression: Box<IntExpression>, rhs_expression: Box<IntExpression>, start_column: NonZeroUsize },
	IntNotEqualTo { lhs_expression: Box<IntExpression>, rhs_expression: Box<IntExpression>, start_column: NonZeroUsize },
	IntLessThanOrEqualTo { lhs_expression: Box<IntExpression>, rhs_expression: Box<IntExpression>, start_column: NonZeroUsize },
	IntGreaterThanOrEqualTo { lhs_expression: Box<IntExpression>, rhs_expression: Box<IntExpression>, start_column: NonZeroUsize },
	FloatLessThan { lhs_expression: Box<FloatExpression>, rhs_expression: Box<FloatExpression>, start_column: NonZeroUsize },
	FloatGreaterThan { lhs_expression: Box<FloatExpression>, rhs_expression: Box<FloatExpression>, start_column: NonZeroUsize },
	FloatEqualTo { lhs_expression: Box<FloatExpression>, rhs_expression: Box<FloatExpression>, start_column: NonZeroUsize },
	FloatNotEqualTo { lhs_expression: Box<FloatExpression>, rhs_expression: Box<FloatExpression>, start_column: NonZeroUsize },
	FloatLessThanOrEqualTo { lhs_expression: Box<FloatExpression>, rhs_expression: Box<FloatExpression>, start_column: NonZeroUsize },
	FloatGreaterThanOrEqualTo { lhs_expression: Box<FloatExpression>, rhs_expression: Box<FloatExpression>, start_column: NonZeroUsize },
	StringLessThan { lhs_expression: Box<StringExpression>, rhs_expression: Box<StringExpression>, start_column: NonZeroUsize },
	StringGreaterThan { lhs_expression: Box<StringExpression>, rhs_expression: Box<StringExpression>, start_column: NonZeroUsize },
	StringEqualTo { lhs_expression: Box<StringExpression>, rhs_expression: Box<StringExpression>, start_column: NonZeroUsize },
	StringNotEqualTo { lhs_expression: Box<StringExpression>, rhs_expression: Box<StringExpression>, start_column: NonZeroUsize },
	StringLessThanOrEqualTo { lhs_expression: Box<StringExpression>, rhs_expression: Box<StringExpression>, start_column: NonZeroUsize },
	StringGreaterThanOrEqualTo { lhs_expression: Box<StringExpression>, rhs_expression: Box<StringExpression>, start_column: NonZeroUsize },
	ComplexEqualTo { lhs_expression: Box<ComplexExpression>, rhs_expression: Box<ComplexExpression>, start_column: NonZeroUsize },
	ComplexNotEqualTo { lhs_expression: Box<ComplexExpression>, rhs_expression: Box<ComplexExpression>, start_column: NonZeroUsize },
	BoolLessThan { lhs_expression: Box<BoolExpression>, rhs_expression: Box<BoolExpression>, start_column: NonZeroUsize },
	BoolGreaterThan { lhs_expression: Box<BoolExpression>, rhs_expression: Box<BoolExpression>, start_column: NonZeroUsize },
	BoolEqualTo { lhs_expression: Box<BoolExpression>, rhs_expression: Box<BoolExpression>, start_column: NonZeroUsize },
	BoolNotEqualTo { lhs_expression: Box<BoolExpression>, rhs_expression: Box<BoolExpression>, start_column: NonZeroUsize },
	BoolLessThanOrEqualTo { lhs_expression: Box<BoolExpression>, rhs_expression: Box<BoolExpression>, start_column: NonZeroUsize },
	BoolGreaterThanOrEqualTo { lhs_expression: Box<BoolExpression>, rhs_expression: Box<BoolExpression>, start_column: NonZeroUsize },
}

impl BoolExpression {
	pub fn get_start_column(&self) -> NonZeroUsize {
		match self {
			Self::ConstantValue { start_column, .. } => *start_column,
			Self::IntIsNonZero(int_expression) => int_expression.get_start_column(),
			Self::FloatIsNonZero(int_expression) => int_expression.get_start_column(),
			Self::ComplexIsNonZero(int_expression) => int_expression.get_start_column(),
			Self::StringIsNotEmpty(int_expression) => int_expression.get_start_column(),
			Self::And { start_column, .. } => *start_column,
			Self::Or { start_column, .. } => *start_column,
			Self::Not { start_column, .. } => *start_column,
			Self::IntGreaterThan { start_column, .. } => *start_column,
			Self::IntGreaterThanOrEqualTo { start_column, .. } => *start_column,
			Self::IntLessThan { start_column, .. } => *start_column,
			Self::IntLessThanOrEqualTo { start_column, .. } => *start_column,
			Self::IntEqualTo { start_column, .. } => *start_column,
			Self::IntNotEqualTo { start_column, .. } => *start_column,
			Self::FloatGreaterThan { start_column, .. } => *start_column,
			Self::FloatGreaterThanOrEqualTo { start_column, .. } => *start_column,
			Self::FloatLessThan { start_column, .. } => *start_column,
			Self::FloatLessThanOrEqualTo { start_column, .. } => *start_column,
			Self::FloatEqualTo { start_column, .. } => *start_column,
			Self::FloatNotEqualTo { start_column, .. } => *start_column,
			Self::ComplexEqualTo { start_column, .. } => *start_column,
			Self::ComplexNotEqualTo { start_column, .. } => *start_column,
			Self::StringGreaterThan { start_column, .. } => *start_column,
			Self::StringGreaterThanOrEqualTo { start_column, .. } => *start_column,
			Self::StringLessThan { start_column, .. } => *start_column,
			Self::StringLessThanOrEqualTo { start_column, .. } => *start_column,
			Self::StringEqualTo { start_column, .. } => *start_column,
			Self::StringNotEqualTo { start_column, .. } => *start_column,
			Self::BoolGreaterThan { start_column, .. } => *start_column,
			Self::BoolGreaterThanOrEqualTo { start_column, .. } => *start_column,
			Self::BoolLessThan { start_column, .. } => *start_column,
			Self::BoolLessThanOrEqualTo { start_column, .. } => *start_column,
			Self::BoolEqualTo { start_column, .. } => *start_column,
			Self::BoolNotEqualTo { start_column, .. } => *start_column,
		}
	}

	pub fn print(&self, depth: usize) {
		for _ in 0..depth {
			print!("-");
		}
		print!(" {:03}: Bool ", self.get_start_column());
		match self {
			Self::ConstantValue { value, .. } => println!("Constant Value {value}"),
			Self::IntIsNonZero(operand) => {
				println!("Check Int is not Zero");
				operand.print(depth + 1);
			},
			Self::FloatIsNonZero(operand) => {
				println!("Check Float is not Zero");
				operand.print(depth + 1);
			},
			Self::ComplexIsNonZero(operand) => {
				println!("Check Complex is not Zero");
				operand.print(depth + 1);
			},
			Self::StringIsNotEmpty(operand) => {
				println!("Check String is not Empty");
				operand.print(depth + 1);
			},
			Self::And { lhs_expression, rhs_expression, .. } => {
				println!("Logical AND");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::Or { lhs_expression, rhs_expression, .. } => {
				println!("Logical OR");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::Not { sub_expression, .. } => {
				println!("Logical NOT");
				sub_expression.print(depth + 1);
			},

			Self::IntEqualTo { lhs_expression, rhs_expression, .. } => {
				println!("Int Equal To");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::IntNotEqualTo { lhs_expression, rhs_expression, .. } => {
				println!("Int Not Equal To");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::IntLessThan { lhs_expression, rhs_expression, .. } => {
				println!("Int Less Than");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::IntLessThanOrEqualTo { lhs_expression, rhs_expression, .. } => {
				println!("Int Less Than or Equal To");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::IntGreaterThan { lhs_expression, rhs_expression, .. } => {
				println!("Int Greater Than");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::IntGreaterThanOrEqualTo { lhs_expression, rhs_expression, .. } => {
				println!("Int Greater Than or Equal To");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},

			Self::FloatEqualTo { lhs_expression, rhs_expression, .. } => {
				println!("Float Equal To");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::FloatNotEqualTo { lhs_expression, rhs_expression, .. } => {
				println!("Float Not Equal To");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::FloatLessThan { lhs_expression, rhs_expression, .. } => {
				println!("Float Less Than");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::FloatLessThanOrEqualTo { lhs_expression, rhs_expression, .. } => {
				println!("Float Less Than or Equal To");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::FloatGreaterThan { lhs_expression, rhs_expression, .. } => {
				println!("Float Greater Than");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::FloatGreaterThanOrEqualTo { lhs_expression, rhs_expression, .. } => {
				println!("Float Greater Than or Equal To");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},

			Self::ComplexEqualTo { lhs_expression, rhs_expression, .. } => {
				println!("Complex Equal To");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::ComplexNotEqualTo { lhs_expression, rhs_expression, .. } => {
				println!("Complex Not Equal To");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},

			Self::StringEqualTo { lhs_expression, rhs_expression, .. } => {
				println!("String Equal To");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::StringNotEqualTo { lhs_expression, rhs_expression, .. } => {
				println!("String Not Equal To");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::StringLessThan { lhs_expression, rhs_expression, .. } => {
				println!("String Less Than");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::StringLessThanOrEqualTo { lhs_expression, rhs_expression, .. } => {
				println!("String Less Than or Equal To");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::StringGreaterThan { lhs_expression, rhs_expression, .. } => {
				println!("String Greater Than");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::StringGreaterThanOrEqualTo { lhs_expression, rhs_expression, .. } => {
				println!("String Greater Than or Equal To");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},

			Self::BoolEqualTo { lhs_expression, rhs_expression, .. } => {
				println!("Bool Equal To");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::BoolNotEqualTo { lhs_expression, rhs_expression, .. } => {
				println!("Bool Not Equal To");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::BoolLessThan { lhs_expression, rhs_expression, .. } => {
				println!("Bool Less Than");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::BoolLessThanOrEqualTo { lhs_expression, rhs_expression, .. } => {
				println!("Bool Less Than or Equal To");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::BoolGreaterThan { lhs_expression, rhs_expression, .. } => {
				println!("Bool Greater Than");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
			Self::BoolGreaterThanOrEqualTo { lhs_expression, rhs_expression, .. } => {
				println!("Bool Greater Than or Equal To");
				lhs_expression.print(depth + 1);
				rhs_expression.print(depth + 1);
			},
		}
	}
}

#[derive(Debug, Clone)]
pub enum AnyTypeExpression {
	Int(IntExpression),
	Float(FloatExpression),
	Complex(ComplexExpression),
	String(StringExpression),
	Bool(BoolExpression),
}

impl AnyTypeExpression {
	pub fn get_start_column(&self) -> NonZeroUsize {
		match self {
			AnyTypeExpression::Bool(value) => value.get_start_column(),
			AnyTypeExpression::Int(value) => value.get_start_column(),
			AnyTypeExpression::Float(value) => value.get_start_column(),
			AnyTypeExpression::Complex(value) => value.get_start_column(),
			AnyTypeExpression::String(value) => value.get_start_column(),
		}
	}

	pub fn print(&self, depth: usize) {
		match self {
			AnyTypeExpression::Int(expression) => expression.print(depth),
			AnyTypeExpression::Float(expression) => expression.print(depth),
			AnyTypeExpression::Complex(expression) => expression.print(depth),
			AnyTypeExpression::Bool(expression) => expression.print(depth),
			AnyTypeExpression::String(expression) => expression.print(depth),
		}
	}

	pub fn is_numeric(&self) -> bool {
		match self {
			AnyTypeExpression::Bool(_) | AnyTypeExpression::Int(_) | AnyTypeExpression::Float(_) | AnyTypeExpression::Complex(_) => true,
			AnyTypeExpression::String(_) => false,
		}
	}

	pub fn is_complex(&self) -> bool {
		match self {
			AnyTypeExpression::Complex(_) => true,
			_ => false,
		}
	}

	pub fn is_valid_function_argument(&self) -> bool {
		match self {
			Self::Float(FloatExpression::LValue(FloatLValue { has_parentheses: false, .. })) => true,
			Self::Int(IntExpression::LValue(IntLValue { has_parentheses: false, .. })) => true,
			Self::Complex(ComplexExpression::LValue(ComplexLValue { has_parentheses: false, .. })) => true,
			Self::String(StringExpression::LValue(StringLValue { has_parentheses: false, .. })) => true,
			_ => false,
		}
	}
}

#[derive(Debug, Clone)]
pub enum AnyTypeLValue {
	Int(IntLValue),
	Float(FloatLValue),
	Complex(ComplexLValue),
	String(StringLValue),
}

impl AnyTypeLValue {
	pub fn get_start_column(&self) -> NonZeroUsize {
		match self {
			Self::Int(l_value) => l_value.start_column,
			Self::Float(l_value) => l_value.start_column,
			Self::Complex(l_value) => l_value.start_column,
			Self::String(l_value) => l_value.start_column,
		}
	}
}

impl AnyTypeLValue {
	pub fn print(&self, depth: usize) {
		match self {
			AnyTypeLValue::Int(l_value) => l_value.print(depth),
			AnyTypeLValue::Float(l_value) => l_value.print(depth),
			AnyTypeLValue::Complex(l_value) => l_value.print(depth),
			AnyTypeLValue::String(l_value) => l_value.print(depth),
		}
	}

	pub fn has_parentheses(&self) -> bool {
		match self {
			AnyTypeLValue::Int(l_value) => l_value.has_parentheses,
			AnyTypeLValue::Float(l_value) => l_value.has_parentheses,
			AnyTypeLValue::Complex(l_value) => l_value.has_parentheses,
			AnyTypeLValue::String(l_value) => l_value.has_parentheses,
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
pub struct Datum {
	pub as_string: StringValue,
	pub as_integer: Option<IntValue>,
	pub as_float: Option<FloatValue>,
	pub as_complex: Option<ComplexValue>,
}

impl Datum {
	pub fn print(&self, depth: usize, start_column: NonZeroUsize) {
		for _ in 0..depth {
			print!("-");
		}
		print!(" {start_column:03}: Datum, String: \"{}\"", self.as_string);
		if let Some(as_integer) = &self.as_integer {
			print!(", Int: {as_integer}")
		}
		if let Some(as_float) = &self.as_float {
			print!(", Float: {as_float}")
		}
		if let Some(as_complex) = &self.as_complex {
			print!(", Complex: {as_complex}")
		}
		println!();
	}
}

#[derive(Debug, Clone)]
pub struct ArrayDimension {
	pub name: Box<str>,
	pub array_type: IdentifierType,
	pub dimensions: Box<[(Option<IntExpression>, IntExpression)]>,
	pub start_column: NonZeroUsize,
}

impl ArrayDimension {
	pub fn print(&self, depth: usize) {
		for _ in 0..depth {
			print!("-");
		}
		print!(" {:03}: ", self.start_column);
		match self.array_type {
			IdentifierType::Integer => print!("Integer/%"),
			IdentifierType::String => print!("String/$"),
			IdentifierType::UnmarkedOrFloat => print!("Float"),
			IdentifierType::ComplexNumber => print!("Complex/#"),
		}
		println!("Array \"{}\"", self.name);
		for (lower_bound, upper_bound) in self.dimensions.iter() {
			let depth = depth + 1;
			for _ in 0..depth {
				print!("-");
			}
			println!(" Dimension");
			if let Some(lower_bound) = lower_bound {
				lower_bound.print(depth + 1);
			}
			upper_bound.print(depth + 1);
		}
	}
}

#[cfg(test)]
mod tests {
	//use super::*;

	#[test]
	fn test_parse_expression() {
		
	}
}