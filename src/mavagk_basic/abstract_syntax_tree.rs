use std::num::NonZeroUsize;

use crate::mavagk_basic::{error::{Error, ErrorVariant}, token::SuppliedFunction, value::{BoolValue, ComplexValue, FloatValue, IntValue, StringValue}};

#[derive(Debug)]
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
			StatementVariant::Print(arguments) => {
				print!("PRINT");
				println!();
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
				if let Some(prompt) = prompt {
					prompt.print(depth + 1);
				}
				if let Some(timeout) = timeout {
					timeout.print(depth + 1);
				}
				if let Some(elapsed) = elapsed {
					elapsed.print(depth + 1);
				}
				println!();
				for input in inputs {
					input.print(depth + 1);
				}
			}
			StatementVariant::Run(argument) => {
				print!("RUN");
				println!();
				if let Some(argument) = argument {
					argument.print(depth + 1);
				}
			}
			StatementVariant::Goto(argument) => {
				print!("GOTO");
				println!();
				if let Some(argument) = argument {
					argument.print(depth + 1);
				}
			}
			StatementVariant::Gosub(argument) => {
				print!("GOSUB");
				println!();
				if let Some(argument) = argument {
					argument.print(depth + 1);
				}
			}
			StatementVariant::AssignInt(l_value, r_value) => {
				print!("LET (Integer)");
				println!();
				l_value.print(depth + 1);
				r_value.print(depth + 1);
			}
			StatementVariant::AssignFloat(l_value, r_value) => {
				print!("LET (Float)");
				println!();
				l_value.print(depth + 1);
				r_value.print(depth + 1);
			}
			StatementVariant::AssignComplex(l_value, r_value) => {
				print!("LET (Complex)");
				println!();
				l_value.print(depth + 1);
				r_value.print(depth + 1);
			}
			StatementVariant::AssignString(l_value, r_value) => {
				print!("LET (String)");
				println!();
				l_value.print(depth + 1);
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
				print!("IF (One Line)");
				println!();
				condition.print(depth + 1);
				then_statement.print(depth + 1);
				if let Some(else_statement) = else_statement {
					else_statement.print(depth + 1);
				}
			}
			StatementVariant::Option(option) => {
				print!("OPTION ");
				match option {
					OptionVariableAndValue::Angle(angle_option) => {
						print!("ANGLE ");
						match angle_option {
							AngleOption::Radians => print!("RADIANS"),
							AngleOption::Degrees => print!("DEGREES"),
							AngleOption::Gradians => print!("GRADIANS"),
							AngleOption::Revolutions => print!("REVOLUTIONS"),
						}
					}
					OptionVariableAndValue::Math(math_option) => {
						print!("MATH ");
						match math_option {
							MathOption::Ansi => print!("ANSI"),
							MathOption::Ieee => print!("IEEE"),
						}
					}
					OptionVariableAndValue::ArithmeticDecimal => print!("ARITHMETIC DECIMAL"),
					OptionVariableAndValue::ArithmeticNative => print!("ARITHMETIC NATIVE"),
				}
				println!();
			}
		}
	}
}

#[derive(Debug)]
pub enum StatementVariant {
	Print(Box<[PrintOperand]>),
	Input { prompt: Option<AnyTypeExpression>, timeout: Option<AnyTypeExpression>, elapsed: Option<AnyTypeExpression>, inputs: Box<[AnyTypeLValue]> },
	Run(Option<IntExpression>),
	Goto(Option<IntExpression>),
	Gosub(Option<IntExpression>),
	AssignInt(IntLValue, IntExpression),
	AssignFloat(FloatLValue, FloatExpression),
	AssignComplex(ComplexLValue, ComplexExpression),
	AssignString(StringLValue, StringExpression),
	List(Option<IntExpression>, Option<IntExpression>),
	OneLineIf { condition_expression: BoolExpression, then_statement: Box<Statement>, else_statement: Option<Box<Statement>> },
	Option(OptionVariableAndValue),
}

#[derive(Debug)]
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

#[derive(Debug, Clone)]
pub enum OptionVariableAndValue {
	Angle(AngleOption),
	Math(MathOption),
	ArithmeticDecimal,
	ArithmeticNative,
}

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
	pub supplied_function: Option<SuppliedFunction>,
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
	pub supplied_function: Option<SuppliedFunction>,
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
	pub supplied_function: Option<SuppliedFunction>,
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
}

impl StringExpression {
	pub fn get_start_column(&self) -> NonZeroUsize {
		match self {
			Self::ConstantValue { start_column, .. } => *start_column,
			Self::LValue(l_value) => l_value.start_column,
			Self::Concatenation { start_column, .. } => *start_column,
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
		}
	}
}

#[derive(Debug, Clone)]
pub struct StringLValue {
	pub name: Box<str>,
	pub arguments: Box<[AnyTypeExpression]>,
	//pub uses_fn_keyword: bool,
	pub has_parentheses: bool,
	pub start_column: NonZeroUsize,
	pub supplied_function: Option<SuppliedFunction>,
}

impl StringLValue {
	pub fn print(&self, depth: usize) {
		for _ in 0..depth {
			print!("-");
		}
		print!(" {:03}: String L-Value \"{}\"", self.start_column, self.name);
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
	//PrintComma(NonZeroUsize),
	//PrintSemicolon(NonZeroUsize),
}

impl AnyTypeExpression {
	pub fn get_start_column(&self) -> NonZeroUsize {
		match self {
			AnyTypeExpression::Bool(value) => value.get_start_column(),
			AnyTypeExpression::Int(value) => value.get_start_column(),
			AnyTypeExpression::Float(value) => value.get_start_column(),
			AnyTypeExpression::Complex(value) => value.get_start_column(),
			AnyTypeExpression::String(value) => value.get_start_column(),
			//AnyTypeExpression::PrintComma(column) => *column,
			//AnyTypeExpression::PrintSemicolon(column) => *column,
		}
	}

	pub fn to_int_expression(self) -> Result<IntExpression, Error> {
		Ok(match self {
			Self::Int(expression) => expression,
			//AnyTypeExpression::PrintComma(..) | AnyTypeExpression::PrintSemicolon(..) => unreachable!(),
			Self::Float(expression) => IntExpression::CastFromFloat(Box::new(expression)),
			Self::Complex(expression) => IntExpression::CastFromFloat(Box::new(FloatExpression::CastFromComplex(Box::new(expression)))),
			Self::Bool(expression) => IntExpression::CastFromBool(Box::new(expression)),
			Self::String(expression) => return Err(ErrorVariant::StringCastToNumber.at_column(expression.get_start_column())),
		})
	}

	pub fn to_float_expression(self) -> Result<FloatExpression, Error> {
		Ok(match self {
			Self::Int(expression) => FloatExpression::CastFromInt(Box::new(expression)),
			Self::Float(expression) => expression,
			Self::Complex(expression) => FloatExpression::CastFromComplex(Box::new(expression)),
			Self::Bool(expression) => FloatExpression::CastFromInt(Box::new(IntExpression::CastFromBool(Box::new(expression)))),
			Self::String(expression) => return Err(ErrorVariant::StringCastToNumber.at_column(expression.get_start_column())),
			//AnyTypeExpression::PrintComma(..) | AnyTypeExpression::PrintSemicolon(..) => unreachable!(),
		})
	}

	pub fn to_complex_expression(self) -> Result<ComplexExpression, Error> {
		Ok(match self {
			Self::Int(expression) => ComplexExpression::CastFromFloat(Box::new(FloatExpression::CastFromInt(Box::new(expression)))),
			Self::Float(expression) => ComplexExpression::CastFromFloat(Box::new(expression)),
			Self::Complex(expression) => expression,
			Self::Bool(expression) => ComplexExpression::CastFromFloat(Box::new(FloatExpression::CastFromInt(Box::new(IntExpression::CastFromBool(Box::new(expression)))))),
			Self::String(expression) => return Err(ErrorVariant::StringCastToNumber.at_column(expression.get_start_column())),
			//AnyTypeExpression::PrintComma(..) | AnyTypeExpression::PrintSemicolon(..) => unreachable!(),
		})
	}

	pub fn to_string_expression(self) -> Result<StringExpression, Error> {
		Ok(match self {
			Self::Int(..) | Self::Float(..) | Self::Complex(..) | Self::Bool(..) => return Err(ErrorVariant::NumberCastToString.at_column(self.get_start_column())),
			Self::String(value) => value,
			//AnyTypeExpression::PrintComma(..) | AnyTypeExpression::PrintSemicolon(..) => unreachable!(),
		})
	}

	pub fn to_bool_expression(self) -> Result<BoolExpression, Error> {
		Ok(match self {
			Self::Bool(value) => value,
			Self::Int(expression) => BoolExpression::IntIsNonZero(Box::new(expression)),
			Self::Float(expression) => BoolExpression::FloatIsNonZero(Box::new(expression)),
			Self::Complex(expression) => BoolExpression::ComplexIsNonZero(Box::new(expression)),
			Self::String(expression) => BoolExpression::StringIsNotEmpty(Box::new(expression)),
			//AnyTypeExpression::PrintComma(..) | AnyTypeExpression::PrintSemicolon(..) => unreachable!(),
		})
	}

	pub fn upcast(self, rhs: Self) -> Result<(Self, Self), Error> {
		Ok(match (&self, &rhs) {
			(Self::Bool(..), Self::Bool(..)) | (Self::Int(..), Self::Int(..)) | (Self::Float(..), Self::Float(..)) | (Self::Complex(..), Self::Complex(..)) | (Self::String(..), Self::String(..)) => (self, rhs),
			(Self::Int(..), Self::Bool(..)) => (self, AnyTypeExpression::Int(rhs.to_int_expression()?)),
			(Self::Bool(..), Self::Int(..)) => (AnyTypeExpression::Int(self.to_int_expression()?), rhs),
			(Self::Float(..), Self::Bool(..) | Self::Int(..)) => (self, AnyTypeExpression::Float(rhs.to_float_expression()?)),
			(Self::Bool(..) | Self::Int(..), Self::Float(..)) => (AnyTypeExpression::Float(self.to_float_expression()?), rhs),
			(Self::Complex(..), Self::Bool(..) | Self::Int(..) | Self::Float(..)) => (self, AnyTypeExpression::Complex(rhs.to_complex_expression()?)),
			(Self::Bool(..) | Self::Int(..) | Self::Float(..), Self::Complex(..)) => (AnyTypeExpression::Complex(self.to_complex_expression()?), rhs),
			(Self::String(..), _) => (self, AnyTypeExpression::Complex(rhs.to_complex_expression()?)),
			(_, Self::String(..)) => (AnyTypeExpression::Complex(self.to_complex_expression()?), rhs),
			//(Self::PrintComma(..) | Self::PrintSemicolon(..), _) | (_, Self::PrintComma(..) | Self::PrintSemicolon(..)) => unreachable!(),
		})
	}

	pub fn print(&self, depth: usize) {
		match self {
			AnyTypeExpression::Int(expression) => expression.print(depth),
			AnyTypeExpression::Float(expression) => expression.print(depth),
			AnyTypeExpression::Complex(expression) => expression.print(depth),
			AnyTypeExpression::Bool(expression) => expression.print(depth),
			AnyTypeExpression::String(expression) => expression.print(depth),
			//AnyTypeExpression::PrintComma(column) => {
			//	for _ in 0..depth {
			//		print!("-");
			//	}
			//	println!(" {:03}: Comma/,", column);
			//}
			//AnyTypeExpression::PrintSemicolon(column) => {
			//	for _ in 0..depth {
			//		print!("-");
			//	}
			//	println!(" {:03}: Semicolon/;", column);
			//}
		}
	}

	pub fn is_numeric(&self) -> bool {
		match self {
			AnyTypeExpression::Bool(_) | AnyTypeExpression::Int(_) | AnyTypeExpression::Float(_) | AnyTypeExpression::Complex(_) => true,
			AnyTypeExpression::String(_) => false,
			//AnyTypeExpression::PrintComma(_) | AnyTypeExpression::PrintSemicolon(_) => unreachable!(),
		}
	}

	pub fn is_complex(&self) -> bool {
		match self {
			AnyTypeExpression::Complex(_) => true,
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
}

#[cfg(test)]
mod tests {
	//use super::*;

	#[test]
	fn test_parse_expression() {
		
	}
}