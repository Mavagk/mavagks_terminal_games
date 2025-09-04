use std::num::NonZeroUsize;

use num::BigInt;

use crate::mavagk_basic::{error::{Error, ErrorVariant}, value::{BoolValue, ComplexValue, IntValue, RealValue, StringValue}};

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
			StatementVariant::AssignReal(l_value, r_value) => {
				print!("LET (Real)");
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
				print!("LIST");
				println!();
				if let Some(from) = from {
					from.print(depth + 1);
				}
				if let Some(to) = to {
					to.print(depth + 1);
				}
			}
		}
	}
}

#[derive(Debug)]
pub enum StatementVariant {
	Print(Box<[AnyTypeExpression]>),
	Run(Option<IntExpression>),
	Goto(Option<IntExpression>),
	Gosub(Option<IntExpression>),
	AssignInt(IntExpression, IntExpression),
	AssignReal(RealExpression, RealExpression),
	AssignComplex(ComplexExpression, ComplexExpression),
	AssignString(StringExpression, StringExpression),
	List(Option<IntExpression>, Option<IntExpression>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct IntExpression {
	pub variant: IntExpressionVariant,
	pub column: NonZeroUsize,
}

impl IntExpression {
	pub fn print(&self, depth: usize) {
		for _ in 0..depth {
			print!("-");
		}
		print!(" {:03}: Int ", self.column);
		match &self.variant {
			IntExpressionVariant::ConstantValue(value) => println!("Constant Value {value}"),
			IntExpressionVariant::BitwiseAnd(lhs, rhs) => {
				println!("Bitwise AND");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			IntExpressionVariant::BitwiseOr(lhs, rhs) => {
				println!("Bitwise OR");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			IntExpressionVariant::CastFromBool(operand) => {
				println!("Cast from Bool");
				operand.print(depth + 1);
			},
			IntExpressionVariant::CastFromReal(operand) => {
				println!("Cast from Real");
				operand.print(depth + 1);
			},
			IntExpressionVariant::IntIdentifierOrFunction { name, arguments, uses_fn_keyword, has_parentheses } => {
				print!("Identifier/Function \"{name}\", ");
				if *uses_fn_keyword {
					print!(", Fn");
				}
				if *has_parentheses {
					print!(", Parenthesised/()");
				}
				println!();
				for argument in arguments {
					argument.print(depth + 1);
				}
			},
		}
	}
}

#[derive(Debug, PartialEq, Clone)]
pub enum IntExpressionVariant {
	ConstantValue(IntValue),
	IntIdentifierOrFunction { name: Box<str>, arguments: Box<[AnyTypeExpression]>, uses_fn_keyword: bool, has_parentheses: bool },
	BitwiseAnd(Box<IntExpression>, Box<IntExpression>),
	BitwiseOr(Box<IntExpression>, Box<IntExpression>),
	CastFromReal(Box<RealExpression>),
	CastFromBool(Box<BoolExpression>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct RealExpression {
	pub variant: RealExpressionVariant,
	pub column: NonZeroUsize,
}

impl RealExpression {
	pub fn print(&self, depth: usize) {
		for _ in 0..depth {
			print!("-");
		}
		print!(" {:03}: Real ", self.column);
		match &self.variant {
			RealExpressionVariant::ConstantValue(value) => println!("Constant Value {value}"),
			RealExpressionVariant::Addition(lhs, rhs) => {
				println!("Addition");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			RealExpressionVariant::Subtraction(lhs, rhs) => {
				println!("Subtraction");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			RealExpressionVariant::Multiplication(lhs, rhs) => {
				println!("Multiplication");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			RealExpressionVariant::Division(lhs, rhs) => {
				println!("Division");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			RealExpressionVariant::Exponentiation(lhs, rhs) => {
				println!("Exponentiation");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			RealExpressionVariant::FlooredDivision(lhs, rhs) => {
				println!("Floored Division");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			RealExpressionVariant::CastFromComplex(operand) => {
				println!("Cast from Complex");
				operand.print(depth + 1);
			},
			RealExpressionVariant::CastFromInt(operand) => {
				println!("Cast from Int");
				operand.print(depth + 1);
			},
			RealExpressionVariant::Negation(operand) => {
				println!("Negation");
				operand.print(depth + 1);
			},
			RealExpressionVariant::BitwiseNot(operand) => {
				println!("Bitwise NOT");
				operand.print(depth + 1);
			},
			RealExpressionVariant::RealIdentifierOrFunction { name, arguments, uses_fn_keyword, has_parentheses } => {
				print!("Identifier/Function \"{name}\", ");
				if *uses_fn_keyword {
					print!(", Fn");
				}
				if *has_parentheses {
					print!(", Parenthesised/()");
				}
				println!();
				for argument in arguments {
					argument.print(depth + 1);
				}
			},
		}
	}
}

#[derive(Debug, PartialEq, Clone)]
pub enum RealExpressionVariant {
	ConstantValue(RealValue),
	RealIdentifierOrFunction { name: Box<str>, arguments: Box<[AnyTypeExpression]>, uses_fn_keyword: bool, has_parentheses: bool },
	Exponentiation(Box<RealExpression>, Box<RealExpression>),
	Negation(Box<RealExpression>),
	Multiplication(Box<RealExpression>, Box<RealExpression>),
	Division(Box<RealExpression>, Box<RealExpression>),
	FlooredDivision(Box<IntExpression>, Box<IntExpression>),
	Addition(Box<RealExpression>, Box<RealExpression>),
	Subtraction(Box<RealExpression>, Box<RealExpression>),
	BitwiseNot(Box<IntExpression>),
	CastFromInt(Box<IntExpression>),
	CastFromComplex(Box<ComplexExpression>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ComplexExpression {
	pub variant: ComplexExpressionVariant,
	pub column: NonZeroUsize,
}

impl ComplexExpression {
	pub fn print(&self, depth: usize) {
		for _ in 0..depth {
			print!("-");
		}
		print!(" {:03}: Complex ", self.column);
		match &self.variant {
			ComplexExpressionVariant::ConstantValue(value) => println!("Constant Value {value}"),
			ComplexExpressionVariant::Addition(lhs, rhs) => {
				println!("Addition");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			ComplexExpressionVariant::Subtraction(lhs, rhs) => {
				println!("Subtraction");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			ComplexExpressionVariant::Multiplication(lhs, rhs) => {
				println!("Multiplication");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			ComplexExpressionVariant::Division(lhs, rhs) => {
				println!("Division");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			ComplexExpressionVariant::Exponentiation(lhs, rhs) => {
				println!("Exponentiation");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			ComplexExpressionVariant::Negation(operand) => {
				println!("Negation");
				operand.print(depth + 1);
			},
			ComplexExpressionVariant::CastFromReal(operand) => {
				println!("Cast from Real");
				operand.print(depth + 1);
			},
			ComplexExpressionVariant::ComplexIdentifierOrFunction { name, arguments, uses_fn_keyword, has_parentheses } => {
				print!("Identifier/Function \"{name}\", ");
				if *uses_fn_keyword {
					print!(", Fn");
				}
				if *has_parentheses {
					print!(", Parenthesised/()");
				}
				println!();
				for argument in arguments {
					argument.print(depth + 1);
				}
			},
		}
	}
}

#[derive(Debug, PartialEq, Clone)]
pub enum ComplexExpressionVariant {
	ConstantValue(ComplexValue),
	ComplexIdentifierOrFunction { name: Box<str>, arguments: Box<[AnyTypeExpression]>, uses_fn_keyword: bool, has_parentheses: bool },
	Exponentiation(Box<ComplexExpression>, Box<ComplexExpression>),
	Negation(Box<ComplexExpression>),
	Multiplication(Box<ComplexExpression>, Box<ComplexExpression>),
	Division(Box<ComplexExpression>, Box<ComplexExpression>),
	Addition(Box<ComplexExpression>, Box<ComplexExpression>),
	Subtraction(Box<ComplexExpression>, Box<ComplexExpression>),
	CastFromReal(Box<RealExpression>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct StringExpression {
	pub variant: StringExpressionVariant,
	pub column: NonZeroUsize,
}

impl StringExpression {
	pub fn print(&self, depth: usize) {
		for _ in 0..depth {
			print!("-");
		}
		print!(" {:03}: String ", self.column);
		match &self.variant {
			StringExpressionVariant::ConstantValue(value) => println!("Constant Value {value}"),
			StringExpressionVariant::Concatenation(lhs, rhs) => {
				println!("Concatenation");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			StringExpressionVariant::StringIdentifierOrFunction { name, arguments, uses_fn_keyword, has_parentheses } => {
				print!("Identifier/Function \"{name}\", ");
				if *uses_fn_keyword {
					print!(", Fn");
				}
				if *has_parentheses {
					print!(", Parenthesised/()");
				}
				println!();
				for argument in arguments {
					argument.print(depth + 1);
				}
			},
		}
	}
}

#[derive(Debug, PartialEq, Clone)]
pub enum StringExpressionVariant {
	ConstantValue(StringValue),
	StringIdentifierOrFunction { name: Box<str>, arguments: Box<[AnyTypeExpression]>, uses_fn_keyword: bool, has_parentheses: bool },
	Concatenation(Box<StringExpression>, Box<StringExpression>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct BoolExpression {
	pub variant: BoolExpressionVariant,
	pub column: NonZeroUsize,
}

impl BoolExpression {
	pub fn print(&self, depth: usize) {
		for _ in 0..depth {
			print!("-");
		}
		print!(" {:03}: Bool ", self.column);
		match &self.variant {
			BoolExpressionVariant::ConstantValue(value) => println!("Constant Value {value}"),
			BoolExpressionVariant::And(lhs, rhs) => {
				println!("Logical AND");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			BoolExpressionVariant::Or(lhs, rhs) => {
				println!("Logical OR");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			BoolExpressionVariant::Not(operand) => {
				println!("Logical NOT");
				operand.print(depth + 1);
			},

			BoolExpressionVariant::IntEqualTo(lhs, rhs) => {
				println!("Int Equal To");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			BoolExpressionVariant::IntNotEqualTo(lhs, rhs) => {
				println!("Int Not Equal To");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			BoolExpressionVariant::IntLessThan(lhs, rhs) => {
				println!("Int Less Than");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			BoolExpressionVariant::IntLessThanOrEqualTo(lhs, rhs) => {
				println!("Int Less Than or Equal To");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			BoolExpressionVariant::IntGreaterThan(lhs, rhs) => {
				println!("Int Greater Than");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			BoolExpressionVariant::IntGreaterThanOrEqualTo(lhs, rhs) => {
				println!("Int Greater Than or Equal To");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},

			BoolExpressionVariant::RealEqualTo(lhs, rhs) => {
				println!("Real Equal To");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			BoolExpressionVariant::RealNotEqualTo(lhs, rhs) => {
				println!("Real Not Equal To");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			BoolExpressionVariant::RealLessThan(lhs, rhs) => {
				println!("Real Less Than");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			BoolExpressionVariant::RealLessThanOrEqualTo(lhs, rhs) => {
				println!("Real Less Than or Equal To");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			BoolExpressionVariant::RealGreaterThan(lhs, rhs) => {
				println!("Real Greater Than");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			BoolExpressionVariant::RealGreaterThanOrEqualTo(lhs, rhs) => {
				println!("Real Greater Than or Equal To");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},

			BoolExpressionVariant::ComplexEqualTo(lhs, rhs) => {
				println!("Complex Equal To");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			BoolExpressionVariant::ComplexNotEqualTo(lhs, rhs) => {
				println!("Complex Not Equal To");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},

			BoolExpressionVariant::StringEqualTo(lhs, rhs) => {
				println!("String Equal To");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			BoolExpressionVariant::StringNotEqualTo(lhs, rhs) => {
				println!("String Not Equal To");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			BoolExpressionVariant::StringLessThan(lhs, rhs) => {
				println!("String Less Than");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			BoolExpressionVariant::StringLessThanOrEqualTo(lhs, rhs) => {
				println!("String Less Than or Equal To");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			BoolExpressionVariant::StringGreaterThan(lhs, rhs) => {
				println!("String Greater Than");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			BoolExpressionVariant::StringGreaterThanOrEqualTo(lhs, rhs) => {
				println!("String Greater Than or Equal To");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},

			BoolExpressionVariant::BoolEqualTo(lhs, rhs) => {
				println!("Bool Equal To");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			BoolExpressionVariant::BoolNotEqualTo(lhs, rhs) => {
				println!("Bool Not Equal To");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			BoolExpressionVariant::BoolLessThan(lhs, rhs) => {
				println!("Bool Less Than");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			BoolExpressionVariant::BoolLessThanOrEqualTo(lhs, rhs) => {
				println!("Bool Less Than or Equal To");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			BoolExpressionVariant::BoolGreaterThan(lhs, rhs) => {
				println!("Bool Greater Than");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
			BoolExpressionVariant::BoolGreaterThanOrEqualTo(lhs, rhs) => {
				println!("Bool Greater Than or Equal To");
				lhs.print(depth + 1);
				rhs.print(depth + 1);
			},
		}
	}
}

#[derive(Debug, PartialEq, Clone)]
pub enum BoolExpressionVariant {
	ConstantValue(BoolValue),
	And(Box<BoolExpression>, Box<BoolExpression>),
	Or(Box<BoolExpression>, Box<BoolExpression>),
	Not(Box<BoolExpression>),
	IntLessThan(Box<IntExpression>, Box<IntExpression>),
	IntGreaterThan(Box<IntExpression>, Box<IntExpression>),
	IntEqualTo(Box<IntExpression>, Box<IntExpression>),
	IntNotEqualTo(Box<IntExpression>, Box<IntExpression>),
	IntLessThanOrEqualTo(Box<IntExpression>, Box<IntExpression>),
	IntGreaterThanOrEqualTo(Box<IntExpression>, Box<IntExpression>),
	RealLessThan(Box<RealExpression>, Box<RealExpression>),
	RealGreaterThan(Box<RealExpression>, Box<RealExpression>),
	RealEqualTo(Box<RealExpression>, Box<RealExpression>),
	RealNotEqualTo(Box<RealExpression>, Box<RealExpression>),
	RealLessThanOrEqualTo(Box<RealExpression>, Box<RealExpression>),
	RealGreaterThanOrEqualTo(Box<RealExpression>, Box<RealExpression>),
	StringLessThan(Box<StringExpression>, Box<StringExpression>),
	StringGreaterThan(Box<StringExpression>, Box<StringExpression>),
	StringEqualTo(Box<StringExpression>, Box<StringExpression>),
	StringNotEqualTo(Box<StringExpression>, Box<StringExpression>),
	StringLessThanOrEqualTo(Box<StringExpression>, Box<StringExpression>),
	StringGreaterThanOrEqualTo(Box<StringExpression>, Box<StringExpression>),
	ComplexEqualTo(Box<ComplexExpression>, Box<ComplexExpression>),
	ComplexNotEqualTo(Box<ComplexExpression>, Box<ComplexExpression>),
	BoolLessThan(Box<BoolExpression>, Box<BoolExpression>),
	BoolGreaterThan(Box<BoolExpression>, Box<BoolExpression>),
	BoolEqualTo(Box<BoolExpression>, Box<BoolExpression>),
	BoolNotEqualTo(Box<BoolExpression>, Box<BoolExpression>),
	BoolLessThanOrEqualTo(Box<BoolExpression>, Box<BoolExpression>),
	BoolGreaterThanOrEqualTo(Box<BoolExpression>, Box<BoolExpression>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum AnyTypeExpression {
	Int(IntExpression),
	Real(RealExpression),
	Complex(ComplexExpression),
	String(StringExpression),
	Bool(BoolExpression),
	PrintComma(NonZeroUsize),
	PrintSemicolon(NonZeroUsize),
}

impl AnyTypeExpression {
	pub fn to_int_expression(self, line_number: Option<&BigInt>) -> Result<IntExpression, Error> {
		Ok(match self {
			Self::Int(expression) => expression,
			Self::Real(expression) => IntExpression { column: expression.column, variant: IntExpressionVariant::CastFromReal(Box::new(expression)) },
			Self::Complex(expression) => IntExpression { column: expression.column, variant: IntExpressionVariant::CastFromReal(Box::new(
				RealExpression { column: expression.column, variant: RealExpressionVariant::CastFromComplex(Box::new(expression)) }
			)) },
			Self::Bool(expression) => IntExpression { column: expression.column, variant: IntExpressionVariant::CastFromBool(Box::new(expression)) },
			Self::String(expression) => return Err(Error { variant: ErrorVariant::StringCastToNumber, column_number: Some(expression.column), line_number: line_number.cloned() }),
			AnyTypeExpression::PrintComma(..) | AnyTypeExpression::PrintSemicolon(..) => unreachable!(),
		})
	}

	pub fn to_real_expression(self, line_number: Option<&BigInt>) -> Result<RealExpression, Error> {
		Ok(match self {
			Self::Int(expression) => RealExpression { column: expression.column, variant: RealExpressionVariant::CastFromInt(Box::new(expression)) },
			Self::Real(expression) => expression,
			Self::Complex(expression) => RealExpression { column: expression.column, variant: RealExpressionVariant::CastFromComplex(Box::new(expression)) },
			Self::Bool(expression) => RealExpression { column: expression.column, variant: RealExpressionVariant::CastFromInt(Box::new(
				IntExpression { column: expression.column, variant: IntExpressionVariant::CastFromBool(Box::new(expression)) },
			)) },
			Self::String(expression) => return Err(Error { variant: ErrorVariant::StringCastToNumber, column_number: Some(expression.column), line_number: line_number.cloned() }),
			AnyTypeExpression::PrintComma(..) | AnyTypeExpression::PrintSemicolon(..) => unreachable!(),
		})
	}

	pub fn to_complex_expression(self, line_number: Option<&BigInt>) -> Result<ComplexExpression, Error> {
		Ok(match self {
			Self::Int(expression) => ComplexExpression { column: expression.column, variant: ComplexExpressionVariant::CastFromReal(Box::new(
				RealExpression { column: expression.column, variant: RealExpressionVariant::CastFromInt(Box::new(expression)) }
			)) },
			Self::Real(expression) => ComplexExpression { column: expression.column, variant: ComplexExpressionVariant::CastFromReal(Box::new(expression)) },
			Self::Complex(expression) => expression,
			Self::Bool(expression) => ComplexExpression { column: expression.column, variant: ComplexExpressionVariant::CastFromReal(Box::new(
				RealExpression { column: expression.column, variant: RealExpressionVariant::CastFromInt(Box::new(
					IntExpression { column: expression.column, variant: IntExpressionVariant::CastFromBool(Box::new(expression)) },
				)) }
			)) },
			Self::String(expression) => return Err(Error { variant: ErrorVariant::StringCastToNumber, column_number: Some(expression.column), line_number: line_number.cloned() }),
			AnyTypeExpression::PrintComma(..) | AnyTypeExpression::PrintSemicolon(..) => unreachable!(),
		})
	}

	pub fn to_string_expression(self, line_number: Option<&BigInt>) -> Result<StringExpression, Error> {
		Ok(match self {
			Self::Int(IntExpression { column, .. }) | Self::Real(RealExpression { column, .. }) |
			Self::Complex(ComplexExpression { column, .. }) | Self::Bool(BoolExpression { column, .. }) =>
				return Err(Error { variant: ErrorVariant::NumberCastToString, column_number: Some(column), line_number: line_number.cloned() }),
			Self::String(value) => value,
			AnyTypeExpression::PrintComma(..) | AnyTypeExpression::PrintSemicolon(..) => unreachable!(),
		})
	}

	pub fn to_bool_expression(self, line_number: Option<&BigInt>) -> Result<BoolExpression, Error> {
		Ok(match self {
			Self::Bool(value) => value,
			Self::String(expression) => return Err(Error { variant: ErrorVariant::StringCastToNumber, column_number: Some(expression.column), line_number: line_number.cloned() }),
			AnyTypeExpression::PrintComma(..) | AnyTypeExpression::PrintSemicolon(..) => unreachable!(),
			_ => todo!(),
		})
	}

	pub fn upcast(self, rhs: Self, line_number: Option<&BigInt>) -> Result<(Self, Self), Error> {
		Ok(match (&self, &rhs) {
			(Self::Bool(..), Self::Bool(..)) | (Self::Int(..), Self::Int(..)) | (Self::Real(..), Self::Real(..)) | (Self::Complex(..), Self::Complex(..)) | (Self::String(..), Self::String(..)) => (self, rhs),
			(Self::Int(..), Self::Bool(..)) => (self, AnyTypeExpression::Int(rhs.to_int_expression(line_number)?)),
			(Self::Bool(..), Self::Int(..)) => (AnyTypeExpression::Int(self.to_int_expression(line_number)?), rhs),
			(Self::Real(..), Self::Bool(..) | Self::Int(..)) => (self, AnyTypeExpression::Real(rhs.to_real_expression(line_number)?)),
			(Self::Bool(..) | Self::Int(..), Self::Real(..)) => (AnyTypeExpression::Real(self.to_real_expression(line_number)?), rhs),
			(Self::Complex(..), Self::Bool(..) | Self::Int(..) | Self::Real(..)) => (self, AnyTypeExpression::Complex(rhs.to_complex_expression(line_number)?)),
			(Self::Bool(..) | Self::Int(..) | Self::Real(..), Self::Complex(..)) => (AnyTypeExpression::Complex(self.to_complex_expression(line_number)?), rhs),
			(Self::String(..), _) => (self, AnyTypeExpression::Complex(rhs.to_complex_expression(line_number)?)),
			(_, Self::String(..)) => (AnyTypeExpression::Complex(self.to_complex_expression(line_number)?), rhs),
			(Self::PrintComma(..) | Self::PrintSemicolon(..), _) | (_, Self::PrintComma(..) | Self::PrintSemicolon(..)) => unreachable!(),
		})
	}

	pub fn print(&self, depth: usize) {
		match self {
			AnyTypeExpression::Int(expression) => expression.print(depth),
			AnyTypeExpression::Real(expression) => expression.print(depth),
			AnyTypeExpression::Complex(expression) => expression.print(depth),
			AnyTypeExpression::Bool(expression) => expression.print(depth),
			AnyTypeExpression::String(expression) => expression.print(depth),
			AnyTypeExpression::PrintComma(column) => {
				for _ in 0..depth {
					print!("-");
				}
				println!(" {:03}: Comma/,", column);
			}
			AnyTypeExpression::PrintSemicolon(column) => {
				for _ in 0..depth {
					print!("-");
				}
				println!(" {:03}: Semicolon/;", column);
			}
		}
	}

	pub fn get_column(&self) -> NonZeroUsize {
		match self {
			AnyTypeExpression::Bool(value) => value.column,
			AnyTypeExpression::Int(value) => value.column,
			AnyTypeExpression::Real(value) => value.column,
			AnyTypeExpression::Complex(value) => value.column,
			AnyTypeExpression::String(value) => value.column,
			AnyTypeExpression::PrintComma(column) => *column,
			AnyTypeExpression::PrintSemicolon(column) => *column,
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