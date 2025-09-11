use std::mem::replace;

use crate::mavagk_basic::{abstract_syntax_tree::{AnyTypeExpression, BoolExpression, ComplexExpression, IntExpression, RealExpression, Statement, StatementVariant, StringExpression}, value::IntValue};

pub fn optimize_statement(statement: &mut Statement) {
	match &mut statement.variant {
		StatementVariant::Goto(branch_to) | StatementVariant::Run(branch_to) | StatementVariant::Gosub(branch_to) => {
			if let Some(branch_to) = branch_to {
				optimize_int_expression(branch_to);
			}
		}
		StatementVariant::List(lhs_expression, rhs_expression) => {
			if let Some(lhs_expression) = lhs_expression {
				optimize_int_expression(lhs_expression);
			}
			if let Some(rhs_expression) = rhs_expression {
				optimize_int_expression(rhs_expression);
			}
		}
		StatementVariant::Print(sub_expressions) => {
			for sub_expression in sub_expressions.iter_mut() {
				optimize_any_type_expression(sub_expression);
			}
		}
		StatementVariant::OneLineIf { condition_expression, then_statement, else_statement } => {
			optimize_bool_expression(condition_expression);
			optimize_statement(then_statement);
			if let Some(else_statement) = else_statement {
				optimize_statement(else_statement);
			}
		}
		StatementVariant::Option(_) => {}
		StatementVariant::AssignInt(l_value, r_value) => {
			for l_value_argument in l_value.arguments.iter_mut() {
				optimize_any_type_expression(l_value_argument);
			}
			optimize_int_expression(r_value);
		}
		StatementVariant::AssignReal(l_value, r_value) => {
			for l_value_argument in l_value.arguments.iter_mut() {
				optimize_any_type_expression(l_value_argument);
			}
			optimize_real_expression(r_value);
		}
		StatementVariant::AssignComplex(l_value, r_value) => {
			for l_value_argument in l_value.arguments.iter_mut() {
				optimize_any_type_expression(l_value_argument);
			}
			optimize_complex_expression(r_value);
		}
		StatementVariant::AssignString(l_value, r_value) => {
			for l_value_argument in l_value.arguments.iter_mut() {
				optimize_any_type_expression(l_value_argument);
			}
			optimize_string_expression(r_value);
		}
	}
}

pub fn optimize_int_expression(expression: &mut IntExpression) {
	match expression {
		IntExpression::BitwiseAnd { lhs_expression, rhs_expression, .. } => {
			optimize_int_expression(lhs_expression);
			optimize_int_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(IntExpression::ConstantValue { .. }, IntExpression::ConstantValue { .. }) => {
					let (lhs_expression, rhs_expression, start_column) =
						match replace(expression, IntExpression::ConstantValue { value: IntValue::zero(), start_column: 1.try_into().unwrap() })
					{
						IntExpression::BitwiseAnd { lhs_expression, rhs_expression, start_column } => (lhs_expression, rhs_expression, start_column),
						_ => unreachable!(),
					};
					let (lhs_value, rhs_value) = match (*lhs_expression, *rhs_expression) {
						(IntExpression::ConstantValue { value: lhs_value, .. }, IntExpression::ConstantValue { value: rhs_value, .. }) => (lhs_value, rhs_value),
						_ => unreachable!(),
					};
					*expression = IntExpression::ConstantValue { value: lhs_value.and(rhs_value), start_column };
				}
				_ => {}
			}
		}
		IntExpression::BitwiseOr { lhs_expression, rhs_expression, .. } => {
			optimize_int_expression(lhs_expression);
			optimize_int_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(IntExpression::ConstantValue { .. }, IntExpression::ConstantValue { .. }) => {
					let (lhs_expression, rhs_expression, start_column) =
						match replace(expression, IntExpression::ConstantValue { value: IntValue::zero(), start_column: 1.try_into().unwrap() })
					{
						IntExpression::BitwiseOr { lhs_expression, rhs_expression, start_column } => (lhs_expression, rhs_expression, start_column),
						_ => unreachable!(),
					};
					let (lhs_value, rhs_value) = match (*lhs_expression, *rhs_expression) {
						(IntExpression::ConstantValue { value: lhs_value, .. }, IntExpression::ConstantValue { value: rhs_value, .. }) => (lhs_value, rhs_value),
						_ => unreachable!(),
					};
					*expression = IntExpression::ConstantValue { value: lhs_value.or(rhs_value), start_column };
				}
				_ => {}
			}
		}
		IntExpression::BitwiseNot { sub_expression, .. } => {
			optimize_int_expression(sub_expression);
			match &**sub_expression {
				IntExpression::ConstantValue { .. } => {
					let (sub_expression, start_column) = match replace(expression, IntExpression::ConstantValue { value: IntValue::zero(), start_column: 1.try_into().unwrap() }) {
						IntExpression::BitwiseNot { sub_expression, start_column, .. } => (sub_expression, start_column),
						_ => unreachable!(),
					};
					let value = match *sub_expression {
						IntExpression::ConstantValue { value, .. } => value,
						_ => unreachable!(),
					};
					*expression = IntExpression::ConstantValue { value: value.not(), start_column };
				}
				_ => {}
			}
		}
		IntExpression::CastFromBool(bool_expression) => {
			optimize_bool_expression(bool_expression);
			match &**bool_expression {
				BoolExpression::ConstantValue { .. } => {
					let bool_expression = match replace(expression, IntExpression::ConstantValue { value: IntValue::zero(), start_column: 1.try_into().unwrap() }) {
						IntExpression::CastFromBool(bool_expression) => bool_expression,
						_ => unreachable!(),
					};
					let (value, start_column) = match *bool_expression {
						BoolExpression::ConstantValue { value, start_column } => (value, start_column),
						_ => unreachable!(),
					};
					*expression = IntExpression::ConstantValue { value: value.to_int(), start_column };
				}
				_ => {}
			}
		}
		IntExpression::CastFromReal(real_expression) => {
			optimize_real_expression(real_expression);
			match &**real_expression {
				RealExpression::ConstantValue { value, start_column } => {
					match value.clone().to_int(None, 1.try_into().unwrap()) {
						Ok(value) => *expression = IntExpression::ConstantValue { value: value, start_column: *start_column },
						_ => {}
					}
				}
				_ => {}
			}
		}
		IntExpression::ConstantValue { .. } => {}
		IntExpression::LValue(l_value) => {
			for l_value_argument in l_value.arguments.iter_mut() {
				optimize_any_type_expression(l_value_argument);
			}
		}
	}
}

pub fn optimize_bool_expression(expression: &mut BoolExpression) {
	match expression {
		// TODO
		_ => {}
	}
}

pub fn optimize_real_expression(expression: &mut RealExpression) {
	match expression {
		// TODO
		_ => {}
	}
}

pub fn optimize_complex_expression(expression: &mut ComplexExpression) {
	match expression {
		// TODO
		_ => {}
	}
}

pub fn optimize_string_expression(expression: &mut StringExpression) {
	match expression {
		// TODO
		_ => {}
	}
}

pub fn optimize_any_type_expression(expression: &mut AnyTypeExpression) {
	match expression {
		AnyTypeExpression::Bool(expression) => optimize_bool_expression(expression),
		AnyTypeExpression::Int(expression) => optimize_int_expression(expression),
		AnyTypeExpression::Real(expression) => optimize_real_expression(expression),
		AnyTypeExpression::Complex(expression) => optimize_complex_expression(expression),
		AnyTypeExpression::String(expression) => optimize_string_expression(expression),
		AnyTypeExpression::PrintComma(_) | AnyTypeExpression::PrintSemicolon(_) => {}
	}
}