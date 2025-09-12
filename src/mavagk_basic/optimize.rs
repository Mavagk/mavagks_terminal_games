use std::mem::replace;

use crate::mavagk_basic::{abstract_syntax_tree::{AnyTypeExpression, BoolExpression, ComplexExpression, IntExpression, RealExpression, Statement, StatementVariant, StringExpression}, value::{BoolValue, ComplexValue, IntValue, RealValue, StringValue}};

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
		BoolExpression::ConstantValue { .. } => {}

		BoolExpression::IntIsNonZero(sub_expression) => {
			optimize_int_expression(sub_expression);
			match &**sub_expression {
				IntExpression::ConstantValue { value, start_column } =>
					*expression = BoolExpression::ConstantValue { value: BoolValue::new(value.is_zero()), start_column: *start_column },
				_ => {}
			}
		}
		BoolExpression::RealIsNonZero(sub_expression) => {
			optimize_real_expression(sub_expression);
			match &**sub_expression {
				RealExpression::ConstantValue { value, start_column } =>
					*expression = BoolExpression::ConstantValue { value: BoolValue::new(value.is_zero()), start_column: *start_column },
				_ => {}
			}
		}
		BoolExpression::ComplexIsNonZero(sub_expression) => {
			optimize_complex_expression(sub_expression);
			match &**sub_expression {
				ComplexExpression::ConstantValue { value, start_column } =>
					*expression = BoolExpression::ConstantValue { value: BoolValue::new(value.is_zero()), start_column: *start_column },
				_ => {}
			}
		}
		BoolExpression::StringIsNotEmpty(sub_expression) => {
			optimize_string_expression(sub_expression);
			match &**sub_expression {
				StringExpression::ConstantValue { value, start_column } =>
					*expression = BoolExpression::ConstantValue { value: BoolValue::new(value.is_empty()), start_column: *start_column },
				_ => {}
			}
		}
		
		BoolExpression::And { lhs_expression, rhs_expression, start_column } => {
			optimize_bool_expression(lhs_expression);
			optimize_bool_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(BoolExpression::ConstantValue { value: lhs_value, .. }, BoolExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.and(*rhs_value), start_column: *start_column },
				_ => {}
			}
		}
		BoolExpression::Or { lhs_expression, rhs_expression, start_column } => {
			optimize_bool_expression(lhs_expression);
			optimize_bool_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(BoolExpression::ConstantValue { value: lhs_value, .. }, BoolExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.or(*rhs_value), start_column: *start_column },
				_ => {}
			}
		}
		BoolExpression::Not { sub_expression, start_column } => {
			optimize_bool_expression(sub_expression);
			match &**sub_expression {
				BoolExpression::ConstantValue { value, .. } =>
					*expression = BoolExpression::ConstantValue { value: BoolValue::new(value.is_zero()), start_column: *start_column },
				_ => {}
			}
		}

		BoolExpression::BoolEqualTo { lhs_expression, rhs_expression, start_column } => {
			optimize_bool_expression(lhs_expression);
			optimize_bool_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(BoolExpression::ConstantValue { value: lhs_value, .. }, BoolExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.equal_to(*rhs_value), start_column: *start_column },
				_ => {}
			}
		}
		BoolExpression::BoolNotEqualTo { lhs_expression, rhs_expression, start_column } => {
			optimize_bool_expression(lhs_expression);
			optimize_bool_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(BoolExpression::ConstantValue { value: lhs_value, .. }, BoolExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.not_equal_to(*rhs_value), start_column: *start_column },
				_ => {}
			}
		}
		BoolExpression::BoolGreaterThan { lhs_expression, rhs_expression, start_column } => {
			optimize_bool_expression(lhs_expression);
			optimize_bool_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(BoolExpression::ConstantValue { value: lhs_value, .. }, BoolExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.greater_than(*rhs_value), start_column: *start_column },
				_ => {}
			}
		}
		BoolExpression::BoolGreaterThanOrEqualTo { lhs_expression, rhs_expression, start_column } => {
			optimize_bool_expression(lhs_expression);
			optimize_bool_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(BoolExpression::ConstantValue { value: lhs_value, .. }, BoolExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.greater_than_or_equal_to(*rhs_value), start_column: *start_column },
				_ => {}
			}
		}
		BoolExpression::BoolLessThan { lhs_expression, rhs_expression, start_column } => {
			optimize_bool_expression(lhs_expression);
			optimize_bool_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(BoolExpression::ConstantValue { value: lhs_value, .. }, BoolExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.less_than(*rhs_value), start_column: *start_column },
				_ => {}
			}
		}
		BoolExpression::BoolLessThanOrEqualTo { lhs_expression, rhs_expression, start_column } => {
			optimize_bool_expression(lhs_expression);
			optimize_bool_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(BoolExpression::ConstantValue { value: lhs_value, .. }, BoolExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.less_than_or_equal_to(*rhs_value), start_column: *start_column },
				_ => {}
			}
		}

		BoolExpression::IntEqualTo { lhs_expression, rhs_expression, start_column } => {
			optimize_int_expression(lhs_expression);
			optimize_int_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(IntExpression::ConstantValue { value: lhs_value, .. }, IntExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.equal_to(rhs_value), start_column: *start_column },
				_ => {}
			}
		}
		BoolExpression::IntNotEqualTo { lhs_expression, rhs_expression, start_column } => {
			optimize_int_expression(lhs_expression);
			optimize_int_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(IntExpression::ConstantValue { value: lhs_value, .. }, IntExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.not_equal_to(rhs_value), start_column: *start_column },
				_ => {}
			}
		}
		BoolExpression::IntGreaterThan { lhs_expression, rhs_expression, start_column } => {
			optimize_int_expression(lhs_expression);
			optimize_int_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(IntExpression::ConstantValue { value: lhs_value, .. }, IntExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.greater_than(rhs_value), start_column: *start_column },
				_ => {}
			}
		}
		BoolExpression::IntGreaterThanOrEqualTo { lhs_expression, rhs_expression, start_column } => {
			optimize_int_expression(lhs_expression);
			optimize_int_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(IntExpression::ConstantValue { value: lhs_value, .. }, IntExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.greater_than_or_equal_to(rhs_value), start_column: *start_column },
				_ => {}
			}
		}
		BoolExpression::IntLessThan { lhs_expression, rhs_expression, start_column } => {
			optimize_int_expression(lhs_expression);
			optimize_int_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(IntExpression::ConstantValue { value: lhs_value, .. }, IntExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.less_than(rhs_value), start_column: *start_column },
				_ => {}
			}
		}
		BoolExpression::IntLessThanOrEqualTo { lhs_expression, rhs_expression, start_column } => {
			optimize_int_expression(lhs_expression);
			optimize_int_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(IntExpression::ConstantValue { value: lhs_value, .. }, IntExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.less_than_or_equal_to(rhs_value), start_column: *start_column },
				_ => {}
			}
		}

		BoolExpression::RealEqualTo { lhs_expression, rhs_expression, start_column } => {
			optimize_real_expression(lhs_expression);
			optimize_real_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(RealExpression::ConstantValue { value: lhs_value, .. }, RealExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.equal_to(rhs_value), start_column: *start_column },
				_ => {}
			}
		}
		BoolExpression::RealNotEqualTo { lhs_expression, rhs_expression, start_column } => {
			optimize_real_expression(lhs_expression);
			optimize_real_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(RealExpression::ConstantValue { value: lhs_value, .. }, RealExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.not_equal_to(rhs_value), start_column: *start_column },
				_ => {}
			}
		}
		BoolExpression::RealGreaterThan { lhs_expression, rhs_expression, start_column } => {
			optimize_real_expression(lhs_expression);
			optimize_real_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(RealExpression::ConstantValue { value: lhs_value, .. }, RealExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.greater_than(rhs_value), start_column: *start_column },
				_ => {}
			}
		}
		BoolExpression::RealGreaterThanOrEqualTo { lhs_expression, rhs_expression, start_column } => {
			optimize_real_expression(lhs_expression);
			optimize_real_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(RealExpression::ConstantValue { value: lhs_value, .. }, RealExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.greater_than_or_equal_to(rhs_value), start_column: *start_column },
				_ => {}
			}
		}
		BoolExpression::RealLessThan { lhs_expression, rhs_expression, start_column } => {
			optimize_real_expression(lhs_expression);
			optimize_real_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(RealExpression::ConstantValue { value: lhs_value, .. }, RealExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.less_than(rhs_value), start_column: *start_column },
				_ => {}
			}
		}
		BoolExpression::RealLessThanOrEqualTo { lhs_expression, rhs_expression, start_column } => {
			optimize_real_expression(lhs_expression);
			optimize_real_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(RealExpression::ConstantValue { value: lhs_value, .. }, RealExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.less_than_or_equal_to(rhs_value), start_column: *start_column },
				_ => {}
			}
		}

		BoolExpression::ComplexEqualTo { lhs_expression, rhs_expression, start_column } => {
			optimize_complex_expression(lhs_expression);
			optimize_complex_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(ComplexExpression::ConstantValue { value: lhs_value, .. }, ComplexExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.equal_to(*rhs_value), start_column: *start_column },
				_ => {}
			}
		}
		BoolExpression::ComplexNotEqualTo { lhs_expression, rhs_expression, start_column } => {
			optimize_complex_expression(lhs_expression);
			optimize_complex_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(ComplexExpression::ConstantValue { value: lhs_value, .. }, ComplexExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.not_equal_to(*rhs_value), start_column: *start_column },
				_ => {}
			}
		}

		BoolExpression::StringEqualTo { lhs_expression, rhs_expression, start_column } => {
			optimize_string_expression(lhs_expression);
			optimize_string_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(StringExpression::ConstantValue { value: lhs_value, .. }, StringExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.equal_to(rhs_value), start_column: *start_column },
				_ => {}
			}
		}
		BoolExpression::StringNotEqualTo { lhs_expression, rhs_expression, start_column } => {
			optimize_string_expression(lhs_expression);
			optimize_string_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(StringExpression::ConstantValue { value: lhs_value, .. }, StringExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.not_equal_to(rhs_value), start_column: *start_column },
				_ => {}
			}
		}
		// TODO
		BoolExpression::StringGreaterThan { .. } | BoolExpression::StringGreaterThanOrEqualTo { .. } | BoolExpression::StringLessThan { .. } | BoolExpression::StringLessThanOrEqualTo { .. } => {}
	}
}

pub fn optimize_real_expression(expression: &mut RealExpression) {
	match expression {
		RealExpression::ConstantValue { .. } => {}

		RealExpression::CastFromInt(sub_expression) => {
			optimize_int_expression(sub_expression);
			match &mut **sub_expression {
				IntExpression::ConstantValue { value, start_column } => {
					*expression = RealExpression::ConstantValue { value: replace(value, IntValue::zero()).to_real(), start_column: *start_column }
				}
				_ => {}
			}
		}
		RealExpression::CastFromComplex(sub_expression) => {
			optimize_complex_expression(sub_expression);
			match &mut **sub_expression {
				ComplexExpression::ConstantValue { value, start_column } => {
					let result = replace(value, ComplexValue::zero()).to_real(None, 1.try_into().unwrap());
					match result {
						Ok(result) => *expression = RealExpression::ConstantValue { value: result, start_column: *start_column },
						Err(_) => {}
					}
				}
				_ => {}
			}
		}
		
		RealExpression::Addition { lhs_expression, rhs_expression, .. } => {
			optimize_real_expression(lhs_expression);
			optimize_real_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(RealExpression::ConstantValue { .. }, RealExpression::ConstantValue { .. }) => {
					let (lhs_expression, rhs_expression, start_column) =
						match replace(expression, RealExpression::ConstantValue { value: RealValue::zero(), start_column: 1.try_into().unwrap() })
					{
						RealExpression::Addition { lhs_expression, rhs_expression, start_column } => (lhs_expression, rhs_expression, start_column),
						_ => unreachable!(),
					};
					let (lhs_value, rhs_value) = match (*lhs_expression, *rhs_expression) {
						(RealExpression::ConstantValue { value: lhs_value, .. }, RealExpression::ConstantValue { value: rhs_value, .. }) => (lhs_value, rhs_value),
						_ => unreachable!(),
					};
					match lhs_value.add(rhs_value, false) {
						Some(result) => *expression = RealExpression::ConstantValue { value: result, start_column },
						None => {}
					}
				}
				_ => {}
			}
		}
		RealExpression::Subtraction { lhs_expression, rhs_expression, .. } => {
			optimize_real_expression(lhs_expression);
			optimize_real_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(RealExpression::ConstantValue { .. }, RealExpression::ConstantValue { .. }) => {
					let (lhs_expression, rhs_expression, start_column) =
						match replace(expression, RealExpression::ConstantValue { value: RealValue::zero(), start_column: 1.try_into().unwrap() })
					{
						RealExpression::Subtraction { lhs_expression, rhs_expression, start_column } => (lhs_expression, rhs_expression, start_column),
						_ => unreachable!(),
					};
					let (lhs_value, rhs_value) = match (*lhs_expression, *rhs_expression) {
						(RealExpression::ConstantValue { value: lhs_value, .. }, RealExpression::ConstantValue { value: rhs_value, .. }) => (lhs_value, rhs_value),
						_ => unreachable!(),
					};
					match lhs_value.sub(rhs_value, false) {
						Some(result) => *expression = RealExpression::ConstantValue { value: result, start_column },
						None => {}
					}
				}
				_ => {}
			}
		}
		RealExpression::Multiplication { lhs_expression, rhs_expression, .. } => {
			optimize_real_expression(lhs_expression);
			optimize_real_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(RealExpression::ConstantValue { .. }, RealExpression::ConstantValue { .. }) => {
					let (lhs_expression, rhs_expression, start_column) =
						match replace(expression, RealExpression::ConstantValue { value: RealValue::zero(), start_column: 1.try_into().unwrap() })
					{
						RealExpression::Multiplication { lhs_expression, rhs_expression, start_column } => (lhs_expression, rhs_expression, start_column),
						_ => unreachable!(),
					};
					let (lhs_value, rhs_value) = match (*lhs_expression, *rhs_expression) {
						(RealExpression::ConstantValue { value: lhs_value, .. }, RealExpression::ConstantValue { value: rhs_value, .. }) => (lhs_value, rhs_value),
						_ => unreachable!(),
					};
					match lhs_value.mul(rhs_value, false) {
						Some(result) => *expression = RealExpression::ConstantValue { value: result, start_column },
						None => {}
					}
				}
				_ => {}
			}
		}
		RealExpression::Division { lhs_expression, rhs_expression, .. } => {
			optimize_real_expression(lhs_expression);
			optimize_real_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(RealExpression::ConstantValue { .. }, RealExpression::ConstantValue { .. }) => {
					let (lhs_expression, rhs_expression, start_column) =
						match replace(expression, RealExpression::ConstantValue { value: RealValue::zero(), start_column: 1.try_into().unwrap() })
					{
						RealExpression::Division { lhs_expression, rhs_expression, start_column } => (lhs_expression, rhs_expression, start_column),
						_ => unreachable!(),
					};
					let (lhs_value, rhs_value) = match (*lhs_expression, *rhs_expression) {
						(RealExpression::ConstantValue { value: lhs_value, .. }, RealExpression::ConstantValue { value: rhs_value, .. }) => (lhs_value, rhs_value),
						_ => unreachable!(),
					};
					match lhs_value.div(rhs_value, false) {
						Some(result) => *expression = RealExpression::ConstantValue { value: result, start_column },
						None => {}
					}
				}
				_ => {}
			}
		}
		RealExpression::Exponentiation { lhs_expression, rhs_expression, .. } => {
			optimize_real_expression(lhs_expression);
			optimize_real_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(RealExpression::ConstantValue { .. }, RealExpression::ConstantValue { .. }) => {
					let (lhs_expression, rhs_expression, start_column) =
						match replace(expression, RealExpression::ConstantValue { value: RealValue::zero(), start_column: 1.try_into().unwrap() })
					{
						RealExpression::Exponentiation { lhs_expression, rhs_expression, start_column } => (lhs_expression, rhs_expression, start_column),
						_ => unreachable!(),
					};
					let (lhs_value, rhs_value) = match (*lhs_expression, *rhs_expression) {
						(RealExpression::ConstantValue { value: lhs_value, .. }, RealExpression::ConstantValue { value: rhs_value, .. }) => (lhs_value, rhs_value),
						_ => unreachable!(),
					};
					match lhs_value.pow(rhs_value, false) {
						Some(result) => *expression = RealExpression::ConstantValue { value: result, start_column },
						None => {}
					}
				}
				_ => {}
			}
		}
		RealExpression::FlooredDivision { lhs_expression, rhs_expression, .. } => {
			optimize_int_expression(lhs_expression);
			optimize_int_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(IntExpression::ConstantValue { .. }, IntExpression::ConstantValue { .. }) => {
					let (lhs_expression, rhs_expression, start_column) =
						match replace(expression, RealExpression::ConstantValue { value: RealValue::zero(), start_column: 1.try_into().unwrap() })
					{
						RealExpression::FlooredDivision { lhs_expression, rhs_expression, start_column } => (lhs_expression, rhs_expression, start_column),
						_ => unreachable!(),
					};
					let (lhs_value, rhs_value) = match (*lhs_expression, *rhs_expression) {
						(IntExpression::ConstantValue { value: lhs_value, .. }, IntExpression::ConstantValue { value: rhs_value, .. }) => (lhs_value, rhs_value),
						_ => unreachable!(),
					};
					match lhs_value.floored_div(rhs_value) {
						Some(result) => *expression = RealExpression::ConstantValue { value: result, start_column },
						None => {}
					}
				}
				_ => {}
			}
		}
		RealExpression::Negation { sub_expression, .. } => {
			optimize_real_expression(sub_expression);
			match &**sub_expression {
				RealExpression::ConstantValue { .. } => {
					let (sub_expression, start_column) = match replace(expression, RealExpression::ConstantValue { value: RealValue::zero(), start_column: 1.try_into().unwrap() }) {
						RealExpression::Negation { sub_expression, start_column, .. } => (sub_expression, start_column),
						_ => unreachable!(),
					};
					let value = match *sub_expression {
						RealExpression::ConstantValue { value, .. } => value,
						_ => unreachable!(),
					};
					*expression = RealExpression::ConstantValue { value: value.neg(), start_column };
				}
				_ => {}
			}
		}
		RealExpression::LValue(l_value) => {
			for l_value_argument in l_value.arguments.iter_mut() {
				optimize_any_type_expression(l_value_argument);
			}
		}
	}
}

pub fn optimize_complex_expression(expression: &mut ComplexExpression) {
	match expression {
		ComplexExpression::ConstantValue { .. } => {}
		ComplexExpression::CastFromReal(sub_expression) => {
			optimize_real_expression(sub_expression);
			match &**sub_expression {
				RealExpression::ConstantValue { value, start_column } =>
					*expression = ComplexExpression::ConstantValue { value: value.to_complex(), start_column: *start_column },
				_ => {}
			}
		}

		ComplexExpression::LValue(l_value) => {
			for l_value_argument in l_value.arguments.iter_mut() {
				optimize_any_type_expression(l_value_argument);
			}
		}
		ComplexExpression::Addition { lhs_expression, rhs_expression, start_column } => {
			optimize_complex_expression(lhs_expression);
			optimize_complex_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(ComplexExpression::ConstantValue { value: lhs_value, .. }, ComplexExpression::ConstantValue { value: rhs_value, .. }) => {
					if let Some(result) = lhs_value.add(*rhs_value, false) {
						*expression = ComplexExpression::ConstantValue { value: result, start_column: *start_column };
					}
				}
				_ => {}
			}
		}
		ComplexExpression::Subtraction { lhs_expression, rhs_expression, start_column } => {
			optimize_complex_expression(lhs_expression);
			optimize_complex_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(ComplexExpression::ConstantValue { value: lhs_value, .. }, ComplexExpression::ConstantValue { value: rhs_value, .. }) => {
					if let Some(result) = lhs_value.sub(*rhs_value, false) {
						*expression = ComplexExpression::ConstantValue { value: result, start_column: *start_column };
					}
				}
				_ => {}
			}
		}
		ComplexExpression::Multiplication { lhs_expression, rhs_expression, start_column } => {
			optimize_complex_expression(lhs_expression);
			optimize_complex_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(ComplexExpression::ConstantValue { value: lhs_value, .. }, ComplexExpression::ConstantValue { value: rhs_value, .. }) => {
					if let Some(result) = lhs_value.mul(*rhs_value, false) {
						*expression = ComplexExpression::ConstantValue { value: result, start_column: *start_column };
					}
				}
				_ => {}
			}
		}
		ComplexExpression::Division { lhs_expression, rhs_expression, start_column } => {
			optimize_complex_expression(lhs_expression);
			optimize_complex_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(ComplexExpression::ConstantValue { value: lhs_value, .. }, ComplexExpression::ConstantValue { value: rhs_value, .. }) => {
					if let Some(result) = lhs_value.div(*rhs_value, false) {
						*expression = ComplexExpression::ConstantValue { value: result, start_column: *start_column };
					}
				}
				_ => {}
			}
		}
		ComplexExpression::Exponentiation { lhs_expression, rhs_expression, start_column } => {
			optimize_complex_expression(lhs_expression);
			optimize_complex_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(ComplexExpression::ConstantValue { value: lhs_value, .. }, ComplexExpression::ConstantValue { value: rhs_value, .. }) => {
					if let Some(result) = lhs_value.pow(*rhs_value, false) {
						*expression = ComplexExpression::ConstantValue { value: result, start_column: *start_column };
					}
				}
				_ => {}
			}
		}
		ComplexExpression::Negation { sub_expression, start_column } => {
			optimize_complex_expression(sub_expression);
			match &**sub_expression {
				ComplexExpression::ConstantValue { value, .. } =>
					*expression = ComplexExpression::ConstantValue { value: value.neg(), start_column: *start_column },
				_ => {}
			}
		}
	}
}

pub fn optimize_string_expression(expression: &mut StringExpression) {
	match expression {
		StringExpression::ConstantValue { .. } => {}
		StringExpression::LValue(l_value) => {
			for l_value_argument in l_value.arguments.iter_mut() {
				optimize_any_type_expression(l_value_argument);
			}
		}
		StringExpression::Concatenation { lhs_expression, rhs_expression, .. } => {
			optimize_string_expression(lhs_expression);
			optimize_string_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(StringExpression::ConstantValue { .. }, StringExpression::ConstantValue { .. }) => {
					let (lhs_expression, rhs_expression, start_column) =
						match replace(expression, StringExpression::ConstantValue { value: StringValue::empty(), start_column: 1.try_into().unwrap() })
					{
						StringExpression::Concatenation { lhs_expression, rhs_expression, start_column } => (lhs_expression, rhs_expression, start_column),
						_ => unreachable!(),
					};
					let (lhs_value, rhs_value) = match (*lhs_expression, *rhs_expression) {
						(StringExpression::ConstantValue { value: lhs_value, .. }, StringExpression::ConstantValue { value: rhs_value, .. }) => (lhs_value, rhs_value),
						_ => unreachable!(),
					};
					*expression = StringExpression::ConstantValue { value: lhs_value.concat(rhs_value), start_column };
				}
				_ => {}
			}
		}
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