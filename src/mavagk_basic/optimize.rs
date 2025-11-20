use std::mem::replace;

use crate::mavagk_basic::{abstract_syntax_tree::{AnyTypeExpression, AnyTypeLValue, BoolExpression, ComplexExpression, ComplexLValue, FloatExpression, FloatLValue, IntExpression, IntLValue, PrintOperand, Statement, StatementVariant, StringExpression, StringLValue}, value::{BoolValue, ComplexValue, IntValue, StringValue}};

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
				optimize_print_operand(sub_expression);
			}
		}
		StatementVariant::Input { prompt, timeout, elapsed, inputs } => {
			if let Some(prompt) = prompt {
				optimize_any_type_expression(prompt);
			}
			if let Some(timeout) = timeout {
				optimize_any_type_expression(timeout);
			}
			if let Some(elapsed) = elapsed {
				optimize_any_type_expression(elapsed);
			}
			for input in inputs {
				optimize_any_type_l_value(input);
			}
		}
		StatementVariant::ForInt { loop_variable, initial, limit, step } => {
			optimize_int_l_value(loop_variable);
			optimize_int_expression(initial);
			optimize_int_expression(limit);
			if let Some(step) = step {
				optimize_int_expression(step);
			}
		}
		StatementVariant::ForFloat { loop_variable, initial, limit, step } => {
			optimize_float_l_value(loop_variable);
			optimize_float_expression(initial);
			optimize_float_expression(limit);
			if let Some(step) = step {
				optimize_float_expression(step);
			}
		}
		StatementVariant::Next(loop_variables) => {
			for loop_variable in loop_variables {
				optimize_any_type_l_value(loop_variable);
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
		StatementVariant::NumericAssignment(l_values, r_value) => {
			for l_value in l_values {
				match l_value {
					AnyTypeLValue::Float(l_value_argument) => {
						for l_value_argument in l_value_argument.arguments.iter_mut() {
							optimize_any_type_expression(l_value_argument);
						}
					}
					AnyTypeLValue::Int(l_value_argument) => {
						for l_value_argument in l_value_argument.arguments.iter_mut() {
							optimize_any_type_expression(l_value_argument);
						}
					}
					AnyTypeLValue::Complex(l_value_argument) => {
						for l_value_argument in l_value_argument.arguments.iter_mut() {
							optimize_any_type_expression(l_value_argument);
						}
					}
					AnyTypeLValue::String(_) => unreachable!(),
				}
			}
			optimize_any_type_expression(r_value);
		}
		StatementVariant::StringAssignment(l_values, r_value) => {
			for l_value in l_values {
				for l_value_argument in l_value.arguments.iter_mut() {
					optimize_any_type_expression(l_value_argument);
				}
			}
			optimize_string_expression(r_value);
		}
		StatementVariant::Load(filename_expression) | StatementVariant::Save(filename_expression) => {
			if let Some(filename_expression) = filename_expression {
				optimize_string_expression(filename_expression);
			}
		}
		StatementVariant::Stop | StatementVariant::End | StatementVariant::Data(_) | StatementVariant::Return | StatementVariant::Randomize |
		StatementVariant::Clear | StatementVariant::Clr | StatementVariant::New => {}
		StatementVariant::Read { to_do_when_data_missing_statement, variables } => {
			if let Some(to_do_when_data_missing_statement) = to_do_when_data_missing_statement {
				optimize_statement(to_do_when_data_missing_statement);
			}
			for variable in variables {
				optimize_any_type_l_value(variable);
			}
		}
		StatementVariant::Restore(restore_to_line_number_expression) => {
			if let Some(restore_to_line_number_expression) = restore_to_line_number_expression {
				optimize_int_expression(restore_to_line_number_expression);
			}
		}
		StatementVariant::Dimension(arrays) => {
			for array in arrays {
				for (lower_bound, upper_bound) in array.dimensions.iter_mut() {
					if let Some(lower_bound) = lower_bound {
						optimize_int_expression(lower_bound);
					}
					optimize_int_expression(upper_bound);
				}
			}
		}
		StatementVariant::DefInt(l_value, expression) => {
			optimize_int_l_value(l_value);
			optimize_int_expression(expression);
		}
		StatementVariant::DefFloat(l_value, expression) => {
			optimize_float_l_value(l_value);
			optimize_float_expression(expression);
		}
		StatementVariant::DefComplex(l_value, expression) => {
			optimize_complex_l_value(l_value);
			optimize_complex_expression(expression);
		}
		StatementVariant::DefString(l_value, expression) => {
			optimize_string_l_value(l_value);
			optimize_string_expression(expression);
		}
		StatementVariant::OnGoto { index, line_numbers, else_statement } | StatementVariant::OnGosub { index, line_numbers, else_statement } => {
			optimize_int_expression(index);
			for line_number in line_numbers {
				optimize_int_expression(line_number);
			}
			if let Some(else_statement) = else_statement {
				optimize_statement(else_statement);
			}
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
		IntExpression::Addition { lhs_expression, rhs_expression, .. } => {
			optimize_int_expression(lhs_expression);
			optimize_int_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(IntExpression::ConstantValue { .. }, IntExpression::ConstantValue { .. }) => {
					let (lhs_expression, rhs_expression, start_column) =
						match replace(expression, IntExpression::ConstantValue { value: IntValue::zero(), start_column: 1.try_into().unwrap() })
					{
						IntExpression::Addition { lhs_expression, rhs_expression, start_column } => (lhs_expression, rhs_expression, start_column),
						_ => unreachable!(),
					};
					let (lhs_value, rhs_value) = match (*lhs_expression, *rhs_expression) {
						(IntExpression::ConstantValue { value: lhs_value, .. }, IntExpression::ConstantValue { value: rhs_value, .. }) => (lhs_value, rhs_value),
						_ => unreachable!(),
					};
					*expression = IntExpression::ConstantValue { value: lhs_value.add(&rhs_value), start_column };
				}
				_ => {}
			}
		}
		IntExpression::Subtraction { lhs_expression, rhs_expression, .. } => {
			optimize_int_expression(lhs_expression);
			optimize_int_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(IntExpression::ConstantValue { .. }, IntExpression::ConstantValue { .. }) => {
					let (lhs_expression, rhs_expression, start_column) =
						match replace(expression, IntExpression::ConstantValue { value: IntValue::zero(), start_column: 1.try_into().unwrap() })
					{
						IntExpression::Subtraction { lhs_expression, rhs_expression, start_column } => (lhs_expression, rhs_expression, start_column),
						_ => unreachable!(),
					};
					let (lhs_value, rhs_value) = match (*lhs_expression, *rhs_expression) {
						(IntExpression::ConstantValue { value: lhs_value, .. }, IntExpression::ConstantValue { value: rhs_value, .. }) => (lhs_value, rhs_value),
						_ => unreachable!(),
					};
					*expression = IntExpression::ConstantValue { value: lhs_value.sub(rhs_value), start_column };
				}
				_ => {}
			}
		}
		IntExpression::Multiplication { lhs_expression, rhs_expression, .. } => {
			optimize_int_expression(lhs_expression);
			optimize_int_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(IntExpression::ConstantValue { .. }, IntExpression::ConstantValue { .. }) => {
					let (lhs_expression, rhs_expression, start_column) =
						match replace(expression, IntExpression::ConstantValue { value: IntValue::zero(), start_column: 1.try_into().unwrap() })
					{
						IntExpression::Multiplication { lhs_expression, rhs_expression, start_column } => (lhs_expression, rhs_expression, start_column),
						_ => unreachable!(),
					};
					let (lhs_value, rhs_value) = match (*lhs_expression, *rhs_expression) {
						(IntExpression::ConstantValue { value: lhs_value, .. }, IntExpression::ConstantValue { value: rhs_value, .. }) => (lhs_value, rhs_value),
						_ => unreachable!(),
					};
					*expression = IntExpression::ConstantValue { value: lhs_value.mul(rhs_value), start_column };
				}
				_ => {}
			}
		}
		IntExpression::FlooredDivision { lhs_expression, rhs_expression, .. } => {
			optimize_int_expression(lhs_expression);
			optimize_int_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(IntExpression::ConstantValue { value: lhs_value, .. }, IntExpression::ConstantValue { value: rhs_value, .. }) => {
					match lhs_value.clone().floored_div(rhs_value.clone()) {
						Ok(result) => *expression = IntExpression::ConstantValue { value: result, start_column: expression.get_start_column() },
						_ => {}
					}
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
		IntExpression::Negation { sub_expression, .. } => {
			optimize_int_expression(sub_expression);
			match &**sub_expression {
				IntExpression::ConstantValue { .. } => {
					let (sub_expression, start_column) = match replace(expression, IntExpression::ConstantValue { value: IntValue::zero(), start_column: 1.try_into().unwrap() }) {
						IntExpression::Negation { sub_expression, start_column, .. } => (sub_expression, start_column),
						_ => unreachable!(),
					};
					let value = match *sub_expression {
						IntExpression::ConstantValue { value, .. } => value,
						_ => unreachable!(),
					};
					*expression = IntExpression::ConstantValue { value: value.neg(), start_column };
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
		IntExpression::CastFromFloat(float_expression) => {
			optimize_float_expression(float_expression);
			match &**float_expression {
				FloatExpression::ConstantValue { value, start_column } => {
					match value.clone().to_int() {
						Ok(value) => *expression = IntExpression::ConstantValue { value: value, start_column: *start_column },
						_ => {}
					}
				}
				_ => {}
			}
		}
		IntExpression::ConstantValue { .. } => {}
		IntExpression::LValue(l_value) => optimize_int_l_value(l_value),
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
		BoolExpression::FloatIsNonZero(sub_expression) => {
			optimize_float_expression(sub_expression);
			match &**sub_expression {
				FloatExpression::ConstantValue { value, start_column } =>
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

		BoolExpression::FloatEqualTo { lhs_expression, rhs_expression, start_column } => {
			optimize_float_expression(lhs_expression);
			optimize_float_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(FloatExpression::ConstantValue { value: lhs_value, .. }, FloatExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.equal_to(rhs_value), start_column: *start_column },
				_ => {}
			}
		}
		BoolExpression::FloatNotEqualTo { lhs_expression, rhs_expression, start_column } => {
			optimize_float_expression(lhs_expression);
			optimize_float_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(FloatExpression::ConstantValue { value: lhs_value, .. }, FloatExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.not_equal_to(rhs_value), start_column: *start_column },
				_ => {}
			}
		}
		BoolExpression::FloatGreaterThan { lhs_expression, rhs_expression, start_column } => {
			optimize_float_expression(lhs_expression);
			optimize_float_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(FloatExpression::ConstantValue { value: lhs_value, .. }, FloatExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.greater_than(rhs_value), start_column: *start_column },
				_ => {}
			}
		}
		BoolExpression::FloatGreaterThanOrEqualTo { lhs_expression, rhs_expression, start_column } => {
			optimize_float_expression(lhs_expression);
			optimize_float_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(FloatExpression::ConstantValue { value: lhs_value, .. }, FloatExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.greater_than_or_equal_to(rhs_value), start_column: *start_column },
				_ => {}
			}
		}
		BoolExpression::FloatLessThan { lhs_expression, rhs_expression, start_column } => {
			optimize_float_expression(lhs_expression);
			optimize_float_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(FloatExpression::ConstantValue { value: lhs_value, .. }, FloatExpression::ConstantValue { value: rhs_value, .. }) =>
					*expression = BoolExpression::ConstantValue { value: lhs_value.less_than(rhs_value), start_column: *start_column },
				_ => {}
			}
		}
		BoolExpression::FloatLessThanOrEqualTo { lhs_expression, rhs_expression, start_column } => {
			optimize_float_expression(lhs_expression);
			optimize_float_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(FloatExpression::ConstantValue { value: lhs_value, .. }, FloatExpression::ConstantValue { value: rhs_value, .. }) =>
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

pub fn optimize_float_expression(expression: &mut FloatExpression) {
	match expression {
		FloatExpression::ConstantValue { .. } => {}

		FloatExpression::CastFromInt(sub_expression) => {
			optimize_int_expression(sub_expression);
			match &mut **sub_expression {
				IntExpression::ConstantValue { value, start_column } => {
					*expression = FloatExpression::ConstantValue { value: replace(value, IntValue::zero()).to_float(), start_column: *start_column }
				}
				_ => {}
			}
		}
		FloatExpression::CastFromComplex(sub_expression) => {
			optimize_complex_expression(sub_expression);
			match &mut **sub_expression {
				ComplexExpression::ConstantValue { value, start_column } => {
					let result = replace(value, ComplexValue::ZERO).to_float();
					match result {
						Ok(result) => *expression = FloatExpression::ConstantValue { value: result, start_column: *start_column },
						_ => {}
					}
				}
				_ => {}
			}
		}
		
		FloatExpression::Addition { lhs_expression, rhs_expression, .. } => {
			optimize_float_expression(lhs_expression);
			optimize_float_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(FloatExpression::ConstantValue { value: lhs_value, .. }, FloatExpression::ConstantValue { value: rhs_value, .. }) => {
					match lhs_value.add(*rhs_value, None) {
						Ok(result) => *expression = FloatExpression::ConstantValue { value: result, start_column: expression.get_start_column() },
						_ => {}
					}
				}
				_ => {}
			}
		}
		FloatExpression::Subtraction { lhs_expression, rhs_expression, .. } => {
			optimize_float_expression(lhs_expression);
			optimize_float_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(FloatExpression::ConstantValue { value: lhs_value, .. }, FloatExpression::ConstantValue { value: rhs_value, .. }) => {
					match lhs_value.sub(*rhs_value, None) {
						Ok(result) => *expression = FloatExpression::ConstantValue { value: result, start_column: expression.get_start_column() },
						_ => {}
					}
				}
				_ => {}
			}
		}
		FloatExpression::Multiplication { lhs_expression, rhs_expression, .. } => {
			optimize_float_expression(lhs_expression);
			optimize_float_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(FloatExpression::ConstantValue { value: lhs_value, .. }, FloatExpression::ConstantValue { value: rhs_value, .. }) => {
					match lhs_value.mul(*rhs_value, None) {
						Ok(result) => *expression = FloatExpression::ConstantValue { value: result, start_column: expression.get_start_column() },
						_ => {}
					}
				}
				_ => {}
			}
		}
		FloatExpression::Division { lhs_expression, rhs_expression, .. } => {
			optimize_float_expression(lhs_expression);
			optimize_float_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(FloatExpression::ConstantValue { value: lhs_value, .. }, FloatExpression::ConstantValue { value: rhs_value, .. }) => {
					match lhs_value.div(*rhs_value, None) {
						Ok(result) => *expression = FloatExpression::ConstantValue { value: result, start_column: expression.get_start_column() },
						_ => {}
					}
				}
				_ => {}
			}
		}
		FloatExpression::Exponentiation { lhs_expression, rhs_expression, .. } => {
			optimize_float_expression(lhs_expression);
			optimize_float_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(FloatExpression::ConstantValue { value: lhs_value, .. }, FloatExpression::ConstantValue { value: rhs_value, .. }) => {
					match lhs_value.pow(*rhs_value, None) {
						Ok(result) => *expression = FloatExpression::ConstantValue { value: result, start_column: expression.get_start_column() },
						_ => {}
					}
				}
				_ => {}
			}
		}
		FloatExpression::Negation { sub_expression, .. } => {
			optimize_float_expression(sub_expression);
			match &**sub_expression {
				FloatExpression::ConstantValue { value, .. } => {
					*expression = FloatExpression::ConstantValue { value: value.neg(), start_column: expression.get_start_column() };
				}
				_ => {}
			}
		}
		FloatExpression::LValue(l_value) => optimize_float_l_value(l_value),
	}
}

pub fn optimize_complex_expression(expression: &mut ComplexExpression) {
	match expression {
		ComplexExpression::ConstantValue { .. } => {}
		ComplexExpression::CastFromFloat(sub_expression) => {
			optimize_float_expression(sub_expression);
			match &**sub_expression {
				FloatExpression::ConstantValue { value, start_column } =>
					*expression = ComplexExpression::ConstantValue { value: value.to_complex(), start_column: *start_column },
				_ => {}
			}
		}

		ComplexExpression::LValue(l_value) => optimize_complex_l_value(l_value),
		ComplexExpression::Addition { lhs_expression, rhs_expression, start_column } => {
			optimize_complex_expression(lhs_expression);
			optimize_complex_expression(rhs_expression);
			match (&**lhs_expression, &**rhs_expression) {
				(ComplexExpression::ConstantValue { value: lhs_value, .. }, ComplexExpression::ConstantValue { value: rhs_value, .. }) => {
					if let Ok(result) = lhs_value.add(*rhs_value, false) {
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
					if let Ok(result) = lhs_value.sub(*rhs_value, false) {
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
					if let Ok(result) = lhs_value.mul(*rhs_value, false) {
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
					if let Ok(result) = lhs_value.div(*rhs_value, false, false) {
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
					if let Ok(result) = lhs_value.pow(*rhs_value, false) {
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
		StringExpression::LValue(l_value) => optimize_string_l_value(l_value),
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
		AnyTypeExpression::Float(expression) => optimize_float_expression(expression),
		AnyTypeExpression::Complex(expression) => optimize_complex_expression(expression),
		AnyTypeExpression::String(expression) => optimize_string_expression(expression),
	}
}

pub fn optimize_print_operand(expression: &mut PrintOperand) {
	match expression {
		PrintOperand::Expression(expression) => optimize_any_type_expression(expression),
		PrintOperand::Comma(_) | PrintOperand::Semicolon(_) => {},
	}
}

pub fn optimize_int_l_value(l_value: &mut IntLValue) {
	for l_value_argument in l_value.arguments.iter_mut() {
		optimize_any_type_expression(l_value_argument);
	}
}

pub fn optimize_float_l_value(l_value: &mut FloatLValue) {
	for l_value_argument in l_value.arguments.iter_mut() {
		optimize_any_type_expression(l_value_argument);
	}
}

pub fn optimize_complex_l_value(l_value: &mut ComplexLValue) {
	for l_value_argument in l_value.arguments.iter_mut() {
		optimize_any_type_expression(l_value_argument);
	}
}

pub fn optimize_string_l_value(l_value: &mut StringLValue) {
	for l_value_argument in l_value.arguments.iter_mut() {
		optimize_any_type_expression(l_value_argument);
	}
}

pub fn optimize_any_type_l_value(l_value: &mut AnyTypeLValue) {
	match l_value {
		AnyTypeLValue::Int(l_value) => optimize_int_l_value(l_value),
		AnyTypeLValue::Float(l_value) => optimize_float_l_value(l_value),
		AnyTypeLValue::Complex(l_value) => optimize_complex_l_value(l_value),
		AnyTypeLValue::String(l_value) => optimize_string_l_value(l_value),
	}
}