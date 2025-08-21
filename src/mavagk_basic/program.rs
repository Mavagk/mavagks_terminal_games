use std::collections::BTreeMap;

use num::BigInt;

use crate::mavagk_basic::statement::Statement;

pub struct Program {
	_lines: BTreeMap<BigInt, (Box<[Statement]>, Box<str>)>,
}