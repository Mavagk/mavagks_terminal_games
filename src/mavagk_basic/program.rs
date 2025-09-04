use std::collections::BTreeMap;

use num::BigInt;

use crate::mavagk_basic::{abstract_syntax_tree::Statement, error::Error};

pub struct Program {
	pub lines: BTreeMap<BigInt, (Box<[Statement]>, Option<Error>, Box<str>)>,
	pub unnumbered_line: Box<[Statement]>,
}

impl Program {
	pub fn new() -> Self {
		Self {
			lines: BTreeMap::new(),
			unnumbered_line: Box::default(),
		}
	}
}