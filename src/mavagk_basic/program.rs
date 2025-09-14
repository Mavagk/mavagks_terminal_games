use std::{collections::BTreeMap, rc::Rc};

use num::BigInt;

use crate::mavagk_basic::{abstract_syntax_tree::Statement, error::FullError};

pub struct Program {
	pub lines: BTreeMap<Rc<BigInt>, (Box<[Statement]>, Option<FullError>, Box<str>)>,
}

impl Program {
	pub fn new() -> Self {
		Self {
			lines: BTreeMap::new(),
		}
	}
}