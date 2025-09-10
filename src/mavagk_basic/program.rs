use std::{collections::BTreeMap, rc::Rc};

use num::BigInt;

use crate::mavagk_basic::{abstract_syntax_tree::Statement, error::Error};

pub struct Program {
	pub lines: BTreeMap<Rc<BigInt>, (Box<[Statement]>, Option<Error>, Box<str>)>,
}

impl Program {
	pub fn new() -> Self {
		Self {
			lines: BTreeMap::new(),
		}
	}
}