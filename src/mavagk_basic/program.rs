use std::{collections::BTreeMap, rc::Rc};

use num::BigInt;

use crate::mavagk_basic::{abstract_syntax_tree::Statement, error::Error};

/// A MavagkBasic program containing all its lines, does not include a direct mode line.
pub struct Program {
	/// Maps line numbers to a (list of optimized statements, list of unoptimized statements, a line error if it exists, the text source code of the line)
	pub lines: BTreeMap<Rc<BigInt>, (Box<[Statement]>, Box<[Statement]>, Option<Error>, Box<str>)>,
}

impl Program {
	pub fn new() -> Self {
		Self {
			lines: BTreeMap::new(),
		}
	}

	pub fn clear_program(&mut self) {
		self.lines = BTreeMap::new();
	}
}