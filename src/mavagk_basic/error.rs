use std::{fmt::{self, Display, Formatter}, num::NonZeroUsize};

pub enum Error {
	InvalidTokenFirstChar(NonZeroUsize, char),
}

impl Display for Error {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			Self::InvalidTokenFirstChar(column, chr) => write!(f, "in column {column}: Invalid token first char '{chr}'."),
		}
	}
}