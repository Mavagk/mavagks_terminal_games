use std::{fmt::{self, Display, Formatter}, num::NonZeroUsize};

pub enum Error {
	InvalidTokenFirstChar(NonZeroUsize, char),
	NotYetImplemented(Option<NonZeroUsize>, NonZeroUsize, String),
}

impl Display for Error {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			Self::InvalidTokenFirstChar(column, chr) => write!(f, "in column {column}: Invalid token first char '{chr}'."),
			Error::NotYetImplemented(line, column, feature) => match line {
				Some(line) => write!(f, "at {line}:{column}: {feature} not yet implemented."),
				None => write!(f, "in column {column}: {feature} not yet implemented."),
			},
		}
	}
}