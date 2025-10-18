use std::fmt::Display;

#[macro_use]
mod macros;

mod error;
mod lexical;
mod nursery;
mod token;

pub mod private {
    pub use std::result::Result::Err;
}

pub type Result<A> = std::result::Result<A, error::Error>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Default for Position {
    fn default() -> Self {
        Self { line: 1, column: 1 }
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}
