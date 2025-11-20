use std::fmt::Display;

#[macro_use]
mod macros;

mod ast;
mod error;
mod lexical;
mod nursery;
mod token;

pub use ast::{Ast, Binary, Node, Operator, Primary, PrimaryKind, Unary, Var};

pub mod private {
    pub use std::result::Result::Err;
}

pub type Result<A> = std::result::Result<A, error::Error>;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
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
