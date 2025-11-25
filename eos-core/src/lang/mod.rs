#[macro_use]
mod macros;

pub mod ast;
mod error;
mod lexical;
mod token;

pub use ast::{Ast, Binary, Operator, Primary, Unary};

pub mod private {
    pub use std::result::Result::Err;
}

pub type Result<A> = std::result::Result<A, error::Error>;
