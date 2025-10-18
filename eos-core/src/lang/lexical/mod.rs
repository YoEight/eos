use std::fmt::Display;

pub mod lexer;
pub mod text;

#[derive(Debug)]
pub enum Error {
    UnexpectedChar(char),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedChar(c) => write!(f, "unexpected character: {}", c),
        }
    }
}
