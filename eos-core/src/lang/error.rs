use crate::lang::{Position, lexical};
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub struct Error {
    pub pos: Position,
    pub kind: ErrorKind,
}

impl std::error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.pos, self.kind)
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    Lexer(lexical::Error),
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lexer(e) => write!(f, "lexer: {e}"),
        }
    }
}

impl From<lexical::Error> for ErrorKind {
    fn from(value: lexical::Error) -> Self {
        Self::Lexer(value)
    }
}
