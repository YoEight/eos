use crate::lang::{ast, lexical, Position};
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
    Parser(ast::Error),
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lexer(e) => write!(f, "lexer: {e}"),
            Self::Parser(e) => write!(f, "parser: {e}"),
        }
    }
}

impl From<lexical::Error> for ErrorKind {
    fn from(value: lexical::Error) -> Self {
        Self::Lexer(value)
    }
}

impl From<ast::Error> for ErrorKind {
    fn from(value: ast::Error) -> Self {
        Self::Parser(value)
    }
}
