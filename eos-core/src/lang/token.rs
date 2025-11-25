use std::fmt::Display;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Sym {
    Number,
    Variable,
    Operator,
    Symbol,
    EOF,
}

impl Display for Sym {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number => write!(f, "number"),
            Self::Variable => write!(f, "variable"),
            Self::Operator => write!(f, "operator"),
            Self::Symbol => write!(f, "symbol"),
            Self::EOF => write!(f, "EOF"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Token<'a> {
    pub sym: Sym,
    pub position: usize,
    pub value: &'a str,
}

impl<'a> Token<'a> {
    pub fn new(sym: Sym, position: usize, value: &'a str) -> Self {
        Self {
            sym,
            position,
            value,
        }
    }
}
