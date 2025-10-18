use crate::lang::Position;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Sym {
    Number,
    Variable,
    Operator,
    Symbol,
    EOF,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Token {
    pub sym: Sym,
    pub position: Position,
}

impl Token {
    pub fn new(sym: Sym, position: Position) -> Self {
        Self { sym, position }
    }
}
