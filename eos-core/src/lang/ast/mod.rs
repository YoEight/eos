use crate::lang::Position;

mod parser;

#[cfg(test)]
mod tests;

pub use parser::Error;

#[derive(Debug, Copy, Clone)]
pub struct Attrs {
    pub position: Position,
}

impl Attrs {
    pub fn new(position: Position) -> Self {
        Self { position }
    }
}

#[derive(Debug)]
pub enum Node {
    Number(u64),
    Var(Var),
    Binary(Operator, Box<Ast>, Box<Ast>),
    Group(Box<Ast>),
    Unary(Operator, Box<Ast>),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Exp,
    Eq,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Var {
    pub id: u64,
    pub exponent: u64,
}

impl Var {
    pub fn new(id: u64) -> Self {
        Self { id, exponent: 1 }
    }
}

#[derive(Debug)]
pub struct Ast {
    pub attrs: Attrs,
    pub node: Node,
}
