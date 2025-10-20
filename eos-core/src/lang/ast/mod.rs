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
    Binary(Binary),
    Group(Box<Ast>),
    Unary(Unary),
}

#[derive(Debug)]
pub struct Binary {
    pub op: Operator,
    pub lhs: Box<Ast>,
    pub rhs: Box<Ast>,
}

#[derive(Debug)]
pub struct Unary {
    pub op: Operator,
    pub rhs: Box<Ast>,
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

impl Ast {
    pub fn as_number(&self) -> u64 {
        match &self.node {
            Node::Number(n) => *n,
            _ => panic!("not a number"),
        }
    }

    pub fn as_var(&self) -> Var {
        match &self.node {
            Node::Var(v) => *v,
            _ => panic!("not a var"),
        }
    }

    pub fn as_binary(&self) -> &Binary {
        match &self.node {
            Node::Binary(b) => b,
            _ => panic!("not a binary"),
        }
    }

    pub fn as_unary(&self) -> &Unary {
        match &self.node {
            Node::Unary(u) => u,
            _ => panic!("not a unary"),
        }
    }
}
