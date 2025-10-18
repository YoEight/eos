#[derive(Debug)]
pub struct Attrs {}

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

#[derive(Debug)]
pub struct Ast {
    pub attrs: Attrs,
    pub node: Node,
}
