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

#[derive(Debug, Clone)]
pub enum Node {
    Number(u64),
    Var(Var),
    Binary(Binary),
    Group(Box<Ast>),
    Unary(Unary),
}

impl From<Primary> for Node {
    fn from(value: Primary) -> Self {
        match value.kind {
            PrimaryKind::Number(n) => Node::Number(n),
            PrimaryKind::Var(v) => Node::Var(v),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub op: Operator,
    pub lhs: Box<Ast>,
    pub rhs: Box<Ast>,
}

#[derive(Debug, Copy, Clone)]
pub struct Primary {
    pub attrs: Attrs,
    pub kind: PrimaryKind,
}

#[derive(Debug, Copy, Clone)]
pub enum PrimaryKind {
    Var(Var),
    Number(u64),
}

#[derive(Debug, Clone)]
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

pub struct Additive<A> {
    pub is_add: bool,
    pub inner: A,
}

#[derive(Debug, Clone)]
pub struct Ast {
    pub attrs: Attrs,
    pub node: Node,
}

impl From<Primary> for Ast {
    fn from(value: Primary) -> Self {
        Self {
            attrs: value.attrs,
            node: match value.kind {
                PrimaryKind::Number(n) => Node::Number(n),
                PrimaryKind::Var(v) => Node::Var(v),
            },
        }
    }
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

    pub fn as_group(&self) -> &Ast {
        match &self.node {
            Node::Group(g) => g,
            _ => panic!("not a group"),
        }
    }

    pub fn is_group(&self) -> bool {
        matches!(self.node, Node::Group(_))
    }

    pub fn collect_additive_terms(self) -> Vec<Additive<Ast>> {
        let mut result = Vec::new();

        match self.node {
            Node::Binary(binary) if matches!(binary.op, Operator::Add | Operator::Sub) => {
                result.extend(binary.lhs.collect_additive_terms());
                let mut rhs = binary.rhs.collect_additive_terms();

                if binary.op == Operator::Sub {
                    for term in rhs.iter_mut() {
                        term.is_add = !term.is_add;
                    }
                }

                result.extend(rhs);
            }

            node => {
                result.push(Additive {
                    is_add: true,
                    inner: Ast {
                        attrs: self.attrs,
                        node,
                    },
                });
            }
        }

        result
    }

    pub fn as_primary(&self) -> Option<Primary> {
        match &self.node {
            Node::Number(n) => Some(Primary {
                attrs: self.attrs,
                kind: PrimaryKind::Number(*n),
            }),

            Node::Var(v) => Some(Primary {
                attrs: self.attrs,
                kind: PrimaryKind::Var(*v),
            }),

            _ => None,
        }
    }
}
