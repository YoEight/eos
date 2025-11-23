use crate::lang::Position;
use std::fmt::{Display, Formatter};

pub mod parser;

#[cfg(test)]
mod tests;

use crate::lang::nursery::Nursery;
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

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Add => write!(f, "+"),
            Operator::Sub => write!(f, "-"),
            Operator::Mul => write!(f, "*"),
            Operator::Div => write!(f, "/"),
            Operator::Exp => write!(f, "^"),
            Operator::Eq => write!(f, "="),
        }
    }
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
    pub fn new(attrs: Attrs, node: Node) -> Self {
        Self { attrs, node }
    }

    pub fn number(attrs: Attrs, n: u64) -> Self {
        Self {
            attrs,
            node: Node::Number(n),
        }
    }

    pub fn pretty_print(&self, nursery: &Nursery) -> String {
        let mut result = String::new();
        self.pretty_print_internal(nursery, &mut result);

        result
    }

    fn pretty_print_internal(&self, nursery: &Nursery, result: &mut String) {
        fn pretty_print_node(node: &Node, nursery: &Nursery, result: &mut String) {
            match node {
                Node::Number(n) => result.push_str(&n.to_string()),

                Node::Var(v) => match v.exponent {
                    0 => result.push_str("1"),
                    1 => result.push_str(nursery.get_var_string_or_panic(v)),
                    _ => result.push_str(&format!(
                        "{}^{}",
                        nursery.get_var_string_or_panic(v),
                        v.exponent
                    )),
                },

                Node::Binary(binary) => {
                    binary.lhs.pretty_print_internal(nursery, result);
                    result.push_str(" ");
                    result.push_str(&binary.op.to_string());
                    result.push_str(" ");
                    binary.rhs.pretty_print_internal(nursery, result);
                }

                Node::Group(g) => {
                    result.push_str("(");
                    g.pretty_print_internal(nursery, result);
                    result.push_str(")");
                }

                Node::Unary(unary) => {
                    result.push_str(&unary.op.to_string());
                    unary.rhs.pretty_print_internal(nursery, result);
                }
            }
        }

        pretty_print_node(&self.node, nursery, result);
    }

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

    pub fn distribute(self, op: Operator, primary: Primary) -> Ast {
        match self.node {
            Node::Binary(binary) => Ast {
                attrs: self.attrs,
                node: Node::Binary(Binary {
                    op: binary.op,
                    lhs: Box::new(Ast {
                        attrs: self.attrs,
                        node: Node::Binary(Binary {
                            op,
                            lhs: Box::new(primary.into()),
                            rhs: binary.lhs,
                        }),
                    }),
                    rhs: binary.rhs,
                }),
            },

            node => Ast {
                attrs: self.attrs,
                node: Node::Binary(Binary {
                    op,
                    lhs: Box::new(primary.into()),
                    rhs: Box::new(Ast {
                        attrs: self.attrs,
                        node,
                    }),
                }),
            },
        }
    }

    pub fn collect_additive_terms(self) -> Vec<Ast> {
        let mut result = Vec::new();

        match self.node {
            Node::Binary(binary) if matches!(binary.op, Operator::Add | Operator::Sub) => {
                result.extend(binary.lhs.collect_additive_terms());
                result.extend(binary.rhs.collect_additive_terms());
            }

            Node::Group(group) => result = group.collect_additive_terms(),

            node => {
                result.push(Ast {
                    attrs: self.attrs,
                    node,
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
