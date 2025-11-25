use std::fmt::{Display, Formatter};

pub mod parser;

#[cfg(test)]
mod tests;

pub use parser::Error;

#[derive(Debug, Copy, Clone)]
pub struct Attrs {
    pub position: usize,
}

impl Attrs {
    pub fn new(position: usize) -> Self {
        Self { position }
    }
}

#[derive(Debug, Clone)]
pub enum Ast<'a> {
    Number(u64),
    Var(Var<'a>),
    Binary(Binary<'a>),
    Group(Box<Ast<'a>>),
    Unary(Unary<'a>),
}

impl<'a> From<Primary<'a>> for Ast<'a> {
    fn from(value: Primary<'a>) -> Self {
        match value {
            Primary::Number(n) => Ast::Number(n),
            Primary::Var(v) => Ast::Var(v),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Binary<'a> {
    pub op: Operator,
    pub lhs: Box<Ast<'a>>,
    pub rhs: Box<Ast<'a>>,
}

#[derive(Debug, Copy, Clone)]
pub enum Primary<'a> {
    Var(Var<'a>),
    Number(u64),
}

#[derive(Debug, Clone)]
pub struct Unary<'a> {
    pub op: Operator,
    pub rhs: Box<Ast<'a>>,
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
pub struct Var<'a> {
    pub name: &'a str,
    pub exponent: u64,
}

impl<'a> Var<'a> {
    pub fn new(name: &'a str) -> Self {
        Self { name, exponent: 1 }
    }
}

impl<'a> Ast<'a> {
    pub fn pretty_print(&self) -> String {
        let mut result = String::new();
        self.pretty_print_internal(&mut result);

        result
    }

    fn pretty_print_internal(&self, result: &mut String) {
        fn pretty_print_ast(node: &Ast, result: &mut String) {
            match node {
                Ast::Number(n) => result.push_str(&n.to_string()),

                Ast::Var(v) => match v.exponent {
                    0 => result.push_str("1"),
                    1 => result.push_str(v.name),
                    _ => result.push_str(&format!("{}^{}", v.name, v.exponent)),
                },

                Ast::Binary(binary) => {
                    binary.lhs.pretty_print_internal(result);
                    result.push_str(" ");
                    result.push_str(&binary.op.to_string());
                    result.push_str(" ");
                    binary.rhs.pretty_print_internal(result);
                }

                Ast::Group(g) => {
                    result.push_str("(");
                    g.pretty_print_internal(result);
                    result.push_str(")");
                }

                Ast::Unary(unary) => {
                    result.push_str(&unary.op.to_string());
                    unary.rhs.pretty_print_internal(result);
                }
            }
        }

        pretty_print_ast(&self, result);
    }

    pub fn as_number(&self) -> u64 {
        match self {
            Ast::Number(n) => *n,
            _ => panic!("not a number"),
        }
    }

    pub fn as_var(&self) -> Var {
        match self {
            Ast::Var(v) => *v,
            _ => panic!("not a var"),
        }
    }

    pub fn as_binary(&self) -> &Binary {
        match self {
            Ast::Binary(b) => b,
            _ => panic!("not a binary"),
        }
    }

    pub fn as_unary(&self) -> &Unary {
        match self {
            Ast::Unary(u) => u,
            _ => panic!("not a unary"),
        }
    }

    pub fn as_group(&self) -> &Ast {
        match self {
            Ast::Group(g) => g,
            _ => panic!("not a group"),
        }
    }

    pub fn is_group(&self) -> bool {
        matches!(self, Ast::Group(_))
    }

    pub fn distribute(self, op: Operator, primary: Primary<'a>) -> Ast<'a> {
        match self {
            Ast::Binary(binary) => Ast::Binary(Binary {
                op: binary.op,
                lhs: Box::new(Ast::Binary(Binary {
                    op,
                    lhs: Box::new(primary.into()),
                    rhs: binary.lhs,
                })),
                rhs: binary.rhs,
            }),

            other => Ast::Binary(Binary {
                op,
                lhs: Box::new(primary.into()),
                rhs: Box::new(other),
            }),
        }
    }

    pub fn is_primary(&self) -> bool {
        matches!(self, Ast::Number(_) | Ast::Var(_))
    }

    pub fn primary_or_panic(self) -> Primary<'a> {
        match self {
            Ast::Number(n) => Primary::Number(n),
            Ast::Var(v) => Primary::Var(v),
            _ => panic!("not a primary"),
        }
    }
}
