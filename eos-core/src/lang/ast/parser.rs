use crate::lang::ast::{Ast, Attrs, Binary, Node, Operator, Unary, Var};
use crate::lang::lexical::lexer::Lexer;
use crate::lang::nursery::Nursery;
use crate::lang::token::{Sym, Token};
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum Error {
    UnexpectedEOF,
    ExpectedEOF,
    UnexpectedSymbol(String),
    Expected(String, String),
    ExpectedSymbol(Sym, String),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedEOF => write!(f, "unexpected end of file"),
            Self::ExpectedEOF => write!(f, "expected end of file"),
            Self::UnexpectedSymbol(sym) => write!(f, "unexpected symbol '{sym}'"),
            Self::Expected(expected, got) => write!(f, "expected '{expected}' but got '{got}'"),
            Self::ExpectedSymbol(expected, got) => {
                write!(f, "expected a '{expected}' but got '{got}'")
            }
        }
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    token: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(expr: &'a str) -> Self {
        Self {
            lexer: Lexer::new(expr),
            token: None,
        }
    }

    fn look_ahead(&mut self, nursery: &mut Nursery) -> crate::lang::Result<Token> {
        if let Some(token) = self.token {
            return Ok(token);
        }

        let token = self.shift(nursery)?;
        self.token = Some(token);

        Ok(token)
    }

    fn shift(&mut self, nursery: &mut Nursery) -> crate::lang::Result<Token> {
        if let Some(token) = self.token.take() {
            return Ok(token);
        }

        self.lexer.next_token(nursery)
    }

    pub fn parse_top_level_ast(&mut self, nursery: &mut Nursery) -> crate::lang::Result<Ast> {
        let ast = self.parse_ast(nursery)?;
        let token = self.shift(nursery)?;

        if token.sym != Sym::EOF {
            bail!(token.position, Error::UnexpectedEOF);
        }

        Ok(ast)
    }

    pub fn parse_ast(&mut self, nursery: &mut Nursery) -> crate::lang::Result<Ast> {
        let token = self.look_ahead(nursery)?;

        if token.sym == Sym::EOF {
            bail!(token.position, Error::UnexpectedEOF);
        }

        if token.sym == Sym::Number || token.sym == Sym::Variable {
            return self.parse_binary(nursery, 0);
        }

        if token.sym == Sym::Symbol && nursery.get_string_or_panic(token.position) == "(" {
            return self.parse_group(nursery);
        }

        if token.sym == Sym::Operator && nursery.get_string_or_panic(token.position) == "-" {
            return self.parse_unary(nursery);
        }

        let tok_str = nursery.get_string_or_panic(token.position);
        bail!(token.position, Error::UnexpectedSymbol(tok_str.to_string()));
    }

    fn parse_binary(&mut self, nursery: &mut Nursery, min_bind: u64) -> crate::lang::Result<Ast> {
        let token = self.look_ahead(nursery)?;

        let mut lhs = if nursery.get_string_or_panic(token.position) == "(" {
            self.parse_group(nursery)?
        } else {
            self.parse_primary(nursery)?
        };

        loop {
            let token = self.look_ahead(nursery)?;

            if token.sym == Sym::EOF {
                return Ok(lhs);
            }

            let token_str = nursery.get_string_or_panic(token.position);

            if token.sym != Sym::Operator {
                bail!(
                    token.position,
                    Error::ExpectedSymbol(Sym::Operator, token_str.to_string()),
                );
            }

            let op = into_operator(token_str);
            let (lhs_bind, rhs_bind) = binding_pow(op);

            if lhs_bind < min_bind {
                break;
            }

            self.shift(nursery)?;
            let rhs = self.parse_binary(nursery, rhs_bind)?;

            lhs = Ast {
                attrs: Attrs::new(lhs.attrs.position),
                node: Node::Binary(Binary {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }),
            };
        }

        Ok(lhs)
    }

    fn parse_group(&mut self, nursery: &mut Nursery) -> crate::lang::Result<Ast> {
        let open_token = self.shift(nursery)?;
        let token_str = nursery.get_string_or_panic(open_token.position);

        if token_str != "(" {
            bail!(
                open_token.position,
                Error::Expected("(".to_string(), token_str.to_string())
            );
        }

        let ast = self.parse_ast(nursery)?;
        let token = self.shift(nursery)?;
        let token_str = nursery.get_string_or_panic(token.position);

        if token_str != ")" {
            bail!(
                token.position,
                Error::Expected(")".to_string(), token_str.to_string())
            );
        }

        Ok(Ast {
            attrs: Attrs::new(open_token.position),
            node: Node::Group(Box::new(ast)),
        })
    }

    fn parse_unary(&mut self, nursery: &mut Nursery) -> crate::lang::Result<Ast> {
        let token = self.shift(nursery)?;
        let token_str = nursery.get_string_or_panic(token.position);

        if token_str != "-" {
            bail!(
                token.position,
                Error::Expected("-".to_string(), token_str.to_string())
            );
        }

        let ast = self.parse_ast(nursery)?;

        Ok(Ast {
            attrs: Attrs::new(token.position),
            node: Node::Unary(Unary {
                op: Operator::Sub,
                rhs: Box::new(ast),
            }),
        })
    }

    fn parse_primary(&mut self, nursery: &mut Nursery) -> crate::lang::Result<Ast> {
        let token = self.shift(nursery)?;

        if token.sym == Sym::Number {
            let value = nursery
                .get_string_or_panic(token.position)
                .parse::<u64>()
                .expect("valid number format");

            return Ok(Ast {
                attrs: Attrs::new(token.position),
                node: Node::Number(value),
            });
        } else if token.sym == Sym::Variable {
            let id = nursery.get_unique_id_or_panic(token.position);

            return Ok(Ast {
                attrs: Attrs::new(token.position),
                node: Node::Var(Var::new(id)),
            });
        }

        bail!(
            token.position,
            Error::UnexpectedSymbol(nursery.get_string_or_panic(token.position).to_string())
        );
    }
}

fn binding_pow(op: Operator) -> (u64, u64) {
    match op {
        Operator::Add | Operator::Sub => (10, 10),
        Operator::Mul | Operator::Div => (20, 20),
        Operator::Exp => (30, 29),
        Operator::Eq => (1, 1),
    }
}

fn into_operator(op: &str) -> Operator {
    match op {
        "+" => Operator::Add,
        "-" => Operator::Sub,
        "*" => Operator::Mul,
        "/" => Operator::Div,
        "^" => Operator::Exp,
        "=" => Operator::Eq,
        _ => unreachable!(),
    }
}
