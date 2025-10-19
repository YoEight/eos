use crate::lang::ast::{Ast, Attrs, Node};
use crate::lang::lexical::lexer::Lexer;
use crate::lang::nursery::Nursery;
use crate::lang::token::{Sym, Token};
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum Error {
    UnexpectedEOF,
    UnexpectedSymbol(String),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedEOF => write!(f, "unexpected end of file"),
            Self::UnexpectedSymbol(sym) => write!(f, "unexpected symbol '{sym}'"),
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
        self.token.take();
        self.lexer.next_token(nursery)
    }

    pub fn parse_ast(&mut self, nursery: &mut Nursery) -> crate::lang::Result<Ast> {
        let token = self.look_ahead(nursery)?;

        if token.sym == Sym::EOF {
            bail!(token.position, Error::UnexpectedEOF);
        }

        if token.sym == Sym::Number || token.sym == Sym::Symbol {
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
            todo!()
        };

        todo!()
    }

    fn parse_group(&mut self, nursery: &mut Nursery) -> crate::lang::Result<Ast> {
        todo!()
    }

    fn parse_unary(&mut self, nursary: &mut Nursery) -> crate::lang::Result<Ast> {
        todo!()
    }

    fn parse_primary(&mut self, nursery: &mut Nursery) -> crate::lang::Result<Ast> {
        let token = self.shift(nursery)?;

        if token.sym == Sym::Number {
            let value = nursery
                .get_string_or_panic(token.position)
                .parse::<u64>()
                .expect("valid number format");
            return Ok(Ast {
                attrs: Attrs {
                    position: token.position,
                },
                node: Node::Number(value),
            });
        } else if token.sym == Sym::Variable {
            todo!()
        }
    }
}
