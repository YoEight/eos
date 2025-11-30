use crate::lang::ast::{Ast, Binary, Operator, Unary, Var};
use crate::lang::lexical::lexer::Lexer;
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
    cache: Option<Token<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(expr: &'a str) -> Self {
        Self {
            lexer: Lexer::new(expr),
            cache: None,
        }
    }

    fn look_ahead<'b>(&'b mut self) -> crate::lang::Result<Token<'a>> {
        if let Some(token) = self.cache {
            return Ok(token);
        }

        let token = self.lexer.next_token()?;
        self.cache = Some(token);

        Ok(token)
    }

    fn shift<'b>(&'b mut self) -> crate::lang::Result<Token<'a>> {
        if let Some(token) = self.cache.take() {
            return Ok(token);
        }

        let token = self.lexer.next_token()?;

        Ok(token)
    }

    pub fn parse_top_level_ast(mut self) -> crate::lang::Result<Ast<'a>> {
        let ast = self.parse_ast()?;
        let token = self.shift()?;

        if token.sym != Sym::Eof {
            bail!(token.position, Error::UnexpectedEOF);
        }

        Ok(ast)
    }

    pub fn parse_ast<'b>(&'b mut self) -> crate::lang::Result<Ast<'a>> {
        let token = self.look_ahead()?;

        if token.sym == Sym::Eof {
            bail!(token.position, Error::UnexpectedEOF);
        }

        if matches!(token.sym, Sym::Number | Sym::Variable) || matches!(token.value, "-" | "+") {
            return self.parse_binary(0);
        }

        if token.value == "(" {
            return self.parse_group();
        }

        bail!(
            token.position,
            Error::UnexpectedSymbol(token.value.to_string())
        );
    }

    fn parse_binary<'b>(&'b mut self, min_bind: u64) -> crate::lang::Result<Ast<'a>> {
        let token = self.look_ahead()?;

        let mut lhs = if token.value == "(" {
            self.parse_group()?
        } else if matches!(token.value, "-" | "+") {
            self.shift()?;

            let mut operators = vec![into_operator(token.value)];

            loop {
                let token = self.look_ahead()?;

                if !matches!(token.value, "-" | "+") {
                    break;
                }

                self.shift()?;
                operators.push(into_operator(token.value));
            }

            let token = self.look_ahead()?;

            let mut expr = if matches!(token.sym, Sym::Number | Sym::Variable) {
                self.parse_primary()?
            } else if token.value == "(" {
                self.parse_group()?
            } else {
                bail!(
                    token.position,
                    Error::UnexpectedSymbol(token.value.to_string())
                );
            };

            for op in operators {
                expr = Ast::Unary(Unary {
                    op,
                    rhs: Box::new(expr),
                });
            }

            expr
        } else {
            self.parse_primary()?
        };

        loop {
            let token = self.look_ahead()?;

            if token.sym == Sym::Eof || token.value == ")" {
                return Ok(lhs);
            }

            if token.sym != Sym::Operator {
                bail!(
                    token.position,
                    Error::ExpectedSymbol(Sym::Operator, token.value.to_string()),
                );
            }

            let op = into_operator(token.value);
            let (lhs_bind, rhs_bind) = binding_pow(op);

            if lhs_bind < min_bind {
                break;
            }

            self.shift()?;
            let rhs = self.parse_binary(rhs_bind)?;

            lhs = Ast::Binary(Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            });
        }

        Ok(lhs)
    }

    fn parse_group(&mut self) -> crate::lang::Result<Ast<'a>> {
        {
            let open_token = self.shift()?;

            if open_token.value != "(" {
                bail!(
                    open_token.position,
                    Error::Expected("(".to_string(), open_token.value.to_string())
                );
            }
        }

        let ast = self.parse_ast()?;
        let token = self.shift()?;

        if token.value != ")" {
            bail!(
                token.position,
                Error::Expected(")".to_string(), token.value.to_string())
            );
        }

        Ok(Ast::Group(Box::new(ast)))
    }

    fn parse_primary<'b>(&'b mut self) -> crate::lang::Result<Ast<'a>> {
        let token = self.shift()?;

        if token.sym == Sym::Number {
            let value = token.value.parse::<u64>().expect("valid number format");
            let var = self.look_ahead()?;

            if var.sym == Sym::Variable {
                self.shift()?;
                let exp = self.look_ahead()?;

                if exp.sym == Sym::Number {
                    self.shift()?;

                    return Ok(Ast::Binary(Binary {
                        op: Operator::Mul,
                        lhs: Box::new(Ast::Number(value)),
                        rhs: Box::new(Ast::Var(Var {
                            name: var.value,
                            exponent: exp.value.parse::<u64>().expect("valid number format"),
                        })),
                    }));
                }

                return Ok(Ast::Binary(Binary {
                    op: Operator::Mul,
                    lhs: Box::new(Ast::Number(value)),
                    rhs: Box::new(Ast::Var(Var {
                        name: var.value,
                        exponent: 1,
                    })),
                }));
            }

            return Ok(Ast::Number(value));
        } else if token.sym == Sym::Variable {
            return Ok(Ast::Var(Var::new(token.value)));
        }

        bail!(
            token.position,
            Error::UnexpectedSymbol(token.value.to_string())
        );
    }
}

fn binding_pow(op: Operator) -> (u64, u64) {
    match op {
        Operator::Add | Operator::Sub => (10, 11),
        Operator::Mul | Operator::Div => (20, 21),
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
