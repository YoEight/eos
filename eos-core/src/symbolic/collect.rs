use crate::Ast;
use crate::lang::Operator;

pub struct Signed<A> {
    pub positive: bool,
    pub inner: A,
}

#[derive(Default)]
pub struct CollectAdditives<'a> {
    inner: Vec<Signed<Ast<'a>>>,
}

impl<'a> CollectAdditives<'a> {
    pub fn into_inner(self) -> Vec<Signed<Ast<'a>>> {
        self.inner
    }

    pub fn collect(&mut self, ast: Ast<'a>) {
        self.collect_internal(true, ast);
    }

    fn collect_internal(&mut self, positive: bool, ast: Ast<'a>) {
        match ast {
            Ast::Binary(binary) => match binary.op {
                Operator::Add => {
                    self.collect_internal(positive, *binary.lhs);
                    self.collect_internal(positive, *binary.rhs);
                }

                Operator::Sub => {
                    self.collect_internal(positive, *binary.lhs);
                    self.collect_internal(false, *binary.rhs);
                }

                Operator::Eq => panic!("cannot collect additives over '=' operator"),

                _ => self.inner.push(Signed {
                    positive,
                    inner: Ast::Binary(binary),
                }),
            },

            Ast::Group(group) => self.collect_internal(positive, *group),

            Ast::Unary(unary) => self.collect_internal(unary.op != Operator::Sub, *unary.rhs),

            other => {
                self.inner.push(Signed {
                    positive,
                    inner: other,
                });
            }
        }
    }
}

#[derive(Default)]
pub struct CollectMultiplicatives<'a> {
    inner: Vec<Signed<Ast<'a>>>,
}
impl<'a> CollectMultiplicatives<'a> {
    pub fn collect(&mut self, ast: Ast<'a>) {
        match ast {
            Ast::Binary(binary) => match binary.op {
                Operator::Mul => {
                    self.collect(*binary.lhs);
                    self.collect(*binary.rhs);
                }

                Operator::Eq => panic!("cannot collect multiplicatives over '=' operator"),

                _ => self.inner.push(Signed {
                    positive: true,
                    inner: Ast::Binary(binary),
                }),
            },

            Ast::Unary(unary) => self.inner.push(Signed {
                positive: unary.op != Operator::Sub,
                inner: *unary.rhs,
            }),

            other => self.inner.push(Signed {
                positive: true,
                inner: other,
            }),
        }
    }

    pub fn into_inner(self) -> Vec<Signed<Ast<'a>>> {
        self.inner
    }
}
