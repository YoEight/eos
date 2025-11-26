use crate::Ast;
use crate::lang::Operator;

pub struct Additive<A> {
    pub is_add: bool,
    pub inner: A,
}

#[derive(Default)]
pub struct CollectAdditives<'a> {
    inner: Vec<Additive<Ast<'a>>>,
}

impl<'a> CollectAdditives<'a> {
    pub fn into_inner(self) -> Vec<Additive<Ast<'a>>> {
        self.inner
    }

    pub fn collect(&mut self, ast: Ast<'a>) {
        self.collect_internal(true, ast);
    }

    fn collect_internal(&mut self, is_add: bool, ast: Ast<'a>) {
        match ast {
            Ast::Binary(binary) => match binary.op {
                Operator::Add => {
                    self.collect_internal(is_add, *binary.lhs);
                    self.collect_internal(is_add, *binary.rhs);
                }

                Operator::Sub => {
                    self.collect_internal(is_add, *binary.lhs);
                    self.collect_internal(false, *binary.rhs);
                }

                Operator::Eq => panic!("cannot collect additives over '=' operator"),

                _ => self.inner.push(Additive {
                    is_add,
                    inner: Ast::Binary(binary),
                }),
            },

            Ast::Group(group) => self.collect_internal(is_add, *group),

            Ast::Unary(unary) => self.collect_internal(unary.op != Operator::Sub, *unary.rhs),

            other => {
                self.inner.push(Additive {
                    is_add,
                    inner: other,
                });
            }
        }
    }
}
