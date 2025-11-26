use crate::lang::Operator;
use crate::Ast;

pub struct Additive<A> {
    pub is_add: bool,
    pub inner: A,
}

#[derive(Default)]
pub struct CollectAdditives<'a, 'b> {
    inner: Vec<Additive<&'a Ast<'b>>>,
}

impl<'a, 'b> CollectAdditives<'a, 'b> {
    pub fn into_inner(self) -> Vec<Additive<&'a Ast<'b>>> {
        self.inner
    }

    pub fn collect(&mut self, ast: &'a Ast<'b>) {
        self.collect_internal(true, ast);
    }

    fn collect_internal(&mut self, is_add: bool, ast: &'a Ast<'b>) {
        match ast {
            Ast::Binary(binary) => match binary.op {
                Operator::Add => {
                    self.collect_internal(is_add, &binary.lhs);
                    self.collect_internal(is_add, &binary.rhs);
                }

                Operator::Sub => {
                    self.collect_internal(is_add, &binary.lhs);
                    self.collect_internal(false, &binary.rhs);
                }

                Operator::Eq => panic!("cannot collect additives over '=' operator"),

                _ => self.inner.push(Additive { is_add, inner: ast }),
            },

            Ast::Group(group) => self.collect_internal(is_add, &group),

            Ast::Unary(unary) => self.collect_internal(unary.op != Operator::Sub, &unary.rhs),

            other => {
                self.inner.push(Additive {
                    is_add,
                    inner: other,
                });
            }
        }
    }
}
