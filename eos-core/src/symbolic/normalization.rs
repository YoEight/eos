use crate::lang::{Ast, Binary, Operator, Primary, Unary};

pub fn normalize(ast: Ast) -> Ast {
    match ast {
        Ast::Binary(binary) => normalize_binary(binary.op, *binary.lhs, *binary.rhs),
        Ast::Group(group) => Ast::Group(normalize_group(group)),
        Ast::Unary(unary) => Ast::Unary(normalize_unary(unary)),
        other => other,
    }
}

pub fn normalize_binary<'a>(op: Operator, mut lhs: Ast<'a>, mut rhs: Ast<'a>) -> Ast<'a> {
    lhs = normalize(lhs);
    rhs = normalize(rhs);

    if op != Operator::Mul {
        return Ast::Binary(Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        });
    }

    if lhs.is_primary() && rhs.is_group() {
        distribute_mul_over_additive(lhs.primary_or_panic(), rhs)
    } else if rhs.is_primary() && lhs.is_group() {
        distribute_mul_over_additive(rhs.primary_or_panic(), lhs)
    } else {
        Ast::Binary(Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
    }
}

fn normalize_group(group: Box<Ast>) -> Box<Ast> {
    Box::new(normalize(*group))
}

fn normalize_unary(unary: Unary) -> Unary {
    Unary {
        op: unary.op,
        rhs: Box::new(normalize(*unary.rhs)),
    }
}

fn distribute_mul_over_additive<'a>(primary: Primary<'a>, target: Ast<'a>) -> Ast<'a> {
    struct Additive<A> {
        is_add: bool,
        inner: A,
    }

    fn collect<'a, 'b>(
        mut nodes: Vec<Additive<&'a Ast<'b>>>,
        is_add: bool,
        ast: &'a Ast<'b>,
    ) -> Vec<Additive<&'a Ast<'b>>> {
        match &ast {
            Ast::Binary(binary) => match binary.op {
                Operator::Add => {
                    nodes = collect(nodes, is_add, &binary.lhs);
                    collect(nodes, is_add, &binary.rhs)
                }

                Operator::Sub => {
                    nodes = collect(nodes, is_add, &binary.lhs);
                    collect(nodes, false, &binary.rhs)
                }

                Operator::Eq => panic!("cannot distribute over '=' operator"),

                _ => {
                    nodes.push(Additive { is_add, inner: ast });
                    nodes
                }
            },

            Ast::Group(group) => collect(nodes, is_add, &group),

            Ast::Unary(unary) => collect(nodes, unary.op != Operator::Sub, &unary.rhs),

            other => {
                nodes.push(Additive {
                    is_add,
                    inner: other,
                });

                nodes
            }
        }
    }

    let additives = collect(Vec::new(), true, &target);
    let mut agg: Ast<'a> = primary.into();

    for (idx, additive) in additives.into_iter().enumerate() {
        agg = if idx == 0 {
            if additive.is_add {
                additive.inner.clone().distribute(Operator::Mul, primary)
            } else {
                Ast::Binary(Binary {
                    op: Operator::Mul,
                    lhs: Box::new(Ast::Unary(Unary {
                        op: Operator::Sub,
                        rhs: Box::new(Ast::Number(1)),
                    })),
                    rhs: Box::new(additive.inner.clone().distribute(Operator::Mul, primary)),
                })
            }
        } else {
            Ast::Binary(Binary {
                op: if additive.is_add {
                    Operator::Add
                } else {
                    Operator::Sub
                },
                lhs: Box::new(agg),
                rhs: Box::new(additive.inner.clone().distribute(Operator::Mul, primary)),
            })
        };
    }

    agg
}
