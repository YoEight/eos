use crate::lang::{Ast, Binary, Operator, Unary};
use crate::symbolic::collect::CollectAdditives;

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
        distribute_mul_over_additive(lhs, rhs)
    } else if rhs.is_primary() && lhs.is_group() {
        distribute_mul_over_additive(rhs, lhs)
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

fn distribute_mul_over_additive<'a>(primary: Ast<'a>, target: Ast<'a>) -> Ast<'a> {
    let mut additive_collector = CollectAdditives::default();
    additive_collector.collect(&target);
    let mut agg: Ast<'a> = primary.clone();

    for (idx, additive) in additive_collector.into_inner().into_iter().enumerate() {
        agg = if idx == 0 {
            if additive.is_add {
                additive
                    .inner
                    .clone()
                    .distribute(Operator::Mul, primary.clone())
            } else {
                Ast::Binary(Binary {
                    op: Operator::Mul,
                    lhs: Box::new(Ast::Unary(Unary {
                        op: Operator::Sub,
                        rhs: Box::new(Ast::Number(1)),
                    })),
                    rhs: Box::new(
                        additive
                            .inner
                            .clone()
                            .distribute(Operator::Mul, primary.clone()),
                    ),
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
                rhs: Box::new(
                    additive
                        .inner
                        .clone()
                        .distribute(Operator::Mul, primary.clone()),
                ),
            })
        };
    }

    agg
}
