use crate::Ast;
use crate::lang::{Binary, Operator, Unary};
use crate::symbolic::collect::{CollectAdditives, CollectMultiplicatives};

pub fn simplify(ast: Ast) -> Ast {
    match ast {
        Ast::Binary(b) => simplify_binary(b),
        Ast::Group(g) => simplify(*g),
        Ast::Unary(u) => simplify_unary(u),
        other => other,
    }
}

fn simplify_binary(mut binary: Binary) -> Ast {
    let lhs = simplify(*binary.lhs);
    let rhs = simplify(*binary.rhs);

    binary = Binary {
        op: binary.op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    };

    if matches!(binary.op, Operator::Add | Operator::Sub) {
        let mut collector = CollectAdditives::default();
        collector.collect(Ast::Binary(binary));

        let mut agg = 0i64;
        let mut other_terms = None;

        for additive in collector.into_inner() {
            match additive.inner {
                Ast::Number(x) => {
                    if additive.positive {
                        agg += x as i64;
                    } else {
                        agg -= x as i64;
                    }
                }

                other => {
                    if let Some(previous) = other_terms.take() {
                        let op = if additive.positive {
                            Operator::Add
                        } else {
                            Operator::Sub
                        };

                        other_terms = Some(Ast::Binary(Binary {
                            op,
                            lhs: Box::new(previous),
                            rhs: Box::new(other),
                        }));
                    } else {
                        other_terms = Some(other);
                    }
                }
            }
        }

        if let Some(other_terms) = other_terms {
            if agg == 0 {
                other_terms
            } else if agg > 0 {
                Ast::Binary(Binary {
                    op: Operator::Add,
                    rhs: Box::new(other_terms),
                    lhs: Box::new(Ast::Number(agg as u64)),
                })
            } else {
                Ast::Binary(Binary {
                    op: Operator::Sub,
                    rhs: Box::new(other_terms),
                    lhs: Box::new(Ast::Number(agg.unsigned_abs())),
                })
            }
        } else if agg >= 0 {
            Ast::Number(agg as u64)
        } else {
            Ast::Unary(Unary {
                op: Operator::Sub,
                rhs: Box::new(Ast::Number(agg.unsigned_abs())),
            })
        }
    } else if binary.op == Operator::Mul {
        let mut collector = CollectMultiplicatives::default();
        collector.collect(Ast::Binary(binary));

        let mut agg = 1i64;
        let mut other_terms = None;

        for term in collector.into_inner() {
            if let Some(x) = term.inner.as_number_opt() {
                if term.positive && x == 1 {
                    continue;
                }

                if x == 0 {
                    return Ast::Number(0);
                }

                agg *= x as i64;

                if !term.positive {
                    agg *= -1;
                }

                continue;
            }

            if !term.positive {
                agg *= -1;
            }

            if let Some(other) = other_terms.take() {
                other_terms = Some(Ast::Binary(Binary {
                    op: Operator::Mul,
                    lhs: Box::new(other),
                    rhs: Box::new(term.inner),
                }));

                continue;
            }

            other_terms = Some(term.inner);
        }

        if let Some(other_terms) = other_terms {
            if agg == 1 {
                other_terms
            } else if agg == -1 {
                Ast::Unary(Unary {
                    op: Operator::Sub,
                    rhs: Box::new(other_terms),
                })
            } else if agg < 0 {
                Ast::Binary(Binary {
                    op: Operator::Mul,
                    lhs: Box::new(Ast::Unary(Unary {
                        op: Operator::Sub,
                        rhs: Box::new(Ast::Number(agg.unsigned_abs())),
                    })),
                    rhs: Box::new(other_terms),
                })
            } else {
                Ast::Binary(Binary {
                    op: Operator::Mul,
                    lhs: Box::new(Ast::Number(agg as u64)),
                    rhs: Box::new(other_terms),
                })
            }
        } else if agg < 0 {
            Ast::Unary(Unary {
                op: Operator::Sub,
                rhs: Box::new(Ast::Number(agg.unsigned_abs())),
            })
        } else {
            Ast::Number(agg as u64)
        }
    } else {
        Ast::Binary(binary)
    }
}

fn simplify_unary(mut unary: Unary) -> Ast {
    let mut rhs = simplify(*unary.rhs);
    let mut is_positive = true;

    while let Ast::Unary(u) = rhs {
        if u.op == Operator::Sub {
            is_positive = !is_positive;
        }

        rhs = *u.rhs;
    }

    if unary.op == Operator::Sub && is_positive || unary.op == Operator::Add && !is_positive {
        unary.op = Operator::Sub;
        unary.rhs = Box::new(rhs);

        return Ast::Unary(unary);
    }

    rhs
}
