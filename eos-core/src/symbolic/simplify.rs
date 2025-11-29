use crate::lang::ast::Var;
use crate::lang::{Binary, Operator, Unary};
use crate::symbolic::collect::{CollectAdditives, CollectMultiplicatives};
use crate::Ast;
use std::collections::BTreeMap;

pub fn simplify(ast: Ast) -> Ast {
    match ast {
        Ast::Binary(b) => simplify_binary(b),
        Ast::Group(g) => simplify(*g),
        Ast::Unary(u) => simplify_unary(u),
        other => other,
    }
}

fn simplify_binary(binary: Binary) -> Ast {
    let lhs = simplify(*binary.lhs);
    let rhs = simplify(*binary.rhs);

    if matches!(binary.op, Operator::Add | Operator::Sub) {
        let (lhs, rhs) = match (lhs, rhs) {
            (Ast::Number(0), other) => {
                return if binary.op == Operator::Add {
                    other
                } else {
                    Ast::Unary(Unary {
                        op: Operator::Sub,
                        rhs: Box::new(other),
                    })
                };
            }

            (other, Ast::Number(0)) => return other,

            (lsh, rhs) => (lsh, rhs),
        };

        let mut collector = CollectAdditives::default();
        collector.collect(Ast::Binary(Binary {
            op: binary.op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }));

        let mut agg = 0i64;
        let mut other_terms = None;
        let mut terms = BTreeMap::<Var, i64>::new();

        for additive in collector.into_inner() {
            match additive.inner {
                Ast::Number(x) => {
                    if additive.positive {
                        agg += x as i64;
                    } else {
                        agg -= x as i64;
                    }
                }

                Ast::Var(v) => {
                    let value = terms.entry(v).or_default();

                    if additive.positive {
                        *value += 1;
                    } else {
                        *value -= 1;
                    }
                }

                Ast::Binary(binary) => {
                    if binary.op == Operator::Mul {
                        let scalar: i64 = if additive.positive { 1 } else { -1 };

                        match (*binary.lhs, *binary.rhs) {
                            (Ast::Number(n), Ast::Var(x)) | (Ast::Var(x), Ast::Number(n)) => {
                                let value = terms.entry(x).or_default();
                                *value += (n as i64) * scalar;
                            }

                            (Ast::Unary(unary), Ast::Var(x)) | (Ast::Var(x), Ast::Unary(unary)) => {
                                match *unary.rhs {
                                    Ast::Number(n) => {
                                        let scalar: i64 =
                                            if unary.op == Operator::Add { 1 } else { -1 };

                                        let value = terms.entry(x).or_default();
                                        *value += (n as i64) * scalar;
                                    }

                                    other => {
                                        let op = if additive.positive {
                                            Operator::Add
                                        } else {
                                            Operator::Sub
                                        };

                                        if let Some(previous) = other_terms.take() {
                                            if op == Operator::Sub {
                                                other_terms = Some(Ast::Binary(Binary {
                                                    op: Operator::Add,
                                                    lhs: Box::new(previous),
                                                    rhs: Box::new(Ast::Binary(Binary {
                                                        op,
                                                        lhs: Box::new(Ast::Var(x)),
                                                        rhs: Box::new(other),
                                                    })),
                                                }))
                                            } else {
                                                other_terms = Some(Ast::Binary(Binary {
                                                    op: Operator::Add,
                                                    lhs: Box::new(previous),
                                                    rhs: Box::new(Ast::Binary(Binary {
                                                        op,
                                                        lhs: Box::new(other),
                                                        rhs: Box::new(Ast::Var(x)),
                                                    })),
                                                }))
                                            }
                                        } else {
                                            other_terms = if op == Operator::Sub {
                                                Some(Ast::Unary(Unary {
                                                    op: unary.op,
                                                    rhs: Box::new(other),
                                                }))
                                            } else {
                                                Some(other)
                                            }
                                        }
                                    }
                                }
                            }

                            (lhs, rhs) => {
                                if let Some(previous) = other_terms.take() {
                                    let op = if additive.positive {
                                        Operator::Add
                                    } else {
                                        Operator::Sub
                                    };

                                    other_terms = Some(Ast::Binary(Binary {
                                        op,
                                        lhs: Box::new(previous),
                                        rhs: Box::new(Ast::Binary(Binary {
                                            op: binary.op,
                                            lhs: Box::new(lhs),
                                            rhs: Box::new(rhs),
                                        })),
                                    }));
                                } else {
                                    other_terms = Some(Ast::Binary(Binary {
                                        op: binary.op,
                                        lhs: Box::new(lhs),
                                        rhs: Box::new(rhs),
                                    }));
                                }
                            }
                        }

                        continue;
                    }

                    if let Some(previous) = other_terms.take() {
                        let op = if additive.positive {
                            Operator::Add
                        } else {
                            Operator::Sub
                        };

                        other_terms = Some(Ast::Binary(Binary {
                            op,
                            lhs: Box::new(previous),
                            rhs: Box::new(Ast::Binary(binary)),
                        }));
                    } else {
                        other_terms = Some(Ast::Binary(binary));
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

        let mut agg_vars = None;
        for (v, value) in terms.into_iter().rev() {
            if value == 0 {
                continue;
            }

            let var = if value == 1 {
                Ast::Var(v)
            } else if value == -1 {
                Ast::Unary(Unary {
                    op: Operator::Sub,
                    rhs: Box::new(Ast::Var(v)),
                })
            } else if value < 0 {
                Ast::Binary(Binary {
                    op: Operator::Mul,
                    lhs: Box::new(Ast::Unary(Unary {
                        op: Operator::Sub,
                        rhs: Box::new(Ast::Number(value.unsigned_abs())),
                    })),
                    rhs: Box::new(Ast::Var(v)),
                })
            } else {
                Ast::Binary(Binary {
                    op: Operator::Mul,
                    lhs: Box::new(Ast::Number(value as u64)),
                    rhs: Box::new(Ast::Var(v)),
                })
            };

            if let Some(vars) = agg_vars.take() {
                agg_vars = Some(Ast::Binary(Binary {
                    op: Operator::Add,
                    lhs: Box::new(var),
                    rhs: Box::new(vars),
                }));

                continue;
            }

            agg_vars = Some(var);
        }

        let scalar = if let Some(other_terms) = other_terms {
            if agg == 0 {
                other_terms
            } else if agg > 0 {
                Ast::Binary(Binary {
                    op: Operator::Add,
                    lhs: Box::new(other_terms),
                    rhs: Box::new(Ast::Number(agg as u64)),
                })
            } else {
                Ast::Binary(Binary {
                    op: Operator::Sub,
                    lhs: Box::new(other_terms),
                    rhs: Box::new(Ast::Number(agg.unsigned_abs())),
                })
            }
        } else if agg >= 0 {
            Ast::Number(agg as u64)
        } else {
            Ast::Unary(Unary {
                op: Operator::Sub,
                rhs: Box::new(Ast::Number(agg.unsigned_abs())),
            })
        };

        if let Some(agg_vars) = agg_vars {
            match scalar {
                Ast::Number(0) => agg_vars,
                other => Ast::Binary(Binary {
                    op: Operator::Add,
                    lhs: Box::new(agg_vars),
                    rhs: Box::new(other),
                }),
            }
        } else {
            scalar
        }
    } else if binary.op == Operator::Mul {
        let mut collector = CollectMultiplicatives::default();
        collector.collect(Ast::Binary(Binary {
            op: binary.op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }));

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
    } else if binary.op == Operator::Exp {
        match (lhs, rhs) {
            (Ast::Number(x), Ast::Number(y)) => Ast::Number(x ^ y),

            (Ast::Var(x), Ast::Number(y)) => {
                if x.exponent == 1 {
                    Ast::Var(Var {
                        name: x.name,
                        exponent: y,
                    })
                } else {
                    Ast::Var(Var {
                        name: x.name,
                        exponent: x.exponent + y,
                    })
                }
            }

            (Ast::Unary(unary), Ast::Number(y)) => match *unary.rhs {
                Ast::Number(x) => {
                    if matches!(unary.op, Operator::Add) {
                        Ast::Number(x ^ y)
                    } else {
                        Ast::Unary(Unary {
                            op: Operator::Sub,
                            rhs: Box::new(Ast::Number(x ^ y)),
                        })
                    }
                }

                Ast::Var(mut var) => {
                    if var.exponent == 1 {
                        var.exponent = y;
                    } else {
                        var.exponent += y;
                    }

                    if unary.op == Operator::Add {
                        Ast::Var(var)
                    } else {
                        Ast::Unary(Unary {
                            op: Operator::Sub,
                            rhs: Box::new(Ast::Var(var)),
                        })
                    }
                }

                other => Ast::Binary(Binary {
                    op: Operator::Exp,
                    lhs: Box::new(Ast::Unary(Unary {
                        op: unary.op,
                        rhs: Box::new(other),
                    })),
                    rhs: Box::new(Ast::Number(y)),
                }),
            },

            (lhs, rhs) => Ast::Binary(Binary {
                op: binary.op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }),
        }
    } else {
        Ast::Binary(Binary {
            op: binary.op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
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
