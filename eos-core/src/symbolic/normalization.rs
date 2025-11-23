use crate::lang::{Ast, Binary, Node, Operator, Position, Primary, Unary};

pub fn normalize(mut ast: Ast) -> Ast {
    match ast.node {
        Node::Number(_) | Node::Var(_) => ast,

        Node::Binary(binary) => {
            ast.node = normalize_binary(ast.attrs.position, binary);
            ast
        }

        Node::Group(group) => {
            ast.node = Node::Group(normalize_group(ast.attrs.position, group));
            ast
        }

        Node::Unary(unary) => {
            ast.node = Node::Unary(normalize_unary(ast.attrs.position, unary));
            ast
        }
    }
}

pub fn normalize_binary(position: Position, mut binary: Binary) -> Node {
    // TODO - shouldn't have to allocate another box here
    binary.lhs = Box::new(normalize(*binary.lhs));
    binary.rhs = Box::new(normalize(*binary.rhs));

    if binary.op != Operator::Mul {
        return Node::Binary(binary);
    }

    if let Some(lhs) = binary.lhs.as_primary()
        && binary.rhs.is_group()
    {
        distribute_mul_over_additive(lhs, *binary.rhs)
    } else if let Some(rhs) = binary.rhs.as_primary()
        && binary.lhs.is_group()
    {
        distribute_mul_over_additive(rhs, *binary.lhs)
    } else {
        Node::Binary(binary)
    }
}

fn normalize_group(position: Position, group: Box<Ast>) -> Box<Ast> {
    Box::new(normalize(*group))
}

fn normalize_unary(position: Position, unary: Unary) -> Unary {
    Unary {
        op: unary.op,
        rhs: Box::new(normalize(*unary.rhs)),
    }
}

fn distribute_mul_over_additive(primary: Primary, target: Ast) -> Node {
    let attrs = target.attrs;

    struct Additive<A> {
        is_add: bool,
        inner: A,
    }

    fn collect(nodes: &mut Vec<Additive<Node>>, is_add: bool, node: Node) {
        match node {
            Node::Binary(binary) => match binary.op {
                Operator::Add => {
                    collect(nodes, is_add, binary.lhs.node);
                    collect(nodes, is_add, binary.rhs.node);
                }

                Operator::Sub => {
                    collect(nodes, is_add, binary.lhs.node);
                    collect(nodes, false, binary.rhs.node);
                }

                Operator::Eq => panic!("cannot distribute over '=' operator"),

                _ => nodes.push(Additive {
                    is_add,
                    inner: Node::Binary(binary),
                }),
            },

            Node::Group(group) => collect(nodes, is_add, group.node),

            Node::Unary(unary) => collect(nodes, unary.op != Operator::Sub, unary.rhs.node),

            node => nodes.push(Additive {
                is_add,
                inner: node,
            }),
        }
    }

    let mut additives = Vec::new();
    collect(&mut additives, true, target.node);

    let mut agg: Node = primary.into();

    for (idx, additive) in additives.into_iter().enumerate() {
        agg = if idx == 0 {
            if additive.is_add {
                Ast::new(attrs, additive.inner)
                    .distribute(Operator::Mul, primary)
                    .node
            } else {
                Node::Binary(Binary {
                    op: Operator::Mul,
                    lhs: Box::new(Ast {
                        attrs,
                        node: Node::Unary(Unary {
                            op: Operator::Sub,
                            rhs: Box::new(Ast::number(attrs, 1)),
                        }),
                    }),
                    rhs: Box::new(
                        Ast::new(attrs, additive.inner).distribute(Operator::Mul, primary),
                    ),
                })
            }
        } else {
            Node::Binary(Binary {
                op: if additive.is_add {
                    Operator::Add
                } else {
                    Operator::Sub
                },
                lhs: Box::new(Ast::new(attrs, agg)),
                rhs: Box::new(Ast::new(attrs, additive.inner).distribute(Operator::Mul, primary)),
            })
        };
    }

    agg
}
