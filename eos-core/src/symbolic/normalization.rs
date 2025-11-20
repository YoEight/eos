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

    if let Some(lhs) = binary.lhs.as_primary() {
        distribute_mul_over_additive(lhs, *binary.rhs)
    } else if let Some(rhs) = binary.rhs.as_primary() {
        distribute_mul_over_additive(rhs, *binary.lhs)
    } else {
        Node::Binary(binary)
    }
}

fn normalize_group(position: Position, group: Box<Ast>) -> Box<Ast> {
    group
}

fn normalize_unary(position: Position, unary: Unary) -> Unary {
    todo!()
}

fn distribute_mul_over_additive(primary: Primary, target: Ast) -> Node {
    todo!()
}
