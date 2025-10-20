use crate::lang::{Ast, Binary, Node, Position, Unary};

pub fn normalize(ast: &mut Ast) {
    match &mut ast.node {
        Node::Number(_) | Node::Var(_) => {}

        Node::Binary(binary) => {
            normalize_binary(ast.attrs.position, binary);
        }

        Node::Group(group) => {
            normalize_group(ast.attrs.position, group);
        }

        Node::Unary(unary) => {
            normalize_unary(ast.attrs.position, unary);
        }
    }
}

pub fn normalize_binary(position: Position, binary: &mut Binary) {}

pub fn normalize_group(position: Position, group: &mut Ast) {}

pub fn normalize_unary(position: Position, unary: &mut Unary) {}
