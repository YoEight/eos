use crate::lang::ast::parser::Parser;

mod lang;
mod symbolic;

pub use lang::{Result, ast::Ast};
pub use symbolic::{normalization::normalize, simplify::simplify};

pub fn parse(expr: &str) -> Result<Ast<'_>> {
    let parser = Parser::new(expr);

    parser.parse_top_level_ast()
}

pub fn evaluate(expr: &str) -> Result<Ast<'_>> {
    let parser = Parser::new(expr);

    Ok(simplify(normalize(parser.parse_top_level_ast()?)))
}
