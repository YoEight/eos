use crate::lang::ast::parser::Parser;
use crate::lang::nursery::Nursery;

#[test]
fn test_parser_sum() -> crate::lang::Result<()> {
    let mut nursery = Nursery::default();
    let mut parser = Parser::new("1 + 2 ^ 3 + 4");

    let ast = parser.parse_ast(&mut nursery)?;

    Ok(())
}
