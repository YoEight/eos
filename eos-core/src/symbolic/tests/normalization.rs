use crate::lang::ast::parser::Parser;
use crate::lang::nursery::Nursery;
use crate::symbolic::normalization::normalize;

#[test]
fn test_normalize_mul_distribution_case_1() -> crate::lang::Result<()> {
    let mut nursery = Nursery::default();
    let mut parser = Parser::new("2 * (1 + 2 + 3)");

    let ast = parser.parse_top_level_ast(&mut nursery)?;
    let ast = normalize(ast);
    let printed = ast.pretty_print(&nursery);

    assert_eq!(printed, "2 * 1 + 2 * 2 + 2 * 3");

    Ok(())
}

#[test]
fn test_normalize_mul_distribution_case_2() -> crate::lang::Result<()> {
    let mut nursery = Nursery::default();
    let mut parser = Parser::new("2 * (1 - 2 + 3)");

    let ast = parser.parse_top_level_ast(&mut nursery)?;
    let ast = normalize(ast);
    let printed = ast.pretty_print(&nursery);

    assert_eq!(printed, "2 * 1 - 2 * 2 + 2 * 3");

    Ok(())
}
