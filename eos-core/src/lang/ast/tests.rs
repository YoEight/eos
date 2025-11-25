use crate::lang::ast::parser::Parser;
use crate::lang::ast::Operator;

#[test]
fn test_parser_sum() -> crate::lang::Result<()> {
    let mut parser = Parser::new("1 + 2 ^ 3 + 4");

    let ast = parser.parse_top_level_ast()?;
    let binary = ast.as_binary();

    assert_eq!(binary.op, Operator::Add);
    // 1 + 2 ^ 3
    let lhs = binary.lhs.as_binary();
    // 4
    let rhs = binary.rhs.as_number();
    assert_eq!(rhs, 4);

    assert_eq!(lhs.op, Operator::Add);
    assert_eq!(lhs.lhs.as_number(), 1);

    // 2 ^ 3
    let rhs = lhs.rhs.as_binary();
    assert_eq!(rhs.op, Operator::Exp);
    assert_eq!(rhs.lhs.as_number(), 2);
    assert_eq!(rhs.rhs.as_number(), 3);

    Ok(())
}

#[test]
fn test_parser_unary() -> crate::lang::Result<()> {
    let mut parser = Parser::new("-1");

    let ast = parser.parse_top_level_ast()?;

    let unary = ast.as_unary();

    assert_eq!(unary.op, Operator::Sub);
    assert_eq!(unary.rhs.as_number(), 1);

    Ok(())
}

#[test]
fn test_parser_group() -> crate::lang::Result<()> {
    let mut parser = Parser::new("(1 + 2)");

    let ast = parser.parse_top_level_ast()?;

    let binary = ast.as_group().as_binary();
    assert_eq!(binary.lhs.as_number(), 1);
    assert_eq!(binary.op, Operator::Add);
    assert_eq!(binary.rhs.as_number(), 2);

    Ok(())
}

#[test]
fn test_parser_sum_var() -> crate::lang::Result<()> {
    let mut parser = Parser::new("1 + x ^ 3 + 4");

    let ast = parser.parse_top_level_ast()?;
    let binary = ast.as_binary();

    assert_eq!(binary.op, Operator::Add);
    // 1 + x ^ 3
    let lhs = binary.lhs.as_binary();
    // 4
    let rhs = binary.rhs.as_number();
    assert_eq!(rhs, 4);

    assert_eq!(lhs.op, Operator::Add);
    assert_eq!(lhs.lhs.as_number(), 1);

    // x ^ 3
    let rhs = lhs.rhs.as_binary();
    assert_eq!(rhs.op, Operator::Exp);

    assert_eq!(rhs.lhs.as_var().name, "x");
    assert_eq!(rhs.rhs.as_number(), 3);

    Ok(())
}

#[test]
fn test_parser_sum_with_sub() -> crate::lang::Result<()> {
    let mut parser = Parser::new("1 - 2 + 3");

    let ast = parser.parse_top_level_ast()?;
    let binary = ast.as_binary();

    assert_eq!(binary.op, Operator::Add);

    let lhs_binary = binary.lhs.as_binary();
    assert_eq!(lhs_binary.lhs.as_number(), 1);
    assert_eq!(lhs_binary.rhs.as_number(), 2);
    assert_eq!(lhs_binary.op, Operator::Sub);

    assert_eq!(binary.rhs.as_number(), 3);

    Ok(())
}
