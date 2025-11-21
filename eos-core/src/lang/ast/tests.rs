use crate::lang::ast::parser::Parser;
use crate::lang::ast::Operator;
use crate::lang::nursery::Nursery;

#[test]
fn test_parser_sum() -> crate::lang::Result<()> {
    let mut nursery = Nursery::default();
    let mut parser = Parser::new("1 + 2 ^ 3 + 4");

    let ast = parser.parse_top_level_ast(&mut nursery)?;
    let binary = ast.as_binary();

    assert_eq!(binary.op, Operator::Add);
    assert_eq!(binary.lhs.as_number(), 1);

    let binary = binary.rhs.as_binary();
    let lhs = binary.lhs.as_binary();

    assert_eq!(lhs.lhs.as_number(), 2);
    assert_eq!(lhs.op, Operator::Exp);
    assert_eq!(lhs.rhs.as_number(), 3);

    assert_eq!(binary.rhs.as_number(), 4);

    Ok(())
}

#[test]
fn test_parser_unary() -> crate::lang::Result<()> {
    let mut nursery = Nursery::default();
    let mut parser = Parser::new("-1");

    let ast = parser.parse_top_level_ast(&mut nursery)?;

    let unary = ast.as_unary();

    assert_eq!(unary.op, Operator::Sub);
    assert_eq!(unary.rhs.as_number(), 1);

    Ok(())
}

#[test]
fn test_parser_group() -> crate::lang::Result<()> {
    let mut nursery = Nursery::default();
    let mut parser = Parser::new("(1 + 2)");

    let ast = parser.parse_top_level_ast(&mut nursery)?;

    let binary = ast.as_group().as_binary();
    assert_eq!(binary.lhs.as_number(), 1);
    assert_eq!(binary.op, Operator::Add);
    assert_eq!(binary.rhs.as_number(), 2);

    Ok(())
}

#[test]
fn test_parser_sum_var() -> crate::lang::Result<()> {
    let mut nursery = Nursery::default();
    let mut parser = Parser::new("1 + x ^ 3 + 4");

    let ast = parser.parse_top_level_ast(&mut nursery)?;
    let binary = ast.as_binary();

    assert_eq!(binary.op, Operator::Add);
    assert_eq!(binary.lhs.as_number(), 1);

    let binary = binary.rhs.as_binary();
    let lhs = binary.lhs.as_binary();
    let var = lhs.lhs.as_var();

    assert_eq!(nursery.get_var_string_or_panic(&var), "x");
    assert_eq!(lhs.op, Operator::Exp);
    assert_eq!(lhs.rhs.as_number(), 3);

    assert_eq!(binary.rhs.as_number(), 4);

    Ok(())
}

#[test]
fn test_collect_additive_terms_all_adds() -> crate::lang::Result<()> {
    let mut nursery = Nursery::default();
    let mut parser = Parser::new("1 + 2 ^ 3 + 4");
    let ast = parser.parse_top_level_ast(&mut nursery)?;
    let xs = ast.collect_additive_terms();

    assert!(!xs.is_empty());
    assert_eq!(xs.len(), 3);

    assert!(xs[0].is_add);
    assert_eq!(xs[0].inner.as_number(), 1);

    assert!(xs[1].is_add);
    assert_eq!(xs[1].inner.as_binary().lhs.as_number(), 2);
    assert_eq!(xs[1].inner.as_binary().rhs.as_number(), 3);

    assert!(xs[2].is_add);
    assert_eq!(xs[2].inner.as_number(), 4);

    Ok(())
}

#[test]
fn test_collect_additive_terms_with_group() -> crate::lang::Result<()> {
    let mut nursery = Nursery::default();
    let mut parser = Parser::new("1 + (2 ^ 3 + 4)");
    let ast = parser.parse_top_level_ast(&mut nursery)?;
    let xs = ast.collect_additive_terms();

    assert!(!xs.is_empty());
    assert_eq!(xs.len(), 2);

    Ok(())
}

#[test]
fn test_collect_additive_terms_with_group_sub() -> crate::lang::Result<()> {
    let mut nursery = Nursery::default();
    let mut parser = Parser::new("1 - (2 ^ 3 + 4)");
    let ast = parser.parse_top_level_ast(&mut nursery)?;
    let xs = ast.collect_additive_terms();

    assert!(!xs.is_empty());
    assert_eq!(xs.len(), 2);
    assert!(!xs[1].is_add);

    Ok(())
}
