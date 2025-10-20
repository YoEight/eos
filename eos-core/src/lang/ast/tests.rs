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
