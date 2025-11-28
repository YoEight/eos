use crate::symbolic::simplify::simplify;
use crate::{normalize, parse};

#[test]
fn test_simplify_simple() -> crate::Result<()> {
    let expr = normalize(parse("1 + 2")?);

    assert_eq!("3", simplify(expr).pretty_print());

    Ok(())
}

#[test]
fn test_simplify_simple_sub() -> crate::Result<()> {
    let expr = normalize(parse("1 - 2")?);

    assert_eq!("-1", simplify(expr).pretty_print());

    Ok(())
}

#[test]
fn test_simplify_simple_sub_1() -> crate::Result<()> {
    let expr = normalize(parse("- 1 - 2")?);

    assert_eq!("-3", simplify(expr).pretty_print());

    Ok(())
}

#[test]
fn test_simplify_group_sub_1() -> crate::Result<()> {
    let expr = normalize(parse("- (1 + 4) - 2")?);

    assert_eq!("-7", simplify(expr).pretty_print());

    Ok(())
}

#[test]
fn test_simplify_unary() -> crate::Result<()> {
    let expr = normalize(parse("--42")?);

    assert_eq!("42", simplify(expr).pretty_print());

    Ok(())
}

#[test]
fn test_simplify_unary_plus() -> crate::Result<()> {
    let expr = normalize(parse("+++++++42")?);

    assert_eq!("42", simplify(expr).pretty_print());

    Ok(())
}

#[test]
fn test_simplify_unary_group() -> crate::Result<()> {
    let expr = normalize(parse("-(2 - 4)")?);

    assert_eq!("2", simplify(expr).pretty_print());

    Ok(())
}

#[test]
fn test_simplify_binary_mul() -> crate::Result<()> {
    let expr = normalize(parse("1 * 2 * 3")?);

    assert_eq!("6", simplify(expr).pretty_print());

    Ok(())
}

#[test]
fn test_simplify_binary_mul_unary() -> crate::Result<()> {
    let expr = normalize(parse("-1 * 2 * 3")?);

    assert_eq!("-6", simplify(expr).pretty_print());

    Ok(())
}

#[test]
fn test_simplify_binary_mul_zero() -> crate::Result<()> {
    let expr = normalize(parse("-1 * 2 * 0")?);

    assert_eq!("0", simplify(expr).pretty_print());

    Ok(())
}

#[test]
fn test_simplify_binary_mul_group_unary() -> crate::Result<()> {
    let expr = normalize(parse("-(1 + 3) * 4 * (4 * 5)")?);

    assert_eq!("-320", simplify(expr).pretty_print());

    Ok(())
}

#[test]
fn test_simplify_vars() -> crate::Result<()> {
    let expr = normalize(parse("2 * x + x + 2")?);

    assert_eq!("3 * x + 2", simplify(expr).pretty_print());

    Ok(())
}

#[test]
fn test_simplify_vars_nullify() -> crate::Result<()> {
    let expr = normalize(parse("2 * x - 2*x + 2")?);

    assert_eq!("2", simplify(expr).pretty_print());

    Ok(())
}

#[test]
fn test_simplify_vars_single_positive() -> crate::Result<()> {
    let expr = normalize(parse("2 * x - 1*x + 2")?);

    assert_eq!("x + 2", simplify(expr).pretty_print());

    Ok(())
}

#[test]
fn test_simplify_vars_single_negative() -> crate::Result<()> {
    let expr = normalize(parse("2 * x - 3 * x + 2")?);

    assert_eq!("-x + 2", simplify(expr).pretty_print());

    Ok(())
}

#[test]
fn test_simplify_vars_unary_positive() -> crate::Result<()> {
    let expr = normalize(parse("-2 * x + 3 * x + 2")?);

    assert_eq!("x + 2", simplify(expr).pretty_print());

    Ok(())
}
