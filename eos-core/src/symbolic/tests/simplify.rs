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
