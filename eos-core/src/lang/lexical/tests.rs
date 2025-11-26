use crate::lang::lexical::lexer::Lexer;
use crate::lang::token::{Sym, Token};

#[test]
fn test_tokenize_sum() -> crate::lang::Result<()> {
    let mut lexer = Lexer::new("1 + 2 * 3");

    let tokens = lexer.collect_tokens()?;
    let tok_one = Token::new(Sym::Number, 0, "1");
    let tok_plus = Token::new(Sym::Operator, 2, "+");
    let tok_two = Token::new(Sym::Number, 4, "2");
    let tok_mul = Token::new(Sym::Operator, 6, "*");
    let tok_three = Token::new(Sym::Number, 8, "3");

    assert_eq!(
        tokens,
        [
            tok_one,
            tok_plus,
            tok_two,
            tok_mul,
            tok_three,
            Token::new(Sym::Eof, 9, ""),
        ]
    );

    Ok(())
}
