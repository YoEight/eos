use crate::lang::lexical::lexer::Lexer;
use crate::lang::nursery::Nursery;
use crate::lang::token::{Sym, Token};
use crate::lang::Position;


#[test]
fn test_tokenize_sum() -> crate::lang::Result<()> {
    let mut nursery = Nursery::default();
    let mut lexer = Lexer::new("1 + 2 * 3");

    let tokens = lexer.collect_tokens(&mut nursery)?;
    let tok_one = Token::new(Sym::Number, Position::new(1, 1));
    let tok_plus = Token::new(Sym::Operator, Position::new(1, 3));
    let tok_two = Token::new(Sym::Number, Position::new(1, 5));
    let tok_mul = Token::new(Sym::Operator, Position::new(1, 7));
    let tok_three = Token::new(Sym::Number, Position::new(1, 9));

    assert_eq!(
        tokens,
        [
            tok_one,
            tok_plus,
            tok_two,
            tok_mul,
            tok_three,
            Token::new(Sym::EOF, Position::new(1, 10)),
        ]
    );

    assert_eq!(nursery.get_string_or_panic(tok_one.position), "1");
    assert_eq!(nursery.get_string_or_panic(tok_plus.position), "+");
    assert_eq!(nursery.get_string_or_panic(tok_two.position), "2");
    assert_eq!(nursery.get_string_or_panic(tok_mul.position), "*");
    assert_eq!(nursery.get_string_or_panic(tok_three.position), "3");

    Ok(())
}
