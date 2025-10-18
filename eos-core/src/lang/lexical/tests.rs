use crate::lang::lexical::lexer::Lexer;
use crate::lang::token::{Sym, Token};
use crate::lang::Position;

fn tokenize(input: &str) -> crate::lang::Result<Vec<Token>> {
    let mut lexer = Lexer::new(input);
    let mut tokens = Vec::new();

    loop {
        let token = lexer.next_token()?;

        tokens.push(token);

        if token.sym == Sym::EOF {
            break;
        }
    }

    Ok(tokens)
}

#[test]
fn test_tokenize_sum() -> crate::lang::Result<()> {
    let tokens = tokenize("1 + 2 * 3")?;

    assert_eq!(
        tokens,
        [
            Token::new(Sym::Number, Position::new(1, 1)),
            Token::new(Sym::Operator, Position::new(1, 3)),
            Token::new(Sym::Number, Position::new(1, 5)),
            Token::new(Sym::Operator, Position::new(1, 7)),
            Token::new(Sym::Number, Position::new(1, 9)),
            Token::new(Sym::EOF, Position::new(1, 10)),
        ]
    );

    Ok(())
}
