use crate::lang::lexical::text::Input;
use crate::lang::lexical::Error;
use crate::lang::token::{Sym, Token};

#[derive(Clone, Copy)]
pub struct Lexer<'a> {
    input: Input<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(expr: &'a str) -> Self {
        Self {
            input: Input::new(expr),
        }
    }

    fn look_ahead(self) -> Option<&'a str> {
        let (result, _) = self.input.peek();

        result
    }

    fn shift(mut self) -> (Option<&'a str>, Self) {
        let (result, next) = self.input.shift();
        self.input = next;

        (result, self)
    }

    fn skip_whitespaces(mut self) -> Self {
        self.input = self.input.skip_whitespaces();
        self
    }

    fn take_while(mut self, fun: impl Fn(char) -> bool) -> (&'a str, Self) {
        let (result, next) = self.input.take_while(fun);
        self.input = next;

        (result, self)
    }

    pub fn next_token(mut self) -> crate::lang::Result<(Token<'a>, Self)> {
        let mut current = self.skip_whitespaces();
        let start = current.input.pos();

        if let Some(value) = current.look_ahead() {
            let c = value.as_bytes()[0] as char;
            match c {
                '(' | ')' => {
                    let (_, next) = current.shift();
                    Ok((Token::new(Sym::Symbol, start, value), next))
                }

                '+' | '-' | '*' | '/' | '=' | '^' => {
                    let (_, next) = current.shift();
                    Ok((Token::new(Sym::Operator, start, value), next))
                }

                _ if c.is_ascii_digit() => {
                    let (numbers, next) = self.take_while(|c| c.is_ascii_digit());

                    Ok((Token::new(Sym::Number, start, numbers), next))
                }

                _ if c.is_ascii_alphabetic() => {
                    let (name, next) = self.take_while(|c| c.is_ascii_alphanumeric());
                    Ok((Token::new(Sym::Variable, start, name), next))
                }

                _ => bail!(start, Error::UnexpectedChar(c)),
            }
        } else {
            Ok((Token::new(Sym::EOF, start, ""), current))
        }
    }

    pub fn collect_tokens(self) -> crate::lang::Result<Vec<Token<'a>>> {
        let mut current = self;
        let mut tokens = Vec::new();

        loop {
            let (token, next) = current.next_token()?;
            current = next;

            tokens.push(token);

            if token.sym == Sym::EOF {
                break;
            }
        }

        Ok(tokens)
    }
}
