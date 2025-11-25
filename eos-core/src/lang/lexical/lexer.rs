use crate::lang::lexical::Error;
use crate::lang::lexical::text::Text;
use crate::lang::token::{Sym, Token};

#[derive(Clone, Copy)]
pub struct Lexer<'a> {
    input: Text<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(expr: &'a str) -> Self {
        Self {
            input: Text::new(expr),
        }
    }

    fn look_ahead<'b>(&'b self) -> Option<&'a str> {
        let (result, _) = self.input.peek();

        result
    }

    fn shift<'b>(&'b mut self) -> Option<&'a str> {
        let (result, next) = self.input.shift();
        self.input = next;

        result
    }

    fn skip_whitespaces(&mut self) {
        self.input = self.input.skip_whitespaces();
    }

    fn take_while<'b>(&'b mut self, fun: impl Fn(char) -> bool) -> &'a str {
        let (result, next) = self.input.take_while(fun);
        self.input = next;

        result
    }

    pub fn next_token<'b>(&'b mut self) -> crate::lang::Result<Token<'a>> {
        self.skip_whitespaces();
        let start = self.input.pos();

        if let Some(value) = self.look_ahead() {
            let c = value.as_bytes()[0] as char;
            match c {
                '(' | ')' => {
                    self.shift();
                    Ok(Token::new(Sym::Symbol, start, value))
                }

                '+' | '-' | '*' | '/' | '=' | '^' => {
                    self.shift();
                    Ok(Token::new(Sym::Operator, start, value))
                }

                _ if c.is_ascii_digit() => {
                    let numbers = self.take_while(|c| c.is_ascii_digit());

                    Ok(Token::new(Sym::Number, start, numbers))
                }

                _ if c.is_ascii_alphabetic() => {
                    let name = self.take_while(|c| c.is_ascii_alphanumeric());
                    Ok(Token::new(Sym::Variable, start, name))
                }

                _ => bail!(start, Error::UnexpectedChar(c)),
            }
        } else {
            Ok(Token::new(Sym::EOF, start, ""))
        }
    }

    #[cfg(test)]
    pub fn collect_tokens<'b>(&'b mut self) -> crate::lang::Result<Vec<Token<'a>>> {
        let mut tokens = Vec::new();

        loop {
            let token = self.next_token()?;

            tokens.push(token);

            if token.sym == Sym::EOF {
                break;
            }
        }

        Ok(tokens)
    }
}
