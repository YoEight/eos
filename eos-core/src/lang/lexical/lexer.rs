use crate::lang::lexical::text::Text;
use crate::lang::lexical::Error;
use crate::lang::nursery::Nursery;
use crate::lang::token::{Sym, Token};

pub struct Lexer<'a> {
    text: Text<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(expr: &'a str) -> Self {
        Self {
            text: Text::new(expr),
        }
    }

    pub fn next_token(&mut self, nursery: &mut Nursery) -> crate::lang::Result<Token> {
        let mut position = self.text.position();

        if let Some(c) = self.text.look_ahead()
            && c.is_whitespace()
        {
            self.text.shift();
            position = self.text.position();
        }

        if let Some(c) = self.text.look_ahead() {
            match c {
                '(' | ')' => {
                    self.text.shift();
                    nursery.register_char(position, c);
                    Ok(Token::new(Sym::Symbol, position))
                }

                '+' | '-' | '*' | '/' | '=' | '^' => {
                    self.text.shift();
                    nursery.register_char(position, c);
                    Ok(Token::new(Sym::Operator, position))
                }

                _ if c.is_ascii_digit() => {
                    self.text.shift();

                    let mut numbers = String::new();
                    numbers.push(c);

                    while let Some(c) = self.text.look_ahead() {
                        if !c.is_ascii_digit() {
                            break;
                        }

                        self.text.shift();
                        numbers.push(c);
                    }

                    nursery.register_string(position, numbers);

                    Ok(Token::new(Sym::Number, position))
                }

                _ if c.is_ascii_alphabetic() => {
                    self.text.shift();

                    let mut name = String::new();
                    name.push(c);

                    while let Some(c) = self.text.look_ahead() {
                        if !c.is_ascii_alphanumeric() {
                            break;
                        }

                        self.text.shift();
                        name.push(c);
                    }

                    nursery.register_string(position, name);

                    Ok(Token::new(Sym::Variable, position))
                }

                _ => bail!(position, Error::UnexpectedChar(c)),
            }
        } else {
            Ok(Token::new(Sym::EOF, position))
        }
    }

    pub fn collect_tokens(&mut self, nursery: &mut Nursery) -> crate::lang::Result<Vec<Token>> {
        let mut tokens = Vec::new();

        loop {
            let token = self.next_token(nursery)?;

            tokens.push(token);

            if token.sym == Sym::EOF {
                break;
            }
        }

        Ok(tokens)
    }
}
