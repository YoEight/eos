use crate::lang::lexical::text::Text;
use crate::lang::lexical::Error;
use crate::lang::nursery::Nursery;
use crate::lang::token::{Sym, Token};

pub struct Lexer<'a> {
    text: Text<'a>,
    nursery: Nursery,
}

impl<'a> Lexer<'a> {
    pub fn new(expr: &'a str) -> Self {
        Self {
            text: Text::new(expr),
            nursery: Nursery::default(),
        }
    }

    pub fn next_token(&mut self) -> crate::lang::Result<Token> {
        let position = self.text.position();

        if let Some(c) = self.text.look_ahead() {
            match c {
                '(' | ')' => {
                    self.text.shift();
                    self.nursery.register_char(position, c);
                    Ok(Token::new(Sym::Symbol, position))
                }

                '+' | '-' | '*' | '/' | '=' | '^' => {
                    self.text.shift();
                    self.nursery.register_char(position, c);
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

                    self.nursery.register_string(position, numbers);

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

                    self.nursery.register_string(position, name);

                    Ok(Token::new(Sym::Variable, position))
                }

                _ => bail!(position, Error::UnexpectedChar(c)),
            }
        } else {
            Ok(Token::new(Sym::EOF, position))
        }
    }
}
