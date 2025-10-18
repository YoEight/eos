use crate::lang::Position;
use std::iter::Peekable;
use std::str::Chars;

pub struct Text<'a> {
    inner: Peekable<Chars<'a>>,
    position: Position,
}

impl<'a> Text<'a> {
    pub fn new(query: &'a str) -> Self {
        Self {
            inner: query.chars().peekable(),
            position: Position::default(),
        }
    }

    pub fn shift(&mut self) -> Option<char> {
        let c = self.inner.next()?;

        if c == '\n' {
            self.position.line += 1;
            self.position.column = 1;
        } else {
            self.position.column += 1;
        }

        Some(c)
    }

    pub fn position(&self) -> Position {
        self.position
    }

    pub fn look_ahead(&mut self) -> Option<char> {
        self.inner.peek().copied()
    }
}
