use crate::lang::Position;
use std::collections::HashMap;

#[derive(Default)]
pub struct Nursery {
    inner: HashMap<Position, String>,
}

impl Nursery {
    pub fn register_string(&mut self, pos: Position, expr: String) {
        self.inner.insert(pos, expr);
    }

    pub fn register_char(&mut self, pos: Position, c: char) {
        self.register_string(pos, c.to_string());
    }

    pub fn get_string_or_panic(&self, pos: Position) -> &str {
        if let Some(s) = self.inner.get(&pos) {
            return s.as_str();
        }

        panic!("nursery: no string at position {pos}")
    }
}
