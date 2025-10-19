use crate::lang::Position;
use std::collections::HashMap;
use std::hash::{BuildHasher, BuildHasherDefault, DefaultHasher};

#[derive(Default)]
pub struct Nursery {
    hasher_builder: BuildHasherDefault<DefaultHasher>,
    locs: HashMap<Position, u64>,
    names: HashMap<u64, String>,
}

impl Nursery {
    pub fn register_string(&mut self, pos: Position, expr: String) {
        let hash = self.hash_expr(&expr);
        self.names.insert(hash, expr);
        self.locs.insert(pos, hash);
    }

    pub fn register_char(&mut self, pos: Position, c: char) {
        self.register_string(pos, c.to_string());
    }

    pub fn get_string_or_panic(&self, pos: Position) -> &str {
        if let Some(h) = self.locs.get(&pos) {
            if let Some(name) = self.names.get(h) {
                return name.as_str();
            }
        }

        panic!("nursery: no string at position {pos}")
    }

    pub fn get_unique_id_or_panic(&self, pos: Position) -> u64 {
        if let Some(h) = self.locs.get(&pos) {
            return *h;
        }

        panic!("nursery: no string at position {pos}")
    }

    fn hash_expr(&mut self, expr: &String) -> u64 {
        self.hasher_builder.hash_one(expr)
    }
}
