use std::collections::HashMap;
use std::hash::Hash;
use std::io::{ Error, Write };

use varnum::*;

pub trait ToBytes {
    fn to_bytes(&self) -> Vec<u8>;
}

impl ToBytes for String {
    fn to_bytes(&self) -> Vec<u8> {
        return self.bytes().collect();
    }
}

pub struct AtomsTableInitializer<T> where T: ToBytes + Eq + Hash {
    entries: HashMap<T, u32>
}
impl<T> AtomsTableInitializer<T> where T: ToBytes + Eq + Hash {
    pub fn new() -> Self {
        AtomsTableInitializer {
            entries: HashMap::with_capacity(100)
        }
    }
    pub fn add(&mut self, key: T) {
        let entry = self.entries.entry(key).or_insert(0);
        *entry += 1;
    }
    pub fn compile(self) -> AtomsTable {
        let mut entries = self.entries;
        let mut entries : Vec<_> = entries.drain().collect();
        entries.sort_by(|a, b| {
            a.1.cmp(&b.1)
        });

        let mut keyed: HashMap<u32, Vec<u8>> = HashMap::with_capacity(entries.len());
        for (entry, i) in entries.drain(..).zip(0..) {
            assert!(keyed.insert(i, entry.0.to_bytes()).is_none());
        }
        AtomsTable {
            entries: keyed
        }
    }
}

pub struct AtomsTable {
    entries: HashMap<u32, Vec<u8>>
}
impl AtomsTable {
    fn len(&self) -> u32 {
        self.entries.len() as u32
    }
    pub fn get(&self, key: u32) -> Option<&Vec<u8>> {
        self.entries.get(&key)
    }

    pub fn write<U>(&self, out: &mut U) -> Result<usize, Error> where U: Write {
        let mut bytes = 0;
        out.write_varnum(self.len())?;
        for atom in self.entries.values() {
            bytes += out.write_varnum(atom.len() as u32)?;
        }
        for atom in self.entries.values() {
            bytes += out.write(&atom)?;
        }
        Ok(bytes)
    }
}