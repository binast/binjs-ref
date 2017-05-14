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

pub struct AtomsTableInitializer<T> where T: ToBytes + Eq + Hash + Clone {
    entries: HashMap<T, u32>
}
impl<T> AtomsTableInitializer<T> where T: ToBytes + Eq + Hash + Clone {
    pub fn new() -> Self {
        AtomsTableInitializer {
            entries: HashMap::with_capacity(100)
        }
    }
    pub fn add(&mut self, key: T) {
        let entry = self.entries.entry(key).or_insert(0);
        *entry += 1;
    }
    pub fn compile(self) -> AtomsTable<T> {
        let mut entries = self.entries;
        let mut entries : Vec<_> = entries.drain().collect();
        entries.sort_by(|a, b| {
            a.1.cmp(&b.1)
        });

        let mut from_key: HashMap<u32, Vec<u8>> = HashMap::with_capacity(entries.len());
        let mut to_key: HashMap<T, u32> = HashMap::with_capacity(entries.len());
        for (entry, i) in entries.iter().zip(0..) {
            assert!(from_key.insert(i, entry.0.to_bytes()).is_none());
            assert!(to_key.insert(entry.0.clone(), i).is_none());
        }
        AtomsTable {
            from_key,
            to_key
        }
    }
}

pub struct AtomsTable<T> where T: Eq + Hash {
    from_key: HashMap<u32, Vec<u8>>,
    to_key: HashMap<T, u32>,
}
impl<T> AtomsTable<T> where T: Eq + Hash {
    fn len(&self) -> u32 {
        self.from_key.len() as u32
    }

    pub fn get(&self, key: u32) -> Option<&Vec<u8>> {
        self.from_key.get(&key)
    }
    pub fn get_key(&self, value: &T) -> Option<u32> {
        self.to_key.get(value).cloned()
    }

    pub fn write_index<U>(&self, out: &mut U) -> Result<usize, Error> where U: Write {
        assert_eq!(self.from_key.len(), self.to_key.len());
        let mut bytes = 0;
        out.write_varnum(self.len())?;
        for atom in self.from_key.values() {
            bytes += out.write_varnum(atom.len() as u32)?;
        }
        for atom in self.from_key.values() {
            bytes += out.write(&atom)?;
        }
        Ok(bytes)
    }
}