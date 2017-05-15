use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::io::{ Error, ErrorKind, Read, Write };

use varnum::*;

pub trait ToBytes {
    fn to_bytes(&self) -> Vec<u8>;
}

pub trait FromBytes where Self: Sized {
    fn from_bytes(&[u8]) -> Result<Self, Error>;
}

impl ToBytes for String {
    fn to_bytes(&self) -> Vec<u8> {
        return self.bytes().collect();
    }
}

impl FromBytes for String {
    fn from_bytes(bytes: &[u8]) -> Result<Self, Error> {
        let bytes = bytes.iter().cloned().collect();
        String::from_utf8(bytes)
            .map_err(|err| Error::new(ErrorKind::InvalidData, err))
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

        let mut from_key: HashMap<u32, T> = HashMap::with_capacity(entries.len());
        let mut to_key: HashMap<T, u32> = HashMap::with_capacity(entries.len());
        for (entry, i) in entries.iter().zip(0..) {
            assert!(from_key.insert(i, entry.0.clone()).is_none());
            assert!(to_key.insert(entry.0.clone(), i).is_none());
        }
        AtomsTable {
            from_key,
            to_key
        }
    }
}

// FIXME: We should probably split the reading part from the writing part
pub struct AtomsTable<T> where T: Eq + Hash + ToBytes {
    from_key: HashMap<u32, T>,
    to_key: HashMap<T, u32>,
}
impl<T> AtomsTable<T> where T: Eq + Hash + ToBytes {
    fn len(&self) -> u32 {
        self.from_key.len() as u32
    }

    pub fn get(&self, key: u32) -> Option<&T> {
        self.from_key.get(&key)
    }
    pub fn get_key(&self, value: &T) -> Option<u32> {
        self.to_key.get(value).cloned()
    }

    /// Write the table, with the following format:
    ///
    /// - number of entries (varnum);
    /// - repeat number of entries times:
    ///   - byte length of Nth entry (varnum);
    /// - repeat number of entries times:
    ///   - content of Nth entry (bytes);
    pub fn write_index<U>(&self, out: &mut U) -> Result<usize, Error> where U: Write {
        assert_eq!(self.from_key.len(), self.to_key.len());
        let mut bytes = 0;
        bytes += out.write_varnum(self.len())?;
        for atom in self.from_key.values() {
            let data = atom.to_bytes();
            bytes += out.write_varnum(data.len() as u32)?;
        }
        for atom in self.from_key.values() {
            let data = atom.to_bytes();
            bytes += out.write(&data)?;
        }
        Ok(bytes)
    }

    pub fn read_index<U>(src: &mut U) -> Result<(usize, Self), Error> where U: Read, T: FromBytes + Debug + ToBytes + Clone {
        let mut bytes = 0;

        // Read number of entries.
        let mut number_of_entries = 0;
        bytes += src.read_varnum(&mut number_of_entries)?;
        let number_of_entries = number_of_entries as usize;

        // Read length of entries.
        let mut byte_lengths = Vec::with_capacity(number_of_entries as usize);
        for _ in 0..number_of_entries {
            let mut length_of_entry = 0;
            bytes += src.read_varnum(&mut length_of_entry)?;
            let length_of_entry = length_of_entry as usize;
            byte_lengths.push(length_of_entry);
        }

        // Read actual entries.
        let mut from_key : HashMap<u32, T> = HashMap::with_capacity(number_of_entries);
        let mut to_key : HashMap<T, u32> = HashMap::with_capacity(number_of_entries);
        for i in 0..number_of_entries {
            let length_of_entry = byte_lengths[i];
            let mut buf = Vec::with_capacity(length_of_entry);
            unsafe { buf.set_len(length_of_entry); }
            src.read_exact(&mut buf)?;
            bytes += length_of_entry;

            let data = T::from_bytes(&buf)?;

            if let Some(_) = to_key.insert(data.clone(), i as u32) {
                // Duplicate entry, that's bad.
                return Err(Error::new(ErrorKind::InvalidData, "Duplicate entry"));
            }

            assert!(from_key.insert(i as u32, data).is_none());
        }

        // Build maps
        let result = AtomsTable {
            from_key,
            to_key
        };
        Ok((bytes, result))
    }
}

#[test]
fn test_create_table() {
    println!("Create atoms table.");
    let mut initializer = AtomsTableInitializer::new();
    for i in 0..100 {
        for j in 0..100 {
            initializer.add(format!("{}", i * j))
        }
    }
    let table = initializer.compile();

    println!("Ensure that are all entries are present.");
    for i in 0..100 {
        for j in 0..100 {
            let string = format!("{}", i * j);
            let key = table.get_key(&string).expect("The entry is present");

            let extracted = table.get(key).expect("The key maps to an entry");
            let extracted_string = String::from_utf8(extracted.clone()).expect("The entry is utf8");
            assert_eq!(string, extracted_string);
        }
    }

    println!("Ensure that the keys are contiguous.");
    for key in 0..table.len() {
        table.get(key).expect("Expecting a value for this key");
    }
}

#[test]
fn test_write_read_index() {
    use std::io::Cursor;
    println!("Create atoms table.");
    let mut initializer = AtomsTableInitializer::new();
    for i in 0..100 {
        for j in 0..100 {
            initializer.add(format!("{}", i * j))
        }
    }
    let table = initializer.compile();

    println!("Write atoms table.");
    let mut vec = vec![];
    let bytes = table.write_index(&mut vec).expect("Writing the atoms table.");
    assert_eq!(bytes, vec.len());

    println!("Read back atoms table.");
    let (bytes2, table2) = AtomsTable::<String>::read_index(&mut Cursor::new(vec)).expect("Reading the atoms table.");
    assert_eq!(bytes2, bytes);

    drop(table); // Make sure that all further tests are performed with `table2`.

    println!("Ensure that are all entries are present.");
    for i in 0..100 {
        for j in 0..100 {
            let string = format!("{}", i * j);
            let key = table2.get_key(&string).expect("The entry is present");

            let extracted = table2.get(key).expect("The key maps to an entry");
            let extracted_string = String::from_utf8(extracted.clone()).expect("The entry is utf8");
            assert_eq!(string, extracted_string);
        }
    }

    println!("Ensure that the keys are contiguous.");
    for key in 0..table2.len() {
        table2.get(key).expect("Expecting a value for this key");
    }
}