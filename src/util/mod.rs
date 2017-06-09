use serde_json::{ Value as JSON, Number };

use std::io::{ Read, Result, Seek, SeekFrom };

pub struct PositionRead<R> where R: Read {
    reader: R,
    position: u64,
}

impl<R> PositionRead<R> where R: Read {
    pub fn new(reader: R) -> Self {
        PositionRead {
            reader,
            position: 0
        }
    }

    pub fn position(&self) -> u64 {
        self.position
    }
}

impl<R> Read for PositionRead<R> where R: Read {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        let bytes = self.reader.read(buf)?;
        self.position += bytes as u64;
        Ok(bytes)
    }
}

impl<R> Seek for PositionRead<R> where R: Read + Seek {
    fn seek(&mut self, pos: SeekFrom) -> Result<u64> {
        self.position = self.reader.seek(pos)?;
        Ok(self.position)
    }
}


/// Strip a tree from meaningless information (location information, comments, ...)
#[allow(unused)]
pub fn strip(tree: &mut JSON) {
    use serde_json::Value::*;
    match *tree {
        Object(ref mut map) => {
            map.remove("loc");
            map.remove("comments");
            map.remove("start");
            map.remove("end");
            for (_, value) in map.iter_mut() {
                strip(value);
            }
        }
        Array(ref mut array) => {
            for value in array.iter_mut() {
                strip(value);
            }
        }
        _ => {}
    }
}

pub fn type_of(tree: &JSON) -> String {
    use serde_json::Value::*;
    match *tree {
        Object(_) => "Object",
        String(_) => "String",
        Number(_) => "Number",
        Null      => "Null",
        Bool(_)   => "Bool",
        Array(_)  => "Array"
    }.to_owned()
}

pub fn f64_of(number: &Number) -> f64 {
    if number.is_i64() {
        number.as_i64().unwrap() as f64
    } else if number.is_u64() {
        number.as_u64().unwrap() as f64
    } else {
        number.as_f64().unwrap()
    }
}