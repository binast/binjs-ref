use kind::*;

use std::io::*;

pub struct TreeReader<T> where T: Read {
    reader: T
}

impl<T> TreeReader<T> where T: Read {
    pub fn read_raw_byte(&mut self) -> Result<u8> {
        unimplemented!()
    }
    pub fn read_float(&mut self) -> Result<f64> {
        unimplemented!()
    }
    pub fn read_atom(&mut self) -> Result<String> {
        unimplemented!()
    }
    pub fn read_kind(&mut self) -> Result<Kind> {
        unimplemented!()
    }
    pub fn read_list_length(&mut self) -> Result<u32> {
        unimplemented!()
    }
    pub fn with_bytelength<F, U>(&mut self, f: F) ->  U where F: FnOnce(&mut Self) -> U {
        unimplemented!()
    }
}