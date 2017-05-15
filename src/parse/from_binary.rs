use atoms::*;
use kind::*;

use std::io::*;

use easter::decl::*;
use easter::expr::*;
use easter::fun::*;
use easter::id::*;
use easter::obj::*;
use easter::patt::*;
use easter::prog::*;
use easter::punc::*;
use easter::stmt::*;

/* Kind of monadic implementation of counting consumed bytes.
struct Sized<T> {
    size: usize,
    value: T
}
impl Sized<()> {
    fn default() -> Self {
        Sized {
            size: 0,
            value: ()
        }
    }
}
impl<T> Sized<T> {
    fn new(size: usize, value: T) -> Self {
        Sized {
            size,
            value
        }
    }
    fn transform<F, U>(self, f: F) -> Sized<U> where F: FnOnce(T) -> U {
        Sized {
            size: self.size,
            value: f(self.value)
        }
    }
}
*/
pub struct TreeReader<'a, T> where T: Read + 'a {
    reader: &'a mut T,
    atoms: &'a AtomsTable<String>,
    kinds: &'a AtomsTable<Kind>,
}
impl<'a, T> TreeReader<'a, T> where T: Read + 'a {
    pub fn new(reader: &'a mut T, atoms: &'a AtomsTable<String>, kinds: &'a AtomsTable<Kind>) -> Self {
        TreeReader {
            reader,
            atoms,
            kinds,
        }
    }
}

impl<'a, T> TreeReader<'a, T> where T: Read {
    pub fn read_raw_byte(&mut self) -> Result<(usize, u8)> {
        unimplemented!()
    }
    pub fn read_float(&mut self) -> Result<(usize, f64)> {
        unimplemented!()
    }
    pub fn read_atom(&mut self) -> Result<(usize, String)> {
        unimplemented!()
    }
    pub fn read_kind(&mut self) -> Result<(usize, Kind)> {
        unimplemented!()
    }
    pub fn read_list_length(&mut self) -> Result<(usize, u32)> {
        unimplemented!()
    }
    pub fn with_bytelength<F, U>(&mut self, f: F) ->  Result<(usize, U)> where F: FnOnce(&mut Self) -> Result<(usize, U)> {
        unimplemented!()
    }

    pub fn parse_tuple<F, U>(&mut self, list_length: u32, mut f: F) -> Result<(usize, Vec<U>)> where F: FnMut(&mut Self) -> Result<(usize, U)> {
        let mut bytes = 0;

        let mut body : Vec<U> = Vec::with_capacity(list_length as usize);
        for _ in 0..list_length {
            let (bytes_item, item) = f(self)?;
            bytes += bytes_item;
            body.push(item)
        }
        Ok((bytes, body))
    }

    pub fn parse_list<F, U>(&mut self, f: F) -> Result<(usize, Vec<U>)> where F: FnMut(&mut Self) -> Result<(usize, U)> {
        let mut bytes = 0;
        let (bytes_list_length, list_length) = self.read_list_length()?;
        bytes += bytes_list_length;

        let (bytes_body, body) = self.with_bytelength(|mut me| {
            me.parse_tuple(list_length, f)
        })?;
        Ok((bytes + bytes_body, body))
    }


    pub fn parse_statement_list_item(&mut self) -> Result<(usize, StmtListItem)> {
        let mut bytes = 0;
        let (bytes_kind, kind) = self.read_kind()?;
        bytes += bytes_kind;
        let (bytes_item, item) = match kind {
            Kind::FunDecl => {
                // FIXME: Read (and discard) environment
                // FIXME: Read optional identifier
                // FIXME: Read parameters
                // FIXME: Read body
                unimplemented!()
            },
            _ => unimplemented!()
        };
        bytes += bytes_item;
        unimplemented!();
        Ok((bytes, item))
    }

    pub fn parse_script(&mut self) -> Result<(usize, Script)> {
        let (bytes, body) = self.parse_list(|mut me| {
            me.parse_statement_list_item()
        })?;
        Ok((bytes, Script {
            location: None,
            body
        }))
    }
}