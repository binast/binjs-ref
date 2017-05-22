use atoms::*;
use kind::*;
use varnum::*;

use std;
use std::io::*;
use std::rc::Rc;

use easter::decl::*;
use easter::expr::*;
use easter::fun::*;
use easter::id::*;
use easter::obj::*;
use easter::patt::*;
use easter::prog::*;
use easter::punc::*;
use easter::stmt::*;
use joker;

// Kind of monadic implementation of counting consumed bytes.
pub struct Sized<T> {
    size: usize,
    value: T
}

impl<T> Sized<T> {
    fn new(size: usize, value: T) -> Self {
        Sized {
            size,
            value
        }
    }
    fn map<F, U>(self, f: F) -> Sized<U> where F: FnOnce(T) -> U {
        Sized {
            size: self.size,
            value: f(self.value)
        }
    }
    pub fn count(self, count: &mut usize) -> T {
        *count += self.size;
        self.value
    }
}

pub struct TreeReader<'a, T> where T: Read + 'a {
    reader: &'a mut T,
    atoms: &'a AtomsTable<Rc<String>>,
    kinds: &'a AtomsTable<Kind>,
}
impl<'a, T> TreeReader<'a, T> where T: Read + 'a {
    pub fn new(reader: &'a mut T, atoms: &'a AtomsTable<Rc<String>>, kinds: &'a AtomsTable<Kind>) -> Self {
        TreeReader {
            reader,
            atoms,
            kinds,
        }
    }
}

impl<'a, T> TreeReader<'a, T> where T: Read {
    pub fn read_raw_byte(&mut self) -> Result<Sized<u8>> {
        let mut buf: [u8; 1] = [0];
        let bytes = self.reader.read(&mut buf)?;
        Ok(Sized::new(bytes, buf[0]))
    }
    pub fn read_float(&mut self) -> Result<Sized<f64>> {
        let mut buf: [u8; 8] = [0, 0, 0, 0, 0, 0, 0, 0];
        assert!(std::mem::size_of::<f64>() == std::mem::size_of::<[u8;8]>());
        let bytes = self.reader.read(&mut buf)?;

        // FIXME: This makes assumptions on endianness.
        let float = unsafe { std::mem::transmute::<[u8;8], f64>(buf) };
        Ok(Sized::new(bytes, float))
    }
    /// Read an atom from the table of atoms.
    ///
    /// This implementation reallocates a string.
    pub fn read_atom(&mut self) -> Result<Sized<Rc<String>>> {
        let mut index = 0;
        let bytes = self.reader.read_varnum(&mut index)?;
        match self.atoms.get(index) {
            Some(data) => Ok(Sized::new(bytes, data.clone())),
            None => Err(Error::new(ErrorKind::InvalidData, "Incorrect Atom index"))
        }
    }
    pub fn read_id(&mut self) -> Result<Sized<Id>> {
        self.read_atom()
            .map(|result| {
                result.map(|data| {
                    Id {
                        location: None,
                        name: joker::word::Name::String(data.as_ref().clone())
                    }
                })
            })
    }

    pub fn read_optional_id(&mut self) -> Result<Sized<Option<Id>>> {
        let mut bytes = 0;
        let data = self.read_atom()?.count(&mut bytes);
        let result = if data.len() == 0 {
            None
        } else {
            Some(Id {
                location: None,
                name: joker::word::Name::String(data.as_ref().clone())
            })
        };
        Ok(Sized::new(bytes, result))
    }

    pub fn read_kind(&mut self) -> Result<Sized<Kind>> {
        let mut index = 0;
        let bytes = self.reader.read_varnum(&mut index)?;
        match self.kinds.get(index) {
            Some(data) => Ok(Sized::new(bytes, data.clone())),
            None => Err(Error::new(ErrorKind::InvalidData, "Incorrect Kind index"))
        }
    }

    pub fn read_list_length(&mut self) -> Result<Sized<u32>> {
        let mut index = 0;
        let bytes = self.reader.read_varnum(&mut index)?;
        Ok(Sized::new(bytes, index))
    }

    pub fn with_bytelength<F, U>(&mut self, f: F) ->  Result<Sized<U>> where F: FnOnce(&mut Self) -> Result<Sized<U>> {
        let mut bytes = 0;
        let mut byte_length = 0;
        bytes += self.reader.read_varnum(&mut byte_length)?;

        let sized = f(self)?;
        if sized.size != byte_length as usize {
            return Err(Error::new(ErrorKind::InvalidData, "Invalid byte length"))
        }
        let data = sized.count(&mut bytes);
        Ok(Sized::new(bytes, data))
    }

    pub fn parse_tuple<F, U>(&mut self, list_length: u32, mut f: F) -> Result<Sized<Vec<U>>> where F: FnMut(&mut Self) -> Result<Sized<U>> {
        let mut bytes = 0;

        let mut body : Vec<U> = Vec::with_capacity(list_length as usize);
        for _ in 0..list_length {
            let item = f(self)?.count(&mut bytes);
            body.push(item)
        }
        Ok(Sized::new(bytes, body))
    }

    pub fn parse_list<F, U>(&mut self, f: F) -> Result<Sized<Vec<U>>> where F: FnMut(&mut Self) -> Result<Sized<U>> {
        let mut bytes = 0;
        let list_length = self.read_list_length()?.count(&mut bytes);

        let body = self.with_bytelength(|mut me| {
            me.parse_tuple(list_length, f)
        })?.count(&mut bytes);
        Ok(Sized::new(bytes, body))
    }

    pub fn parse_pattern(&mut self) -> Result<Sized<Patt<Id>>> {
        let mut bytes = 0;
        match self.read_kind()?.count(&mut bytes) {
            Kind::Name => {
                let id = self.read_id()?
                    .count(&mut bytes);
                Patt::Simple(id)
            }
            Kind::BindingPattern(BindingPattern::Array) => {
/*
                let list : Vec<_> = self.parse_list(|me|
                    match me.read_kind()?.count(&mut bytes) {
                        Kind::Empty => unimplemented!(),
                        _ => unimplemented!()
                    }
                );
*/
                unimplemented!()
            }
            Kind::BindingPattern(BindingPattern::Object) => unimplemented!(),
            _ => return Err(Error::new(ErrorKind::InvalidData, "Invalid pattern"))
        };
        unimplemented!()
    }

    pub fn parse_fun(&mut self) -> Result<Sized<Fun>> {
        let mut bytes = 0;

        // Read (and discard) environment
        let _looks_like_direct_eval = self.read_raw_byte()?.count(&mut bytes);
        let _let_list = self.parse_list(|me| {
            me.read_atom()
        })?.count(&mut bytes);
        let _var_list = self.parse_list(|me| {
            me.read_atom()
        })?.count(&mut bytes);

        let id = self.read_optional_id()?.count(&mut bytes);

        let params = Params {
            location: None,
            list: self.parse_list(|me| {
                    me.parse_pattern()
                })?.count(&mut bytes)
        };

        let body = self.parse_list(|mut me| {
            me.parse_statement_list_item()
        })?.count(&mut bytes);

        Ok(Sized::new(bytes, Fun {
            location: None,
            id,
            params,
            body
        }))
    }
    pub fn parse_statement_list_item(&mut self) -> Result<Sized<StmtListItem>> {
        let mut bytes = 0;
        let kind = self.read_kind()?.count(&mut bytes);
        let item = match kind {
            Kind::FunDecl => self.parse_fun()?
                .map(Decl::Fun)
                .map(StmtListItem::Decl),
            _ => unimplemented!()
        }.count(&mut bytes);

        Ok(Sized::new(bytes, item))
    }

    pub fn parse_script(&mut self) -> Result<Sized<Script>> {
        let mut bytes = 0;
        let body = self.parse_list(|mut me| {
            me.parse_statement_list_item()
        })?.count(&mut bytes);
        Ok(Sized::new(bytes, Script {
            location: None,
            body
        }))
    }
}