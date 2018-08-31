use binjs_io::{ Deserialization, Guard, TokenReader, TokenReaderError };
pub use binjs_io::{ Serialization, TokenSerializer, TokenWriter };
use binjs_shared::Offset;



/// A structure used for deserialization purposes.
pub struct Deserializer<R> where R: TokenReader {
    pub reader: R
}
impl<R> Deserializer<R> where R: TokenReader {
    pub fn new(reader: R) -> Self {
        Self {
            reader
        }
    }
}


impl<R> Deserialization<R, Option<bool>> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self) -> Result<Option<bool>, R::Error> {
        self.reader.bool()
    }
}
impl<R> Deserialization<R, bool> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self) -> Result<bool, R::Error> {
        let maybe = self.reader.bool()?;
        match maybe {
            None => Err(From::from(TokenReaderError::EmptyBool)),
            Some(x) => Ok(x)
        }
    }
}
impl<R> Deserialization<R, Option<f64>> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self) -> Result<Option<f64>, R::Error> {
        self.reader.float()
    }
}
impl<R> Deserialization<R, f64> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self) -> Result<f64, R::Error> {
        let maybe = self.reader.float()?;
        match maybe {
            None => Err(From::from(TokenReaderError::EmptyBool)),
            Some(x) => Ok(x)
        }
    }
}
impl<R> Deserialization<R, u32> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self) -> Result<u32, R::Error> {
        self.reader.unsigned_long()
    }
}
impl<R> Deserialization<R, Offset> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self) -> Result<Offset, R::Error> {
        Ok(Offset(self.reader.offset()?))
    }
}
impl<R> Deserialization<R, Option<String>> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self) -> Result<Option<String>, R::Error> {
        self.reader.string()
    }
}
impl<R> Deserialization<R, String> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self) -> Result<String, R::Error> {
        let maybe = self.reader.string()?;
        match maybe {
            None => Err(From::from(TokenReaderError::EmptyString)),
            Some(x) => Ok(x)
        }
    }
}
impl<R, T> Deserialization<R, Vec<T>> for Deserializer<R> where R: TokenReader, Self: Deserialization<R, T> {
    fn deserialize(&mut self) -> Result<Vec<T>, R::Error> {
        let (len, guard) = self.reader.list()?;
        if len > 0 {
            print_file_structure!(self.reader, "list (length={}) [", len);
        } else {
            print_file_structure!(self.reader, "list (length=0) []");
        }
        let mut result = Vec::with_capacity(len as usize);
        for _ in 0..len {
            result.push(self.deserialize()?);
        }
        if len > 0 {
            print_file_structure!(self.reader, "]");
        }
        guard.done()?;
        Ok(result)
    }
}



/// A structure used for deserialization purposes.
///
/// Use `Serializer.deserialize` to read a structure from a token self.writer.
pub struct Serializer<W> where W: TokenWriter {
    pub writer: W
}
impl<W> Serializer<W> where W: TokenWriter {
    pub fn new(writer: W) -> Self {
        Self {
            writer
        }
    }
    pub fn serialize<T>(&mut self, value: T) -> Result<W:: Tree, W::Error> where Self: Serialization<W, T> {
        (self as &mut Serialization<W, T>).serialize(value)
    }
}

impl<W> TokenSerializer<W> for Serializer<W> where W: TokenWriter {
    fn done(self) -> Result<(W::Data, W::Statistics), W::Error> {
        self.writer.done()
    }
}

impl<W> Serialization<W, Option<bool>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: Option<bool>) -> Result<W::Tree, W::Error> {
        self.writer.bool(value)
    }
}
impl<W> Serialization<W, bool> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: bool) -> Result<W::Tree, W::Error> {
        self.writer.bool(Some(value))
    }
}
impl<W> Serialization<W, Option<f64>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: Option<f64>) -> Result<W::Tree, W::Error> {
        self.writer.float(value)
    }
}
impl<W> Serialization<W, f64> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: f64) -> Result<W::Tree, W::Error> {
        self.writer.float(Some(value))
    }
}
impl<W> Serialization<W, u32> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: u32) -> Result<W::Tree, W::Error> {
        self.writer.unsigned_long(value)
    }
}
impl<'a, W> Serialization<W, &'a Option<bool>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a Option<bool>) -> Result<W::Tree, W::Error> {
        self.writer.bool(value.clone())
    }
}
impl<'a, W> Serialization<W, &'a bool> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a bool) -> Result<W::Tree, W::Error> {
        self.writer.bool(Some(*value))
    }
}
impl<'a, W> Serialization<W, &'a Option<f64>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a Option<f64>) -> Result<W::Tree, W::Error> {
        self.writer.float(value.clone())
    }
}
impl<'a, W> Serialization<W, &'a f64> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a f64) -> Result<W::Tree, W::Error> {
        self.writer.float(Some(*value))
    }
}
impl<'a, W> Serialization<W, &'a u32> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a u32) -> Result<W::Tree, W::Error> {
        self.writer.unsigned_long(value.clone())
    }
}
impl<'a, W> Serialization<W, Option<&'a str>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: Option<&'a str>) -> Result<W::Tree, W::Error> {
        self.writer.string(value)
    }
}
impl<'a, W> Serialization<W, &'a str> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a str) -> Result<W::Tree, W::Error> {
         self.writer.string(Some(value))
   }
}
impl<'a, W> Serialization<W, &'a Option<String>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a Option<String>) -> Result<W::Tree, W::Error> {
        match *value {
            None => self.writer.string(None),
            Some(ref str) => self.writer.string(Some(&*str))
        }
    }
}
impl<'a, W> Serialization<W, &'a String> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a String) -> Result<W::Tree, W::Error> {
         self.writer.string(Some(&*value))
   }
}
impl<'a, W> Serialization<W, &'a Offset> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, _: &'a Offset) -> Result<W::Tree, W::Error> {
         self.writer.offset()
   }
}

/*
impl<'a, W, T> Serialization<W, &'a Vec<T>> for Serializer<W> where W: TokenWriter, Serializer<W>: Serialization<W, &'a T> {
    fn serialize(&mut self, value: &'a Vec<T>) -> Result<W::Tree, W::Error> {
        let mut children = Vec::with_capacity(value.len());
        for child in value.iter() {
            children.push(self.serialize(child)?);
        }
        self.writer.list(children)
    }
}
*/
