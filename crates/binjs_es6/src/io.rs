use binjs_io::{ self, Deserialization, Guard, TokenReader, TokenReaderError, TokenWriterError };
pub use binjs_io::{ Serialization, TokenSerializer, TokenWriter };
use binjs_shared::{ IdentifierDeclaration, IdentifierReference, Offset, self };

use std::io::{ Read, Seek };
use std::rc::Rc;

pub type IOPath = binjs_shared::ast::Path<Rc<String>, (usize, Rc<String>)>;

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
    fn deserialize(&mut self, path: &mut IOPath) -> Result<Option<bool>, R::Error> {
        self.reader.bool_at(path)
    }
}
impl<R> Deserialization<R, bool> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self, path: &mut IOPath) -> Result<bool, R::Error> {
        let maybe = self.reader.bool_at(path)?;
        match maybe {
            None => Err(From::from(TokenReaderError::EmptyBool)),
            Some(x) => Ok(x)
        }
    }
}
impl<R> Deserialization<R, Option<f64>> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self, path: &mut IOPath) -> Result<Option<f64>, R::Error> {
        self.reader.float_at(path)
    }
}
impl<R> Deserialization<R, f64> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self, path: &mut IOPath) -> Result<f64, R::Error> {
        let maybe = self.reader.float_at(path)?;
        match maybe {
            None => Err(From::from(TokenReaderError::EmptyBool)),
            Some(x) => Ok(x)
        }
    }
}
impl<R> Deserialization<R, u32> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self, path: &mut IOPath) -> Result<u32, R::Error> {
        self.reader.unsigned_long_at(path)
    }
}
impl<R> Deserialization<R, Offset> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self, path: &mut IOPath) -> Result<Offset, R::Error> {
        Ok(Offset(self.reader.offset_at(path)?))
    }
}
impl<R> Deserialization<R, Option<String>> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self, path: &mut IOPath) -> Result<Option<String>, R::Error> {
        self.reader.string_at(path)
    }
}
impl<R> Deserialization<R, String> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self, path: &mut IOPath) -> Result<String, R::Error> {
        let maybe = self.reader.string_at(path)?;
        match maybe {
            None => Err(From::from(TokenReaderError::EmptyString)),
            Some(x) => Ok(x)
        }
    }
}

impl<R> Deserialization<R, IdentifierDeclaration> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self, path: &mut IOPath) -> Result<IdentifierDeclaration, R::Error> {
        self.reader.identifier_declaration_at(path)
    }
}
impl<R> Deserialization<R, IdentifierReference> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self, path: &mut IOPath) -> Result<IdentifierReference, R::Error> {
        self.reader.identifier_reference_at(path)
    }
}

impl<R> Deserialization<R, Option<IdentifierDeclaration>> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self, path: &mut IOPath) -> Result<Option<IdentifierDeclaration>, R::Error> {
        self.reader.identifier_declaration_at(path)
            .map(Some)
    }
}
impl<R> Deserialization<R, Option<IdentifierReference>> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self, path: &mut IOPath) -> Result<Option<IdentifierReference>, R::Error> {
        self.reader.identifier_reference_at(path)
            .map(Some)
    }
}
impl<R, T> Deserialization<R, Vec<T>> for Deserializer<R> where R: TokenReader, Self: Deserialization<R, T> {
    fn deserialize(&mut self, path: &mut IOPath) -> Result<Vec<T>, R::Error> {
        let (len, guard) = self.reader.list_at(path)?;
        if len > 0 {
            print_file_structure!(self.reader, "list (length={}) [", len);
        } else {
            print_file_structure!(self.reader, "list (length=0) []");
        }
        let mut result = Vec::with_capacity(len as usize);
        for _ in 0..len {
            result.push(self.deserialize(path)?);
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
    pub fn serialize<T>(&mut self, value: T, path: &mut IOPath) -> Result<W:: Tree, TokenWriterError> where Self: Serialization<W, T> {
        (self as &mut Serialization<W, T>).serialize(value, path)
    }
}

impl<W> TokenSerializer<W> for Serializer<W> where W: TokenWriter {
    fn done(self) -> Result<(W::Data, W::Statistics), TokenWriterError> {
        self.writer.done()
    }
}

impl<W> Serialization<W, Option<bool>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: Option<bool>, path: &mut IOPath) -> Result<W::Tree, TokenWriterError> {
        self.writer.bool_at(value, path)
    }
}
impl<W> Serialization<W, bool> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: bool, path: &mut IOPath) -> Result<W::Tree, TokenWriterError> {
        self.writer.bool_at(Some(value), path)
    }
}
impl<W> Serialization<W, Option<f64>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: Option<f64>, path: &mut IOPath) -> Result<W::Tree, TokenWriterError> {
        self.writer.float_at(value, path)
    }
}
impl<W> Serialization<W, f64> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: f64, path: &mut IOPath) -> Result<W::Tree, TokenWriterError> {
        self.writer.float_at(Some(value), path)
    }
}
impl<W> Serialization<W, u32> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: u32, path: &mut IOPath) -> Result<W::Tree, TokenWriterError> {
        self.writer.unsigned_long_at(value, path)
    }
}
impl<'a, W> Serialization<W, &'a Option<bool>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a Option<bool>, path: &mut IOPath) -> Result<W::Tree, TokenWriterError> {
        self.writer.bool_at(value.clone(), path)
    }
}
impl<'a, W> Serialization<W, &'a bool> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a bool, path: &mut IOPath) -> Result<W::Tree, TokenWriterError> {
        self.writer.bool_at(Some(*value), path)
    }
}
impl<'a, W> Serialization<W, &'a Option<f64>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a Option<f64>, path: &mut IOPath) -> Result<W::Tree, TokenWriterError> {
        self.writer.float_at(value.clone(), path)
    }
}
impl<'a, W> Serialization<W, &'a f64> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a f64, path: &mut IOPath) -> Result<W::Tree, TokenWriterError> {
        self.writer.float_at(Some(*value), path)
    }
}
impl<'a, W> Serialization<W, &'a u32> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a u32, path: &mut IOPath) -> Result<W::Tree, TokenWriterError> {
        self.writer.unsigned_long_at(value.clone(), path)
    }
}
impl<'a, W> Serialization<W, Option<&'a str>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: Option<&'a str>, path: &mut IOPath) -> Result<W::Tree, TokenWriterError> {
        self.writer.string_at(value, path)
    }
}
impl<'a, W> Serialization<W, &'a str> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a str, path: &mut IOPath) -> Result<W::Tree, TokenWriterError> {
         self.writer.string_at(Some(value), path)
   }
}
impl<'a, W> Serialization<W, &'a Option<String>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a Option<String>, path: &mut IOPath) -> Result<W::Tree, TokenWriterError> {
        match *value {
            None => self.writer.string_at(None, path),
            Some(ref str) => self.writer.string_at(Some(&*str), path)
        }
    }
}
impl<'a, W> Serialization<W, &'a String> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a String, path: &mut IOPath) -> Result<W::Tree, TokenWriterError> {
         self.writer.string_at(Some(&*value), path)
   }
}
impl<'a, W> Serialization<W, &'a IdentifierDeclaration> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a IdentifierDeclaration, path: &mut IOPath) -> Result<W::Tree, TokenWriterError> {
         self.writer.identifier_declaration_at(Some(&value.0), path)
   }
}
impl<'a, W> Serialization<W, &'a IdentifierReference> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a IdentifierReference, path: &mut IOPath) -> Result<W::Tree, TokenWriterError> {
         self.writer.identifier_reference_at(Some(&value.0), path)
   }
}
impl<'a, W> Serialization<W, &'a Option<IdentifierDeclaration>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a Option<IdentifierDeclaration>, path: &mut IOPath) -> Result<W::Tree, TokenWriterError> {
        match value {
            None => self.writer.identifier_declaration_at(None, path),
            Some(ref x) => self.writer.identifier_declaration_at(Some(&x.0), path)
        }
   }
}
impl<'a, W> Serialization<W, &'a Option<IdentifierReference>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a Option<IdentifierReference>, path: &mut IOPath) -> Result<W::Tree, TokenWriterError> {
        match value {
            None => self.writer.identifier_reference_at(None, path),
            Some(ref x) => self.writer.identifier_reference_at(Some(&x.0), path)
        }
   }
}
impl<'a, W> Serialization<W, &'a Offset> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, _: &'a Offset, path: &mut IOPath) -> Result<W::Tree, TokenWriterError> {
         self.writer.offset_at(path)
   }
}

pub struct Decoder;
impl Decoder {
    pub fn new() -> Self {
        Decoder
    }
    pub fn decode<'a, R: Read + Seek, AST>(&self, format: &mut binjs_io::Format, source: R) -> Result<AST, TokenReaderError>
        where
            Deserializer<binjs_io::simple::TreeTokenReader<R>> : Deserialization<binjs_io::simple::TreeTokenReader<R>, AST>,
            Deserializer<binjs_io::multipart::TreeTokenReader> : Deserialization<binjs_io::multipart::TreeTokenReader, AST>,
    {
        let mut path = IOPath::new();
        match *format {
            binjs_io::Format::Simple { .. } => {
                let reader = binjs_io::simple::TreeTokenReader::new(source);
                let mut deserializer = Deserializer::new(reader);
                let ast = deserializer.deserialize(&mut path)?;
                Ok(ast)
            }
            binjs_io::Format::Multipart { .. } => {
                let reader = binjs_io::multipart::TreeTokenReader::new(source)?;
                let mut deserializer = Deserializer::new(reader);
                let ast = deserializer.deserialize(&mut path)?;
                Ok(ast)
            }
            _ => unimplemented!()
        }
    }
}
pub struct Encoder;
impl Encoder {
    pub fn new() -> Self {
        Encoder
    }
    pub fn encode<'a, AST>(&self, format: &'a mut binjs_io::Format, ast: &'a AST) -> Result<Box<AsRef<[u8]>>, TokenWriterError>
        where
            Serializer<binjs_io::simple::TreeTokenWriter> : Serialization<binjs_io::simple::TreeTokenWriter, &'a AST>,
            Serializer<binjs_io::multipart::TreeTokenWriter> : Serialization<binjs_io::multipart::TreeTokenWriter, &'a AST>,
            Serializer<binjs_io::xml::Encoder> : Serialization<binjs_io::xml::Encoder, &'a AST>,
            Serializer<binjs_io::multiarith::write::TreeTokenWriter<'a>> : Serialization<binjs_io::multiarith::write::TreeTokenWriter<'a>, &'a AST>
/*
        #[cfg(multistream)]
        where
            Serializer<binjs_io::multistream::TreeTokenWriter> : Serialization<binjs_io::multistream::TreeTokenWriter, &'a AST>,
            Serializer<binjs_io::repair::Encoder> : Serialization<binjs_io::repair::Encoder, &'a AST>,
*/
    {
        let mut path = IOPath::new();
        match *format {
            binjs_io::Format::Simple { .. } => {
                let writer = binjs_io::simple::TreeTokenWriter::new();
                let mut serializer = Serializer::new(writer);
                serializer.serialize(ast, &mut path)?;
                let (data, _) = serializer.done()?;
                Ok(Box::new(data))
            }
            binjs_io::Format::Multipart { ref mut targets, .. } => {
                let writer = binjs_io::multipart::TreeTokenWriter::new(targets.clone());
                let mut serializer = Serializer::new(writer);
                serializer.serialize(ast, &mut path)?;
                let (data, _) = serializer.done()?;
                Ok(Box::new(data))
            }
            #[cfg(multistream)]
            binjs_io::Format::MultiStream { ref mut targets, ref options } => {
                targets.reset();
                let writer = binjs_io::multistream::TreeTokenWriter::new(options.clone(), targets.clone());
                let mut serializer = Serializer::new(writer);
                serializer.serialize(&ast, &mut path)
                    .expect("Could not encode AST");
                let (data, _) = serializer.done()
                    .expect("Could not finalize AST encoding");
                Ok(Box::new(data))
            }
            #[cfg(multistream)]
            binjs_io::Format::TreeRePair { ref options } => {
                let writer = binjs_io::repair::Encoder::new(options.clone());
                let mut serializer = Serializer::new(writer);
                serializer.serialize(ast, &mut path)?;
                let (data, _) = serializer.done()?;
                Ok(Box::new(data))
            }
            binjs_io::Format::XML => {
                let writer = binjs_io::xml::Encoder::new();
                let mut serializer = Serializer::new(writer);
                serializer.serialize(ast, &mut path)?;
                let (data, _) = serializer.done()?;
                Ok(Box::new(data))
            }
            binjs_io::Format::Arithmetic { ref model, ref options } => {
                let writer = binjs_io::multiarith::write::TreeTokenWriter::new(model.as_ref(), options.clone());
                let mut serializer = Serializer::new(writer);
                serializer.serialize(ast, &mut path)?;
                let (data, _) = serializer.done()?;
                Ok(Box::new(data))
            }
        }
    }
}
