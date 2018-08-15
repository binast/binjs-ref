use binjs_io::{ self, Deserialization, Guard, TokenReader, TokenReaderError, TokenWriterError };
pub use binjs_io::{ Serialization, TokenSerializer, TokenWriter };
use binjs_shared::{ IdentifierDefinition, IdentifierReference, Offset };

use std::io::{ Read, Seek };
use std::rc::Rc;


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

impl<R> Deserialization<R, IdentifierDefinition> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self) -> Result<IdentifierDefinition, R::Error> {
        let maybe = self.reader.string()?;
        match maybe {
            None => Err(From::from(TokenReaderError::EmptyString)),
            Some(x) => Ok(IdentifierDefinition(Rc::new(x)))
        }
    }
}
impl<R> Deserialization<R, IdentifierReference> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self) -> Result<IdentifierReference, R::Error> {
        let maybe = self.reader.string()?;
        match maybe {
            None => Err(From::from(TokenReaderError::EmptyString)),
            Some(x) => Ok(IdentifierReference(Rc::new(x)))
        }
    }
}

impl<R> Deserialization<R, Option<IdentifierDefinition>> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self) -> Result<Option<IdentifierDefinition>, R::Error> {
        let maybe = self.reader.string()?;
        match maybe {
            None => Ok(None),
            Some(x) => Ok(Some(IdentifierDefinition(Rc::new(x))))
        }
    }
}
impl<R> Deserialization<R, Option<IdentifierReference>> for Deserializer<R> where R: TokenReader {
    fn deserialize(&mut self) -> Result<Option<IdentifierReference>, R::Error> {
        let maybe = self.reader.string()?;
        match maybe {
            None => Ok(None),
            Some(x) => Ok(Some(IdentifierReference(Rc::new(x))))
        }
    }
}
impl<R, T> Deserialization<R, Vec<T>> for Deserializer<R> where R: TokenReader, Self: Deserialization<R, T> {
    fn deserialize(&mut self) -> Result<Vec<T>, R::Error> {
        let (len, guard) = self.reader.list()?;
        let mut result = Vec::with_capacity(len as usize);
        for _ in 0..len {
            result.push(self.deserialize()?);
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
    pub fn serialize<T>(&mut self, value: T) -> Result<W:: Tree, TokenWriterError> where Self: Serialization<W, T> {
        (self as &mut Serialization<W, T>).serialize(value)
    }
}

impl<W> TokenSerializer<W> for Serializer<W> where W: TokenWriter {
    fn done(self) -> Result<(W::Data, W::Statistics), TokenWriterError> {
        self.writer.done()
    }
}

impl<W> Serialization<W, Option<bool>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: Option<bool>) -> Result<W::Tree, TokenWriterError> {
        self.writer.bool(value)
    }
}
impl<W> Serialization<W, bool> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: bool) -> Result<W::Tree, TokenWriterError> {
        self.writer.bool(Some(value))
    }
}
impl<W> Serialization<W, Option<f64>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: Option<f64>) -> Result<W::Tree, TokenWriterError> {
        self.writer.float(value)
    }
}
impl<W> Serialization<W, f64> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: f64) -> Result<W::Tree, TokenWriterError> {
        self.writer.float(Some(value))
    }
}
impl<'a, W> Serialization<W, &'a Option<bool>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a Option<bool>) -> Result<W::Tree, TokenWriterError> {
        self.writer.bool(value.clone())
    }
}
impl<'a, W> Serialization<W, &'a bool> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a bool) -> Result<W::Tree, TokenWriterError> {
        self.writer.bool(Some(*value))
    }
}
impl<'a, W> Serialization<W, &'a Option<f64>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a Option<f64>) -> Result<W::Tree, TokenWriterError> {
        self.writer.float(value.clone())
    }
}
impl<'a, W> Serialization<W, &'a f64> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a f64) -> Result<W::Tree, TokenWriterError> {
        self.writer.float(Some(*value))
    }
}
impl<'a, W> Serialization<W, Option<&'a str>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: Option<&'a str>) -> Result<W::Tree, TokenWriterError> {
        self.writer.string(value)
    }
}
impl<'a, W> Serialization<W, &'a str> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a str) -> Result<W::Tree, TokenWriterError> {
         self.writer.string(Some(value))
   }
}
impl<'a, W> Serialization<W, &'a Option<String>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a Option<String>) -> Result<W::Tree, TokenWriterError> {
        match *value {
            None => self.writer.string(None),
            Some(ref str) => self.writer.string(Some(&*str))
        }
    }
}
impl<'a, W> Serialization<W, &'a String> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a String) -> Result<W::Tree, TokenWriterError> {
         self.writer.string(Some(&*value))
   }
}
impl<'a, W> Serialization<W, &'a IdentifierDefinition> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a IdentifierDefinition) -> Result<W::Tree, TokenWriterError> {
         self.writer.identifier_definition(Some(&value.0))
   }
}
impl<'a, W> Serialization<W, &'a IdentifierReference> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a IdentifierReference) -> Result<W::Tree, TokenWriterError> {
         self.writer.identifier_reference(Some(&value.0))
   }
}
impl<'a, W> Serialization<W, &'a Option<IdentifierDefinition>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a Option<IdentifierDefinition>) -> Result<W::Tree, TokenWriterError> {
        match value {
            None => self.writer.identifier_definition(None),
            Some(ref x) => self.writer.identifier_definition(Some(&x.0))
        }
   }
}
impl<'a, W> Serialization<W, &'a Option<IdentifierReference>> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, value: &'a Option<IdentifierReference>) -> Result<W::Tree, TokenWriterError> {
        match value {
            None => self.writer.identifier_reference(None),
            Some(ref x) => self.writer.identifier_reference(Some(&x.0))
        }
   }
}
impl<'a, W> Serialization<W, &'a Offset> for Serializer<W> where W: TokenWriter {
    fn serialize(&mut self, _: &'a Offset) -> Result<W::Tree, TokenWriterError> {
         self.writer.offset()
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
        match *format {
            binjs_io::Format::Simple { .. } => {
                let reader = binjs_io::simple::TreeTokenReader::new(source);
                let mut deserializer = Deserializer::new(reader);
                let ast = deserializer.deserialize()?;
                Ok(ast)
            }
            binjs_io::Format::Multipart { .. } => {
                let reader = binjs_io::multipart::TreeTokenReader::new(source)?;
                let mut deserializer = Deserializer::new(reader);
                let ast = deserializer.deserialize()?;
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
            Serializer<binjs_io::multistream::TreeTokenWriter> : Serialization<binjs_io::multistream::TreeTokenWriter, &'a AST>,
            Serializer<binjs_io::repair::Encoder> : Serialization<binjs_io::repair::Encoder, &'a AST>,
            Serializer<binjs_io::xml::Encoder> : Serialization<binjs_io::xml::Encoder, &'a AST>,
            Serializer<binjs_io::multiarith::write::TreeTokenWriter<'a>> : Serialization<binjs_io::multiarith::write::TreeTokenWriter<'a>, &'a AST>,
    {
        match *format {
            binjs_io::Format::Simple { .. } => {
                let writer = binjs_io::simple::TreeTokenWriter::new();
                let mut serializer = Serializer::new(writer);
                serializer.serialize(ast)?;
                let (data, _) = serializer.done()?;
                Ok(Box::new(data))
            }
            binjs_io::Format::Multipart { ref mut targets, .. } => {
                let writer = binjs_io::multipart::TreeTokenWriter::new(targets.clone());
                let mut serializer = Serializer::new(writer);
                serializer.serialize(ast)?;
                let (data, _) = serializer.done()?;
                Ok(Box::new(data))
            }
            binjs_io::Format::MultiStream { ref mut targets, ref options } => {
                targets.reset();
                let writer = binjs_io::multistream::TreeTokenWriter::new(options.clone(), targets.clone());
                let mut serializer = Serializer::new(writer);
                serializer.serialize(&ast)
                    .expect("Could not encode AST");
                let (data, _) = serializer.done()
                    .expect("Could not finalize AST encoding");
                Ok(Box::new(data))
            }
            binjs_io::Format::TreeRePair { ref options } => {
                let writer = binjs_io::repair::Encoder::new(options.clone());
                let mut serializer = Serializer::new(writer);
                serializer.serialize(ast)?;
                let (data, _) = serializer.done()?;
                Ok(Box::new(data))
            }
            binjs_io::Format::XML => {
                let writer = binjs_io::xml::Encoder::new();
                let mut serializer = Serializer::new(writer);
                serializer.serialize(ast)?;
                let (data, _) = serializer.done()?;
                Ok(Box::new(data))
            }
            binjs_io::Format::Arithmetic { ref model, ref options } => {
                let writer = binjs_io::multiarith::write::TreeTokenWriter::new(model.as_ref(), options.clone());
                let mut serializer = Serializer::new(writer);
                serializer.serialize(ast)?;
                let (data, _) = serializer.done()?;
                Ok(Box::new(data))
            }
        }
    }
}
