use binjs_io::{ Deserialization, Guard, TokenReader, TokenReaderError };
use binjs_io::{ Serialization, TokenWriter };
use binjs_shared::Offset;



/// A structure used for deserialization purposes.
///
/// Use `Deserializer.deserialize` to read a structure from a token reader.
pub struct Deserializer;

impl<R> Deserialization<R, Option<bool>> for Deserializer where R: TokenReader {
    fn deserialize(&self, reader: &mut R) -> Result<Option<bool>, R::Error> {
        reader.bool()
    }
}
impl<R> Deserialization<R, bool> for Deserializer where R: TokenReader {
    fn deserialize(&self, reader: &mut R) -> Result<bool, R::Error> {
        let maybe = reader.bool()?;
        match maybe {
            None => Err(From::from(TokenReaderError::EmptyBool)),
            Some(x) => Ok(x)
        }
    }
}
impl<R> Deserialization<R, Option<f64>> for Deserializer where R: TokenReader {
    fn deserialize(&self, reader: &mut R) -> Result<Option<f64>, R::Error> {
        reader.float()
    }
}
impl<R> Deserialization<R, f64> for Deserializer where R: TokenReader {
    fn deserialize(&self, reader: &mut R) -> Result<f64, R::Error> {
        let maybe = reader.float()?;
        match maybe {
            None => Err(From::from(TokenReaderError::EmptyBool)),
            Some(x) => Ok(x)
        }
    }
}
impl<R> Deserialization<R, Offset> for Deserializer where R: TokenReader {
    fn deserialize(&self, reader: &mut R) -> Result<Offset, R::Error> {
        Ok(Offset(reader.offset()?))
    }
}
impl<R> Deserialization<R, Option<String>> for Deserializer where R: TokenReader {
    fn deserialize(&self, reader: &mut R) -> Result<Option<String>, R::Error> {
        reader.string()
    }
}
impl<R> Deserialization<R, String> for Deserializer where R: TokenReader {
    fn deserialize(&self, reader: &mut R) -> Result<String, R::Error> {
        let maybe = reader.string()?;
        match maybe {
            None => Err(From::from(TokenReaderError::EmptyString)),
            Some(x) => Ok(x)
        }
    }
}
impl<R, T> Deserialization<R, Vec<T>> for Deserializer where R: TokenReader, Deserializer: Deserialization<R, T> {
    fn deserialize(&self, reader: &mut R) -> Result<Vec<T>, R::Error> {
        let (len, guard) = reader.list()?;
        let mut result = Vec::with_capacity(len as usize);
        for _ in 0..len {
            result.push(Deserializer.deserialize(reader)?);
        }
        guard.done()?;
        Ok(result)
    }
}



/// A structure used for deserialization purposes.
///
/// Use `Serializer.deserialize` to read a structure from a token writer.
pub struct Serializer;

impl<W> Serialization<W, Option<bool>> for Serializer where W: TokenWriter {
    fn serialize(&self, writer: &mut W, value: Option<bool>) -> Result<W::Tree, W::Error> {
        writer.bool(value)
    }
}
impl<W> Serialization<W, bool> for Serializer where W: TokenWriter {
    fn serialize(&self, writer: &mut W, value: bool) -> Result<W::Tree, W::Error> {
        writer.bool(Some(value))
    }
}
impl<W> Serialization<W, Option<f64>> for Serializer where W: TokenWriter {
    fn serialize(&self, writer: &mut W, value: Option<f64>) -> Result<W::Tree, W::Error> {
        writer.float(value)
    }
}
impl<W> Serialization<W, f64> for Serializer where W: TokenWriter {
    fn serialize(&self, writer: &mut W, value: f64) -> Result<W::Tree, W::Error> {
        writer.float(Some(value))
    }
}
impl<'a, W> Serialization<W, &'a Option<bool>> for Serializer where W: TokenWriter {
    fn serialize(&self, writer: &mut W, value: &'a Option<bool>) -> Result<W::Tree, W::Error> {
        writer.bool(value.clone())
    }
}
impl<'a, W> Serialization<W, &'a bool> for Serializer where W: TokenWriter {
    fn serialize(&self, writer: &mut W, value: &'a bool) -> Result<W::Tree, W::Error> {
        writer.bool(Some(*value))
    }
}
impl<'a, W> Serialization<W, &'a Option<f64>> for Serializer where W: TokenWriter {
    fn serialize(&self, writer: &mut W, value: &'a Option<f64>) -> Result<W::Tree, W::Error> {
        writer.float(value.clone())
    }
}
impl<'a, W> Serialization<W, &'a f64> for Serializer where W: TokenWriter {
    fn serialize(&self, writer: &mut W, value: &'a f64) -> Result<W::Tree, W::Error> {
        writer.float(Some(*value))
    }
}
impl<'a, W> Serialization<W, Option<&'a str>> for Serializer where W: TokenWriter {
    fn serialize(&self, writer: &mut W, value: Option<&'a str>) -> Result<W::Tree, W::Error> {
        writer.string(value)
    }
}
impl<'a, W> Serialization<W, &'a str> for Serializer where W: TokenWriter {
    fn serialize(&self, writer: &mut W, value: &'a str) -> Result<W::Tree, W::Error> {
         writer.string(Some(value))
   }
}
impl<'a, W> Serialization<W, &'a Option<String>> for Serializer where W: TokenWriter {
    fn serialize(&self, writer: &mut W, value: &'a Option<String>) -> Result<W::Tree, W::Error> {
        match *value {
            None => writer.string(None),
            Some(ref str) => writer.string(Some(&*str))
        }
    }
}
impl<'a, W> Serialization<W, &'a String> for Serializer where W: TokenWriter {
    fn serialize(&self, writer: &mut W, value: &'a String) -> Result<W::Tree, W::Error> {
         writer.string(Some(&*value))
   }
}
impl<'a, W> Serialization<W, &'a Offset> for Serializer where W: TokenWriter {
    fn serialize(&self, writer: &mut W, _: &'a Offset) -> Result<W::Tree, W::Error> {
         writer.offset()
   }
}
impl<'a, W, T> Serialization<W, &'a Vec<T>> for Serializer where W: TokenWriter, Serializer: Serialization<W, &'a T> {
    fn serialize(&self, writer: &mut W, value: &'a Vec<T>) -> Result<W::Tree, W::Error> {
        let mut children = Vec::with_capacity(value.len());
        for child in value {
            children.push(Serializer.serialize(writer, child)?);
        }
        writer.list(children)
    }
}
