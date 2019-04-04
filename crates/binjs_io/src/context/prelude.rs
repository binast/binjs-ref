use context::Name;
use TokenReaderError;

use bytes::varnum::ReadVarNum;
use util::PosRead;

use std::io::Read;

/// A compression format used in the prelude.
#[derive(Clone, Debug)]
pub enum Compression {
    /// The subsection is compressed using brotli.
    Brotli,
    /// The subsection is uncompressed.
    Identity,
}
impl Compression {
    /// Return the representation as a 4 bytes name.
    pub fn as_bytes(&self) -> &'static Name {
        match *self {
            Compression::Brotli => b"BROT",
            Compression::Identity => b"NONE",
        }
    }

    /// Parse the 4 bytes name.
    pub fn from_bytes(bytes: &Name) -> Option<Compression> {
        match bytes {
            b"BROT" => Some(Compression::Brotli),
            b"NONE" => Some(Compression::Identity),
            _ => None,
        }
    }
}

/// The name of a subsection of the prelude.
#[derive(Clone, Debug)]
pub enum SubSection {
    /// Definitions of floating-point values.
    Float,

    /// Definitions of unsigned longs.
    UnsignedLong,

    /// Definitions of list lengths.
    ListLength,

    /// Definitions of property keys: single string concatenation of all property keys.
    /// Needs `PropertyKeyLen` to be cut back into a sequence of strings.
    PropertyKeyContent,

    /// Definitions of identifier names, as a single string concatenation of all identifier names.
    /// Needs `IdentifierNameLen` to be cut back into a sequence of strings.
    IdentifierNameContent,

    /// Definitions of literal strings, as a single string concatenation of all literal strings.
    /// Needs `LiteralStringLen` to be cut back into a sequence of strings.
    LiteralStringContent,

    /// The sequence of length of strings in `PropertyKeyContent`.
    PropertyKeyLen,

    /// The sequence of length of strings in `IdentifierNameLen`.
    IdentifierNameLen,

    /// The sequence of length of strings in `LiteralStringLen`.
    LiteralStringLen,
}
impl SubSection {
    /// Return the representation as a 4 bytes name.
    pub fn as_bytes(&self) -> &'static Name {
        match *self {
            SubSection::Float => b"FLOA",
            SubSection::UnsignedLong => b"ULON",
            SubSection::ListLength => b"LIST",
            SubSection::PropertyKeyContent => b"PROC",
            SubSection::PropertyKeyLen => b"PROL",
            SubSection::IdentifierNameContent => b"IDEC",
            SubSection::IdentifierNameLen => b"IDEL",
            SubSection::LiteralStringContent => b"STRC",
            SubSection::LiteralStringLen => b"STRL",
        }
    }

    /// Parse the 4 bytes name.
    pub fn from_bytes(bytes: &Name) -> Option<Self> {
        match bytes {
            b"FLOA" => Some(SubSection::Float),
            b"ULON" => Some(SubSection::UnsignedLong),
            b"LIST" => Some(SubSection::ListLength),
            b"PROC" => Some(SubSection::PropertyKeyContent),
            b"PROL" => Some(SubSection::PropertyKeyLen),
            b"IDEC" => Some(SubSection::IdentifierNameContent),
            b"IDEL" => Some(SubSection::IdentifierNameLen),
            b"STRC" => Some(SubSection::LiteralStringContent),
            b"STRL" => Some(SubSection::LiteralStringLen),
            _ => None,
        }
    }
}

/// Decode a prelude into an iterator of subsections.
pub struct PreludeDecoder<T>
where
    T: Read,
{
    reader: T,
    name: Option<Name>,
}
impl<T> PreludeDecoder<T>
where
    T: Read,
{
    /// Create a `PreludeDecoder`.
    ///
    /// The `reader` is expected to contain the *content* of the prelude, *without*
    /// its name `PREL`.
    pub fn new(reader: T) -> Self {
        PreludeDecoder { reader, name: None }
    }

    /// Finalize and recover the reader.
    pub fn into_input(self) -> T {
        self.reader
    }

    /// Return the latest name encountered by the `PreludeDecoder`.
    ///
    /// Usually, this is the name of the latest SubSection. However, if we have reached the
    /// end of the `PreludeDecoder`, this is the name of the next Section
    pub fn name(&self) -> Option<&Name> {
        self.name.as_ref()
    }

    /// Read the next section, update `self.name()`.
    fn read_stream(&mut self) -> Result<Option<(SubSection, Vec<u8>)>, TokenReaderError> {
        // Read next name.
        let mut name = [0; 4];
        self.reader
            .read_exact(&mut name)
            .map_err(TokenReaderError::ReadError)?;
        let section = SubSection::from_bytes(&name);
        self.name = Some(name);
        let section = match section {
            None => {
                // We have reached the end of the section.
                return Ok(None);
            }
            Some(section) => section,
        };

        // Read compression format.
        let mut name = [0; 4];
        self.reader
            .read_exact(&mut name)
            .map_err(TokenReaderError::ReadError)?;
        let compression = Compression::from_bytes(&name).ok_or_else(|| {
            TokenReaderError::BadCompression(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!(
                    "Invalid compression format {:?} for prelude subsection {:?}",
                    name, section
                ),
            ))
        })?;

        // Read byte length.
        let byte_len = self
            .reader
            .read_varnum()
            .map_err(TokenReaderError::ReadError)? as usize;

        // Now read and decode data.
        let data = match compression {
            Compression::Identity => {
                let mut result = Vec::with_capacity(byte_len);
                result.resize_with(byte_len as usize, u8::default);
                self.reader
                    .read_exact(&mut result)
                    .map_err(TokenReaderError::ReadError)?;
                result
            }
            Compression::Brotli => {
                let mut result = Vec::new();
                let mut slice = PosRead::new(self.reader.by_ref().take(byte_len as u64));
                brotli::BrotliDecompress(&mut slice, &mut result)
                    .map_err(TokenReaderError::ReadError)?;
                // Ensure that all bytes have been read.
                if slice.pos() != byte_len {
                    return Err(TokenReaderError::BadLength {
                        expected: byte_len,
                        got: slice.pos(),
                    });
                }
                result
            }
        };
        Ok(Some((section, data)))
    }
}

impl<'a, T> Iterator for &'a mut PreludeDecoder<T>
where
    T: Read,
{
    type Item = Result<(SubSection, Vec<u8>), TokenReaderError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.read_stream() {
            Ok(Some(result)) => Some(Ok(result)),
            Ok(None) => None,
            Err(err) => Some(Err(err)),
        }
    }
}
