use context::Name;
use TokenReaderError;
use TokenWriterError;

use bytes::varnum::{ReadVarNum, WriteVarNum};
use util::PosRead;

use std::collections::HashSet;
use std::io::{Read, Write};
use std::path::PathBuf;

/// Highest Brotli compression level.
///
/// Higher values provide better file size but slower compression.
// FIXME: Should probably become a compression parameter.
const BROTLI_QUALITY: u32 = 11;

/// An arbitrary window size for Brotli compression.
///
/// Higher values provide better file size but slower compression.
// FIXME: SHould probably become a compression parameter.
const BROTLI_LG_WINDOW_SIZE: u32 = 20;

/// A compression format used in the prelude.
#[derive(Clone, Copy, Debug)]
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

    /// Return a file extension for this compression format.
    /// Used typically for dumping samples.
    pub fn extension(&self) -> &'static str {
        match *self {
            Compression::Brotli => "br",
            Compression::Identity => "raw",
        }
    }
}

/// The name of a subsection of the prelude.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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
    /// end of the `PreludeDecoder`, this is the name of the next Section (if there is
    /// a next Section) or None (otherwise).
    pub fn name(&self) -> Option<&Name> {
        self.name.as_ref()
    }

    /// Read the next section, update `self.name()`.
    fn read_stream(&mut self) -> Result<Option<(SubSection, Vec<u8>)>, TokenReaderError> {
        // Read next name.
        let mut name = [0; 4];
        if let Err(e) = self.reader.read_exact(&mut name) {
            if e.kind() == std::io::ErrorKind::UnexpectedEof {
                // We have reached the end of the prelude.
                self.name = None;
                return Ok(None);
            }
            return Err(TokenReaderError::ReadError(e));
        }
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

pub struct PreludeEncoder<T>
where
    T: Write,
{
    output: T,
    /// The set of subsections already written.
    subsections: HashSet<SubSection>,
    dump_path: Option<PathBuf>,
}
impl<T> PreludeEncoder<T>
where
    T: Write,
{
    /// Create a PreludeEncoder.
    ///
    /// If `dump_path` is specified, each of the subsections will be dumped at the corresponding path.
    pub fn new(output: T, dump_path: Option<PathBuf>) -> Self {
        PreludeEncoder {
            output,
            subsections: HashSet::new(),
            dump_path,
        }
    }
    pub fn add_subsection(
        &mut self,
        subsection: SubSection,
        compression: Compression,
        data: &[u8],
    ) -> Result<(), TokenWriterError> {
        if !self.subsections.insert(subsection) {
            // The subsection was already present.
            return Err(TokenWriterError::DuplicateEntry(format!(
                "Subsection {:?} of Prelude",
                subsection
            )));
        }

        // Write subsection name
        self.output
            .write_all(subsection.as_bytes())
            .map_err(TokenWriterError::WriteError)?;

        // Write compression method
        self.output
            .write_all(compression.as_bytes())
            .map_err(TokenWriterError::WriteError)?;

        // Write number of bytes + content
        match compression {
            Compression::Identity => {
                self.output
                    .write_varnum(data.len() as u32)
                    .map_err(TokenWriterError::WriteError)?;
                self.output
                    .write_all(data)
                    .map_err(TokenWriterError::WriteError)?;
            }
            Compression::Brotli => {
                // Actually compress the data.
                // Note that the only way I have found to ensure that the `CompressorWriter` completely
                // flushes, regardless of the data, is to drop the `CompressorWriter`.
                let mut brotli_compressed = vec![];
                {
                    let mut brotli = brotli::CompressorWriter::new(
                        &mut brotli_compressed,
                        data.len(),
                        BROTLI_QUALITY,
                        BROTLI_LG_WINDOW_SIZE,
                    );
                    brotli.write_all(&data).unwrap(); // This should never fail.
                }
                self.output
                    .write_varnum(brotli_compressed.len() as u32)
                    .map_err(TokenWriterError::WriteError)?;
                self.output
                    .write_all(&brotli_compressed)
                    .map_err(TokenWriterError::WriteError)?;
                self.dump_if_necessary(&brotli_compressed, compression)
                    .map_err(TokenWriterError::WriteError)?;
            }
        }

        self.dump_if_necessary(data, Compression::Identity)
            .map_err(TokenWriterError::WriteError)?;
        Ok(())
    }

    pub fn into_output(self) -> T {
        self.output
    }

    fn dump_if_necessary(
        &mut self,
        data: &[u8],
        compression: Compression,
    ) -> Result<(), std::io::Error> {
        if let Some(ref path) = self.dump_path {
            // Create directory if necessary.
            let dir = path.parent().unwrap();
            std::fs::DirBuilder::new().recursive(true).create(dir)?;

            // Give a name to the file.
            let extension = match path.extension() {
                None => std::ffi::OsString::from(compression.extension()),
                Some(ext) => {
                    let mut as_os_string = ext.to_os_string();
                    as_os_string.push(compression.extension());
                    as_os_string
                }
            };

            let path = path.with_extension(extension);
            std::fs::write(path, &data)?;
        }
        Ok(())
    }
}

#[test]
fn test_context_prelude() {
    use std::io::Cursor;
    for compression in &[Compression::Brotli, Compression::Identity] {
        eprintln!("test_context_prelude: Testing with {:?}", compression);

        // Encode.
        let mut encoder = PreludeEncoder::new(vec![], None);
        let mut reference = vec![];

        for (noise, subsection) in &[(7, SubSection::Float), (17, SubSection::LiteralStringLen)] {
            // Fill subsection with arbitrary data.
            let len = 1001 * *noise;
            let mut data = Vec::with_capacity(len);
            for i in 0..len {
                data.push((i * i % 256) as u8);
            }

            // Add subsection.
            encoder
                .add_subsection(*subsection, *compression, &data)
                .unwrap();

            // Keep for future check.
            reference.push(data);
        }

        let encoded = encoder.into_output();
        let encoded_len = encoded.len();

        // Now decode.
        let mut decoder = PreludeDecoder::new(Cursor::new(encoded));
        {
            let mut decoder = &mut decoder;

            let subsection = decoder.next().expect("End of prelude").expect("Read error");
            assert_eq!(subsection.0, SubSection::Float);
            assert_eq!(subsection.1, reference[0]);

            let subsection = decoder.next().expect("End of prelude").expect("Read error");
            assert_eq!(subsection.0, SubSection::LiteralStringLen);
            assert_eq!(subsection.1, reference[1]);

            assert!(decoder.next().is_none());
        }

        // Have we consumed all bytes?
        let leftover = decoder.into_input();
        assert_eq!(leftover.position() as usize, encoded_len);
    }
}
