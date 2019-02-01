//! Data structures used to read the prelude of a compressed file,
//! i.e. the definitions of additional dictionaries.

use entropy::rw::*;
use entropy::util::*;
use TokenReaderError;

use bytes::float::ReadVarFloat;
use bytes::varnum::ReadVarNum;

use std::io::{Cursor, Read};

/// A name read from the prelude.
///
/// It may be either a section name or a stream name.
#[derive(Debug)]
pub enum Name<T> {
    /// The name of a stream.
    Stream(T),

    /// The name of a section.
    Section(T),

    /// No name has been read yet.
    Nothing,
}

/// Decode a section of compressed streams into
/// sequences of names and bytes.
///
/// We expect the following binary structure:
/// ```md
/// - `[[section1]]`
/// - `[stream1]`
/// - compression_format (at the time of this writing, only `brotli;` is accepted)
/// - stream_length (varnum)
/// - stream_content ([u8; stream_length])
/// - `[stream2]`
/// - ...
/// - `[[section2]]`
/// ```
///
/// This decoder accept an input immediately **after** `[[section1]]` (i.e. the bytes `b"[[section1]]"`
/// must already have been consumed) and stops parsing immediately **after** `[[section2]]` (i.e.
/// it consumes the bytes `b"[[section2]]"`).
pub struct SectionDecoder<T>
where
    T: Read,
{
    /// The latest name read.
    ///
    /// This may be the name of a section or a stream.
    name: Name<NameData>,

    /// The input from which data will be read.
    input: T,

    /// The next pending char.
    next_char: [u8; 1],
}
impl<T> SectionDecoder<T>
where
    T: Read,
{
    /// Create a new SectionDecoder to parse a given input.
    pub fn new(input: T) -> Self {
        Self {
            name: Name::Nothing,
            input,
            next_char: [b'?'], // Arbitrary char.
        }
    }

    /// The latest name read. This is either the name of a stream
    /// or the name of the next section.
    ///
    /// This method returns
    ///
    /// - `Name::Nothing` if we have not encountered a name yet;
    /// - `Name::Stream(&'foobar')` if the latest name we have encountered is `[foobar]`
    ///    (single-brackets indicate a stream name)
    /// - `Name::Section(&'snafu')` if the latest name we have encountered is `[[snafu]]`
    ///    (double-brackets indicate a section name).
    pub fn name(&self) -> Name<&[u8]> {
        match self.name {
            Name::Nothing => Name::Nothing,
            Name::Stream(ref s) => Name::Stream(s.as_slice()),
            Name::Section(ref s) => Name::Section(s.as_slice()),
        }
    }

    /// Finalize this SectionReader and recover the input.
    pub fn into_input(self) -> T {
        self.input
    }

    /// Continue reading a name.
    ///
    /// This method is designed to be called from `read_stream`, after `read_stream`
    /// has read the `[` that precedes the name.
    fn read_name(&mut self) -> Result<NameData, TokenReaderError> {
        if self.next_char[0] == b'[' {
            self.input
                .read_exact(&mut self.next_char)
                .map_err(TokenReaderError::ReadError)?;
        }
        let mut name = NameData::new();
        while self.next_char[0] != b']' && name.len() < NAME_MAX_LEN {
            name.push(self.next_char[0]);
            self.input
                .read_exact(&mut self.next_char)
                .map_err(TokenReaderError::ReadError)?;
        }
        Ok(name)
    }

    /// Read the next stream.
    ///
    /// A stream starts with a name `[foo_bar]`, followed by a number of compressed
    /// bytes, followed by the actual bytes. In case of success, this returns the
    /// buffer containing the decompressed bytes. To access the name of the stream,
    /// use `self.name()`.
    fn read_stream(&mut self) -> Result<Option<Vec<u8>>, TokenReaderError> {
        debug!(target: "read", "SectionDecoder::read_stream");
        self.input
            .expect(b"[")
            .map_err(|e| TokenReaderError::ReadError(e))?;

        self.input
            .read_exact(&mut self.next_char)
            .map_err(TokenReaderError::ReadError)?;

        if self.next_char[0] == b'[' {
            debug!(target: "read", "SectionDecoder::read_stream it's a [[");
            // `[[` means that we have ended the current section. Fetch the next name, but we're done.
            let section_name = self.read_name()?;
            debug!(target: "read", "SectionDecoder::read_stream the next section is {:?}",
                std::str::from_utf8(&section_name));
            self.name = Name::Section(section_name);
            self.input
                .expect(b"]")
                .map_err(TokenReaderError::ReadError)?;
            return Ok(None);
        } else {
            // Find the name of the current stream.
            self.name = Name::Stream(self.read_name()?);
        }

        debug!(target: "read", "SectionDecoder::read_stream reading compression format");
        // Read the compression format.
        self.input
            .expect(FORMAT_BROTLI) // FIXME: Extend to other formats.
            .map_err(TokenReaderError::ReadError)?;

        debug!(target: "read", "SectionDecoder::read_stream reading byte length");
        // Read the byte length.
        let byte_len = self
            .input
            .read_varnum()
            .map_err(TokenReaderError::ReadError)? as usize;

        debug!(target: "read", "SectionDecoder::read_stream preparing to decompress {} bytes", byte_len);

        // Extract slice.
        let mut buf = Vec::with_capacity(byte_len); // FIXME: We may want some maximal size here.
        unsafe {
            buf.set_len(byte_len);
        }
        self.input
            .read_exact(buf.as_mut_slice())
            .map_err(TokenReaderError::ReadError)?;

        // Note: This is the only way I have found to get Brotli to decompress all the
        // bytes we feed it.
        use std::io::Write;

        let mut result = Vec::new();
        {
            let mut brotli = brotli::DecompressorWriter::new(&mut result, 32);
            brotli
                .write_all(&buf)
                .map_err(TokenReaderError::ReadError)?;
            brotli.flush().map_err(TokenReaderError::ReadError)?;
        }

        debug!(target: "read", "SectionDecoder::read_stream I have decompressed {} bytes into {} bytes",
            byte_len,
            result.len());
        Ok(Some(result))
    }
}

impl<'a, T> Iterator for &'a mut SectionDecoder<T>
where
    T: Read,
{
    type Item = Result<(NameData, Vec<u8>), TokenReaderError>;
    /// Read the next stream.
    ///
    /// If there is no stream left in this section, this returns `None`.
    /// In such a case, use `name()` to get the name of the next
    /// section.
    fn next(&mut self) -> Option<Self::Item> {
        match self.read_stream() {
            Ok(Some(buf)) => match self.name {
                Name::Stream(ref stream) => Some(Ok((stream.clone(), buf))),
                ref other => panic!("SectionDecoder: Unexpected state: {:?}", other),
            },
            Ok(None) => None,
            Err(err) => Some(Err(err)),
        }
    }
}

pub struct VarFloatDecoder {
    data: Cursor<Vec<u8>>,
}
impl VarFloatDecoder {
    pub fn new(data: Vec<u8>) -> Self {
        VarFloatDecoder {
            data: Cursor::new(data),
        }
    }
}
impl Iterator for VarFloatDecoder {
    type Item = Result<Option<binjs_shared::F64>, TokenReaderError>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.data.position() == self.data.get_ref().len() as u64 {
            return None;
        }
        match self.data.read_maybe_varfloat() {
            Ok(Some(result)) => Some(Ok(Some(result.into()))),
            Ok(None) => Some(Ok(None)),
            Err(err) => Some(Err(TokenReaderError::ReadError(err))),
        }
    }
}

pub struct MaybeVarNumDecoder {
    data: Cursor<Vec<u8>>,
}
impl MaybeVarNumDecoder {
    pub fn new(data: Vec<u8>) -> Self {
        MaybeVarNumDecoder {
            data: Cursor::new(data),
        }
    }
}
impl Iterator for MaybeVarNumDecoder {
    type Item = Result<Option<u32>, TokenReaderError>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.data.position() == self.data.get_ref().len() as u64 {
            return None;
        }
        match self.data.read_maybe_varnum() {
            Ok(result) => Some(Ok(result)),
            Err(err) => Some(Err(TokenReaderError::ReadError(err))),
        }
    }
}

pub struct StringDecoder {
    data: Cursor<Vec<u8>>,
    lengths: Cursor<Vec<u8>>,
}
impl StringDecoder {
    pub fn try_new(
        data: Option<Vec<u8>>,
        lengths: Option<Vec<u8>>,
    ) -> Result<StringDecoder, TokenReaderError> {
        match (data, lengths) {
            (Some(data), Some(lengths)) => Ok(Self::new(data, lengths)),
            (None, None) => Ok(Self::new(vec![], vec![])),
            _ => Err(TokenReaderError::BadStringDecoder),
        }
    }
    pub fn new(data: Vec<u8>, lengths: Vec<u8>) -> Self {
        StringDecoder {
            data: Cursor::new(data),
            lengths: Cursor::new(lengths),
        }
    }
}
impl Iterator for StringDecoder {
    type Item = Result<Option<String>, TokenReaderError>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.lengths.position() == self.lengths.get_ref().len() as u64 {
            debug!(target: "read", "StringDecoder::next - end reached");
            return None;
        }
        fn aux(myself: &mut StringDecoder) -> Result<Option<String>, TokenReaderError> {
            let byte_len = myself
                .lengths
                .read_maybe_varnum()
                .map_err(TokenReaderError::ReadError)?;
            match byte_len {
                None => Ok(None),
                Some(byte_len) => {
                    debug!(target: "read", "StringDecoder::next - byte length: {}", byte_len);
                    let value = myself
                        .data
                        .read_string(byte_len as usize)
                        .map_err(TokenReaderError::ReadError)?;
                    debug!(target: "read", "StringDecoder::next - value: {:?}", value);
                    Ok(Some(value))
                }
            }
        }
        Some(aux(self))
    }
}
