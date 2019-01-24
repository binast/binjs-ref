//! Data structures used to read the content streams of a compressed file,
//! i.e. sequences of indices that map either into a static dictionary
//! or into a dynamic dictionary.

use bytes::varnum::ReadVarNum;
use TokenReaderError;

use binjs_shared::SharedString;

use std::io::Cursor;

/// A data structure used to read a content stream, sequences of indices
/// that map either into a static dictionary or into a dynamic dictionary.
///
/// `T` is the type of items in the dictionary, e.g. `Option<u32>`, `IdentifierName`,
/// ...
pub struct DictionaryStreamDecoder<'a, T> {
    /// A dictionary shared between many files.
    shared_dictionary: &'a [T],

    /// A dictionary defined in the prelude of the current file.
    prelude_dictionary: Vec<T>,

    /// A stream of varnums. Each varnum `n` is an index into either the shared_dictionary
    /// (if `n < shared_dictionary.len()`) or the prelude dictionary (otherwise).
    ///
    /// May be `None` if the file does not actually contain a content stream.
    stream: Option<Cursor<Vec<u8>>>,

    /// The name of this dictionary. Used for debugging/error reporting.
    name: SharedString,
}
impl<'a, T> DictionaryStreamDecoder<'a, T> {
    /// Create a decoder from a shared dictionary, a prelude dictionary and a stream of varnum-encoded values.
    pub fn new(
        shared_dictionary: &'a [T],
        prelude_dictionary: Vec<T>,
        name: SharedString,
        stream: Option<Vec<u8>>,
    ) -> Self {
        debug!(target: "read", "DictionaryStreamDecoder::new {} a {}",
            name.as_str(),
            match stream {
                None => "EMPTY stream".to_string(),
                Some(ref vec) => format!("non-empty ({} bytes) stream", vec.len())
            }
        );
        Self {
            shared_dictionary,
            prelude_dictionary,
            stream: stream.map(Cursor::new),
            name,
        }
    }
}
impl<'a, T> Iterator for DictionaryStreamDecoder<'a, T>
where
    T: Clone + std::fmt::Debug,
{
    type Item = Result<T, TokenReaderError>;
    fn next(&mut self) -> Option<Self::Item> {
        debug!(target: "read", "DictionaryStreamDecoder::next on a {} stream",
            match self.stream {
                None => "EMPTY",
                _ => "non-empty"
            }
        );
        match self.stream {
            None => return None,
            Some(ref mut stream) => {
                debug!(target: "read", "DictionaryStreamDecoder::next position: {} / {}",
                    stream.position(),
                    stream.get_ref().len());
                if stream.position() == stream.get_ref().len() as u64 {
                    // We have reached the end of this stream.
                    return None;
                }
                let index = match stream.read_varnum() {
                    Ok(result) => result as usize,
                    Err(err) => return Some(Err(TokenReaderError::ReadError(err))),
                };

                debug!(target: "read", "DictionaryStreamDecoder::next index: {}", index);
                if index < self.shared_dictionary.len() {
                    debug!(target: "read", "That's in the shared dictionary");
                    return Some(Ok(self.shared_dictionary[index].clone()));
                }
                if index < self.shared_dictionary.len() + self.prelude_dictionary.len() {
                    debug!(target: "read", "That's in the prelude dictionary, at index {}: {:?}",
                        index - self.shared_dictionary.len(),
                        self.prelude_dictionary
                    );
                    return Some(Ok(self.prelude_dictionary
                        [index - self.shared_dictionary.len()]
                    .clone()));
                }
                return Some(Err(TokenReaderError::BadDictionaryIndex {
                    index: index as u32,
                    dictionary: self.name.clone(),
                }));
            }
        }
    }
}
