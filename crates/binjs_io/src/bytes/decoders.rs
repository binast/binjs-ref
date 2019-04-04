use TokenReaderError;

use bytes::float::ReadVarFloat;
use bytes::strings::ReadStr;
use bytes::varnum::ReadVarNum;

use std::io::Cursor;

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
