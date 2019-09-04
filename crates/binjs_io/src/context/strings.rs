use context::varnum::ReadVaru32;

use std::io::{Error, ErrorKind, Read};

/// The magic header for external strings dictionaries.
const DICTIONARY_MAGIC_HEADER: &'static [u8; 7] = b"astdict";

/// A table of strings.
///
/// Generally wrapped either in a StringPrelude (if read as part of a compressed binast
/// file) or in an external `StringDictionary`.
pub struct StringTable {
    strings: Vec<String>,
}

impl StringTable {
    /// The strings in the table, in the order in which they appear in the stream.
    pub fn strings(&self) -> &[String] {
        self.strings.as_ref()
    }

    /// Read a string table from a stream.
    ///
    /// Consumes exactly the bytes used to define the string table.
    pub fn read<R: Read>(input: &mut R) -> Result<Self, Error> {
        let string_count = input.read_varu32_no_normalization()?.value;

        // The strings we decode, in the order in which we decode them.
        let mut strings = Vec::new();

        for _ in 0..string_count {
            let mut chars = Vec::new();
            // Read bytes one by one to handle escapes.
            loop {
                let mut buf = [0];
                input.read_exact(&mut buf)?;

                let byte = match buf[0] {
                    0 => {
                        // NUL terminator.
                        break;
                    }
                    1 => {
                        // Escaped char.
                        input.read_exact(&mut buf)?;
                        match buf[0] {
                            0 => 0,
                            1 => 1,
                            _ => return Err(Error::new(ErrorKind::InvalidData, "Invalid escape")),
                        }
                    }
                    byte => byte,
                };
                chars.push(byte);
            }
            // Attempt to decode string as UTF-8.
            let string = String::from_utf8(chars)
                .or_else(|_| Err(Error::new(ErrorKind::InvalidData, "Invalid utf-8")))?;
            strings.push(string);
        }

        Ok(StringTable { strings })
    }
}

pub struct ExternalStringDictionary {
    strings: StringTable,
}

impl ExternalStringDictionary {
    /// The strings in this dictionary, in the order in which they appear in the stream.
    pub fn strings(&self) -> &[String] {
        self.strings.strings()
    }

    /// Read a string dictionary from a stream.
    ///
    /// Consumes exactly the bytes used to define the string dictionary.
    pub fn read<R: Read>(input: &mut R) -> Result<Self, Error> {
        // Check magic header
        let mut header_bytes = [0; 7];
        assert_eq!(DICTIONARY_MAGIC_HEADER.len(), header_bytes.len());

        input.read_exact(&mut header_bytes)?;
        if &header_bytes != DICTIONARY_MAGIC_HEADER {
            return Err(Error::new(
                ErrorKind::InvalidData,
                "Format error: Not an external strings dictionary",
            ));
        }

        // Read contents
        let strings = StringTable::read(input)?;
        Ok(ExternalStringDictionary { strings })
    }
}
