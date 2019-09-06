use context::varnum::{ReadVaru32, WriteVaru32};

use std::io::{Error, ErrorKind, Read, Write};

/// The magic header for external strings dictionaries.
const DICTIONARY_MAGIC_HEADER: &'static [u8; 7] = b"astdict";

/// A table of strings.
///
/// Generally wrapped either in a StringPrelude (if read as part of a compressed binast
/// file) or in an external `StringDictionary`.
pub struct StringTable {
    strings: Vec<String>,
}

// ---- Reading

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

#[test]
fn test_empty_external_string_dictionary() {
    let bytes = b"astdict\0";
    let mut input = std::io::Cursor::new(&bytes);
    let dictionary = ExternalStringDictionary::read(&mut input).unwrap();
    assert_eq!(dictionary.strings().len(), 0);
}

// ----- Writing

impl StringTable {
    pub fn new() -> Self {
        StringTable {
            strings: Vec::new(),
        }
    }
    pub fn with_capacity(capacity: usize) -> Self {
        StringTable {
            strings: Vec::with_capacity(capacity),
        }
    }
    pub fn from_strings(strings: Vec<String>) -> Self {
        StringTable { strings }
    }
    pub fn add_string(&mut self, string: String) {
        self.strings.push(string);
    }
    pub fn write(&self, mut out: &mut dyn Write) -> Result<(), Error> {
        // Write the length.
        out.write_varu32(self.strings.len() as u32)?;

        // Write each string, NUL-terminated.
        for string in &self.strings {
            // Write bytes one by one to handle escapes.
            for byte in string.as_bytes() {
                match byte {
                    0 | 1 =>
                    /* escape byte */
                    {
                        out.write_all(&[1, *byte])
                    }
                    _ => out.write_all(&[*byte]),
                }?;
            }
            // NUL terminator
            out.write_all(&[0])?;
        }

        Ok(())
    }
}

#[test]
fn test_write_and_read_string_tables() {
    use std::io::Cursor;

    // Generate samples with a few embedded '0's and '1's.
    let samples: Vec<String> = (0..300)
        .map(|len| {
            (0..len)
                .map(|i| {
                    let b = i as u8;
                    let c: char = b.into();
                    c
                })
                .collect()
        })
        .collect();

    let reference = StringTable::from_strings(samples);

    // Write table, read it back.
    let mut buf = Vec::new();
    reference.write(&mut buf).unwrap();

    let result = StringTable::read(&mut Cursor::new(buf)).unwrap();

    assert_eq!(result.strings(), reference.strings());
}
