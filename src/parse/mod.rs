mod from_binary;

use atoms::*;
use kind::*;
use varnum::*;

use easter::prog::*;

use std;
use std::io::*;
use std::rc::Rc;

impl FromBytes for Rc<String> {
    fn from_bytes(bytes: &[u8]) -> std::result::Result<Self, Error> {
        String::from_bytes(bytes).map(Rc::new)
    }
}

impl ToBytes for Rc<String> {
    fn to_bytes(&self) -> Vec<u8> {
        return self.as_ref().to_bytes()
    }
}

/// Read a BinJS stream into an AST
// FIXME: Improve error handling.
pub fn read<T>(src: &mut T) -> Result<(usize, Script)> where T: Read {
    let mut bytes = 0;

    // 1. Read header
    let expected_const_header = "BINJS";
    let mut buf_header : [ u8; 5 ] = [0,0,0,0,0];
    src.read_exact(&mut buf_header)?;
    bytes += buf_header.len();

    match std::str::from_utf8(&buf_header) {
        Ok(ref string) if *string == expected_const_header => {},
        Ok(_) => return Err(Error::new(ErrorKind::InvalidData, "Invalid format header")),
        Err(ref err) => return Err(Error::new(ErrorKind::InvalidData, *err))
    }

    let mut version : u32 = 0;
    bytes += src.read_varnum(&mut version)?;

    if version != 0 {
        return Err(Error::new(ErrorKind::InvalidData, "Invalid format version"));
    }

    // 2. Read strings table
    let (bytes_strings, strings_table) = AtomsTable::<Rc<String>>::read_index(src)?;
    bytes += bytes_strings;

    // 3. Read kinds table
    let (bytes_kinds, kinds_table) = AtomsTable::<Kind>::read_index(src)?;
    bytes += bytes_kinds;

    // 4. Actually parse script
    let mut tree_reader = from_binary::TreeReader::new(src, &strings_table, &kinds_table);
    let script = tree_reader.parse_script()?.count(&mut bytes);

    Ok((bytes, script))
}
