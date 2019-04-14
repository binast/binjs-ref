use std::io::Cursor;

/// Utility: An extension of `Read` that reads `bytes` bytes and attempts
/// to convert them into a string.
pub trait ReadStr {
    fn read_string(&mut self, bytes: usize) -> Result<String, std::io::Error>;
}
impl ReadStr for Cursor<Vec<u8>> {
    fn read_string(&mut self, bytes: usize) -> Result<String, std::io::Error> {
        use std::io::*;
        let mut buf = Vec::with_capacity(bytes);
        unsafe {
            buf.set_len(bytes);
        }
        self.read_exact(&mut buf)?;
        let result = String::from_utf8(buf)
            .map_err(|_| Error::new(ErrorKind::InvalidData, "Invalid UTF-8 data"))?;
        Ok(result)
    }
}
