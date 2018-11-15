use std::io::{ Error, Write };

#[derive(Default, AddAssign, Into, From, Clone, Copy)]
pub struct Bytes(usize);

/// An implementation of `Write` that discards its data but remembers
/// how many bytes were written.
#[derive(Default)]
pub struct LengthWriter {
    bytes_written: usize,
}
impl LengthWriter {
    /// Create an empty LengthWriter.
    pub fn new() -> Self {
        LengthWriter {
            bytes_written: 0
        }
    }

    /// Return the number of bytes written since creation.
    pub fn len(&self) -> Bytes {
        self.bytes_written.into()
    }
}

impl Write for LengthWriter {
    /// Do nothing.
    fn flush(&mut self) -> Result<(), Error> {
        Ok(())
    }

    /// Pretend to successfully write bytes, only record the number
    /// of bytes meant to be written.
    fn write(&mut self, buf: &[u8]) -> Result<usize, Error> {
        self.bytes_written += buf.len();
        Ok(buf.len())
    }
}
