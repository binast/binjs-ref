//! Miscellaneous utilities used for entropy (de)coding.

use std::io::Read;

/// Utility: an extension of `Read` with a method `expect` that may be
/// used to ensure the presence of a sequence of bytes.
pub trait Expect {
    /// Fail if the following bytes are not identical to `bytes`.
    ///
    /// State of the stream is not specified in case of failure. In
    /// case of success, exactly `bytes` are consumed.
    fn expect(&mut self, bytes: &[u8]) -> Result<(), std::io::Error>;
}

impl<T> Expect for T
where
    T: Read,
{
    fn expect(&mut self, bytes: &[u8]) -> Result<(), std::io::Error> {
        let mut buf = [b'?'];
        for b in bytes {
            self.read_exact(&mut buf)?;
            if b != &buf[0] {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "Not the expected bytes",
                ));
            }
        }
        Ok(())
    }
}
