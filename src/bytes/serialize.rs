use std::io::{ Error, Read, Seek };

/// A value that may be deserialized from bytes, optionally decompressed.
pub trait Deserializer where Self::Target: Sized {
    type Target;
    fn read<R: Read + Seek>(&self, &mut R) -> Result<Self::Target, Error>;
}
