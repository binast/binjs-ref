use std;
use std::io::Seek;

/// An object (typically a reader) that knows its position and size.
pub trait Pos {
    /// The current position in the stream, in bytes.
    fn pos(&mut self) -> usize;

    /// The total number of bytes available in the stream.
    fn size(&mut self) -> usize;
}
impl<T> Pos for T
where
    T: Seek,
{
    fn pos(&mut self) -> usize {
        self.seek(std::io::SeekFrom::Current(0))
            .expect("Could not check position") as usize
    }
    fn size(&mut self) -> usize {
        let old = self
            .seek(std::io::SeekFrom::Current(0))
            .expect("Could not check position");
        let size = self
            .seek(std::io::SeekFrom::End(0))
            .expect("Could not look for end of stream");
        self.seek(std::io::SeekFrom::Start(old))
            .expect("Could not rewind");
        size as usize
    }
}

/// An extension of `Read` that knows how to check that the following few bytes
/// match some value.
pub trait ReadConst {
    /// Succeed if the next few bytes match `bytes`, otherwise fail.
    fn read_const(&mut self, bytes: &[u8]) -> Result<(), std::io::Error>;
}
impl<T> ReadConst for T
where
    T: std::io::Read,
{
    fn read_const(&mut self, data: &[u8]) -> Result<(), std::io::Error> {
        let mut buf = Vec::with_capacity(data.len());
        unsafe {
            buf.set_len(data.len());
        }
        let bytes = self.read(&mut buf)?;
        if bytes != data.len() || &buf as &[u8] != data {
            debug!(target: "read_const", "Invalid data {:?}, expected {:?}",
                String::from_utf8(buf.to_vec()),
                String::from_utf8(data.to_vec())
            );
            let details = String::from_utf8(data.to_vec())
                .unwrap_or_else(|_| "<invalid read_const string>".to_string());
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                details,
            ));
        }
        Ok(())
    }
}

/// A structure used to make sure that every access to a state
/// goes through `try` and that any error poisons the state.
pub struct PoisonLock<S> {
    state: S,
    poisoned: bool,
}

impl<S> PoisonLock<S> {
    pub fn new(state: S) -> Self {
        PoisonLock {
            state,
            poisoned: false,
        }
    }

    /// Access the state for an operation.
    ///
    /// If the operation fails, the state is poisoned.
    ///
    /// # Panics
    ///
    /// If the `ReaderState` is poisoned.
    pub fn try<T, E, F>(&mut self, f: F) -> Result<T, E>
    where
        F: FnOnce(&mut S) -> Result<T, E>,
    {
        assert!(!self.poisoned, "State is poisoned");
        f(&mut self.state).map_err(|err| {
            self.poisoned = true;
            err
        })
    }

    pub fn poison(&mut self) {
        self.poisoned = true;
    }

    pub fn is_poisoned(&self) -> bool {
        self.poisoned
    }
}

/*
pub trait Counter {
    fn internal_make(value: usize) -> Self;
}
#[derive(Default)]
pub struct GenericCounter<T> where T: Counter {
    count: usize,
    phantom: std::marker::PhantomData<T>,
}
impl<T> GenericCounter<T> where T: Counter {
    pub fn new() -> Self {
        GenericCounter {
            count: 0,
            phantom: std::marker::PhantomData,
        }
    }
    pub fn next(&mut self) -> T {
        let result = T::internal_make(self.count);
        self.count += 1;
        result
    }
}
*/
