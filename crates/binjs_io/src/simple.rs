//! Minimal implementation of encoding/decoding to binary.
//! Used for testing purposes. Not meant to be
//! included in release builds.

use bytes;
use io::*;
use ::{ TokenReaderError, TokenWriterError };
use util::{ PoisonLock, Pos, ReadConst };

use std;
use std::cell::RefCell;
use std::io::{ Read, Seek };
use std::rc::Rc;

/// The state of the `TreeTokenReader`.
///
/// Use a `PoisonLock` to access this state.
struct ReaderState<R> where R: Read + Seek {
    reader: R,
}
impl<R> ReaderState<R> where R: Read + Seek {
    pub fn position(&mut self) -> u64 {
        self.reader.pos() as u64
    }

    pub fn read_u32(&mut self) -> Result<u32, TokenReaderError> {
        let mut buf : [u8; 4] = [0, 0, 0, 0];
        debug_assert!(std::mem::size_of::<u32>() == std::mem::size_of_val(&buf));
        self.reader.read(&mut buf)
            .map_err(TokenReaderError::ReadError)?;

        let result =
              buf[0] as u32
            | (buf[1] as u32) << 8
            | (buf[2] as u32) << 16
            | (buf[3] as u32) << 24;
        Ok(result)
    }

    fn read_string(&mut self) -> Result<String, TokenReaderError> {
        let mut bytes = Vec::new();
        let mut buf: [u8;1] = [0];
        loop {
            self.reader.read(&mut buf)
                .map_err(TokenReaderError::ReadError)?;
            if buf[0] == 0 {
                return String::from_utf8(bytes)
                    .map_err(TokenReaderError::Encoding)
            }
            bytes.push(buf[0])
        }
    }
}

impl<R> Pos for PoisonLock<ReaderState<R>> where R: Read + Seek {
    fn pos(&mut self) -> usize {
        let result : Result<usize, std::io::Error> = self.try(|state| {
            Ok(state.reader.pos())
        });
        result.unwrap() // closure cannot fail.
    }
    fn size(&mut self) -> usize {
        let result : Result<usize, std::io::Error> = self.try(|state| {
            Ok(state.reader.size())
        });
        result.unwrap() // closure cannot fail.
    }
}

pub struct ListGuard<R> where R: Read + Seek {
    finalized: bool,
    expected_end: u64,
    start: u64,
    owner: Rc<RefCell<PoisonLock<ReaderState<R>>>>,
}

impl<R> ListGuard<R> where R: Read + Seek {
    fn new(owner: Rc<RefCell<PoisonLock<ReaderState<R>>>>, start: u64, byte_len: u64) -> Self {
        ListGuard {
            finalized: false,
            start,
            expected_end: start + byte_len,
            owner
        }
    }
}
impl<R> Guard for ListGuard<R> where R: Read + Seek {
    type Error = TokenReaderError;
    fn done(mut self) -> Result<(), Self::Error> {
        self.finalized = true;
        let mut owner = self.owner.borrow_mut();
        if owner.is_poisoned() {
            return Ok(())
        }

        owner.try(|state| {
            let found = state.reader.pos() as u64;
            if found != self.expected_end {
                return Err(TokenReaderError::EndOffsetError {
                    start: self.start,
                    expected: self.expected_end,
                    found,
                    description: "list".to_string()
                })
            }
            state.reader.read_const(b"</list>")
                .map_err(TokenReaderError::ReadError)
        })?;

        Ok(())
    }
}
impl<R> Drop for ListGuard<R> where R: Read + Seek {
    fn drop(&mut self) {
        let owner = self.owner.borrow_mut();
        if owner.is_poisoned() {
            return;
        }
        assert!(self.finalized)
    }
}


pub struct TaggedGuard<R> where R: Read + Seek {
    finalized: bool,
    owner: Rc<RefCell<PoisonLock<ReaderState<R>>>>,
}

impl<R> TaggedGuard<R> where R: Read + Seek {
    fn new(owner: Rc<RefCell<PoisonLock<ReaderState<R>>>>) -> Self {
        TaggedGuard {
            finalized: false,
            owner
        }
    }
}
impl<R> Guard for TaggedGuard<R> where R: Read + Seek {
    type Error = TokenReaderError;
    fn done(mut self) -> Result<(), Self::Error> {
        self.finalized = true;
        let mut owner = self.owner.borrow_mut();
        if owner.is_poisoned() {
            return Ok(())
        }

        owner.try(|state| {
            state.reader.read_const(b"</tuple>")
                .map_err(TokenReaderError::ReadError)
        })?;
        Ok(())
    }
}
impl<R> Drop for TaggedGuard<R> where R: Read + Seek {
    fn drop(&mut self) {
        if self.owner.borrow().is_poisoned() {
            return;
        }
        assert!(self.finalized)
    }
}


pub struct UntaggedGuard<R> where R: Read + Seek {
    finalized: bool,
    owner: Rc<RefCell<PoisonLock<ReaderState<R>>>>,
}

impl<R> UntaggedGuard<R> where R: Read + Seek {
    fn new(owner: Rc<RefCell<PoisonLock<ReaderState<R>>>>) -> Self {
        UntaggedGuard {
            finalized: false,
            owner
        }
    }
}
impl<R> Guard for UntaggedGuard<R> where R: Read + Seek {
    type Error = TokenReaderError;
    fn done(mut self) -> Result<(), Self::Error> {
        self.finalized = true;
        let mut owner = self.owner.borrow_mut();
        if owner.is_poisoned() {
            return Ok(())
        }

        owner.try(|state| {
            state.reader.read_const(b"</tuple>")
                .map_err(TokenReaderError::ReadError)
        })?;
        Ok(())
    }
}
impl<R> Drop for UntaggedGuard<R> where R: Read + Seek {
    fn drop(&mut self) {
        if self.owner.borrow().is_poisoned() {
            return;
        }
        assert!(self.finalized)
    }
}

pub struct TreeTokenReader<R> where R: Read + Seek {
    owner: Rc<RefCell<PoisonLock<ReaderState<R>>>>,
}

impl<R> TreeTokenReader<R> where R: Read + Seek {
    pub fn new(reader: R) -> Self {
        let owner = ReaderState {
            reader,
        };
        TreeTokenReader {
            owner: Rc::new(RefCell::new(PoisonLock::new(owner)))
        }
    }
}


impl<R> TokenReader for TreeTokenReader<R> where R: Read + Seek {
    type ListGuard = ListGuard<R>;
    type TaggedGuard = TaggedGuard<R>;
    type UntaggedGuard = UntaggedGuard<R>;
    type Error = TokenReaderError;

    fn poison(&mut self) {
        self.owner.borrow_mut().poison();
    }

    fn bool(&mut self) -> Result<Option<bool>, Self::Error> {
        debug!("TreeTokenReader: bool");
        let mut buf : [u8; 1] = [0];
        let mut owner = self.owner.borrow_mut();
        owner.try(|state| {
            state.reader.read(&mut buf)
                .map_err(TokenReaderError::ReadError)?;
            match bytes::bool::bool_of_bytes(&buf) {
                Ok(x) => Ok(x),
                Err(_) => Err(TokenReaderError::InvalidValue)
            }
        })
    }

    fn float(&mut self) -> Result<Option<f64>, Self::Error> {
        let mut owner = self.owner.borrow_mut();
        owner.try(|state| {
            let mut buf : [u8; 8] = [0, 0, 0, 0, 0, 0, 0, 0];
            state.reader.read(&mut buf)
                .map_err(TokenReaderError::ReadError)?;
            Ok(bytes::float::float_of_bytes(&buf))
        })
    }

    fn string(&mut self) -> Result<Option<String>, Self::Error> {
        debug!("TreeTokenReader: string");
        let mut owner = self.owner.borrow_mut();
        owner.try(|state| {
            state.reader.read_const(b"<string>")
                .map_err(TokenReaderError::ReadError)?;
            let byte_len = state.read_u32()?;

            let mut bytes : Vec<u8> = vec![0 as u8; byte_len as usize];
            state.reader.read(&mut bytes)
                .map_err(TokenReaderError::ReadError)?;

            state.reader.read_const(b"</string>")
                .map_err(TokenReaderError::ReadError)?;

            if byte_len == 2 && bytes[0] == 255 && bytes[1] == 0 {
                return Ok(None)
            }
            match String::from_utf8(bytes) {
                Ok(x) => Ok(Some(x)),
                Err(err) => Err(TokenReaderError::Encoding(err))
            }
        })
    }

    fn list(&mut self) -> Result<(u32, Self::ListGuard), Self::Error> {
        debug!("TreeTokenReader: list");
        let clone = self.owner.clone();
        self.owner.borrow_mut().try(|state| {
            state.reader.read_const(b"<list>")
                .map_err(TokenReaderError::ReadError)?;
            let byte_len = state.read_u32()?;

            let guard = ListGuard::new(clone, state.position() as u64, byte_len as u64);
            let list_len = state.read_u32()?;
            debug!("TreeTokenReader: list has {} items, {} bytes", list_len, byte_len);
            Ok((list_len, guard))
        })
    }

    fn tagged_tuple(&mut self) -> Result<(String, Rc<Box<[String]>>, Self::TaggedGuard), Self::Error> {
        debug!("TreeTokenReader: tagged_tuple");
        let clone = self.owner.clone();
        let mut owner = self.owner.borrow_mut();
        owner.try(|state| {
            state.reader.read_const(b"<tuple>")
                .map_err(TokenReaderError::ReadError)?;
            state.reader.read_const(b"<head>")
                .map_err(TokenReaderError::ReadError)?;

            // Read the kind.
            let kind_name = state.read_string()?;

            // Read the field names
            let len = state.read_u32()?;
            let mut fields = Vec::with_capacity(len as usize);
            for _ in 0..len {
                let name = state.read_string()?;
                fields.push(name);
            }

            state.reader.read_const(b"</head>")
                .map_err(TokenReaderError::ReadError)?;

            let guard = TaggedGuard::new(clone);

            debug!("TreeTokenReader: tagged_tuple has name {:?}, fields {:?}", kind_name, fields);
            Ok((kind_name, Rc::new(fields.into_boxed_slice()), guard))
        })
    }

    fn untagged_tuple(&mut self) -> Result<Self::UntaggedGuard, Self::Error> {
        debug!("TreeTokenReader: untagged_tuple");
        let mut owner = self.owner.borrow_mut();
        owner.try(|state| {
            state.reader.read_const(b"<tuple>")
                .map_err(TokenReaderError::ReadError)
        })?;
        Ok(UntaggedGuard::new(self.owner.clone()))
    }
}

/// A trivial tree writer, without any kind of optimization.
pub struct TreeTokenWriter {
    root: Rc<Vec<u8>>
}
impl TreeTokenWriter {
    pub fn new() -> Self {
        TreeTokenWriter {
            root: Rc::new(Vec::new())
        }
    }
    pub fn data(&self) -> &[u8] {
        self.root.as_ref()
    }

    fn register(&mut self, data: Vec<u8>) -> Rc<Vec<u8>> {
        let result = Rc::new(data);
        self.root = result.clone();
        result
    }
}

pub struct Data(pub Rc<Vec<u8>>);
impl AsRef<[u8]> for Data {
    fn as_ref(&self) -> &[u8] {
        self.0.as_ref().as_ref()
    }
}

#[derive(Default)]
/// This encoder doesn't produce useful statistics.
pub struct Statistics;
impl std::fmt::Display for Statistics {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "No statistics available for this encoder")
    }
}

impl std::ops::Add for Statistics {
    type Output = Self;
    fn add(self, _: Self) -> Self::Output {
        Statistics
    }
}
impl std::ops::AddAssign for Statistics {
    fn add_assign(&mut self, _: Self) {
        // Nothing to do.
    }
}


impl TokenWriter for TreeTokenWriter {
    type Tree = Rc<Vec<u8>>;
    type Error = TokenWriterError;
    type Data = Data;
    type Statistics = Statistics;

    fn done(self) -> Result<(Self::Data, Self::Statistics), Self::Error> {
        Ok((Data(self.root), Statistics))
    }

    fn float(&mut self, data: Option<f64>) -> Result<Self::Tree, Self::Error> {
        let bytes = bytes::float::bytes_of_float(data);
        Ok(self.register(bytes.iter().cloned().collect()))
    }

    fn bool(&mut self, data: Option<bool>) -> Result<Self::Tree, Self::Error> {
        debug!("TreeTokenWriter: bool");
        let result = bytes::bool::bytes_of_bool(data).iter().cloned().collect();
        Ok(self.register(result))
    }

    // Strings are represented as len + UTF-8
    // The None string is represented as len + [255, 0]
    fn string(&mut self, data: Option<&str>) -> Result<Self::Tree, Self::Error> {
        debug!("TreeTokenWriter: string {:?}", data);
        const EMPTY_STRING: [u8; 2] = [255, 0];
        let byte_len = match data {
            None => EMPTY_STRING.len(),
            Some(ref x) => x.len()
        } as u32;
        let buf_len : [u8; 4] = unsafe { std::mem::transmute(byte_len) }; // FIXME: Make this little-endian
        assert!(std::mem::size_of_val(&buf_len) == std::mem::size_of_val(&byte_len));


        let mut buf = Vec::new();
        buf.extend_from_str("<string>");
        buf.extend_from_slice(&buf_len);
        match data {
            None => buf.extend_from_slice(&EMPTY_STRING),
            Some(ref x) => buf.extend(x.bytes())
        }
        buf.extend_from_str("</string>");

        Ok(self.register(buf))
    }

    /// Lists are represented as:
    /// - "<list>"
    /// - number of bytes (u32);
    /// - number of items (u32);
    /// - items
    /// - "</list>"
    ///
    /// The number of bytes is the total size of
    /// - number of items;
    /// - items.
    fn list(&mut self, items: Vec<Self::Tree>) -> Result<Self::Tree, Self::Error> {
        debug!("TreeTokenWriter: list");
        let prefix = "<list>";
        let suffix = "</list>";
        let mut result = Vec::new();
        result.extend_from_str(prefix);// Sole purpose of this constant is testing

        let number_of_items = items.len() as u32;
        let mut buf : [u8; 4] = unsafe { std::mem::transmute(number_of_items) };
        assert!(std::mem::size_of_val(&buf) == std::mem::size_of_val(&number_of_items));

        // Placeholder for `byte_len`
        result.extend_from_slice(&buf);

        // Actual number of items
        result.extend_from_slice(&buf);

        // Put actual data
        for item in items {
            result.extend_from_slice(&*item)
        }

        // Now compute bytelength and put it back
        let byte_len = (result.len() - prefix.len() - std::mem::size_of_val(&buf)) as u32;
        assert!(std::mem::size_of_val(&buf) == std::mem::size_of_val(&byte_len));
        buf = unsafe { std::mem::transmute(byte_len) };

        for i in 0..buf.len() {
            result[i + prefix.len()] = buf[i];
        }

        result.extend_from_str(suffix);// Sole purpose of this constant is testing
        debug!("TreeTokenWriter: list has {} items, {} bytes", number_of_items, byte_len);
        assert_eq!(byte_len as usize,
            result.len() - prefix.len() - suffix.len() - std::mem::size_of_val(&number_of_items),
            "TreeTokenWriter: incorrect byte_len");
        Ok(self.register(result))
    }

    /// For this example, we use a very, very, very suboptimal encoding.
    /// - (if specified)
    ///   - kind (string, \0 terminated)
    ///   - field names (string, \0 terminated)
    /// - contents
    fn tagged_tuple(&mut self, tag: &str, children: &[(&str, Self::Tree)]) -> Result<Self::Tree, Self::Error> {
        debug!("TreeTokenWriter: tagged_tuple");
        let mut prefix = Vec::new();
        prefix.extend_from_str("<head>");
        prefix.extend_from_str(tag);
        prefix.push(0);

        let number_of_items = children.len() as u32;
        let buf : [u8; 4] = unsafe { std::mem::transmute(number_of_items) };
        assert!(std::mem::size_of_val(&buf) == std::mem::size_of_val(&number_of_items));
        prefix.extend_from_slice(&buf);

        for &(ref field, _) in children.iter() {
            prefix.extend_from_str(field);
            prefix.push(0);
        }
        prefix.extend_from_str("</head>");

        let mut untagged = Vec::new();
        untagged.push(Rc::new(prefix));
        for &(_, ref child) in children.iter() {
            untagged.push(child.clone())
        }

        self.untagged_tuple(&untagged)
    }
    fn untagged_tuple(&mut self, children: &[Self::Tree]) -> Result<Self::Tree, Self::Error> {
        debug!("TreeTokenWriter: untagged_tuple");
        let mut result = Vec::new();
        result.extend_from_str("<tuple>"); // Sole purpose of this constant is testing
        for item in children {
            result.extend_from_slice(&*item)
        }
        result.extend_from_str("</tuple>"); // Sole purpose of this constant is testing
        Ok(self.register(result))
    }
}

trait ExtendFromUTF8 {
    fn extend_from_str(&mut self, &str);
}

impl ExtendFromUTF8 for Vec<u8> {
    fn extend_from_str(&mut self, data: &str) {
        self.extend_from_slice(data.as_bytes());
    }
}

#[test]
fn test_simple_io() {
    use std::fs::*;

    use std::io::{ Cursor, Write };

    debug!("Testing string I/O");

    {
        let mut writer = TreeTokenWriter::new();
        writer.string(Some("simple string"))
            .expect("Writing simple string");

        File::create("/tmp/test-simple-string.binjs").unwrap()
            .write_all(writer.data()).unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(writer.data()));
        let simple_string = reader.string()
            .expect("Reading simple string")
            .expect("Non-null string");
        assert_eq!(&simple_string, "simple string");
    }


    {
        let data = "string with escapes \u{0}\u{1}\u{0}";
        let mut writer = TreeTokenWriter::new();
        writer.string(Some(data))
            .expect("Writing string with escapes");

        File::create("/tmp/test-string-with-escapes.binjs").unwrap()
            .write_all(writer.data()).unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(writer.data()));
        let escapes_string = reader.string()
            .expect("Reading string with escapes")
            .expect("Non-null string");
        assert_eq!(&escapes_string, data);
    }

    debug!("Testing untagged tuple I/O");

    {
        let mut writer = TreeTokenWriter::new();
        writer.untagged_tuple(&[])
            .expect("Writing empty untagged tuple");

        File::create("/tmp/test-empty-untagged-tuple.binjs").unwrap()
            .write_all(writer.data()).unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(writer.data()));
        let guard = reader.untagged_tuple()
            .expect("Reading empty untagged tuple");

        guard.done()
            .expect("Empty untagged tuple read properly");

    }

    {
        let mut writer = TreeTokenWriter::new();
        let item_0 = writer.string(Some("foo")).unwrap();
        let item_1 = writer.string(Some("bar")).unwrap();
        writer.untagged_tuple(&[item_0, item_1])
            .expect("Writing trivial untagged tuple");

        File::create("/tmp/test-trivial-untagged-tuple.binjs").unwrap()
            .write_all(writer.data()).unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(writer.data()));
        let guard = reader.untagged_tuple()
            .expect("Reading trivial untagged tuple");
        let simple_string = reader.string()
            .expect("Reading trivial tuple[0]")
            .expect("Non-null string");
        assert_eq!(&simple_string, "foo");
        let simple_string = reader.string()
            .expect("Reading trivial tuple[1]")
            .expect("Non-null string");
        assert_eq!(&simple_string, "bar");

        guard.done()
            .expect("Untagged tuple read properly");
    }

    debug!("Testing tagged tuple I/O");

    {
        let mut writer = TreeTokenWriter::new();
        let item_0 = writer.string(Some("foo")).unwrap();
        let item_1 = writer.float(Some(3.1415)).unwrap();
        writer.tagged_tuple("some tuple", &[("id", item_0), ("value", item_1)])
            .expect("Writing trivial tagged tuple");

        File::create("/tmp/test-simple-tagged-tuple.binjs").unwrap()
            .write_all(writer.data()).unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(writer.data()));
        let (name, fields, guard) = reader.tagged_tuple()
            .expect("Reading trivial tagged tuple");
        assert_eq!(&name, "some tuple");

        // Order of fields is deterministic
        assert!(&fields[0] == "id");
        assert!(&fields[1] == "value");
        let simple_string = reader.string()
            .expect("Reading trivial tagged tuple[0]")
            .expect("Reading a non-null string");
        assert_eq!(simple_string, "foo");
        let simple_float = reader.float()
            .expect("Reading trivial tagged tuple[1]")
            .expect("Reading a non-null float");
        assert_eq!(simple_float, 3.1415);

        guard.done()
            .expect("Trivial tagged tuple read properly");
    }

    debug!("Testing list I/O");

    {
        let mut writer = TreeTokenWriter::new();
        writer.list(vec![])
            .expect("Writing empty list");

        File::create("/tmp/test-empty-list.binjs").unwrap()
                .write_all(writer.data()).unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(writer.data()));
        let (len, guard) = reader.list()
            .expect("Reading empty list");
        assert_eq!(len, 0);

        guard.done()
            .expect("Empty list read properly");
    }

    {
        let mut writer = TreeTokenWriter::new();
        let item_0 = writer.string(Some("foo")).unwrap();
        let item_1 = writer.string(Some("bar")).unwrap();
        writer.list(vec![item_0, item_1])
            .expect("Writing trivial list");

        File::create("/tmp/test-trivial-list.binjs").unwrap()
            .write_all(writer.data()).unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(writer.data()));
        let (len, guard) = reader.list()
            .expect("Reading trivial list");
        assert_eq!(len, 2);

        let simple_string = reader.string()
            .expect("Reading trivial list[0]")
            .expect("Non-null string");
        assert_eq!(&simple_string, "foo");
        let simple_string = reader.string()
            .expect("Reading trivial list[1]")
            .expect("Non-null string");
        assert_eq!(&simple_string, "bar");

        guard.done()
            .expect("Trivial list read properly");
    }

    {
        let mut writer = TreeTokenWriter::new();
        let item_0 = writer.string(Some("foo")).unwrap();
        let item_1 = writer.string(Some("bar")).unwrap();
        let list = writer.list(vec![item_0, item_1])
            .expect("Writing inner list");
        writer.list(vec![list])
            .expect("Writing outer list");

        File::create("/tmp/test-nested-lists.binjs").unwrap()
            .write_all(writer.data()).unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(writer.data()));
        let (len, guard) = reader.list()
            .expect("Reading outer list");
        assert_eq!(len, 1);

        let (len, inner_guard) = reader.list()
            .expect("Reading inner list");
        assert_eq!(len, 2);

        let simple_string = reader.string()
            .expect("Reading trivial list[0]")
            .expect("Non-null string");
        assert_eq!(&simple_string, "foo");
        let simple_string = reader.string()
            .expect("Reading trivial list[1]")
            .expect("Non-null string");
        assert_eq!(&simple_string, "bar");

        inner_guard.done()
            .expect("Inner list read properly");
        guard.done()
            .expect("Inner list read properly");

    }

}
