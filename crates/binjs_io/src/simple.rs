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
    owner: Rc<RefCell<PoisonLock<ReaderState<R>>>>,
}

impl<R> ListGuard<R> where R: Read + Seek {
    fn new(owner: Rc<RefCell<PoisonLock<ReaderState<R>>>>) -> Self {
        ListGuard {
            finalized: false,
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

impl<R> FileStructurePrinter for TreeTokenReader<R> where R: Read + Seek {}

impl<R> TokenReader for TreeTokenReader<R> where R: Read + Seek {
    type ListGuard = ListGuard<R>;
    type TaggedGuard = TaggedGuard<R>;
    type UntaggedGuard = UntaggedGuard<R>;
    type Error = TokenReaderError;

    fn poison(&mut self) {
        self.owner.borrow_mut().poison();
    }

    fn bool(&mut self) -> Result<Option<bool>, Self::Error> {
        debug!(target: "simple_reader", "bool");
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

    fn offset(&mut self) -> Result<u32, Self::Error> {
        debug!(target: "simple_reader", "offset");
        let mut owner = self.owner.borrow_mut();
        owner.try(|state| {
            state.read_u32()
        })
    }

    fn float(&mut self) -> Result<Option<f64>, Self::Error> {
        debug!(target: "simple_reader", "float");
        let mut owner = self.owner.borrow_mut();
        owner.try(|state| {
            let mut buf : [u8; 8] = [0, 0, 0, 0, 0, 0, 0, 0];
            state.reader.read(&mut buf)
                .map_err(TokenReaderError::ReadError)?;
            Ok(bytes::float::float_of_bytes(&buf))
        })
    }

    fn unsigned_long(&mut self) -> Result<u32, Self::Error> {
        debug!(target: "simple_reader", "unsigned_long");
        let mut owner = self.owner.borrow_mut();
        owner.try(|state| {
            let result = state.read_u32()?;
            Ok(result)
        })
    }

    fn string(&mut self) -> Result<Option<String>, Self::Error> {
        debug!(target: "simple_reader", "string");
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
        debug!(target: "simple_reader", "list");
        let clone = self.owner.clone();
        self.owner.borrow_mut().try(|state| {
            state.reader.read_const(b"<list>")
                .map_err(TokenReaderError::ReadError)?;

            let guard = ListGuard::new(clone);
            let list_len = state.read_u32()?;
            debug!(target: "simple_writer", "TreeTokenReader: list has {} items", list_len);
            Ok((list_len, guard))
        })
    }

    fn tagged_tuple(&mut self) -> Result<(String, Option<Rc<Box<[String]>>>, Self::TaggedGuard), Self::Error> {
        debug!(target: "simple_reader", "tagged tuple");
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
            debug!(target: "simple_reader", "/tagged tuple");
            Ok((kind_name, Some(Rc::new(fields.into_boxed_slice())), guard))
        })
    }

    fn untagged_tuple(&mut self) -> Result<Self::UntaggedGuard, Self::Error> {
        debug!(target: "simple_reader", "untagged tuple");
        let mut owner = self.owner.borrow_mut();
        owner.try(|state| {
            state.reader.read_const(b"<tuple>")
                .map_err(TokenReaderError::ReadError)
        })?;
        debug!(target: "simple_reader", "/untagged tuple");
        Ok(UntaggedGuard::new(self.owner.clone()))
    }
}

/// A trivial tree writer, without any kind of optimization.
pub struct TreeTokenWriter {
    root: Rc<TreeItem>
}
impl TreeTokenWriter {
    pub fn new() -> Self {
        TreeTokenWriter {
            root: Rc::new(TreeItem::Bytes(Vec::new()))
        }
    }
    pub fn data(&self) -> Option<&[u8]> {
        match *self.root {
            TreeItem::Bytes(ref bytes) => Some(bytes),
            _ => None,
        }
    }

    fn register(&mut self, data: Vec<u8>) -> AbstractTree {
        let result = Rc::new(TreeItem::Bytes(data));
        self.root = result.clone();
        AbstractTree(result)
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

#[derive(Clone)]
enum TreeItem {
    Bytes(Vec<u8>),
    Offset,
}

/// Abstract type for the contents of the tree.
#[derive(Clone)]
pub struct AbstractTree(Rc<TreeItem>);

impl TokenWriter for TreeTokenWriter {
    type Tree = AbstractTree;
    type Error = TokenWriterError;
    type Data = Vec<u8>;
    type Statistics = Statistics;

    fn done(self) -> Result<(Self::Data, Self::Statistics), Self::Error> {
        let unwrapped = Rc::try_unwrap(self.root)
            .unwrap_or_else(|e| panic!("We still have {} references to the root", Rc::strong_count(&e)));
        match unwrapped {
            TreeItem::Bytes(bytes) => Ok((bytes, Statistics)),
            TreeItem::Offset => Err(TokenWriterError::InvalidOffsetField),
        }
    }

    fn float(&mut self, data: Option<f64>) -> Result<Self::Tree, Self::Error> {
        let bytes = bytes::float::bytes_of_float(data);
        Ok(self.register(bytes.iter().cloned().collect()))
    }

    fn unsigned_long(&mut self, data: u32) -> Result<Self::Tree, Self::Error> {
        let u1 = (data & 0xff) as u8;
        let u2 = ((data >> 8) & 0xff) as u8;
        let u3 = ((data >> 16) & 0xff) as u8;
        let u4 = ((data >> 24) & 0xff) as u8;
        let bytes = [u1, u2, u3, u4].to_vec();
        Ok(self.register(bytes.iter().cloned().collect()))
    }

    fn bool(&mut self, data: Option<bool>) -> Result<Self::Tree, Self::Error> {
        debug!(target: "simple_writer", "TreeTokenWriter: bool");
        let result = bytes::bool::bytes_of_bool(data).iter().cloned().collect();
        Ok(self.register(result))
    }

    fn offset(&mut self) -> Result<Self::Tree, Self::Error> {
        let tree = Rc::new(TreeItem::Offset);
        self.root = tree.clone();
        Ok(AbstractTree(tree))
    }

    // Strings are represented as len + UTF-8
    // The None string is represented as len + [255, 0]
    fn string(&mut self, data: Option<&str>) -> Result<Self::Tree, Self::Error> {
        debug!(target: "simple_writer", "TreeTokenWriter: string {:?}", data);
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
    /// - number of items (u32);
    /// - items
    /// - "</list>"
    ///
    /// The number of bytes is the total size of
    /// - number of items;
    /// - items.
    fn list(&mut self, items: Vec<Self::Tree>) -> Result<Self::Tree, Self::Error> {
        debug!(target: "simple_writer", "TreeTokenWriter: list");
        let prefix = "<list>";
        let suffix = "</list>";
        let mut result = Vec::new();
        result.extend_from_str(prefix);// Sole purpose of this constant is testing

        let number_of_items = items.len() as u32;
        let buf : [u8; 4] = unsafe { std::mem::transmute(number_of_items) };
        assert!(std::mem::size_of_val(&buf) == std::mem::size_of_val(&number_of_items));

        // Actual number of items
        result.extend_from_slice(&buf);

        // Put actual data
        for item in items {
            match *item.0 {
                TreeItem::Bytes(ref bytes) => result.extend_from_slice(bytes),
                TreeItem::Offset => {
                    // An offset field makes no sense in a list.
                    return Err(TokenWriterError::InvalidOffsetField);
                }
            }
        }

        for i in 0..buf.len() {
            result[i + prefix.len()] = buf[i];
        }

        result.extend_from_str(suffix);// Sole purpose of this constant is testing
        debug!(target: "simple_writer", "TreeTokenWriter: list has {} items", number_of_items);
        Ok(self.register(result))
    }

    /// For this example, we use a very, very, very suboptimal encoding.
    /// - <head>
    ///   - kind (string, \0 terminated)
    ///   - number of items (varnum)
    ///   - field names (string, \0 terminated)
    /// - </head>
    /// - contents
    fn tagged_tuple(&mut self, tag: &str, children: &[(&str, Self::Tree)]) -> Result<Self::Tree, Self::Error> {
        debug!(target: "simple_writer", "TreeTokenWriter: tagged_tuple");
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
        untagged.push(AbstractTree(Rc::new(TreeItem::Bytes(prefix))));
        for &(_, ref child) in children.iter() {
            untagged.push(child.clone())
        }

        self.untagged_tuple(&untagged)
    }
    fn untagged_tuple(&mut self, children: &[Self::Tree]) -> Result<Self::Tree, Self::Error> {
        debug!(target: "simple_writer", "TreeTokenWriter: untagged_tuple");
        let mut result = Vec::new();
        result.extend_from_str("<tuple>"); // Sole purpose of this constant is testing

        const FOOTER: &'static [u8; 8] = b"</tuple>";

        // To be substituted to any Offset field.
        let mut byte_len = FOOTER.len() as u32;

        // First check if children[1] is Offset. If so, compute the length of the rest
        // of the children and substitute that Length to Offset. Note that Offset at any
        // other position than children[1] is an error.
        for (i, item) in children.iter().enumerate() {
            match *(item.0) {
                TreeItem::Bytes(_) if i < 2 => {
                    // That's before any instance of Offset, ignore it.
                }
                TreeItem::Bytes(ref bytes) => {
                    byte_len += bytes.len() as u32;
                }
                TreeItem::Offset if i == 1 => {
                    // Ok, that's the only place where `Offset` is valid.
                }
                TreeItem::Offset => {
                    return Err(TokenWriterError::InvalidOffsetField)
                }
            }
        }

        for item in children {
            match *(item.0) {
                TreeItem::Bytes(ref bytes) => {
                    result.extend_from_slice(bytes);
                }
                TreeItem::Offset => {
                    let buf : [u8; 4] = unsafe { std::mem::transmute(byte_len) };
                    result.extend_from_slice(&buf);
                }
            }
        }
        result.extend_from_slice(FOOTER); // Sole purpose of this constant is testing
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

    eprintln!("Testing string I/O");

    {
        let mut writer = TreeTokenWriter::new();
        writer.string(Some("simple string"))
            .expect("Writing simple string");

        let data = writer.data().unwrap();
        File::create("/tmp/test-simple-string.binjs").unwrap()
            .write_all(data)
            .unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(data));
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

        let result = writer.data().unwrap();
        File::create("/tmp/test-string-with-escapes.binjs").unwrap()
            .write_all(result).unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(result));
        let escapes_string = reader.string()
            .expect("Reading string with escapes")
            .expect("Non-null string");
        assert_eq!(&escapes_string, data);
    }

    eprintln!("Testing untagged tuple I/O");

    {
        let mut writer = TreeTokenWriter::new();
        writer.untagged_tuple(&[])
            .expect("Writing empty untagged tuple");

        let data = writer.data().unwrap();
        File::create("/tmp/test-empty-untagged-tuple.binjs").unwrap()
            .write_all(data).unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(data));
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

        let data = writer.data().unwrap();
        File::create("/tmp/test-trivial-untagged-tuple.binjs").unwrap()
            .write_all(data).unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(data));
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

    eprintln!("Testing tagged tuple I/O");

    {
        let mut writer = TreeTokenWriter::new();
        let item_0 = writer.string(Some("foo")).unwrap();
        let item_1 = writer.float(Some(3.1415)).unwrap();
        writer.tagged_tuple("BindingIdentifier", &[("label", item_0), ("value", item_1)])
            .expect("Writing trivial tagged tuple");

        let data = writer.data().unwrap();
        File::create("/tmp/test-simple-tagged-tuple.binjs").unwrap()
            .write_all(data).unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(data));
        let (name, fields, guard) = reader.tagged_tuple()
            .expect("Reading trivial tagged tuple");
        assert_eq!(&name, "BindingIdentifier");

        // Order of fields is deterministic
        let fields = fields.expect("Missing fields");
        assert!(&fields[0] == "label");
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

    eprintln!("Testing list I/O");

    {
        let mut writer = TreeTokenWriter::new();
        writer.list(vec![])
            .expect("Writing empty list");

        let data = writer.data().unwrap();
        File::create("/tmp/test-empty-list.binjs").unwrap()
                .write_all(data).unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(data));
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

        let data = writer.data().unwrap();
        File::create("/tmp/test-trivial-list.binjs").unwrap()
            .write_all(data).unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(data));
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

        let data = writer.data().unwrap();
        File::create("/tmp/test-nested-lists.binjs").unwrap()
            .write_all(data).unwrap();

        let mut reader = TreeTokenReader::new(Cursor::new(data));
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
