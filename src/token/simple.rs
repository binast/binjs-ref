//! Minimal implementation of encoding/decoding to binary.
//! Used for testing purposes. Not included in release builds.

use ast::grammar::{ Field, Syntax };
use token::io::*;
use util::*;

use std;
use std::cell::RefCell;
use std::io::{ Read, Seek, SeekFrom };
use std::rc::Rc;
use std::string::FromUtf8Error;


#[derive(Debug)]
pub enum TokenReaderError {
    Reader(std::io::Error),
    Encoding(FromUtf8Error),
    NotInList,
    NoSuchKind(String),
    NoSuchField(String),
    HeaderOrFooterNotFound {
        expected: String,
        found: Vec<u8>,
        description: String,
    },
    EndOffsetError {
        suffix: Option<String>,
        start: u64,
        expected: u64,
        found: u64,
        description: String,
    },
    BailingOutBecauseOfPreviousError,
    ChildBytesNotConsumed,
}

struct TreeTokenReaderImpl<'a, R> where R: Read + Seek {
    reader: PositionRead<R>,
    grammar: &'a Syntax,
}

pub struct TreeTokenReader<'a, R> where R: Read + Seek {
    // Shared with all children.
    implem: Rc<RefCell<TreeTokenReaderImpl<'a, R>>>,
    my_pending_error: Rc<RefCell<Option<TokenReaderError>>>,
    parent_pending_error: Rc<RefCell<Option<TokenReaderError>>>,

    description: String,

    /// The current position.
    pos_current: u64,

    /// The position at which we started.
    pos_start: u64,

    /// If specified, the position at which extraction must end.
    pos_end: Option<u64>,

    /// If specified, a suffix for the extraction.
    /// This suffix appears *after* `pos_end`.
    suffix: Option<String>
}
impl<'a, R> TreeTokenReader<'a, R> where R: Read + Seek {
    /// Create a new toplevel TreeTokenReader.
    pub fn new(reader: R, grammar: &'a Syntax) -> Self {
        let implem = TreeTokenReaderImpl {
            reader: PositionRead::new(reader),
            grammar, // FIXME: We probably don't need the grammar at this layer.
        };
        TreeTokenReader {
            description: "Top".to_owned(),
            pos_current: 0,
            pos_start: 0,
            implem: Rc::new(RefCell::new(implem)),
            my_pending_error: Rc::new(RefCell::new(None)),
            parent_pending_error: Rc::new(RefCell::new(None)),
            pos_end: None,
            suffix: None,
        }
    }

    /// Derive a TreeTokenReader for reading a subset of the stream.
    fn sub(&self, options: SubOptions) -> Self {

        let position = self.implem.borrow().reader.position();
        let pos_end = options.byte_len.map(|length| position + length);
        debug!("TreeTokenReader: sub {:?}, starting at {} => {:?}", options.suffix, position, pos_end);
        TreeTokenReader {
            implem: self.implem.clone(),
            parent_pending_error: self.my_pending_error.clone(),
            my_pending_error: Rc::new(RefCell::new(None)),
            pos_end,
            suffix: options.suffix,
            pos_current: position,
            pos_start: position,
            description: options.description,
        }
    }

    /// Read data to a buffer.
    ///
    /// If an error is pending, propagate the error.
    /// If reading causes an error, any further call to `read()` will
    /// cause an error of type
    /// `TokenReaderError::BailingOutBecauseOfPreviousError`.
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, TokenReaderError> {
        let error = self.my_pending_error.borrow_mut().take();
        if let Some(error) = error {
            *self.my_pending_error.borrow_mut() = Some(TokenReaderError::BailingOutBecauseOfPreviousError);
            return Err(error)
        }
        let ref mut reader = self.implem.borrow_mut().reader;
        let result = match reader.read_exact(buf) {
            Ok(_) => Ok(buf.len()),
            Err(err) => {
                let error = TokenReaderError::Reader(err);
                *self.my_pending_error.borrow_mut() = Some(TokenReaderError::BailingOutBecauseOfPreviousError);
                Err(error)
            }
        };
        self.pos_current = reader.position();
        result
    }

    fn read_u32(&mut self) -> Result<u32, TokenReaderError> {
        let result : u32;
        let mut buf : [u8; 4] = unsafe { std::mem::uninitialized() };
        debug_assert!(std::mem::size_of::<u32>() == std::mem::size_of_val(&buf));
        self.read(&mut buf)?;
        result = unsafe { std::mem::transmute(buf) };
        Ok(result)
    }

    fn read_string(&mut self) -> Result<String, TokenReaderError> {
        let mut bytes = Vec::new();
        let mut buf: [u8;1] = [0];
        loop {
            self.read(&mut buf)?;
            if buf[0] == 0 {
                return String::from_utf8(bytes)
                    .map_err(TokenReaderError::Encoding);
            }
            bytes.push(buf[0])
        }
    }

    fn read_constant(&mut self, value: &str) -> Result<(), TokenReaderError> {
        let mut buf : Vec<u8> = value.bytes().collect();
        self.read(&mut buf)?;
        for (expected, found) in buf.iter().zip(value.bytes()) {
            if *expected != found {
                return Err(TokenReaderError::HeaderOrFooterNotFound {
                    found: buf.clone(),
                    expected: value.to_string(),
                    description: self.description.clone(),
                });
            }
        }
        Ok(())
    }
}
impl<'a, R> Drop for TreeTokenReader<'a, R> where R: Read + Seek {
    fn drop(&mut self) {
        {
            if self.parent_pending_error.borrow().is_some() {
                // Propagate error.
                return;
            }
        }
        {
            let my_pending_error = self.my_pending_error.borrow_mut().take();
            if my_pending_error.is_some() {
                // Propagate error.
                *self.parent_pending_error.borrow_mut() = my_pending_error;
                return;
            }
        }

        // FIXME: Wait, we are relying upon order of destruction!
        let position = { self.implem.borrow().reader.position() };

        // Check byte_len, if available.
        if let Some(expected) = self.pos_end {
            if position != expected {
                *self.parent_pending_error.borrow_mut() =
                    Some(TokenReaderError::EndOffsetError {
                        start: self.pos_start,
                        suffix: self.suffix.clone(),
                        expected,
                        found: position,
                        description: self.description.clone()
                    });
                warn!("TokenTreeReader: This subextractor goes {} => {}, expected {} => {}, ({})",
                    self.pos_start,
                    position,
                    self.pos_start,
                    expected,
                    self.description);
                return;
            }
        }
        // Check suffix, if available.
        if let Some(ref suffix) = self.suffix.take() {
            if let Err(err) = self.read_constant(suffix) {
                *self.parent_pending_error.borrow_mut() = Some(err);
                warn!("TokenTreeReader: This subextractor should end with {} ({})", suffix, self.description);
                return;
            }
        }

        debug!("TreeTokenReader: drop() {:?} {:?} OK", self.pos_end, self.suffix);
    }
}
impl<'a, R> TokenReader for TreeTokenReader<'a, R> where R: Read + Seek {
    type Error = TokenReaderError;

    fn position(&self) -> u64 {
        self.pos_current
    }

    fn skip(&mut self) -> Result<(), Self::Error> {
        debug!("TreeTokenReader: skip");
        if let Some(end) = self.pos_end {
            self.pos_current = self.implem.borrow_mut().reader.seek(SeekFrom::Start(end))
                .map_err(TokenReaderError::Reader)?;
            Ok(())
        } else {
            Err(TokenReaderError::NotInList)
        }
    }

    fn bool(&mut self) -> Result<bool, Self::Error> {
        debug!("TreeTokenReader: bool");
        let mut buf : [u8; 1] = unsafe { std::mem::uninitialized() };
        self.read(&mut buf)?;
        Ok(buf[0] != 0)
    }

    fn float(&mut self) -> Result<f64, Self::Error> {
        debug!("TreeTokenReader: float");
        let mut buf : [u8; 8] = unsafe { std::mem::uninitialized() };
        assert!(std::mem::size_of_val(&buf) == std::mem::size_of::<f64>());
        self.read(&mut buf)?;
        Ok(unsafe { std::mem::transmute::<_, f64>(buf) })
    }

    fn string(&mut self) -> Result<String, Self::Error> {
        debug!("TreeTokenReader: string");
        let mut bytes = Vec::new();
        // Read until we get a \0.
        loop {
            let mut buf = [ 0 ];
            self.read(&mut buf)?;
            if buf[0] == 0 {
                break;
            }
            bytes.push(buf[0]);
        }
        let result = String::from_utf8(bytes)
            .map_err(TokenReaderError::Encoding)?;
        debug!("TreeTokenReader: string => {}", result);
        Ok(result)
    }

    fn list(&mut self) -> Result<(u32, Self), Self::Error> {
        debug!("TreeTokenReader: list");
        self.read_constant("<list>")?;
        let byte_len = self.read_u32()?;
        let mut extractor = self.sub(SubOptions {
            description: "list".to_owned(),
            byte_len: Some(byte_len as u64),
            suffix: Some("</list>".to_string())
        });
        let list_len = extractor.read_u32()?;
        debug!("TreeTokenReader: list has {} items, {} bytes", list_len, byte_len);
        Ok((list_len, extractor))
    }

    fn tagged_tuple(&mut self) -> Result<(String, Rc<Box<[Field]>>, Self), Self::Error> {
        debug!("TreeTokenReader: tagged_tuple");
        self.read_constant("<tuple>")?;
        self.read_constant("<head>")?;

        // Read (and validate) the kind.
        let kind_name = self.read_string()?;
        let kind = { self.implem.borrow().grammar.get_kind(&kind_name)
            .ok_or_else(|| TokenReaderError::NoSuchKind(kind_name.clone()))? };
        let interface = { self.implem.borrow().grammar.get_interface_by_kind(&kind)
            .ok_or_else(|| TokenReaderError::NoSuchKind(kind_name.clone()))? };

        // Read the field names
        let len = self.read_u32()?;
        let mut field_names = Vec::with_capacity(len as usize);
        for _ in 0..len {
            let string_name = self.read_string()?;
            let field_name = self.implem.borrow().grammar.get_field_name(&string_name)
                .ok_or_else(|| TokenReaderError::NoSuchField(string_name.clone()))?;
            field_names.push(field_name);
        }

        // Attach types
        let mut fields = Vec::with_capacity(len as usize);
        let obj = interface.contents();
        'fields: for field_name in field_names.drain(..) {
            for field in obj.fields() {
                if field_name == *field.name() {
                    fields.push(field.clone());
                    continue 'fields
                }
            }
        }
        self.read_constant("</head>")?;
        let extractor = self.sub(SubOptions {
            description: format!("Tagged tuple: {}", kind_name),
            suffix: Some("</tuple>".to_string()),
            ..SubOptions::default()
        });

        debug!("TreeTokenReader: tagged_tuple has name {:?}, fields {:?}", kind_name, fields);
        Ok((kind_name, Rc::new(fields.into_boxed_slice()), extractor))
    }

    fn untagged_tuple(&mut self) -> Result<Self, Self::Error> {
        debug!("TreeTokenReader: untagged_tuple");
        self.read_constant("<tuple>")?;
        Ok(self.sub(SubOptions {
            description: "Untagged tuple".to_owned(),
            suffix: Some("</tuple>".to_string()),
            ..SubOptions::default()
        }))
    }
}

struct SubOptions {
    description: String,
    byte_len: Option<u64>,
    suffix: Option<String>,
}
impl Default for SubOptions {
    fn default() -> Self {
        SubOptions {
            description: "".to_owned(),
            byte_len: None,
            suffix: None,
        }
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

#[derive(Debug)]
pub enum TokenWriterError {
    MissingKind,
}
impl TokenWriter for TreeTokenWriter {
    type Tree = Rc<Vec<u8>>;
    type Error = TokenWriterError;

    fn float(&mut self, data: f64) -> Result<Self::Tree, Self::Error> {
        debug!("TreeTokenWriter: float");
        let buf : [u8; 8] = unsafe { std::mem::transmute(data) };
        assert!(std::mem::size_of_val(&buf) == std::mem::size_of_val(&data));
        let result = buf.iter().cloned().collect();
        Ok(self.register(result))
    }

    fn bool(&mut self, data: bool) -> Result<Self::Tree, Self::Error> {
        debug!("TreeTokenWriter: bool");
        let buf = [if data { 1 } else { 0 }];
        let result = buf.iter().cloned().collect();
        Ok(self.register(result))
    }

    // Strings are represented as UTF-8, \0-terminated.
    fn string(&mut self, data: &str) -> Result<Self::Tree, Self::Error> {
        debug!("TreeTokenWriter: string {:?}", data);
        let mut buf : Vec<_> = data.as_bytes().iter().cloned().collect();
        buf.push(0);
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
        result.extend_from_slice(&buf);
        for item in items {
            result.extend_from_slice(&*item)
        }

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

    /// For this example, use a very, very, very suboptimal encoding.
    /// - (if specified)
    ///   - kind (string, \0 terminated)
    ///   - field names (string, \0 terminated)
    /// - contents
    fn tagged_tuple(&mut self, tag: &str, children: &[(&Field, Self::Tree)]) -> Result<Self::Tree, Self::Error> {
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
            prefix.extend_from_str(&field.name().to_string());
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
    use ast::grammar::*;
    use std::io::Cursor;

    debug!("Setting up syntax");
    let mut builder = SyntaxBuilder::new();
    let kinded = builder.node_name("Kinded");
    let field_string = Field::new(builder.field_name("some_string"), Type::String);
    let field_number = Field::new(builder.field_name("some_number"), Type::Number);

    builder.add_kinded_interface(&kinded).unwrap()
        .with_own_field(field_string.clone())
        .with_own_field(field_number.clone());

    let syntax = builder.into_syntax(&kinded);

    debug!("Testing string I/O");

    {
        let mut writer = TreeTokenWriter::new();
        writer.string(None)
            .expect("Writing empty string");

        let mut reader = TreeTokenReader::new(Cursor::new(writer.data()), &syntax);
        let empty_string = reader.string()
            .expect("Reading empty string");
        assert!(empty_string.is_none());
    }

    {
        let mut writer = TreeTokenWriter::new();
        writer.string(Some("simple string"))
            .expect("Writing simple string");

        let mut reader = TreeTokenReader::new(Cursor::new(writer.data()), &syntax);
        let simple_string = reader.string()
            .expect("Reading simple string");
        assert_matches!(simple_string, Some(ref s) if s == "simple string");
    }

    debug!("Testing untagged tuple I/O");

    {
        let mut writer = TreeTokenWriter::new();
        writer.untagged_tuple(&[])
            .expect("Writing empty untagged tuple");

        let mut reader = TreeTokenReader::new(Cursor::new(writer.data()), &syntax);
        let _ = reader.untagged_tuple()
            .expect("Reading empty untagged tuplelist");
    }

    {
        let mut writer = TreeTokenWriter::new();
        let item_0 = writer.string(Some("foo")).unwrap();
        let item_1 = writer.string(Some("bar")).unwrap();
        writer.untagged_tuple(&[item_0, item_1])
            .expect("Writing trivial untagged tuple");

        let mut reader = TreeTokenReader::new(Cursor::new(writer.data()), &syntax);
        let mut sub = reader.untagged_tuple()
            .expect("Reading trivial untagged tuple");
        let simple_string = sub.string()
            .expect("Reading trivial tuple[0]");
        assert_matches!(simple_string, Some(ref s) if s == "foo");
        let simple_string = sub.string()
            .expect("Reading trivial tuple[1]");
        assert_matches!(simple_string, Some(ref s) if s == "bar");
    }

    debug!("Testing tagged tuple I/O");

    {
        let mut writer = TreeTokenWriter::new();
        let item_0 = writer.string(Some("foo")).unwrap();
        let item_1 = writer.float(3.1415).unwrap();
        writer.tagged_tuple(kinded.to_str(), &[(&field_string, item_0), (&field_number, item_1)])
            .expect("Writing trivial tagged tuple");

        let mut reader = TreeTokenReader::new(Cursor::new(writer.data()), &syntax);
        let (name, fields, mut sub) = reader.tagged_tuple()
            .expect("Reading trivial tagged tuple");
        assert_eq!(name, "Kinded".to_string());

        // Order of fields is not deterministic
        if fields[0].name().to_string() == &"some_string".to_string() {
            assert_eq!(fields[0].name().to_string(), &"some_string".to_string());
            assert_matches!(*fields[0].type_(), Type::String);
            assert_eq!(fields[1].name().to_string(), &"some_number".to_string());
            assert_matches!(*fields[1].type_(), Type::Number);
            let simple_string = sub.string()
                .expect("Reading trivial tagged tuple[0]");
            let simple_float = sub.float()
                .expect("Reading trivial tagged tuple[1]");
            assert_matches!(simple_string, Some(ref s) if s == "foo");
            assert_eq!(simple_float, 3.1415);
        } else {
            assert_eq!(fields[1].name().to_string(), &"some_string".to_string());
            assert_matches!(*fields[1].type_(), Type::String);
            assert_eq!(fields[0].name().to_string(), &"some_number".to_string());
            assert_matches!(*fields[0].type_(), Type::Number);
            let simple_float = sub.float()
                .expect("Reading trivial tagged tuple[1]");
            let simple_string = sub.string()
                .expect("Reading trivial tagged tuple[0]");
            assert_matches!(simple_string, Some(ref s) if s == "foo");
            assert_eq!(simple_float, 3.1415);
        }
    }

    debug!("Testing list I/O");

    {
        let mut writer = TreeTokenWriter::new();
        writer.list(vec![])
            .expect("Writing empty list");

        let mut reader = TreeTokenReader::new(Cursor::new(writer.data()), &syntax);
        let (len, _) = reader.list()
            .expect("Reading empty list");
        assert_eq!(len, 0);
    }

    {
        let mut writer = TreeTokenWriter::new();
        let item_0 = writer.string(Some("foo")).unwrap();
        let item_1 = writer.string(Some("bar")).unwrap();
        writer.list(vec![item_0, item_1])
            .expect("Writing trivial list");

        let mut reader = TreeTokenReader::new(Cursor::new(writer.data()), &syntax);
        let (len, mut sub) = reader.list()
            .expect("Reading trivial list");
        assert_eq!(len, 2);

        let simple_string = sub.string()
            .expect("Reading trivial list[0]");
        assert_matches!(simple_string, Some(ref s) if s == "foo");
        let simple_string = sub.string()
            .expect("Reading trivial list[1]");
        assert_matches!(simple_string, Some(ref s) if s == "bar");
    }

    {
        let mut writer = TreeTokenWriter::new();
        let item_0 = writer.string(Some("foo")).unwrap();
        let item_1 = writer.string(Some("bar")).unwrap();
        let list = writer.list(vec![item_0, item_1])
            .expect("Writing inner list");
        writer.list(vec![list])
            .expect("Writing outer list");

        let mut reader = TreeTokenReader::new(Cursor::new(writer.data()), &syntax);
        let (len, mut sub) = reader.list()
            .expect("Reading outer list");
        assert_eq!(len, 1);

        let (len, mut sub) = sub.list()
            .expect("Reading inner list");
        assert_eq!(len, 2);

        let simple_string = sub.string()
            .expect("Reading trivial list[0]");
        assert_matches!(simple_string, Some(ref s) if s == "foo");
        let simple_string = sub.string()
            .expect("Reading trivial list[1]");
        assert_matches!(simple_string, Some(ref s) if s == "bar");
    }

    {
        let mut writer = TreeTokenWriter::new();
        let item_0 = writer.string(None).unwrap();
        let item_1 = writer.string(Some("bar")).unwrap();
        let list = writer.list(vec![item_0, item_1])
            .expect("Writing inner list");
        writer.list(vec![list])
            .expect("Writing outer list");

        let mut reader = TreeTokenReader::new(Cursor::new(writer.data()), &syntax);
        let (len, mut sub) = reader.list()
            .expect("Reading outer list");
        assert_eq!(len, 1);

        let (len, mut sub) = sub.list()
            .expect("Reading inner list");
        assert_eq!(len, 2);

        let simple_string = sub.string()
            .expect("Reading trivial list[0]");
        assert_matches!(simple_string, None);
        let simple_string = sub.string()
            .expect("Reading trivial list[1]");
        assert_matches!(simple_string, Some(ref s) if s == "bar");
    }

    {
        let mut writer = TreeTokenWriter::new();
        let item_0 = writer.string(Some("foo")).unwrap();
        let item_1 = writer.string(None).unwrap();
        let list = writer.list(vec![item_0, item_1])
            .expect("Writing inner list");
        writer.list(vec![list])
            .expect("Writing outer list");

        let mut reader = TreeTokenReader::new(Cursor::new(writer.data()), &syntax);
        let (len, mut sub) = reader.list()
            .expect("Reading outer list");
        assert_eq!(len, 1);

        let (len, mut sub) = sub.list()
            .expect("Reading inner list");
        assert_eq!(len, 2);

        let simple_string = sub.string()
            .expect("Reading trivial list[0]");
        assert_matches!(simple_string, Some(ref s) if s == "foo");
        let simple_string = sub.string()
            .expect("Reading trivial list[1]");
        assert_matches!(simple_string, None);
    }
}
