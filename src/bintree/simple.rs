//! Minimal implementation of encoding/decoding to binary.

use estree::grammar::{ Field, InterfaceNode, Syntax };
use estree::io::*;

use util::*;

use std;
use std::cell::RefCell;
use std::io::{ Read, Seek, SeekFrom };
use std::rc::Rc;
use std::string::FromUtf8Error;


#[derive(Debug)]
pub enum ExtractorError {
    Reader(std::io::Error),
    Encoding(FromUtf8Error),
    NotInList,
    NoSuchKind(String),
    NoSuchField(String),
    HeaderOrFooterNotFound { expected: String, found: Vec<u8> },
    EndOffsetError { expected: u64, found: u64 },
    BailingOutBecauseOfPreviousError
}

struct TreeExtractorImpl<'a, R> where R: Read + Seek {
    reader: PositionRead<R>,
    grammar: &'a Syntax,
}

pub struct TreeExtractor<'a, R> where R: Read + Seek {
    // Shared with all children.
    implem: Rc<RefCell<TreeExtractorImpl<'a, R>>>,
    my_pending_error: Rc<RefCell<Option<ExtractorError>>>,
    parent_pending_error: Rc<RefCell<Option<ExtractorError>>>,

    // If specified, the position at which extraction must end.
    pos_end: Option<u64>,

    // If specified, a suffix for the extraction.
    // This suffix appears *after* `pos_end`.
    suffix: Option<String>
}
impl<'a, R> TreeExtractor<'a, R> where R: Read + Seek {
    /// Create a new toplevel TreeExtractor.
    pub fn new(reader: R, grammar: &'a Syntax) -> Self {
        let implem = TreeExtractorImpl {
            reader: PositionRead::new(reader),
            grammar,
        };
        TreeExtractor {
            implem: Rc::new(RefCell::new(implem)),
            my_pending_error: Rc::new(RefCell::new(None)),
            parent_pending_error: Rc::new(RefCell::new(None)),
            pos_end: None,
            suffix: None,
        }
    }

    /// Derive a TreeExtractor for reading a subset of the stream.
    fn sub(&self, options: SubOptions) -> Self {
        let position = self.implem.borrow().reader.position();
        TreeExtractor {
            implem: self.implem.clone(),
            parent_pending_error: self.my_pending_error.clone(),
            my_pending_error: Rc::new(RefCell::new(None)),
            pos_end: options.byte_len.map(|length| position + length),
            suffix: options.suffix
        }
    }

    /// Read data to a buffer.
    ///
    /// If an error is pending, propagate the error.
    /// If reading causes an error, any further call to `read()` will
    /// cause an error of type
    /// `ExtractorError::BailingOutBecauseOfPreviousError`.
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, ExtractorError> {
        let error = self.my_pending_error.borrow_mut().take();
        if let Some(error) = error {
            *self.my_pending_error.borrow_mut() = Some(ExtractorError::BailingOutBecauseOfPreviousError);
            return Err(error)
        }
        let ref mut reader = self.implem.borrow_mut().reader;
        match reader.read_exact(buf) {
            Ok(ok) => Ok(buf.len()),
            Err(err) => {
                let error = ExtractorError::Reader(err);
                *self.my_pending_error.borrow_mut() = Some(ExtractorError::BailingOutBecauseOfPreviousError);
                Err(error)
            }
        }
    }

    fn read_u32(&mut self) -> Result<u32, ExtractorError> {
        let result : u32;
        let mut buf : [u8; 4] = unsafe { std::mem::uninitialized() };
        debug_assert!(std::mem::size_of::<u32>() == std::mem::size_of_val(&buf));
        self.read(&mut buf)?;
        result = unsafe { std::mem::transmute(buf) };
        Ok(result)
    }

    fn read_string(&mut self) -> Result<String, ExtractorError> {
        let mut bytes = Vec::new();
        let mut buf: [u8;1] = [0];
        loop {
            self.read(&mut buf)?;
            if buf[0] == 0 {
                return String::from_utf8(bytes)
                    .map_err(ExtractorError::Encoding);
            }
            bytes.push(buf[0])
        }
    }

    fn read_constant(&mut self, value: &str) -> Result<(), ExtractorError> {
        let mut buf : Vec<u8> = value.bytes().collect();
        self.read(&mut buf)?;
        for (expected, found) in buf.iter().zip(value.bytes()) {
            if *expected != found {
                return Err(ExtractorError::HeaderOrFooterNotFound {
                    found: buf.clone(),
                    expected: value.to_string()
                });
            }
        }
        Ok(())
    }
}
impl<'a, R> Drop for TreeExtractor<'a, R> where R: Read + Seek {
    fn drop(&mut self) {
        // Check byte_len, if available.
        if let Some(expected) = self.pos_end {
            let ref reader = self.implem.borrow().reader;
            if reader.position() != expected {
                *self.parent_pending_error.borrow_mut() =
                    Some(ExtractorError::EndOffsetError {
                        expected,
                        found: reader.position()
                    });
                return;
            }
        }
        // Check suffix, if available.
        if let Some(ref suffix) = self.suffix.take() {
            if let Err(err) = self.read_constant(suffix) {
                *self.parent_pending_error.borrow_mut() = Some(err);
                return;
            }
        }
    }
}
impl<'a, R> Extractor for TreeExtractor<'a, R> where R: Read + Seek {
    type Error = ExtractorError;

    fn skip(&mut self) -> Result<(), Self::Error> {
        if let Some(end) = self.pos_end {
            self.implem.borrow_mut().reader.seek(SeekFrom::Start(end))
                .map_err(ExtractorError::Reader)?;
            Ok(())
        } else {
            Err(ExtractorError::NotInList)
        }
    }

    fn bool(&mut self) -> Result<bool, Self::Error> {
        let mut buf : [u8; 1] = unsafe { std::mem::uninitialized() };
        self.read(&mut buf)?;
        Ok(buf[0] != 0)
    }

    fn float(&mut self) -> Result<f64, Self::Error> {
        let mut buf : [u8; 8] = unsafe { std::mem::uninitialized() };
        assert!(std::mem::size_of_val(&buf) == std::mem::size_of::<f64>());
        self.read(&mut buf)?;
        Ok(unsafe { std::mem::transmute::<_, f64>(buf) })
    }

    fn string(&mut self) -> Result<Option<String>, Self::Error> {
        let mut bytes = Vec::new();
        // Read until we get a \0.
        loop {
            let mut buf = [ 0 ];
            self.read(&mut buf)?;
            if buf[0] == 0 {
                // Sniff magic sequence [255, 0].
                if bytes.len() == 1 && bytes[0] == 255 {
                    return Ok(None)
                }
                break;
            }
            bytes.push(buf[0]);
        }
        match String::from_utf8(bytes) {
            Ok(string) => Ok(Some(string)),
            Err(err) => Err(ExtractorError::Encoding(err))
        }
    }

    fn list(&mut self) -> Result<(u32, Self), Self::Error> {
        self.read_constant("<list>")?;
        let byte_len = self.read_u32()?;
        let mut extractor = self.sub(SubOptions {
            byte_len: Some(byte_len as u64),
            suffix: Some("</list>".to_string())
        });
        let list_len = extractor.read_u32()?;
        Ok((list_len, extractor))
    }

    fn tagged_tuple(&mut self) -> Result<(String, Rc<Box<[Field]>>, Self), Self::Error> {
        self.read_constant("<tuple>")?;
        // Read (and validate) the kind.
        let kind_name = self.read_string()?;
        let kind = { self.implem.borrow().grammar.get_kind(&kind_name)
            .ok_or_else(|| ExtractorError::NoSuchKind(kind_name.clone()))? };
        let interface = { self.implem.borrow().grammar.get_interface_by_kind(&kind)
            .ok_or_else(|| ExtractorError::NoSuchKind(kind_name.clone()))? };

        // Read the field names
        let len = self.read_u32()?;
        let mut field_names = Vec::with_capacity(len as usize);
        for _ in 0..len {
            let string_name = self.read_string()?;
            let field_name = self.implem.borrow().grammar.get_field_name(&string_name)
                .ok_or_else(|| ExtractorError::NoSuchField(string_name.clone()))?;
            field_names.push(field_name);
        }

        // Attach types
        let fields = Vec::with_capacity(len as usize);
        let obj = interface.contents();
        'fields: for field_name in field_names.drain(..) {
            for field in obj.fields() {
                if field_name == *field.name() {
                    fields.push(field.clone());
                    continue 'fields
                }
            }
        }
        let extractor = self.sub(SubOptions {
            suffix: Some("</tuple>".to_string()),
            ..SubOptions::default()
        });
        Ok((kind_name, Rc::new(fields.into_boxed_slice()), extractor))
    }

    fn untagged_tuple(&mut self) -> Result<Self, Self::Error> {
        self.read_constant("<tuple>")?;
        Ok(self.sub(SubOptions {
            suffix: Some("</tuple>".to_string()),
            ..SubOptions::default()
        }))
    }
}

struct SubOptions {
    byte_len: Option<u64>,
    suffix: Option<String>,
}
impl Default for SubOptions {
    fn default() -> Self {
        SubOptions {
            byte_len: None,
            suffix: None,
        }
    }
}

/// A trivial tree writer, without any kind of optimization.
pub struct TreeBuilder {
    root: Rc<Vec<u8>>
}
impl TreeBuilder {
    fn register(&mut self, data: Vec<u8>) -> Rc<Vec<u8>> {
        let result = Rc::new(data);
        self.root = result.clone();
        result
    }
}

pub enum BuilderError {
    MissingKind,
}
impl Builder for TreeBuilder {
    type Tree = Rc<Vec<u8>>;
    type Error = BuilderError;

    fn float(&mut self, data: f64) -> Result<Self::Tree, Self::Error> {
        let buf : [u8; 8] = unsafe { std::mem::transmute(data) };
        assert!(std::mem::size_of_val(&buf) == std::mem::size_of_val(&data));
        let result = buf.iter().cloned().collect();
        Ok(self.register(result))
    }

    fn bool(&mut self, data: bool) -> Result<Self::Tree, Self::Error> {
        let buf = [if data { 1 } else { 0 }];
        let result = buf.iter().cloned().collect();
        Ok(self.register(result))
    }

    // Strings are represented as UTF-8, \0-terminated.
    fn string(&mut self, data: &str) -> Result<Self::Tree, Self::Error> {
        let mut result : Vec<_> = data.as_bytes().iter().cloned().collect();
        result.push(0);
        Ok(self.register(result))
    }

    fn no_string(&mut self) -> Result<Self::Tree, Self::Error> {
        let buf = vec![255, 0]; // This is an invalid UTF8 string.
        Ok(self.register(buf))
    }

    /// Lists are represented as:
    /// - number of bytes (u32);
    /// - number of items (u32);
    /// - items
    fn list(&mut self, items: Vec<Self::Tree>) -> Result<Self::Tree, Self::Error> {
        let mut result = Vec::new();
        result.extend_from_slice(&"<list>".as_bytes());// Sole purpose of this constant is testing

        let number_of_items = items.len() as u32;
        let mut buf : [u8; 4] = unsafe { std::mem::transmute(number_of_items) };
        assert!(std::mem::size_of_val(&buf) == std::mem::size_of_val(&number_of_items));

        // Placeholder for `byte_len`
        result.extend_from_slice(&buf);
        result.extend_from_slice(&buf);
        for item in items {
            result.extend_from_slice(&*item)
        }

        let byte_len = result.len() as u32;
        assert!(std::mem::size_of_val(&buf) == std::mem::size_of_val(&byte_len));
        buf = unsafe { std::mem::transmute(number_of_items) };

        for i in 0..buf.len() {
            result[i] = buf[i];
        }

        result.extend_from_slice(&"</list>".as_bytes());// Sole purpose of this constant is testing
        Ok(self.register(result))
    }

    /// For this example, use a very, very, very suboptimal encoding.
    /// - (if specified)
    ///   - kind (string, \0 terminated)
    ///   - field names (string, \0 terminated)
    /// - contents
    fn tuple(&mut self, children: Vec<Self::Tree>, interface: Option<&InterfaceNode>) -> Result<Self::Tree, Self::Error> {
        let mut result = Vec::new();
        result.extend_from_slice(&"<tuple>".as_bytes()); // Sole purpose of this constant is testing
        if let Some(node) = interface {
            let kind = node.kind()
                .ok_or_else(|| BuilderError::MissingKind)?;
            let bytes : Vec<_> = kind.to_string().bytes().collect();
            result.extend_from_slice(&bytes);
            result.push(0);

            let number_of_items = node.contents().fields().len() as u32;
            let mut buf : [u8; 4] = unsafe { std::mem::transmute(number_of_items) };
            assert!(std::mem::size_of_val(&buf) == std::mem::size_of_val(&number_of_items));
            result.extend_from_slice(&buf);

            for field in node.contents().fields() {
                let bytes : Vec<_> = field.name().to_string().bytes().collect();
                result.extend_from_slice(&bytes);
                result.push(0);
            }
        }
        for item in children {
            result.extend_from_slice(&*item)
        }
        result.extend_from_slice(&"</tuple>".as_bytes()); // Sole purpose of this constant is testing
        Ok(self.register(result))
    }
}
