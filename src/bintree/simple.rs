//! Minimal implementation of encoding/decoding to binary.
//! FIXME: This module should probably move to `examples/`.

use estree::grammar::{ Field, InterfaceNode };
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
    EndOffsetError { expected: u64, found: u64 },
    BailingOutBecauseOfPreviousError
}

struct TreeExtractorImpl<R> where R: Read + Seek {
    reader: PositionRead<R>
}

pub struct TreeExtractor<R> where R: Read + Seek {
    // Shared with all children.
    implem: Rc<RefCell<TreeExtractorImpl<R>>>,
    my_pending_error: Rc<RefCell<Option<ExtractorError>>>,
    parent_pending_error: Rc<RefCell<Option<ExtractorError>>>,
    pos_end: Option<u64>,
}
impl<R> TreeExtractor<R> where R: Read + Seek {
    /// Create a new toplevel TreeExtractor.
    pub fn new(reader: R) -> Self {
        let implem = TreeExtractorImpl {
            reader: PositionRead::new(reader)
        };
        TreeExtractor {
            implem: Rc::new(RefCell::new(implem)),
            my_pending_error: Rc::new(RefCell::new(None)),
            parent_pending_error: Rc::new(RefCell::new(None)),
            pos_end: None
        }
    }

    /// Derive a TreeExtractor for reading a subset of the stream.
    fn sub(&self, length: u64) -> Self {
        let position = self.implem.borrow().reader.position();
        TreeExtractor {
            implem: self.implem.clone(),
            parent_pending_error: self.my_pending_error.clone(),
            my_pending_error: Rc::new(RefCell::new(None)),
            pos_end: Some(position + length)
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
        match reader.read(buf) {
            Ok(ok) => Ok(ok),
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
}
impl<R> Drop for TreeExtractor<R> where R: Read + Seek {
    fn drop(&mut self) {
        if let Some(expected) = self.pos_end {
            let ref reader = self.implem.borrow().reader;
            if reader.position() != expected {
                *self.parent_pending_error.borrow_mut() =
                    Some(ExtractorError::EndOffsetError {
                        expected,
                        found: reader.position()
                    })
            }
        }
    }
}
impl<R> Extractor for TreeExtractor<R> where R: Read + Seek {
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
        let byte_len = self.read_u32()?;
        let mut extractor = self.sub(byte_len as u64);
        let list_len = extractor.read_u32()?;
        Ok((list_len, extractor))
    }

    fn tag(&mut self) -> Result<(String, &[&Field], Self), Self::Error> {
        unimplemented!()
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

        Ok(self.register(result))
    }

    /// For this example, use a very, very, very suboptimal encoding.
    /// - (if specified)
    ///   - kind (string, \0 terminated)
    ///   - field names (string, \0 terminated)
    /// - contents
    fn tuple(&mut self, children: Vec<Self::Tree>, interface: Option<&InterfaceNode>) -> Result<Self::Tree, Self::Error> {
        let mut result = Vec::new();
        if let Some(node) = interface {
            let kind = node.kind()
                .ok_or_else(|| BuilderError::MissingKind)?;
            let bytes : Vec<_> = kind.to_string().bytes().collect();
            result.extend_from_slice(&bytes);
            result.push(0);
            for field in node.contents().fields() {
                let bytes : Vec<_> = field.name().to_string().bytes().collect();
                result.extend_from_slice(&bytes);
                result.push(0);
            }
        }
        for item in children {
            result.extend_from_slice(&*item)
        }
        Ok(self.register(result))
    }
}
