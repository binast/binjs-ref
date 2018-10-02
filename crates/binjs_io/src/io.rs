//! Mid-level API used to write individual components of an AST tree to a
//! stream of tokens.
//!
//! Simple implementations may write bytes as they come, while complex
//! ones may decide to provide sophisticated compression.
//!
//! In practice, this API is kept as a trait to simplify unit testing and
//! experimentation of sophisticated compression schemes.

use binjs_shared::{ IdentifierName, InterfaceName, FieldName, PropertyKey, SharedString, self };

use ::{ TokenWriterError };

#[cfg(multistream)]
use std::collections::HashMap;
use std::fmt::{ Debug, Display };
#[cfg(multistream)]
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::Add;
use std::rc::Rc;

#[cfg(multistream)]
use itertools::Itertools;

/// An API for printing the binary representation and its structural
/// interpretation of the file.
///
/// A struct which implements this trait is supposed to print the binary
/// representation of the file in the first column internally, and print the
/// structural interpretation in the second column, which is triggered by
/// print_file_structure!() macro below, from various places that interprets
/// the file structure.
///
/// ```ignore
/// (first column)   (second column)
/// 01             # Script {
///                # .scope
/// 08             # AssertedScriptGlobalScope {
///                # .declared_names
/// 02             # list (length=0) [
/// ...            # ...
///                # ]
///                # }
///                # }
/// ```
///
/// Default no-op implementations are provided for all methods, which can be
/// used by the following declaration:
///
/// ```ignore
/// impl FileStructurePrinter for T {}
/// ```
///
pub trait FileStructurePrinter {
    /// Enables printing the binary representation and the structural
    /// interpretation.
    fn enable_file_structure_print(&mut self) {}

    /// Disables printing the binary representation and the structural
    /// interpretation.
    fn disable_file_structure_print(&mut self) {}

    /// True if file structure print is enabled.
    /// With default implementation, nothing is printed.
    fn is_file_structure_print_enabled(&mut self) -> bool {
        false
    }

    /// Prints the column separator ("# " characters), and padding before it
    /// if necessary.
    ///
    /// Before calling this method, the output is supposed to be the following:
    ///
    /// ```ignore
    /// 00 01 02<= cursor position
    /// ```
    ///
    /// After calling this method, the output is supposed to be the following:
    ///
    /// ```ignore
    /// 00 01 02     # <= cursor position
    /// ```
    ///
    /// The column width is not defined but the implementation is supposed to
    /// keep the same column width through the entire output, regardless of the
    /// number of bytes it reads for each line.
    fn prepare_file_structure_column(&mut self) {}

    /// Prints newline after printing the structural interpretation column.
    /// The implementation is supposed to print newline character(s) and also
    /// reset the current column position internally, in order to print the
    /// right number of spaces in prepare_file_structure_column.
    ///
    /// Before calling this method, the output is supposed to be the following:
    ///
    /// ```ignore
    /// 00 01 02     # string="hello"<= cursor position
    /// ```
    ///
    /// After calling this method, the output is supposed to be the following:
    ///
    /// ```ignore
    /// 00 01 02     # string="hello"
    /// <= cursor position
    /// ```
    ///
    fn newline_for_file_structure_print(&mut self) {}
}

/// Prints the structural interpretation of the data read after the last time
/// this macro is called.
///
/// Before calling this macro, the output is supposed to be the following:
///
/// ```ignore
/// 00 01 02<= cursor position
/// ```
///
/// After calling this macro with the following code:
///
/// ```ignore
/// print_file_structure!(reader, "string=\"hello\"");
/// ```
///
/// the output is supposed to be the following:
///
/// ```ignore
/// 00 01 02     # string="hello"
/// <= cursor position
/// ```
///
/// This macro may be called multiple times before reading any data, and in
/// that case the output is supposed to be the following:
///
/// ```ignore
/// 00 01 02     # string="hello"
///              # }
///              # .directives
/// <= cursor position
/// ```
///
#[macro_export]
macro_rules! print_file_structure(
    ( $reader:expr, $fmt:expr $( , $more:expr )* ) => (
        if $reader.is_file_structure_print_enabled() {
            $reader.prepare_file_structure_column();
            print!( $fmt $( , $more )* );
            $reader.newline_for_file_structure_print();
        }
    )
);


pub type Path = binjs_shared::ast::Path<InterfaceName, /* Field */ (usize, FieldName)>;

/// An API for reading tokens.
///
/// Note that a `TokenReader` by itself *cannot* determine the nature of the
/// following token. Rather, the driver of the `TokenReader` must be able to
/// deduce the nature of the following token from what it has previously
/// read.
///
/// All the reading methods offer a version suffixed with `_at(path: &Path)`,
/// which lets the reader determine what item we're reading in the AST. This
/// may be used both for debugging purposes and for encodings that depend
/// on the current position in the AST (e.g. entropy coding).
pub trait TokenReader: FileStructurePrinter where Self::Error: Debug + From<::TokenReaderError>,
                                                  Self::ListGuard: Guard<Error = Self::Error>,
                                                  Self::TaggedGuard: Guard<Error = Self::Error>,
                                                  Self::UntaggedGuard: Guard<Error = Self::Error>,
                                                  Self: Sized
{
    /// An error returned by the extractor.
    ///
    /// Errors are *not* recoverable within a `TokenReader`.
    ///
    /// For instance, if attempting to read with `string()`
    /// fails, any further attempt to use the `TokenReader`
    /// or any of its parents will also raise an error.
    type Error;

    /// A guard, used to make sure that the consumer has properly read a list.
    ///
    /// See the documentation of `self.list`.
    type ListGuard;

    /// A guard, used to make sure that the consumer has properly read a tagged tuple.
    ///
    /// See the documentation of `self.tagged_tuple`.
    type TaggedGuard;

    /// A guard, used to make sure that the consumer has properly read an untagged tuple.
    ///
    /// See the documentation of `self.untagged_tuple`.
    type UntaggedGuard;

    /// Poison the reader, ensuring that it will never be used for reading again.
    fn poison(&mut self) {
        // Do nothing.
    }

    /// Read a single UTF-8 string.
    ///
    /// The returned string MUST be valid UTF-8.
    fn string(&mut self) -> Result<Option<SharedString>, Self::Error>;
    fn string_at(&mut self, _path: &Path) -> Result<Option<SharedString>, Self::Error> {
        self.string()
    }

    /// Read a single UTF-8 value from a string enumeration.
    ///
    /// The default implementation uses `self.string``, but some encodings may use
    /// the extra information e.g. to represent the enumeration by an index in the
    /// list of possible values, or to encode string enums as interfaces.
    ///
    /// The returned string MUST be valid UTF-8.
    fn string_enum(&mut self) -> Result<SharedString, Self::Error> {
        self.string()?
            .ok_or_else(|| ::TokenReaderError::EmptyVariant.into())
    }
    fn string_enum_at(&mut self, _path: &Path) -> Result<SharedString, Self::Error> {
        self.string_enum()
    }

    /// Read a single identifier name.
    ///
    /// The default implementation uses `self.string`/`self.string_at`, but
    /// some encodings may use the extra information e.g. to represent the
    /// identifier as a DeBruijn index.
    fn identifier_name(&mut self) -> Result<Option<IdentifierName>, Self::Error> {
        let result = self.string()?
            .map(IdentifierName);
        Ok(result)
    }
    fn identifier_name_at(&mut self, _path: &Path) -> Result<Option<IdentifierName>, Self::Error> {
        self.identifier_name()
    }

    /// Read a single property name.
    ///
    /// The default implementation uses `self.string`/`self.string_at`, but
    /// some encodings may use the extra information e.g. to initialize
    /// dictionaries.
    fn property_key(&mut self) -> Result<Option<PropertyKey>, Self::Error> {
        let result = self.string()?
            .map(PropertyKey);
        Ok(result)
    }
    fn property_key_at(&mut self, _path: &Path) -> Result<Option<PropertyKey>, Self::Error> {
        self.property_key()
    }

    /// Read a single `f64`. Note that all user-level numbers are `f64`.
    fn float(&mut self) -> Result<Option<f64>, Self::Error>;
    fn float_at(&mut self, _path: &Path) -> Result<Option<f64>, Self::Error> {
        self.float()
    }

    /// Read a single `u32`.
    fn unsigned_long(&mut self) -> Result<u32, Self::Error>;
    fn unsigned_long_at(&mut self, _path: &Path) -> Result<u32, Self::Error> {
        self.unsigned_long()
    }

    /// Read a single `bool`.
    fn bool(&mut self) -> Result<Option<bool>, Self::Error>;
    fn bool_at(&mut self, _path: &Path) -> Result<Option<bool>, Self::Error> {
        self.bool()
    }

    /// Read a single number of bytes.
    fn offset(&mut self) -> Result<u32, Self::Error>;
    fn offset_at(&mut self, _path: &Path) -> Result<u32, Self::Error> {
        self.offset()
    }

    /// Start reading a list.
    ///
    /// Returns a guard for that list and the number of elements
    /// in the list. Once the list is read entirely, callers MUST
    /// call `guard.done()` to ensure that the list was properly
    /// read (in particular that all bytes were consumed). In most
    /// implementations, failure to do so will raise an assertion.
    fn list(&mut self) -> Result<(u32, Self::ListGuard), Self::Error>;
    fn list_at(&mut self, _path: &Path) -> Result<(u32, Self::ListGuard), Self::Error> {
        self.list()
    }

    /// Start reading a tagged tuple. If the stream was encoded
    /// properly, the tag is attached to an **ordered** tuple of
    /// fields that may be extracted **in order**.
    ///
    /// Returns the tag name, the ordered array of fields in which
    /// the contents must be read, and a guard for that tuple.
    /// Once the tuple is read entirely, callers MUST
    /// call `guard.done()` to ensure that the tuple was properly
    /// read (in particular that all bytes were consumed). In most
    /// implementations, failure to do so will raise an assertion.
    fn tagged_tuple(&mut self) -> Result<(InterfaceName, Option<Rc<Box<[FieldName]>>>, Self::TaggedGuard), Self::Error>;
    fn tagged_tuple_at(&mut self, _path: &Path) -> Result<(InterfaceName, Option<Rc<Box<[FieldName]>>>, Self::TaggedGuard), Self::Error> {
        self.tagged_tuple()
    }

    /// Start reading an untagged tuple.
    ///
    /// Once the tuple is read entirely, callers MUST
    /// call `guard.done()` to ensure that the tuple was properly
    /// read (in particular that all bytes were consumed). In most
    /// implementations, failure to do so will raise an assertion.
    fn untagged_tuple(&mut self) -> Result<Self::UntaggedGuard, Self::Error>;
    fn untagged_tuple_at(&mut self, _path: &Path) -> Result<Self::UntaggedGuard, Self::Error> {
        self.untagged_tuple()
    }
}

/// Build an in-memory representation of a BinTree.
///
/// Implementations may for instance introduce atoms,
/// maximal sharing, etc.
///
/// All the reading methods offer a version suffixed with `_at(..., path: &Path)`,
/// which lets the writer determine what item we're reading in the AST. This
/// may be used both for debugging purposes and for encodings that depend
/// on the current position in the AST (e.g. entropy coding).
pub trait TokenWriter where Self::Statistics: Display + Sized + Add + Default {
    /// The type of trees manipulated by this writer.
    type Tree;

    /// Statistics produced by this writer.
    type Statistics;

    /// The type of data generated by this writer.
    /// Typically some variant of `Vec<u8>`.
    type Data: AsRef<[u8]>;

    /// Finish writing, produce data.
    fn done(self) -> Result<(Self::Data, Self::Statistics), TokenWriterError>;

    /// Write a tagged tuple.
    ///
    /// The number of items is specified by the grammar, so it MAY not be
    /// recorded by the `TokenWriter`.
    fn tagged_tuple(&mut self, _tag: &InterfaceName, _children: &[(FieldName, Self::Tree)]) -> Result<Self::Tree, TokenWriterError> {
        unimplemented!()
    }
    fn tagged_tuple_at(&mut self, tag: &InterfaceName, children: &[(FieldName, Self::Tree)], _path: &Path) -> Result<Self::Tree, TokenWriterError> {
        self.tagged_tuple(tag, children)
    }
    fn enter_tagged_tuple_at(&mut self, _tag: &InterfaceName, _children: usize, _path: &Path) -> Result<(), TokenWriterError> {
        Ok(())
    }
    fn exit_tagged_tuple_at(&mut self, _tag: &InterfaceName, _path: &Path) -> Result<(), TokenWriterError> {
        Ok(())
    }

    /// Write a list.
    ///
    /// By opposition to a tuple, the number of items is variable and MUST
    /// be somehow recorded by the `TokenWriter`.
    fn list(&mut self, Vec<Self::Tree>) -> Result<Self::Tree, TokenWriterError> {
        unimplemented!()
    }
    fn list_at(&mut self, items: Vec<Self::Tree>, _path: &Path) -> Result<Self::Tree, TokenWriterError> {
        self.list(items)
    }
    fn enter_list_at(&mut self, _len: usize, _path: &Path) -> Result<(), TokenWriterError> {
        Ok(())
    }
    fn exit_list_at(&mut self, _path: &Path) -> Result<(), TokenWriterError> {
        Ok(())
    }

    /// Write a single UTF-8 string.
    ///
    /// If specified, the string MUST be UTF-8.
    fn string(&mut self, Option<&SharedString>) -> Result<Self::Tree, TokenWriterError> {
        unimplemented!()
    }
    fn string_at(&mut self, value: Option<&SharedString>, _path: &Path) -> Result<Self::Tree, TokenWriterError> {
        self.string(value)
    }

    /// Write a single UTF-8 value from a string enumeration.
    ///
    /// The default implementation uses `self.string``, but some encodings may use
    /// the extra information e.g. to represent the enumeration by an index in the
    /// list of possible values, or to encode string enums as interfaces.
    fn string_enum(&mut self, str: &SharedString) -> Result<Self::Tree, TokenWriterError> {
        self.string(Some(str))
    }
    fn string_enum_at(&mut self, value: &SharedString, path: &Path) -> Result<Self::Tree, TokenWriterError> {
        self.string_at(Some(value), path)
    }

    /// Write a single number.
    fn float(&mut self, Option<f64>) -> Result<Self::Tree, TokenWriterError> {
        unimplemented!()
    }
    fn float_at(&mut self, value: Option<f64>, _path: &Path) -> Result<Self::Tree, TokenWriterError> {
        self.float(value)
    }

    /// Write a single u32.
    fn unsigned_long(&mut self, u32) -> Result<Self::Tree, TokenWriterError> {
        unimplemented!()
    }
    fn unsigned_long_at(&mut self, value: u32, _path: &Path) -> Result<Self::Tree, TokenWriterError> {
        self.unsigned_long(value)
    }

    /// Write single bool.
    // FIXME: Split `bool` from `maybe_bool`.
    fn bool(&mut self, Option<bool>) -> Result<Self::Tree, TokenWriterError> {
        unimplemented!()
    }
    fn bool_at(&mut self, value: Option<bool>, _path: &Path) -> Result<Self::Tree, TokenWriterError> {
        self.bool(value)
    }

    /// Write the number of bytes left in this tuple.
    fn offset(&mut self) -> Result<Self::Tree, TokenWriterError> {
        unimplemented!()
    }
    fn offset_at(&mut self, _path: &Path) -> Result<Self::Tree, TokenWriterError> {
        self.offset()
    }


    fn property_key(&mut self, value: Option<&PropertyKey>) -> Result<Self::Tree, TokenWriterError> {
        let string = value.map(PropertyKey::as_shared_string);
        self.string(string)
    }
    fn property_key_at(&mut self, value: Option<&PropertyKey>, _path: &Path) -> Result<Self::Tree, TokenWriterError> {
        self.property_key(value)
    }
    // FIXME: Split `property_key` from `maybe_property_key`.

    fn identifier_name(&mut self, value: Option<&IdentifierName>) -> Result<Self::Tree, TokenWriterError> {
        let string = value.map(IdentifierName::as_shared_string);
        self.string(string)
    }
    fn identifier_name_at(&mut self, value: Option<&IdentifierName>, _path: &Path) -> Result<Self::Tree, TokenWriterError> {
        self.identifier_name(value)
    }
    // FIXME: Split `identifier_name` from `maybe_identifier_name`.
}


/// A guard used to ensure that some subset of the input stream was read properly.
pub trait Guard {
    /// The type of errors returned by the guard. This is typically
    /// `TokenReader::Error`.
    type Error;

    /// Ensure that the subset of the input stream was read properly.
    fn done(self) -> Result<(), Self::Error>;
}

/// Trivial implementation of a guard.
///
/// This implementation serves as a placeholder or as a building block for
/// more sophisticated implementations: it does not check anything
/// meaningful in `done()` but ensures that `done()` is eventually called.
pub struct TrivialGuard<Error> {
    phantom: PhantomData<Error>,

    /// `true` once `done()` has been called, `false` otherwise.
    pub finalized: bool,
}
impl<E> TrivialGuard<E> {
    /// Create a `TrivialGuard`.
    ///
    /// If the `TrivialGuard` is dropped before `done()` is called
    /// or `self.finalized` is set to `true`, the drop will cause
    /// an assertion failure.
    pub fn new() -> Self {
        TrivialGuard {
            phantom: PhantomData,
            finalized: false
        }
    }
}

impl<Error> Guard for TrivialGuard<Error> {
    type Error = Error;

    /// Mark the guard as safe to be dropped.
    fn done(mut self) -> Result<(), Self::Error> {
        self.finalized = true;
        Ok(())
    }
}

impl<Error> Drop for TrivialGuard<Error> {
    /// # Failures
    ///
    /// If the `TrivialGuard` is dropped before `done()` is called
    /// or `self.finalized` is set to `true`, the drop will cause
    /// an assertion failure.
    fn drop(&mut self) {
        assert!(self.finalized)
    }
}

pub trait Serialization<W, T> where W: TokenWriter, T: Sized {
    /// Serialize a piece of data.
    ///
    /// `path` indicates the path from the root of the AST.
    fn serialize(&mut self, data: T, path: &mut Path) -> Result<W::Tree, TokenWriterError>;
}
pub trait TokenSerializer<W> where W: TokenWriter {
    fn done(self) -> Result<(W::Data, W::Statistics), TokenWriterError>;
}
pub trait RootedTokenSerializer<W, T>: Serialization<W, T> + TokenSerializer<W> where W: TokenWriter, T: Sized { }
pub trait TokenSerializerFamily<T> {
    fn make<W>(&self, writer: W) -> Box<RootedTokenSerializer<W, T>> where W: TokenWriter;
}

pub trait Deserialization<R, T> where R: TokenReader, T: Sized {
    fn deserialize(&mut self, &mut Path) -> Result<T, R::Error>;
}
pub trait InnerDeserialization<R, T> where R: TokenReader, T: Sized {
    fn deserialize_inner(&mut self, &mut Path) -> Result<T, R::Error>;
}
