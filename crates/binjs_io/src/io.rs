//! Mid-level API used to write individual components of an AST tree to a
//! stream of tokens.
//!
//! Simple implementations may write bytes as they come, while complex
//! ones may decide to provide sophisticated compression.
//!
//! In practice, this API is kept as a trait to simplify unit testing and
//! experimentation of sophisticated compression schemes.

use std::fmt::{ Debug, Display };
use std::marker::PhantomData;
use std::ops::Add;
use std::rc::Rc;

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

/// An API for reading tokens.
///
/// Note that a `TokenReader` by itself *cannot* determine the nature of the
/// following token. Rather, the driver of the `TokenReader` must be able to
/// deduce the nature of the following token from what it has previously
/// read.
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
    fn poison(&mut self);

    /// Read a single UTF-8 string.
    ///
    /// The returned string MUST be valid UTF-8.
    fn string(&mut self) -> Result<Option<String>, Self::Error>;

    /// Read a single `f64`. Note that all numbers are `f64`.
    fn float(&mut self) -> Result<Option<f64>, Self::Error>;

    /// Read a single `u32`.
    fn unsigned_long(&mut self) -> Result<u32, Self::Error>;

    /// Read a single `bool`.
    fn bool(&mut self) -> Result<Option<bool>, Self::Error>;

    /// Read a single number of bytes.
    fn offset(&mut self) -> Result<u32, Self::Error>;

    /// Start reading a list.
    ///
    /// Returns a guard for that list and the number of elements
    /// in the list. Once the list is read entirely, callers MUST
    /// call `guard.done()` to ensure that the list was properly
    /// read (in particular that all bytes were consumed). In most
    /// implementations, failure to do so will raise an assertion.
    fn list(&mut self) -> Result<(u32, Self::ListGuard), Self::Error>;

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
    fn tagged_tuple(&mut self) -> Result<(String, Option<Rc<Box<[String]>>>, Self::TaggedGuard), Self::Error>;

    /// Start reading an untagged tuple.
    ///
    /// Once the tuple is read entirely, callers MUST
    /// call `guard.done()` to ensure that the tuple was properly
    /// read (in particular that all bytes were consumed). In most
    /// implementations, failure to do so will raise an assertion.
    fn untagged_tuple(&mut self) -> Result<Self::UntaggedGuard, Self::Error>;
}

/// Build an in-memory representation of a BinTree.
///
/// Implementations may for instance introduce atoms,
/// maximal sharing, etc.
pub trait TokenWriter where Self::Error: Debug, Self::Statistics: Display + Sized + Add + Default {
    /// The type of trees manipulated by this writer.
    type Tree;

    /// Statistics produced by this writer.
    type Statistics;

    /// The type of data generated by this writer.
    /// Typically some variant of `Vec<u8>`.
    type Data: AsRef<[u8]>;

    /// An error returned by this writer.
    ///
    /// Note that errors are *not* recoverable within the life
    /// of this `TokenWriter`.
    type Error;

    /// Finish writing, produce data.
    fn done(self) -> Result<(Self::Data, Self::Statistics), Self::Error>;

    /// Write a tagged tuple.
    ///
    /// The number of items is specified by the grammar, so it MAY not be
    /// recorded by the `TokenWriter`.
    ///
    /// The interface MUST have a Tag.
    fn tagged_tuple(&mut self, tag: &str, &[(&str, Self::Tree)]) -> Result<Self::Tree, Self::Error>;

    /// Write an untagged tuple.
    ///
    /// The number of items is specified by the grammar, so it MAY not be
    /// recorded by the `TokenWriter`.
    fn untagged_tuple(&mut self, &[Self::Tree]) -> Result<Self::Tree, Self::Error>;

    /// Write a list.
    ///
    /// By opposition to a tuple, the number of items is variable and MUST
    /// be somehow recorded by the `TokenWriter`.
    fn list(&mut self, Vec<Self::Tree>) -> Result<Self::Tree, Self::Error>;

    /// Write a single UTF-8 string.
    ///
    /// If specified, the string MUST be UTF-8.
    fn string(&mut self, Option<&str>) -> Result<Self::Tree, Self::Error>;

    /// Write a single number.
    fn float(&mut self, Option<f64>) -> Result<Self::Tree, Self::Error>;

    /// Write a single u32.
    fn unsigned_long(&mut self, u32) -> Result<Self::Tree, Self::Error>;

    /// Write single bool.
    fn bool(&mut self, Option<bool>) -> Result<Self::Tree, Self::Error>;

    /// Write the number of bytes left in this tuple.
    fn offset(&mut self) -> Result<Self::Tree, Self::Error>;
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
    fn serialize(&mut self, data: T) -> Result<W::Tree, W::Error>;
}
pub trait TokenSerializer<W> where W: TokenWriter {
    fn done(self) -> Result<(W::Data, W::Statistics), W::Error>;
}


pub trait Deserialization<R, T> where R: TokenReader, T: Sized {
    fn deserialize(&mut self) -> Result<T, R::Error>;
}
pub trait InnerDeserialization<R, T> where R: TokenReader, T: Sized {
    fn deserialize_inner(&mut self) -> Result<T, R::Error>;
}
