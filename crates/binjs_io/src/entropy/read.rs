//! An entropy decoder

use ::TokenReaderError;
use ::io::{ FileStructurePrinter, Path, TokenReader, TrivialGuard };

use binjs_shared::{ F64, FieldName, IdentifierName, InterfaceName, PropertyKey, SharedString };

use range_encoding::opus;

pub struct Decoder {
    reader: opus::Reader<std::io::Cursor<Vec<u8>>>,
    options: ::entropy::Options,
}

impl FileStructurePrinter for Decoder {

}

/// Read a single symbol.
///
/// Used instead of a method as we need to generality wrt the field name.
macro_rules! symbol {
    ( $me: ident, $table:ident, $path:expr ) => {
        {
            use std::ops::DerefMut;
            let path = $path.tail($me.options.path_depth);
            let index = {
                let symbol = $me.options.probability_tables
                    .$table
                    .at(path)
                    .ok_or_else(|| TokenReaderError::NotInDictionary(format!("{}", stringify!($ident))))?;
                let mut borrow = symbol.distribution
                    .borrow_mut();
                let index = $me.reader.symbol(borrow.deref_mut())
                    .map_err(TokenReaderError::ReadError)?;
                index
            };
            let value = $me.options.probability_tables
                .$table
                .get_at(path, index as usize)
                .ok_or_else(|| TokenReaderError::NotInDictionary(format!("{} [{}]", stringify!($ident), index)))?;
            Ok(value.clone())
        }
    }
}

impl TokenReader for Decoder {
    type ListGuard = TrivialGuard;
    type TaggedGuard = TrivialGuard;
    type UntaggedGuard = TrivialGuard;

    // ---- String types

    fn string_at(&mut self, path: &Path) -> Result<Option<SharedString>, TokenReaderError> {
        symbol!(self, string_literal_by_path, path)
    }

    fn string_enum_at(&mut self, path: &Path) -> Result<SharedString, TokenReaderError> {
        symbol!(self, string_enum_by_path, path)
    }

    fn identifier_name_at(&mut self, path: &Path) -> Result<Option<IdentifierName>, TokenReaderError> {
        symbol!(self, identifier_name_by_path, path)
    }

    fn property_key_at(&mut self, path: &Path) -> Result<Option<PropertyKey>, TokenReaderError> {
        symbol!(self, property_key_by_path, path)
    }

    // ---- Primitive types

    fn float_at(&mut self, path: &Path) -> Result<Option<f64>, TokenReaderError> {
        let value = symbol!(self, float_by_path, path)?;
        Ok(value.map(F64::into))
    }

    fn unsigned_long_at(&mut self, path: &Path) -> Result<u32, TokenReaderError> {
        symbol!(self, unsigned_long_by_path, path)
    }

    fn bool_at(&mut self, path: &Path) -> Result<Option<bool>, TokenReaderError> {
        symbol!(self, bool_by_path, path)
    }

    // ---- Lazy

    fn offset_at(&mut self, _path: &Path) -> Result<u32, TokenReaderError> {
        unimplemented!()
    }

    // ---- Composed types

    fn list_at(&mut self, path: &Path) -> Result<(u32, Self::ListGuard), TokenReaderError> {
        let length = symbol!(self, list_length_by_path, path)?
            .ok_or_else(|| TokenReaderError::EmptyList)?;
            // For the moment, we cannot read an optional list.
        Ok((length, TrivialGuard::new()))
    }

    fn tagged_tuple_at(&mut self, path: &Path) -> Result<(InterfaceName, Option<std::rc::Rc<Box<[FieldName]>>>, Self::TaggedGuard), TokenReaderError> {
        let name = symbol!(self, interface_name_by_path, path)?;
        Ok((name, None, TrivialGuard::new()))
    }

    fn untagged_tuple_at(&mut self, _path: &Path) -> Result<Self::UntaggedGuard, TokenReaderError> {
        unimplemented!()
    }
}