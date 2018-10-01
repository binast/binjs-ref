use entropy::{ ASTPath, DecodingModel, Model };

use io::{FileStructurePrinter, TrivialGuard, TokenReader};
use ::TokenReaderError;

use binjs_shared:: { FieldName, IdentifierName, InterfaceName, PropertyKey, SharedString };

use range_encoding::CumulativeDistributionFrequency;
use range_encoding::opus;

use std;
use std::io::{ Cursor, Read };
use std::rc::Rc;

struct Decompressor {
    decoder: opus::Reader<Cursor<Vec<u8>>>,
}
impl Decompressor {
    fn decode_uncompressed_bit(&mut self) -> Result<bool, std::io::Error> {
        unimplemented!()
    }
    fn decode_uncompressed_bits(&mut self, bits: &mut[bool]) -> Result<(), std::io::Error> {
        unimplemented!()
    }
    fn decode_uncompressed_varnum(&mut self) -> Result<Option<u32>, std::io::Error> {
        unimplemented!()
    }
    fn decode_uncompressed_float(&mut self) -> Result<Option<f64>, std::io::Error> {
        unimplemented!()
    }
    fn decode_uncompressed_frequency(&mut self) -> Result<u32, std::io::Error> {
        unimplemented!()
    }
    fn decode_uncompressed_cdf(&mut self) -> Result<Vec<u32>, std::io::Error> {
        unimplemented!()
    }

    fn tagged_tuple(&mut self, cdf: &mut CumulativeDistributionFrequency) -> Result<(InterfaceName, Option<Rc<Box<[FieldName]>>>, TrivialGuard<TokenReaderError>), TokenReaderError>
    {
        // FIXME: 1. Get the tag.
        // FIXME: 2. Update the Path.
        // FIXME: 3. Wait a second, how do we update the path betwen tuple items?
        // FIXME: 4. Path should be something handled by the driver, right?
        // FIXME: 5. Create the guard that will pop the Path.
        unimplemented!()
    }
}
struct TreeTokenReader<M> where M: DecodingModel {
    decompressor: Decompressor,
    path: ASTPath,
    model: M,
}

impl<M> FileStructurePrinter for TreeTokenReader<M> where M: DecodingModel {

}
impl<M> TreeTokenReader<M> where M: DecodingModel {
    fn update_model(&mut self) -> Result<(), TokenReaderError> {
        // 1. Do we have the cdf for `path`?
            // If so, return.
            // Otherwise
            // - Read the width (varnum).
            // - Read individual widths (varnum) until we reache `width`
        unimplemented!()
    }
}

impl<M> TokenReader for TreeTokenReader<M> where M: DecodingModel {
    type Error = TokenReaderError;
    type ListGuard = TrivialGuard<TokenReaderError>;
    type TaggedGuard = TrivialGuard<TokenReaderError>;
    type UntaggedGuard = TrivialGuard<TokenReaderError>;

    fn poison(&mut self) {
        // FIXME: Do we really need to implement it anymore?
    }

    fn string(&mut self) -> Result<Option<SharedString>, Self::Error> {
        self.update_model()?;
        unimplemented!()
    }

    fn float(&mut self) -> Result<Option<f64>, Self::Error> {
        self.update_model()?;
        unimplemented!()
    }

    fn bool(&mut self) -> Result<Option<bool>, Self::Error> {
        self.update_model()?;
        unimplemented!()
    }

    fn offset(&mut self) -> Result<u32, Self::Error> {
        self.update_model()?;
        unimplemented!()
    }

    fn unsigned_long(&mut self) -> Result<u32, Self::Error> {
        unimplemented!()
    }

    fn list(&mut self) -> Result<(u32, Self::ListGuard), Self::Error> {
        self.update_model()?;
        unimplemented!()
    }

    fn tagged_tuple(&mut self) -> Result<(InterfaceName, Option<Rc<Box<[FieldName]>>>, Self::TaggedGuard), Self::Error> {
        if let Some(cdf) = self.model.tag_frequency_for_decoding(&self.path) {
            return self.decompressor.tagged_tuple(cdf)
        }
        // Otherwise, we first need to initialize the CDF
        let cdf = self.decompressor.decode_uncompressed_cdf().unwrap(); // FIXME: Handle errors
        self.model.init_tag_frequency_for_decoding(&self.path, cdf);
        let cdf = self.model.tag_frequency_for_decoding(&self.path)
            .unwrap(); // We just installed it above.
        self.decompressor.tagged_tuple(cdf)
    }

    fn untagged_tuple(&mut self) -> Result<Self::UntaggedGuard, Self::Error> {
        self.update_model()?;
        unimplemented!()
    }
}

