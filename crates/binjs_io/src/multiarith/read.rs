use multiarith::Path;
use multiarith::tree::Tag;

use io::{TrivialGuard, TokenReader};
use ::TokenReaderError;

use std;
use std::io::Read;
use std::rc::Rc;

struct TreeTokenReader<R> where R: Read {
    inp: R,
    path: Path<(Tag, usize)>,
}

impl<R> TreeTokenReader<R> where R: Read {
    fn update_model(&mut self) -> Result<(), TokenReaderError> {
        // 1. Do we have the cdf for `path`?
            // If so, return.
            // Otherwise
            // - Read the width (varnum).
            // - Read individual widths (varnum) until we reache `width`
        unimplemented!()
    }
}

impl<R> TokenReader for TreeTokenReader<R> where R: Read {
    type Error = TokenReaderError;
    type ListGuard = TrivialGuard<TokenReaderError>;
    type TaggedGuard = TrivialGuard<TokenReaderError>;
    type UntaggedGuard = TrivialGuard<TokenReaderError>;

    fn poison(&mut self) {
        // FIXME: Do we really need to implement it anymore?
    }

    fn string(&mut self) -> Result<Option<String>, Self::Error> {
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

    fn list(&mut self) -> Result<(u32, Self::ListGuard), Self::Error> {
        self.update_model()?;
        unimplemented!()
    }

    fn tagged_tuple(&mut self) -> Result<(String, Option<Rc<Box<[String]>>>, Self::TaggedGuard), Self::Error> {
        // FIXME: Update Path!
        self.update_model()?;
        unimplemented!()
    }

    fn untagged_tuple(&mut self) -> Result<Self::UntaggedGuard, Self::Error> {
        self.update_model()?;
        unimplemented!()
    }
}

