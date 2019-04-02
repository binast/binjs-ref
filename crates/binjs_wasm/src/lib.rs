use wasm_bindgen::prelude::*;
use js_sys::Error;

use binjs_io::{Compression, TokenWriterTreeAdapter, TokenSerializer, Serialization};
use binjs_io::multipart::{Targets, TreeTokenWriter};
use binjs_es6::io::{Serializer, IOPath};
use binjs_es6::ast::Script;
use binjs_es6::Enrich;

fn convert_error(err: impl std::fmt::Debug) -> Error {
    Error::new(&format!("{:?}", err))
}

#[wasm_bindgen(js_name = encodeMultipart)]
pub fn encode_multipart(ast: JsValue) -> Result<Box<[u8]>, JsValue> {
    let mut ast: Script = serde_wasm_bindgen::from_value(ast)?;
    Enrich::default().enrich(&mut ast).map_err(convert_error)?;
    let mut serializer = Serializer::new(TokenWriterTreeAdapter::new(TreeTokenWriter::new(Targets::new(Compression::Identity))));
    serializer.serialize(&ast, &mut IOPath::new()).map_err(convert_error)?;
    Ok(serializer.done().map_err(convert_error)?)
}
