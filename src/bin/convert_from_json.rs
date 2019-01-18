//! Convert the JSON file which contains possibly invalid structure into BinAST
//! file.

extern crate binjs;
extern crate clap;
extern crate env_logger;
extern crate log;

use binjs::io::{TokenReader, TokenWriter};

use binjs::generic::Node;

use std::io::*;
use std::thread;

use clap::*;

/// A dummy struct for enter_tagged_tuple_at/exit_tagged_tuple_at call.
struct DummyNode {}
impl DummyNode {
    fn new() -> Self {
        Self {}
    }
}
impl Node for DummyNode {
    fn name(&self) -> &'static str {
        "dummy"
    }
}

/// READ all items from reader, and write them to writer.
fn convert<R: Read, W: TokenWriter>(
    reader: &mut binjs::io::json::read::Decoder<R>,
    writer: &mut W,
) {
    loop {
        match reader.next_type() {
            binjs::io::json::read::NextType::String => {
                let path = reader.get_path();
                let val = reader.string_at(&path).expect("Error");
                match val {
                    Some(s) => {
                        writer.string_at(Some(&s), &path).expect("Error");
                    }
                    None => {
                        writer.string_at(None, &path).expect("Error");
                    }
                }
            }
            binjs::io::json::read::NextType::Enum => {
                let path = reader.get_path();
                let val = reader.string_enum_at(&path).expect("Error");
                writer.string_enum_at(&val, &path).expect("Error");
            }
            binjs::io::json::read::NextType::IdentifierName => {
                let path = reader.get_path();
                let val = reader.identifier_name_at(&path).expect("Error");
                match val {
                    Some(s) => {
                        writer.identifier_name_at(Some(&s), &path).expect("Error");
                    }
                    None => {
                        writer.identifier_name_at(None, &path).expect("Error");
                    }
                }
            }
            binjs::io::json::read::NextType::PropertyKey => {
                let path = reader.get_path();
                let val = reader.property_key_at(&path).expect("Error");
                match val {
                    Some(s) => {
                        writer.property_key_at(Some(&s), &path).expect("Error");
                    }
                    None => {
                        writer.property_key_at(None, &path).expect("Error");
                    }
                }
            }
            binjs::io::json::read::NextType::Float => {
                let path = reader.get_path();
                let val = reader.float_at(&path).expect("Error");
                writer.float_at(val, &path).expect("Error");
            }
            binjs::io::json::read::NextType::UnsignedLong => {
                let path = reader.get_path();
                let val = reader.unsigned_long_at(&path).expect("Error");
                writer.unsigned_long_at(val, &path).expect("Error");
            }
            binjs::io::json::read::NextType::Bool => {
                let path = reader.get_path();
                let val = reader.bool_at(&path).expect("Error");
                writer.bool_at(val, &path).expect("Error");
            }
            binjs::io::json::read::NextType::List => {
                let path = reader.get_path();
                let len = reader.enter_list_at(&path).expect("Error");
                writer.enter_list_at(len as usize, &path).expect("Error");
            }
            binjs::io::json::read::NextType::EndOfList => {
                let path = reader.get_path();
                reader.exit_list_at(&path).expect("Error");
                writer.exit_list_at(&path).expect("Error");
            }
            binjs::io::json::read::NextType::TaggedTuple => {
                let path = reader.get_path();
                let (name, maybe_field_names) = reader.enter_tagged_tuple_at(&path).expect("Error");
                let field_names = maybe_field_names.expect("Field names should be populated");

                let mut children = Vec::new();
                for name in field_names.iter() {
                    children.push(name);
                }

                let node = DummyNode::new();
                writer
                    .enter_tagged_tuple_at(&node, &name, children.as_slice(), &path)
                    .expect("Error");
            }
            binjs::io::json::read::NextType::EndOfTaggedTuple => {
                let (name, field_names) = reader.current_tagged_tuple_info();

                let path = reader.get_path();
                reader.exit_tagged_tuple_at(&path).expect("Error");

                let mut children = Vec::new();
                for name in field_names.iter() {
                    children.push(name);
                }

                let node = DummyNode::new();
                writer
                    .exit_tagged_tuple_at(&node, &name, children.as_slice(), &path)
                    .expect("Error");
            }
            binjs::io::json::read::NextType::End => {
                break;
            }
            binjs::io::json::read::NextType::Error => {
                break;
            }
        }
    }
}

fn main() {
    thread::Builder::new()
        .name("large stack dedicated thread".to_string())
        .stack_size(20 * 1024 * 1024)
        .spawn(|| {
            main_aux();
        })
        .expect("Could not launch dedicated thread")
        .join()
        .expect("Error in dedicated thread");
}

fn main_aux() {
    env_logger::init();

    let matches = App::new("Converter from BinAST JSON format to other BinAST format")
        .author("arai <arai@mozilla.com>")
        .about("Convert a BinAST JSON file to other BinAST format file. The JSON file is exported by binjs_encode with advanced json option")
        .args(&[
            Arg::with_name("quiet")
                .long("quiet")
                .short("q")
                .help("Do not print progress"),
        ])
        .subcommand(binjs::io::Format::subcommand())
        .get_matches();

    // Format options.
    let mut format =
        binjs::io::Format::from_matches(&matches).expect("Could not parse encoding format");

    let mut reader = binjs::io::json::read::Decoder::new(stdin()).expect("Failed to parse");

    match format {
        binjs_io::Format::Simple { .. } => {
            let w = binjs::io::simple::TreeTokenWriter::new();
            let mut writer = binjs::io::TokenWriterTreeAdapter::new(w);
            convert(&mut reader, &mut writer);
            let data = writer.done().expect("Error");
            stdout()
                .write((*data).as_ref())
                .expect("Could not write to stdout");
        }
        binjs::io::Format::Multipart {
            ref mut targets, ..
        } => {
            let w = binjs::io::multipart::TreeTokenWriter::new(targets.clone());
            let mut writer = binjs::io::TokenWriterTreeAdapter::new(w);
            convert(&mut reader, &mut writer);
            let data = writer.done().expect("Error");
            stdout()
                .write((*data).as_ref())
                .expect("Could not write to stdout");
        }
        binjs_io::Format::Entropy { ref options } => {
            let mut writer = binjs_io::entropy::write::Encoder::new((*options).clone());
            convert(&mut reader, &mut writer);
            let data = writer.done().expect("Error");
            stdout()
                .write((*data).as_ref())
                .expect("Could not write to stdout");
        }
        _ => {
            panic!("unsupported format");
        }
    }
}
