//! Encode a BinJS, then decode it, ensure that we obtain the same AST.

extern crate binjs;
extern crate env_logger;
extern crate glob;
#[macro_use]
extern crate log;
extern crate rand;

use binjs::generic::*;
use binjs::io::bytes::compress::*;
use binjs::io::multipart::*;
use binjs::io::*;
use binjs::source::*;
use binjs::specialized::es6::ast::{ Path, Script, Visitor, Walker };

use std::io::Cursor;
use std::thread;

use rand::Rng;

/// This test takes 1h+ on Travis, which is too long, so we need to
/// reduce it. So each individual file + options combination has
/// a `CHANCES_TO_SKIP` probability of being skipped.
const CHANCES_TO_SKIP : f64 = 0.8;

const PATHS : [&'static str; 2] = ["tests/data/facebook/single/**/*.js", "tests/data/frameworks/*.js"];

fn progress() {
    // Make sure that we see progress in the logs, without spamming these logs.
    eprint!(".");
}

fn should_skip(rng: &mut Rng) -> bool {
    let float = rng.next_f64();
    float < CHANCES_TO_SKIP
}

/// A visitor designed to resent offsets to 0.
struct OffsetCleanerVisitor;
impl Visitor<()> for OffsetCleanerVisitor {
    fn visit_offset(&mut self, _path: &Path, node: &mut Offset) -> Result<(), ()> {
        *node = binjs::generic::Offset(0);
        Ok(())
    }
}

#[test]
fn test_roundtrip() {
    thread::Builder::new()
        .name("test_roundtrip large stack dedicated thread".to_string())
        .stack_size(20 * 1024 * 1024)
        .spawn(|| {
            main();
        })
        .expect("Could not launch dedicated thread")
        .join()
        .expect("Error in dedicated thread");
}

fn main() {
    env_logger::init();
    let mut rng = rand::thread_rng();

    let parser = Shift::new();

    // All combinations of options for compression.
    let all_options = {
        use self::Compression::*;
        let mut vec = vec![];
        let compressions = [Identity, Gzip, /*Deflate seems broken upstream,*/ Brotli, /*Lzw doesn't work yet*/]
            .into_iter()
            .cloned()
            .map(SectionOption::Compression)
            .collect::<Vec<_>>();
        for grammar_table in &compressions {
            for strings_table in &compressions {
                for tree in &compressions {
                    vec.push(WriteOptions {
                        grammar_table: grammar_table.clone(),
                        strings_table: strings_table.clone(),
                        tree: tree.clone(),
                    });
                }
            }
        }
        vec
    };

    eprint!("\nTesting roundtrip with laziness");
    for path_suffix in &PATHS {
        let path = format!("{}/{}", env!("CARGO_MANIFEST_DIR"), path_suffix);
        debug!(target: "test_roundtrip", "Starting laziness test_roundtrip from {}", path);

        'laziness_per_entry: for entry in glob::glob(&path)
            .expect("Invalid glob pattern")
        {
            // Randomly skip instances.
            if should_skip(&mut rng) {
                continue 'laziness_per_entry;
            }


            let mut path = binjs::specialized::es6::ast::Path::new();
            let entry = entry.expect("Invalid entry");
            eprint!("\n{:?}.", entry);

            // Parse and preprocess file.

            let json = parser.parse_file(entry.clone())
                .expect("Could not parse source");
            let mut ast = binjs::specialized::es6::ast::Script::import(&json)
                .expect("Could not import AST");
            binjs::specialized::es6::scopes::AnnotationVisitor::new()
                .annotate_script(&mut ast);

            // Immutable copy.
            let reference_ast = ast;
            'per_level: for level in &[0, 1, 2, 3, 4, 5] {

                let mut ast = reference_ast.clone();
                let mut visitor = binjs::specialized::es6::lazy::LazifierVisitor::new(*level);
                ast.walk(&mut path, &mut visitor)
                    .expect("Could not introduce laziness");
                progress();

                let options = WriteOptions {
                    grammar_table: SectionOption::Compression(Compression::Identity),
                    strings_table: SectionOption::Compression(Compression::Identity),
                    tree: SectionOption::Compression(Compression::Identity),
                };
                debug!(target: "test_roundtrip", "Encoding.");
                let writer  = binjs::io::multipart::TreeTokenWriter::new(options.clone());
                let mut serializer = binjs::specialized::es6::io::Serializer::new(writer);
                serializer.serialize(&ast)
                    .expect("Could not encode AST");
                let (data, _) = serializer.done()
                    .expect("Could not finalize AST encoding");
                progress();

                debug!(target: "test_roundtrip", "Decoding.");
                let source = Cursor::new(data);
                let reader = binjs::io::multipart::TreeTokenReader::new(source)
                    .expect("Could not decode AST container");
                let mut deserializer = binjs::specialized::es6::io::Deserializer::new(reader);

                let mut decoded : Script = deserializer.deserialize()
                    .expect("Could not decode");
                progress();

                debug!(target: "test_roundtrip", "Checking.");
                // At this stage, we have a problem: offsets are 0 in `ast`, but not 0
                // in `decoded`.
                decoded.walk(&mut path, &mut OffsetCleanerVisitor)
                    .expect("Could not cleanup offsets");
                progress();
                assert_eq!(ast, decoded);
            }
        }
    }

    eprint!("\nCompression tests");
    for path_suffix in &PATHS {
        let path = format!("{}/{}", env!("CARGO_MANIFEST_DIR"), path_suffix);
        debug!(target: "test_roundtrip", "Starting test_roundtrip from {}", path);

        'compression_per_entry: for entry in glob::glob(&path)
            .expect("Invalid glob pattern")
        {
            // Randomly skip instances.
            if should_skip(&mut rng) {
                continue 'compression_per_entry;
            }
            let entry = entry.expect("Invalid entry");

            // Parse and preprocess file.

            eprint!("\n{:?}.", entry);
            let json    = parser.parse_file(entry.clone())
                .expect("Could not parse source");
            let mut ast = binjs::specialized::es6::ast::Script::import(&json)
                .expect("Could not import AST");
            binjs::specialized::es6::scopes::AnnotationVisitor::new()
                .annotate_script(&mut ast);

            {
                progress();
                debug!(target: "test_roundtrip", "Starting simple round trip for {:?}", entry);

                // Roundtrip `simple`
                debug!(target: "test_roundtrip", "Encoding");
                let mut writer = binjs::io::simple::TreeTokenWriter::new();
                let mut serializer = binjs::specialized::es6::io::Serializer::new(writer);
                serializer.serialize(&ast)
                    .expect("Could not encode AST");
                let (data, _) = serializer.done()
                    .expect("Could not finalize AST encoding");

                progress();
                debug!(target: "test_roundtrip", "Decoding.");
                let source = Cursor::new(data);
                let reader = binjs::io::simple::TreeTokenReader::new(source);
                let mut deserializer = binjs::specialized::es6::io::Deserializer::new(reader);

                let decoded = deserializer.deserialize()
                    .expect("Could not decode");
                progress();

                debug!(target: "test_roundtrip", "Checking.");
                assert_eq!(ast, decoded);

                debug!(target: "test_roundtrip", "Completed simple round trip for {:?}", entry);
            }

            // Roundtrip `multipart`

            'per_option: for options in &all_options {
                // Randomly skip instances.
                if should_skip(&mut rng) {
                    continue 'per_option;
                }
                progress();
                debug!(target: "test_roundtrip", "Starting multipart round trip for {:?} with options {:?}", entry, options);
                debug!(target: "test_roundtrip", "Encoding.");
                let writer  = binjs::io::multipart::TreeTokenWriter::new(options.clone());
                let mut serializer = binjs::specialized::es6::io::Serializer::new(writer);
                serializer.serialize(&ast)
                    .expect("Could not encode AST");
                let (data, _) = serializer.done()
                    .expect("Could not finalize AST encoding");

                progress();

                debug!(target: "test_roundtrip", "Decoding.");
                let source = Cursor::new(data);
                let reader = binjs::io::multipart::TreeTokenReader::new(source)
                    .expect("Could not decode AST container");
                let mut deserializer = binjs::specialized::es6::io::Deserializer::new(reader);

                let decoded = deserializer.deserialize()
                    .expect("Could not decode");
                progress();

                debug!(target: "test_roundtrip", "Checking.");
                assert_eq!(ast, decoded);

                debug!(target: "test_roundtrip", "Completed multipart round trip for {:?} with options {:?}", entry, options);
                progress();
            }
        }
    }
}

