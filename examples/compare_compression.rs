//! Compare compression results

extern crate binjs;
extern crate clap;
extern crate env_logger;
extern crate glob;
extern crate rand;

use binjs::io::bytes::compress::*;
use binjs::io::multipart::{ SectionOption, WriteOptions };
use binjs::io::{ Format, TokenSerializer };
use binjs::generic::FromJSON;
use binjs::source::*;

use clap::*;

use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Write;
use std::process::Command;
use std::rc::Rc;

#[derive(Clone)]
struct Sizes {
    uncompressed: usize,
    gzip: usize,
    bzip2: usize,
    brotli: usize,
}
/*
impl std::ops::Add for Sizes {
    type Output = Self;
    fn add(self, rhs: Sizes) -> Sizes {
        self.uncompressed += rhs.uncompressed;
        self.gzip += rhs.gzip;
        self.bzip2 += rhs.bzip2;
        self.brotli += rhs.brotli;
        self
    }
}
*/
#[derive(Clone)]
struct FileStats {
    from_text: Sizes,
    from_binjs: Sizes,
}

fn get_compressed_sizes(path: &std::path::Path) -> Sizes {
    let uncompressed = std::fs::metadata(path)
        .expect("Could not open source")
        .len() as usize;
    let gzip = {
        let out = Command::new("gzip")
            .arg("--keep")
            .arg("--best")
            .arg("--stdout")
            .arg(path)
            .output()
            .expect("Error during gzip");
        assert!(out.status.success());
        assert!(out.stdout.len() != 0);
        out.stdout.len()
    };
    let bzip2 = {
        let out = Command::new("bzip2")
            .arg("--keep")
            .arg("--best")
            .arg("--stdout")
            .arg(path)
            .output()
            .expect("Error during bzip2");
        assert!(out.status.success());
        assert!(out.stdout.len() != 0);
        out.stdout.len()
    };
    let brotli = {
        let out = Command::new("brotli")
            .arg("--best")
            .arg("--keep")
            .arg("--stdout")
            .arg(path)
            .output()
            .expect("Error during brotli");
        assert!(out.status.success());
        assert!(out.stdout.len() != 0);
        out.stdout.len()
    };
    Sizes {
        bzip2,
        brotli,
        gzip,
        uncompressed,
    }
}

fn main() {
    env_logger::init();
    let dest_path_binjs = "/tmp/binjs-test.js.binjs";

    let matches = App::new("Compare BinJS compression and brotli/gzip compression")
        .author("David Teller <dteller@mozilla.com>")
        .args(&[
            Arg::with_name("in")
                .long("in")
                .short("i")
                .multiple(true)
                .required(true)
                .takes_value(true)
                .help("Glob path towards source files"),
            Arg::with_name("compression")
                .long("compression")
                .short("c")
                .required(true)
                .takes_value(true)
                .possible_values(&["identity", "gzip", "br", "deflate", "trp"])
                .help("Compression format for the binjs files"),
        ])
        .get_matches();

    let format = {
        let make_multipart = |compression: Compression| {
            Format::Multipart {
                stats: Rc::new(RefCell::new(binjs::io::multipart::Statistics::default()
                    .with_source_bytes(0))),
                options: WriteOptions {
                    grammar_table: SectionOption::Compression(compression.clone()),
                    strings_table: SectionOption::Compression(compression.clone()),
                    tree: SectionOption::Compression(compression)
                }
            }
        };
        match matches.value_of("compression") {
            Some("identity") => make_multipart(Compression::Identity),
            Some("gzip") => make_multipart(Compression::Gzip),
            Some("br") => make_multipart(Compression::Brotli),
            Some("trp") => Format::TreeRePair,
            otherwise => panic!("Unsupported compression: {:?}", otherwise)
        }
    };

    let parser = Shift::new();

    let mut all_stats = HashMap::new();

    for path in matches.values_of("in").expect("Missing `in`") {
        for source_path in glob::glob(&path).expect("Invalid pattern") {
            let source_path = source_path.expect("I/O error");
            eprintln!("Source: {}", source_path.to_str().expect("Could not display path"));

            let from_text = get_compressed_sizes(&source_path);

            eprintln!("Compressing with binjs");
            let json = parser.parse_file(source_path.clone())
                .expect("Could not parse source");
            let mut ast = binjs::specialized::es6::ast::Script::import(&json)
                .expect("Could not import AST");
            binjs::specialized::es6::scopes::AnnotationVisitor::new()
                .annotate_script(&mut ast);

            let data: Box<AsRef<[u8]>>  = match format {
                Format::Multipart { ref options, .. } => {
                    let writer = binjs::io::multipart::TreeTokenWriter::new(options.clone());
                    let mut serializer = binjs::specialized::es6::io::Serializer::new(writer);
                    serializer.serialize(&ast)
                        .expect("Could not encode AST");
                    let (data, _) = serializer.done()
                        .expect("Could not finalize AST encoding");
                    Box::new(data)
                }
                Format::TreeRePair => {
                    let writer = binjs::io::repair::Encoder::new();
                    let mut serializer = binjs::specialized::es6::io::Serializer::new(writer);
                    serializer.serialize(&ast)
                        .expect("Could not encode AST");
                    let (data, _) = serializer.done()
                        .expect("Could not finalize AST encoding");
                    Box::new(data)
                }
                _ => unimplemented!()
            };

            {
                let mut binjs_encoded = std::fs::File::create(&dest_path_binjs)
                    .expect("Could not create binjs-encoded file");
                binjs_encoded.write_all((*data).as_ref())
                    .expect("Could not write binjs-encoded file");
            }

            let from_binjs = get_compressed_sizes(&std::path::Path::new(dest_path_binjs));

            let file_stats = FileStats {
                from_binjs,
                from_text,
            };

            eprintln!("Compression results: source {source}b, source+gzip {source_gzip}, source+brotli {source_brotli}, source+bzip2 {source_bzip2}, binjs {binjs}b, binjs+gzip {binjs_gzip}, binjs+brotli {binjs_brotli}, binjs+bzip2 {binjs_bzip2}",
                source = file_stats.from_text.uncompressed,
                source_gzip = file_stats.from_text.gzip,
                source_brotli = file_stats.from_text.brotli,
                source_bzip2 = file_stats.from_text.bzip2,

                binjs = file_stats.from_binjs.uncompressed,
                binjs_gzip = file_stats.from_binjs.gzip,
                binjs_brotli = file_stats.from_binjs.brotli,
                binjs_bzip2 = file_stats.from_binjs.bzip2,
            );

            all_stats.insert(source_path, file_stats);
        }
    }

    eprintln!("*** Done");
    eprintln!("File, Source (b), Source+Gzip (b), Source+Brotli (b), Source+BZip2 (b), BinAST (b), BinAST/Source, BinAST+GZip (b), BinAST+GZip/Source+GZip, BinAST+GZip/BinAST, BinAST+Brotli (b), BinAST+Brotli/Source+Brotli, BinAST+Brotli/BinAST, BinAST+BZip2 (b), BinAST+BZip2/Source+BZip2, BinAST+BZip2/BinAST");
    for (path, file_stats) in &all_stats {
        println!("{path:?}, {source}, {source_gzip}, {source_brotli}, {source_bzip2}, {binjs}, {uncompressed_to_uncompressed:2}, {binjs_gzip}, {gzip_to_gzip:2}, {gzip_to_uncompressed:2}, {binjs_brotli}, {brotli_to_brotli:2}, {brotli_to_uncompressed:2}, {binjs_bzip2}, {bzip2_to_bzip2:2}, {bzip2_to_uncompressed:2}",
            source = file_stats.from_text.uncompressed,
            source_gzip = file_stats.from_text.gzip,
            source_brotli = file_stats.from_text.brotli,
            source_bzip2 = file_stats.from_text.bzip2,

            binjs = file_stats.from_binjs.uncompressed,
            uncompressed_to_uncompressed = (file_stats.from_binjs.uncompressed as f64) / (file_stats.from_text.uncompressed as f64),
            binjs_gzip = file_stats.from_binjs.gzip,
            gzip_to_gzip = (file_stats.from_binjs.gzip as f64) / (file_stats.from_text.gzip as f64),
            gzip_to_uncompressed = (file_stats.from_binjs.gzip as f64) / (file_stats.from_binjs.uncompressed as f64),

            binjs_brotli = file_stats.from_binjs.brotli,
            brotli_to_brotli = (file_stats.from_binjs.brotli as f64) / (file_stats.from_text.brotli as f64),
            brotli_to_uncompressed = (file_stats.from_binjs.brotli as f64) / (file_stats.from_binjs.uncompressed as f64),

            binjs_bzip2 = file_stats.from_binjs.bzip2,
            bzip2_to_bzip2 = (file_stats.from_binjs.bzip2 as f64) / (file_stats.from_text.bzip2 as f64),
            bzip2_to_uncompressed = (file_stats.from_binjs.bzip2 as f64) / (file_stats.from_binjs.uncompressed as f64),

            path = path);
    }
}

