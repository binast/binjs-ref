//! Compare compression results

extern crate binjs;
extern crate clap;
extern crate env_logger;
extern crate glob;
extern crate rand;

use binjs::bytes::compress::*;
use binjs::token::encode::*;
use binjs::source::*;

use clap::*;

use std::collections::HashMap;
use std::process::Command;

#[derive(Default, Clone)]
struct FileStats {
    before: u64,
    after_binjs: u64,
    after_gzip: u64,
    after_br: u64,
    binjs_compression: binjs::token::multipart::Statistics,
}

impl FileStats {
    fn ratio_binjs(&self) -> f64 {
        self.after_binjs as f64 / self.before as f64
    }
    fn ratio_gz(&self) -> f64 {
        self.after_gzip as f64 / self.before as f64
    }
    fn ratio_br(&self) -> f64 {
        self.after_br as f64 / self.before as f64
    }
}

fn main() {
    env_logger::init().unwrap();
    let dest_path_brotli = "/tmp/binjs-test.js.bro";

    let matches = App::new("Compare BinJS compression and brotli/gzip compression")
        .author("David Teller <dteller@mozilla.com>")
        .args(&[
            Arg::with_name("in")
                .long("in")
                .short("i")
                .required(true)
                .takes_value(true)
                .help("Glob path towards source files"),
            Arg::with_name("compression")
                .long("compression")
                .short("c")
                .required(true)
                .takes_value(true)
                .possible_values(&["identity", "gzip", "br", "deflate"])
                .help("Compression format for the binjs files"),
        ])
        .get_matches();

    let compression = matches.value_of("compression")
        .expect("Missing compression format");
    let compression = Compression::parse(Some(compression))
        .expect("Could not parse compression format");
    let binjs_options = {
        binjs::token::multipart::WriteOptions {
            strings_table: compression.clone(),
            grammar_table: compression.clone(),
            tree: compression.clone()
        }
    };

    let parser = Babel::new();
    let grammar = binjs::ast::library::syntax(binjs::ast::library::Level::Latest);

    let mut multipart_stats = binjs::token::multipart::Statistics::default()
        .with_source_bytes(0);

    let mut all_stats = HashMap::new();

    for path in matches.values_of("in").expect("Missing `in`") {
        for source_path in glob::glob(&path).expect("Invalid pattern") {
            let source_path = source_path.expect("I/O error");
            eprintln!("Source: {}", source_path.to_str().expect("Could not display path"));

            let source_len = std::fs::metadata(&source_path)
                .expect("Could not open source")
                .len();

            let mut file_stats = FileStats::default();
            file_stats.before = source_len;

            {
                eprintln!("Compressing with binjs");
                let mut ast = parser.parse_file(&source_path)
                    .expect("Could not parse source");
                grammar.annotate(&mut ast)
                    .expect("Could not infer annotations");

                let writer = binjs::token::multipart::TreeTokenWriter::new(binjs_options.clone(), &grammar);
                let encoder = binjs::token::encode::Encoder::new(&grammar, writer);
                encoder
                    .encode(&ast)
                    .expect("Could not encode AST");
                let (data, stats) = encoder.done()
                    .expect("Could not finalize AST encoding");

                file_stats.binjs_compression = stats.clone();
                multipart_stats = multipart_stats + stats.with_source_bytes(source_len as usize);

                file_stats.after_binjs = data.len() as u64;
            }

            {
                eprintln!("Comparing with gzip");

                let out = Command::new("gzip")
                    .arg("--keep")
                    .arg("--best")
                    .arg("--stdout")
                    .arg(&source_path)
                    .output()
                    .expect("Error during gzip");
                assert!(out.status.success());
                assert!(out.stdout.len() != 0);
                file_stats.after_gzip = out.stdout.len() as u64;
            }

            {
                eprintln!("Comparing with brotli");
                let _ = std::fs::remove_file(dest_path_brotli);
                let _ = Command::new("bro")
                    .args(&["--quality", "9"])
                    .arg("--input")
                    .arg(&source_path)
                    .args(&["--output", dest_path_brotli])
                    .spawn()
                    .expect("Couldn't start bro")
                    .wait()
                    .expect("Error during bro");
                file_stats.after_br = std::fs::metadata(&dest_path_brotli)
                    .expect("Could not open gzip destination")
                    .len();
            }

            eprintln!("Compression results: source {}b, binjs+{} {binjs}b (x{binjs_ratio:.2}), gzip {gzip}b (x{gzip_ratio:.2}), brotli {br}b (x{br_ratio:.2})",
                file_stats.before,
                compression.code(),
                binjs=file_stats.after_binjs,
                gzip=file_stats.after_gzip,
                br=file_stats.after_br,
                binjs_ratio=file_stats.ratio_binjs(),
                gzip_ratio=file_stats.ratio_gz(),
                br_ratio=file_stats.ratio_br()
            );

            all_stats.insert(source_path, file_stats);
        }
    }

    eprintln!("*** Done");
    println!("File, Original size, Binjs size, Gzip size, Brotli size, Number of strings, Number of identifiers, Number of grammar entries");
    let identifier_node = grammar.get_node_name("Identifier").unwrap();
    for (path, stats) in &all_stats {
        let number_of_identifiers = match stats.binjs_compression.per_kind_name.get(&identifier_node) {
            None => 0,
            Some(identifiers) => identifiers.entries
        };

        println!("{path:?}, {before}, {after_binjs}, {after_gz}, {after_br}, {strings}, {identifiers}, {grammar_entries}",
            before=stats.before,
            after_binjs=stats.after_binjs,
            after_gz=stats.after_gzip,
            after_br=stats.after_br,
            strings=stats.binjs_compression.strings_table.entries,
            identifiers=number_of_identifiers,
            grammar_entries=stats.binjs_compression.grammar_table.entries,
            path=path);
    }
}
