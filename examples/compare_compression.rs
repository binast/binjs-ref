//! Compare compression results

extern crate binjs;
extern crate clap;
extern crate env_logger;
extern crate glob;
extern crate rand;

use binjs::io::bytes::compress::*;
use binjs::io::TokenSerializer;
use binjs::generic::FromJSON;
use binjs::source::*;

use clap::*;

use std::collections::HashMap;
use std::io::Write;
use std::process::Command;

#[derive(Default, Clone)]
struct FileStats {
    before: u64,
    after_binjs: u64,
    after_binjs_post: u64,
    after_gzip: u64,
    after_br: u64,
    binjs_compression: binjs::io::multipart::Statistics,
}

impl FileStats {
    fn ratio_binjs_post(&self) -> f64 {
        self.after_binjs_post as f64 / self.before as f64
    }
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
    env_logger::init();
    let dest_path_binjs = "/tmp/binjs-test.js.binjs";
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
            Arg::with_name("post-compression")
                .long("post")
                .takes_value(true)
                .possible_values(&["gzip", "br"])
                .help("Post-compression format to apply to the binjs files"),
            Arg::with_name("compression")
                .long("compression")
                .short("c")
                .required(true)
                .takes_value(true)
                .possible_values(&["identity", "gzip", "br", "deflate"])
                .help("Compression format for the binjs files"),
        ])
        .get_matches();

    let post_compression = match matches.value_of("post-compression") {
        None => None,
        Some("gzip") => Some(Compression::Gzip),
        Some("br") => Some(Compression::Brotli),
        _ => panic!()
    };
    let compression = matches.value_of("compression")
        .expect("Missing compression format");
    let compression = Compression::parse(Some(compression))
        .expect("Could not parse compression format");
    let binjs_options = {
        binjs::io::multipart::WriteOptions {
            strings_table: compression.clone(),
            grammar_table: compression.clone(),
            tree: compression.clone()
        }
    };

    let parser = Shift::new();

    let mut multipart_stats = binjs::io::multipart::Statistics::default()
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
                let json = parser.parse_file(source_path.clone())
                    .expect("Could not parse source");
                let mut ast = binjs::specialized::es6::ast::Script::import(&json)
                    .expect("Could not import AST");
                binjs::specialized::es6::scopes::AnnotationVisitor::new()
                    .annotate_script(&mut ast);

                let writer = binjs::io::multipart::TreeTokenWriter::new(binjs_options.clone());
                let mut serializer = binjs::specialized::es6::io::Serializer::new(writer);
                serializer.serialize(&ast)
                    .expect("Could not encode AST");
                let (data, stats) = serializer.done()
                    .expect("Could not finalize AST encoding");

                file_stats.binjs_compression = stats.clone();
                multipart_stats = multipart_stats + stats.with_source_bytes(source_len as usize);

                let len = data.len() as u64;
                let len_post : u64 = match post_compression {
                    None => data.len() as u64,
                    Some(Compression::Gzip) => {
                        eprintln!("Post-compressing with gzip");
                        let mut file = std::fs::File::create(&dest_path_binjs)
                            .expect("Could not create tmp .binjs file");
                        file.write_all(&data)
                            .expect("Could not write to tmp .bijs file");
                        drop(file);
                        let out = Command::new("gzip")
                            .arg("--keep")
                            .arg("--best")
                            .arg("--stdout")
                            .arg(&dest_path_binjs)
                            .output()
                            .expect("Error during gzip");
                        assert!(out.status.success());
                        assert!(out.stdout.len() != 0);
                        out.stdout.len() as u64
                    }
                    Some(Compression::Brotli) => {
                        eprintln!("Post-compressing with brotli");
                        let mut file = std::fs::File::create(&dest_path_binjs)
                            .expect("Could not create tmp .binjs file");
                        file.write_all(&data)
                            .expect("Could not write to tmp .bijs file");
                        drop(file);
                        let _ = std::fs::remove_file(dest_path_brotli);
                        let _ = Command::new("brotli")
                            .arg("-9")
                            .arg(&dest_path_binjs)
                            .args(&["-o", dest_path_brotli])
                            .spawn()
                            .expect("Couldn't start bro")
                            .wait()
                            .expect("Error during bro");
                        std::fs::metadata(&dest_path_brotli)
                            .expect("Could not open bro destination")
                            .len()
                    }
                    _ => panic!()
                };
                file_stats.after_binjs = len;
                file_stats.after_binjs_post = len_post;
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
                let _ = Command::new("brotli")
                    .arg("-9")
                    .arg(&source_path)
                    .args(&["-o", dest_path_brotli])
                    .spawn()
                    .expect("Couldn't start bro")
                    .wait()
                    .expect("Error during bro");
                file_stats.after_br = std::fs::metadata(&dest_path_brotli)
                    .expect("Could not open bro destination")
                    .len();
            }

            eprintln!("Compression results: source {source}b, {binjs}b, binjs+{compression} (x{binjs_ratio:.2}) {binjs_compressed}b (x{binjs_ratio_post:.2}), gzip {gzip}b (x{gzip_ratio:.2}), brotli {br}b (x{br_ratio:.2})",
                source = file_stats.before,
                binjs = file_stats.after_binjs,
                compression = compression.code(),
                binjs_compressed=file_stats.after_binjs_post,
                gzip=file_stats.after_gzip,
                br=file_stats.after_br,
                binjs_ratio=file_stats.ratio_binjs(),
                binjs_ratio_post=file_stats.ratio_binjs_post(),
                gzip_ratio=file_stats.ratio_gz(),
                br_ratio=file_stats.ratio_br()
            );

            all_stats.insert(source_path, file_stats);
        }
    }

    eprintln!("*** Done");
    println!("File, Original size, Gzip size, Brotli size, Binjs size, Binjs + compression, Number of strings, Number of identifiers, Number of grammar entries");
    for (path, stats) in &all_stats {
        let number_of_binding_identifiers = match stats.binjs_compression.per_kind_name.get("BindingIdentifier") {
            None => 0,
            Some(identifiers) => identifiers.entries
        };
        let number_of_expression_identifiers = match stats.binjs_compression.per_kind_name.get("IdentifierExpression") {
            None => 0,
            Some(identifiers) => identifiers.entries
        };

        println!("{path:?}, {before}, {after_gz}, {after_br}, {after_binjs}, {after_binjs_post}, {strings}, {identifiers}, {grammar_entries}",
            before=stats.before,
            after_binjs=stats.after_binjs,
            after_binjs_post=stats.after_binjs_post,
            after_gz=stats.after_gzip,
            after_br=stats.after_br,
            strings=stats.binjs_compression.strings_table.entries,
            identifiers=number_of_binding_identifiers + number_of_expression_identifiers,
            grammar_entries=stats.binjs_compression.grammar_table.entries,
            path=path);
    }
}

