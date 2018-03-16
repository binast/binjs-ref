//! Encode a text source to a BinJS.

extern crate binjs;
extern crate binjs_generic;
extern crate binjs_es6;
extern crate binjs_io;
extern crate binjs_meta;
extern crate clap;
extern crate env_logger;

use binjs::source::{ Shift, SourceParser };
use binjs_generic::annotate::Annotator;
use binjs_generic::io::encode::Encode;
use binjs_meta::spec::{ Spec, SpecBuilder, SpecOptions };

use std::cell::RefCell;
use std::fs::*;
use std::io::*;
use std::path::{ Path, PathBuf };

use clap::*;

struct Options<'a> {
    parser: &'a Shift,
    spec: &'a Spec,
    annotator: &'a Annotator,
    multipart_stats: RefCell<binjs_io::multipart::Statistics>,
    simple_stats: RefCell<binjs_io::simple::Statistics>,
    dump_ast: bool,
    compression: Option<binjs_io::multipart::WriteOptions>,
    dest_dir: Option<PathBuf>
}

fn handle_path<'a>(options: &Options<'a>,
    source_path: &Path,
    sub_dir: &Path)
{
    println!("Treating {:?} ({:?})", source_path, sub_dir);
    let is_dir = std::fs::metadata(source_path)
        .unwrap()
        .is_dir();
    if is_dir {
        let file_name = source_path.file_name()
            .unwrap_or_else(|| panic!("Invalid source path {:?}", source_path));
        let sub_dir = sub_dir.join(file_name);
        for entry in std::fs::read_dir(source_path)
            .expect("Could not open directory")
            .map(|dir| dir.unwrap())
        {
            handle_path(options, entry.path().as_path(), &sub_dir);
        }
        return;
    }
    if let Some(Some("js")) = source_path.extension().map(std::ffi::OsStr::to_str) {
        // Proceed
    } else {
        println!("Skipping {:?}", source_path);
        return;
    }
    let (dest_txt_path, dest_bin_path) = match options.dest_dir {
        None => (None, None), // Do not write
        Some(ref d) => {
            let file_name = source_path.file_stem()
                .expect("Could not extract file name");

            std::fs::create_dir_all(d.join(sub_dir))
                .expect("Could not find or create destination directory");

            let mut bin_path = d.join(sub_dir);
            bin_path.push(file_name);
            bin_path.set_extension("binjs");

            let mut txt_path = d.join(sub_dir);
            txt_path.push(file_name);
            txt_path.set_extension("js");

            (Some(txt_path), Some(bin_path))
        }
    };

    if let Some(ref bin_path) = dest_bin_path {
        println!("Output: {}", bin_path.to_string_lossy());
    } else {
        println!("Compressing to memory");
    }

    let source_len = std::fs::metadata(source_path)
        .expect("Could not open source")
        .len();

    println!("Parsing.");
    let mut ast    = options.parser.parse_file(source_path)
        .expect("Could not parse source");

    println!("Annotating.");
    options.annotator.annotate(&mut ast);

    if options.dump_ast {
        println!("Dumping AST.\n{:2}", ast.pretty(2));
    }

    println!("Encoding.");
    let data: Box<AsRef<[u8]>> = {
        match options.compression {
            None => {
                let writer = binjs_io::simple::TreeTokenWriter::new();
                let encoder = binjs_generic::io::encode::Encoder::new(options.spec, writer);
                encoder
                    .encode(&ast)
                    .expect("Could not encode AST");
                let (data, stats) = encoder.done()
                    .expect("Could not finalize AST encoding");

                let mut borrow = options.simple_stats.borrow_mut();
                *borrow += stats;
                Box::new(data)
            }
            Some(ref compression) => {
                let writer = binjs_io::multipart::TreeTokenWriter::new(compression.clone());
                let encoder = binjs_generic::io::encode::Encoder::new(options.spec, writer);
                encoder
                    .encode(&ast)
                    .expect("Could not encode AST");
                let (data, stats) = encoder.done()
                    .expect("Could not finalize AST encoding");

                let mut borrow = options.multipart_stats.borrow_mut();
                *borrow += stats.with_source_bytes(source_len as usize);
                Box::new(data)
            }
        }
    };

    let dest_len = data.as_ref().as_ref().len();

    if let Some(ref bin_path) = dest_bin_path {
        println!("Writing binary file.");
        let mut dest = File::create(bin_path)
            .unwrap_or_else(|e| panic!("Could not create destination file {:?}: {:?}", bin_path, e));
        dest.write((*data).as_ref())
            .expect("Could not write destination file");
    } else {
        println!("Skipping write.");
    }

    if let Some(ref txt_path) = dest_txt_path {
        println!("Copying source file.");
        std::fs::copy(source_path, txt_path)
            .expect("Could not copy source file");
    }

    println!("Successfully compressed {} bytes => {} bytes", source_len, dest_len);
}
fn main() {
    env_logger::init();

    let matches = App::new("BinJS encoder")
        .author("David Teller, <dteller@mozilla.com>")
        .about("Encode a JavaScript text source to a JavaScript binary source in the BinJS format.")
        .args(&[
            Arg::with_name("in")
                .long("in")
                .short("i")
                .multiple(true)
                .takes_value(true)
                .help("Input files to use. Must be JS source file."),
            Arg::with_name("out")
                .long("out")
                .short("o")
                .takes_value(true)
                .help("Output directory to use. Files in this directory may be overwritten. Required if `in` is specified more than once."),
            Arg::with_name("format")
                .long("format")
                .takes_value(true)
                .possible_values(&["simple", "multipart"])
                .help("Format to use for writing to OUTPUT. Defaults to `multipart`."),
            Arg::with_name("strings")
                .long("strings")
                .takes_value(true)
                .possible_values(&["identity", "gzip", "deflate", "br", "lzw"])
                .help("Compression format for strings. Defaults to identity."),
            Arg::with_name("sections")
                .long("sections")
                .takes_value(true)
                .possible_values(&["identity", "gzip", "deflate", "br", "lzw"])
                .help("Compression format for all sections. Defaults to identity."),
            Arg::with_name("grammar")
                .long("grammar")
                .takes_value(true)
                .possible_values(&["identity", "gzip", "deflate", "br", "lzw"])
                .help("Compression format for the grammar table. Defaults to identity."),
            Arg::with_name("tree")
                .long("tree")
                .takes_value(true)
                .possible_values(&["identity", "gzip", "deflate", "br", "lzw"])
                .help("Compression format for the tree. Defaults to identity."),
            Arg::with_name("statistics")
                .long("show-stats")
                .help("Show statistics."),
            Arg::with_name("dump")
                .long("dump")
                .help("Dup JSON AST tree"),
        ])
        .group(ArgGroup::with_name("multipart")
            .args(&["strings", "grammar", "tree"])
            .multiple(true)
        )
        .get_matches();

    let sources : Vec<_> = matches.values_of("in")
        .expect("Missing `in`")
        .map(Path::new)
        .collect();

    let dest_dir = match matches.value_of("out") {
        None => None,
        Some(path) => Some(Path::new(path).to_path_buf())
    };

    let compression = {
        let is_compressed =
            match matches.value_of("format") {
                None | Some("multipart") => true,
                _ if matches.values_of("sections").is_some()
                   || matches.value_of("strings").is_some()
                   || matches.value_of("grammar").is_some()
                   || matches.value_of("tree").is_some()
                 => {
                    println!("Error: Cannot specify `strings`, `grammar` or `tree` with this format.\n{}", matches.usage());
                    std::process::exit(-1);
                 }
                _ => false
            };
        if is_compressed {
            if let Some(ref compression) = matches.value_of("sections") {
                let compression = binjs_io::bytes::compress::Compression::parse(Some(compression))
                    .expect("Could not parse sections compression format");
                Some(binjs_io::multipart::WriteOptions {
                    strings_table: compression.clone(),
                    grammar_table: compression.clone(),
                    tree: compression,
                })
            } else {
                let strings = binjs_io::bytes::compress::Compression::parse(matches.value_of("strings"))
                    .expect("Could not parse string compression format");
                let grammar = binjs_io::bytes::compress::Compression::parse(matches.value_of("grammar"))
                    .expect("Could not parse grammar compression format");
                let tree = binjs_io::bytes::compress::Compression::parse(matches.value_of("tree"))
                    .expect("Could not parse tree compression format");
                Some(binjs_io::multipart::WriteOptions {
                    strings_table: strings,
                    grammar_table: grammar,
                    tree
                })
            }
        } else {
            println!("Format: simple");
            None
        }
    };
    let show_stats = matches.is_present("statistics");
    let dump_ast = matches.is_present("dump");

    // Setup.
    let parser = Shift::new();
    let mut spec_builder = SpecBuilder::new();
    let library = binjs_generic::es6::Library::new(&mut spec_builder);
    let spec = spec_builder.into_spec(SpecOptions {
        null: &library.null,
        root: &library.program,
    });

    let multipart_stats = binjs_io::multipart::Statistics::default()
        .with_source_bytes(0);
    let simple_stats = binjs_io::simple::Statistics::default();

    let options = Options {
        parser: &parser,
        spec: &spec,
        multipart_stats: RefCell::new(multipart_stats),
        simple_stats: RefCell::new(simple_stats),
        annotator: &library,
        compression,
        dump_ast,
        dest_dir,
    };
    for source_path in sources {
        handle_path(&options, source_path, PathBuf::new().as_path());
    }

    if show_stats {
        if options.compression.is_none() {
            println!("Statistics: {}", options.simple_stats.borrow());
        } else {
            println!("Statistics: {}", options.multipart_stats.borrow());
        }
    }
}
