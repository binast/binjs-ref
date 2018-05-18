//! Encode a text source to a BinJS.

extern crate binjs;
extern crate clap;
extern crate env_logger;

use binjs::io::{ DictionaryPlacement, Format, NumberingStrategy, TokenSerializer };
use binjs::io::multipart::{ SectionOption, WriteOptions };
use binjs::source::{ Shift, SourceParser };
use binjs::generic::FromJSON;
use binjs::specialized::es6::ast::Walker;

use std::cell::RefCell;
use std::fs::*;
use std::io::*;
use std::thread;
use std::path::{ Path, PathBuf };
use std::rc::Rc;

use clap::*;

fn parse_compression(option: Option<&str>) -> std::result::Result<SectionOption, String> {
    use binjs::io::bytes::compress::Compression;
    let result = match option {
        None | Some("identity") => SectionOption::Compression(Compression::Identity),
        Some("gzip") => SectionOption::Compression(Compression::Gzip),
        Some("deflate") => SectionOption::Compression(Compression::Deflate),
        Some("lzw") => SectionOption::Compression(Compression::Lzw),
        Some("export") => SectionOption::AppendToBuffer(std::rc::Rc::new(RefCell::new(Vec::new()))),
        Some(other) => return Err(other.to_string())
    };
    Ok(result)
}

fn write_extract(dest_bin_path: &Option<PathBuf>, option: &SectionOption, extension: &str) {
    if let SectionOption::AppendToBuffer(ref buf) = *option {
        let path = dest_bin_path
            .clone()
            .expect("Cannot write partial file without a destination")
            .with_extension(extension);
        let mut file = File::create(path.clone())
            .unwrap_or_else(|e| panic!("Could not create destination file {:?}: {:?}", path, e));
        file.write(buf.borrow().as_ref())
            .expect("Could not write destination file");
    }
}

struct Options<'a> {
    parser: &'a Shift,
    format: Format,
    dest_dir: Option<PathBuf>,
    lazification: u32,
    show_ast: bool,
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
    let json = options.parser.parse_file(source_path)
        .expect("Could not parse source");
    let mut ast = binjs::specialized::es6::ast::Script::import(&json)
        .expect("Could not import AST");
    binjs::specialized::es6::scopes::AnnotationVisitor::new()
        .annotate_script(&mut ast);

    if options.lazification > 0 {
        println!("Introducing laziness.");
        let mut path = binjs::specialized::es6::ast::Path::new();
        let mut visitor = binjs::specialized::es6::skip::LazifierVisitor::new(options.lazification);
        ast.walk(&mut path, &mut visitor)
            .expect("Could not introduce laziness");
    }

    if options.show_ast {
        use binjs::generic::ToJSON;
        let json = ast.export();
        println!("{:#}", json);
    }

    println!("Encoding.");
    let data: Box<AsRef<[u8]>> = {
        match Format::new(&options.format) {
            Format::Simple { stats: ref my_stats } => {
                let writer = binjs::io::simple::TreeTokenWriter::new();
                let mut serializer = binjs::specialized::es6::io::Serializer::new(writer);
                serializer.serialize(&ast)
                    .expect("Could not encode AST");
                let (data, stats) = serializer.done()
                    .expect("Could not finalize AST encoding");

                let mut borrow = my_stats.borrow_mut();
                *borrow += stats;
                Box::new(data)
            }
            Format::TreeRePair { ref options } => {
                let writer = binjs::io::repair::Encoder::new(options.clone());
                let mut serializer = binjs::specialized::es6::io::Serializer::new(writer);
                serializer.serialize(&ast)
                    .expect("Could not encode AST");
                let (data, _) = serializer.done()
                    .expect("Could not finalize AST encoding");

                Box::new(data)
            }
            Format::XML => {
                let writer = binjs::io::xml::Encoder::new();
                let mut serializer = binjs::specialized::es6::io::Serializer::new(writer);
                serializer.serialize(&ast)
                    .expect("Could not encode AST");
                let (data, _) = serializer.done()
                    .expect("Could not finalize AST encoding");

                Box::new(data)
            }
            Format::MultiStream { ref options } => {
                let writer = binjs::io::multistream::TreeTokenWriter::new(options.clone());
                let mut serializer = binjs::specialized::es6::io::Serializer::new(writer);
                serializer.serialize(&ast)
                    .expect("Could not encode AST");
                let (data, _) = serializer.done()
                    .expect("Could not finalize AST encoding");

                Box::new(data)
            }
            Format::Multipart {
                stats: ref my_stats,
                options: ref compression,
            } => {
                let writer = binjs::io::multipart::TreeTokenWriter::new(compression.clone());
                let mut serializer = binjs::specialized::es6::io::Serializer::new(writer);
                serializer.serialize(&ast)
                    .expect("Could not encode AST");
                let (data, stats) = serializer.done()
                    .expect("Could not finalize AST encoding");

                write_extract(&dest_bin_path, &compression.grammar_table, "grammar");
                write_extract(&dest_bin_path, &compression.strings_table, "strings");
                write_extract(&dest_bin_path, &compression.tree, "tree");


                let mut borrow = my_stats.borrow_mut();
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
        if txt_path.exists() {
            println!("A file with name {:?} already exists, skipping copy.", txt_path);
        } else {
            println!("Copying source file.");
            std::fs::copy(source_path, txt_path)
                .expect("Could not copy source file");
        }
    }

    println!("Successfully compressed {} bytes => {} bytes", source_len, dest_len);
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

    let matches = App::new("BinJS encoder")
        .author("David Teller, <dteller@mozilla.com>")
        .about("Encode a JavaScript text source to a JavaScript binary source in the BinJS format.")
        .args(&[
            Arg::with_name("in")
                .long("in")
                .short("i")
                .multiple(true)
                .takes_value(true)
                .required(true)
                .help("Input files to use. Must be JS source file. May be specified multiple times"),
            Arg::with_name("out")
                .long("out")
                .short("o")
                .takes_value(true)
                .required(true)
                .help("Output directory to use. Files in this directory may be overwritten."),
            Arg::with_name("format")
                .long("format")
                .takes_value(true)
                .possible_values(&["simple", "multipart", "trp", "xml", "multistream"])
                .help("Format to use for writing to OUTPUT. Defaults to `multipart`."),
            Arg::with_name("trp-rank")
                .long("trp-rank")
                .takes_value(true)
                .help("Maximal rank for trp. Ignored if the format isn't trp. Number of 'none'."),
            Arg::with_name("strings")
                .long("strings")
                .takes_value(true)
                .possible_values(&["export", "discard", "identity", "gzip", "deflate", "br", "lzw"])
                .help("Compression format for strings. Defaults to identity."),
            Arg::with_name("sections")
                .long("sections")
                .takes_value(true)
                .possible_values(&["export", "discard", "identity", "gzip", "deflate", "br", "lzw"])
                .help("Compression format for all sections. Defaults to identity."),
            Arg::with_name("grammar")
                .long("grammar")
                .takes_value(true)
                .possible_values(&["export", "discard", "identity", "gzip", "deflate", "br", "lzw"])
                .help("Compression format for the grammar table. Defaults to identity."),
            Arg::with_name("tree")
                .long("tree")
                .takes_value(true)
                .possible_values(&["identity", "gzip", "deflate", "br", "lzw"])
                .help("Compression format for the tree. Defaults to identity."),
            Arg::with_name("numbering")
                .long("numbering")
                .takes_value(true)
                .possible_values(&["mru", "frequency", "parent"])
                .help("Numbering strategy for the tree. Defaults to frequency."),
            Arg::with_name("dictionary")
                .long("dictionary")
                .takes_value(true)
                .possible_values(&["inline", "header"])
                .help("Where to place the dictionary."),
            Arg::with_name("statistics")
                .long("show-stats")
                .help("Show statistics."),
            Arg::with_name("show-ast")
                .long("show-ast")
                .help("Show pos-processed ast"),
            Arg::with_name("lazify")
                .long("lazify")
                .takes_value(true)
                .default_value("0")
                .validator(|s| s.parse::<u32>()
                    .map(|_| ())
                    .map_err(|e| format!("Invalid number {}", e)))
                .help("Number of layers of functions to lazify. 0 = no lazification, 1 = functions at toplevel, 2 = also functions in functions at toplevel, etc."),
        ])
        .group(ArgGroup::with_name("trp")
            .args(&["trp-rank"])
        )
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

    let dictionary_placement = match matches.value_of("dictionary") {
        None => None,
        Some("inline") => Some(DictionaryPlacement::Inline),
        Some("header") => Some(DictionaryPlacement::Header),
        Some(other) => panic!("Invalid dictionary '{}'", other)
    };
    let numbering_strategy = match matches.value_of("numbering") {
        None | Some("frequency") => NumberingStrategy::GlobalFrequency,
        Some("mru") => NumberingStrategy::MRU,
        Some("parent") => NumberingStrategy::Prediction,
        Some(other) => panic!("Unexpected argument {}", other)
    };
    let format = match matches.value_of("format") {
        None | Some("multipart") => {
            let stats = Rc::new(RefCell::new(binjs::io::multipart::Statistics::default()
                .with_source_bytes(0)));
            if let Some(ref spec) = matches.value_of("sections") {
                let compression = parse_compression(Some(spec))
                    .expect("Could not parse sections compression format");
                Format::Multipart {
                    options: WriteOptions {
                        strings_table: compression.clone(),
                        grammar_table: compression.clone(),
                        tree: compression,
                    },
                    stats
                }
            } else {
                let strings = parse_compression(matches.value_of("strings"))
                    .expect("Could not parse string compression format");
                let grammar = parse_compression(matches.value_of("grammar"))
                    .expect("Could not parse grammar compression format");
                let tree = parse_compression(matches.value_of("tree"))
                    .expect("Could not parse tree compression format");
                Format::Multipart {
                    options: WriteOptions {
                        strings_table: strings,
                        grammar_table: grammar,
                        tree
                    },
                    stats
                }
            }
        },
        Some("trp") => {
            let max_rank = match matches.value_of("trp-rank") {
                None | Some("none") => None,
                Some(ref num) => Some(usize::from_str_radix(num, 10).expect("Could not parse trp-rank"))
            };
            Format::TreeRePair {
                options: binjs::io::repair::Options {
                    max_rank,
                    numbering_strategy,
                    dictionary_placement: dictionary_placement.unwrap_or(DictionaryPlacement::Inline),
                }
            }
        }
        Some("xml") => Format::XML,
        Some("multistream") => Format::MultiStream {
            options: {
                binjs::io::multistream::Options {
                    sibling_labels_together: false,
                    dictionary_placement: dictionary_placement.unwrap_or(DictionaryPlacement::Inline),
                }
            }
        },
        Some("simple") => Format::Simple {
            stats: Rc::new(RefCell::new(binjs::io::simple::Statistics::default()))
        },
        _ => panic!()
    };
    let show_stats = matches.is_present("statistics");

    // Setup.
    let parser = Shift::new();

    let lazification = str::parse(matches.value_of("lazify").expect("Missing lazify"))
        .expect("Invalid number");

    let options = Options {
        parser: &parser,
        format,
        dest_dir,
        lazification,
        show_ast: matches.is_present("show-ast"),
    };

    for source_path in sources {
        handle_path(&options, source_path, PathBuf::new().as_path());
    }

    if show_stats {
        if let Format::Multipart { ref stats, .. } = options.format {
            println!("Statistics: {}", stats.borrow());
        }
    }
}
