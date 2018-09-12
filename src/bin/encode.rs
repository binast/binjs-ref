//! Encode a text source to a BinJS.

extern crate binjs;
extern crate clap;
extern crate env_logger;
extern crate log;

use binjs::io::{ CompressionTarget, Format };
use binjs::source::{ Shift, SourceParser };
use binjs::generic::FromJSON;
use binjs::specialized::es6::io::Encoder;
use binjs::specialized::es6::ast::Walker;

use std::fs::*;
use std::io::*;
use std::thread;
use std::path::{ Path, PathBuf };

use clap::*;

fn export_section(dest_bin_path: &Option<PathBuf>, target: &mut CompressionTarget, extension: &str) {
    let path = dest_bin_path
        .clone()
        .expect("Cannot write partial file without a destination")
        .with_extension(extension);
    let mut file = File::create(path.clone())
        .unwrap_or_else(|e| panic!("Could not create destination file {:?}: {:?}", path, e));
    let (data, _) = target.done()
        .expect("Could not finalize compression");
    file.write(data.as_ref())
        .expect("Could not write destination file");
    target.reset();
}

struct Options<'a> {
    parser: &'a Shift,
    format: Format,
    dest_dir: Option<PathBuf>,
    lazification: u32,
    show_ast: bool,
}

fn handle_path<'a>(options: &mut Options<'a>,
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
        let mut visitor = binjs::specialized::es6::lazy::LazifierVisitor::new(options.lazification);
        ast.walk(&mut path, &mut visitor)
            .expect("Could not introduce laziness");
    }

    if options.show_ast {
        use binjs::generic::ToJSON;
        let json = ast.export();
        println!("{:#}", json);
    }

    println!("Encoding.");
    let encoder = Encoder::new();
    let data = encoder.encode(&mut options.format, &ast)
        .expect("Could not encode");
    options.format.with_sections::<_, ()>(|contents, name| {
        export_section(&dest_bin_path, contents, name);
        Ok(())
    })
        .expect("Could not write sections");

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
                .possible_values(&["simple", "multipart", "trp", "xml", "arithmetic"])
                .help("Format to use for writing to OUTPUT. Defaults to `multipart`."),
            Arg::with_name("trp-rank")
                .long("trp-rank")
                .takes_value(true)
                .help("Maximal rank for trp. Ignored if the format isn't trp. Number of 'none'."),
            Arg::with_name("compression")
                .long("compression")
                .takes_value(true)
                .possible_values(&["identity", "gzip", "deflate", "br", "lzw"])
                .help("Compression format for all sections. Defaults to identity."),
            Arg::with_name("numbering")
                .long("numbering")
                .takes_value(true)
                .possible_values(&["mru", "frequency"])
                .help("Numbering strategy for the tree. Defaults to frequency."),
            Arg::with_name("statistics")
                .long("show-stats")
                .help("Show statistics."),
            Arg::with_name("show-ast")
                .long("show-ast")
                .help("Show pos-processed ast"),
            Arg::with_name("export-sections")
                .long("export-sections")
                .help("If specified, write sections to individual files, for easier analysis"),
            Arg::with_name("lazify")
                .long("lazify")
                .takes_value(true)
                .default_value("0")
                .validator(|s| s.parse::<u32>()
                    .map(|_| ())
                    .map_err(|e| format!("Invalid number {}", e)))
                .help("Number of layers of functions to lazify. 0 = no lazification, 1 = functions at toplevel, 2 = also functions in functions at toplevel, etc."),
            Arg::with_name("arithmetic-dictionaries")
                .long("arithmetic-dictionaries"),
            Arg::with_name("arithmetic-no-dictionaries")
                .long("arithmetic-no-dictionaries"),
            Arg::with_name("arithmetic-tags")
                .long("arithmetic-tags"),
            Arg::with_name("arithmetic-no-tags")
                .long("arithmetic-no-tags"),
            Arg::with_name("arithmetic-bools")
                .long("arithmetic-bools"),
            Arg::with_name("arithmetic-no-bools")
                .long("arithmetic-no-bools"),
            Arg::with_name("arithmetic-strings")
                .long("arithmetic-strings"),
            Arg::with_name("arithmetic-no-strings")
                .long("arithmetic-no-strings"),
            Arg::with_name("arithmetic-identifiers")
                .long("arithmetic-identifiers"),
            Arg::with_name("arithmetic-no-identifiers")
                .long("arithmetic-no-identifiers"),
            Arg::with_name("arithmetic-list-lengths")
                .long("arithmetic-list-lengths"),
            Arg::with_name("arithmetic-no-list-lengths")
                .long("arithmetic-no-list-lengths"),
            Arg::with_name("arithmetic-numbers")
                .long("arithmetic-numbers"),
            Arg::with_name("arithmetic-no-numbers")
                .long("arithmetic-no-numbers"),
        ])
        .group(ArgGroup::with_name("trp")
            .args(&["trp-rank"])
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

    let additional_flags = binjs::io::entropy::write::Options::KEYS
        .into_iter()
        .filter_map(|key| {
            let full_key = format!("arithmetic-{}", key);
            if matches.is_present(&full_key) {
                Some(*key)
            } else {
                None
            }
        });

    let format = Format::parse(matches.value_of("format"))
        .expect("Invalid `format`")
        .with_compression_str(matches.value_of("compression"))
        .expect("Invalid `compression`")
        .with_flags(additional_flags)
        .expect("Invalid flag");

    let show_stats = matches.is_present("statistics");

    // Setup.
    let parser = Shift::new();

    let lazification = str::parse(matches.value_of("lazify").expect("Missing lazify"))
        .expect("Invalid number");

    let mut options = Options {
        parser: &parser,
        format,
        dest_dir,
        lazification,
        show_ast: matches.is_present("show-ast"),
    };

    for source_path in sources {
        handle_path(&mut options, source_path, PathBuf::new().as_path());
    }

    if show_stats {
        if let Format::Multipart { ref stats, .. } = options.format {
            println!("Statistics: {}", stats.borrow());
        }
    }
}
