extern crate bincode;
extern crate binjs;
extern crate clap;
extern crate env_logger;
extern crate log;

use binjs::cli::generate_dictionary;
use binjs::source::Shift;

use std::fs::{DirBuilder, File};
use std::path::Path;
use std::thread;

use clap::{App, Arg};


fn main() {
    env_logger::init();
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

    let matches = App::new("BinJS encoder")
        .author("David Teller, <dteller@mozilla.com>")
        .about("Generate a static prediction table from a bunch of JS source files.")
        .args(&[
            Arg::with_name("in")
                .long("in")
                .short("i")
                .multiple(true)
                .takes_value(true)
                .required(true)
                .help("Input files to use. Must be JS source file. May be specified multiple times."),
            Arg::with_name("out")
                .required(true)
                .long("out")
                .short("o")
                .takes_value(true)
                .help("Output directory to use for writing the dictionaries. May be overwritten."),
            Arg::with_name("lazify")
                .long("lazify")
                .takes_value(true)
                .default_value("0")
                .validator(|s| s.parse::<u32>()
                    .map(|_| ())
                    .map_err(|e| format!("Invalid number {}", e)))
                .help("Number of layers of functions to lazify. 0 = no lazification, 1 = functions at toplevel, 2 = also functions in functions at toplevel, etc."),
            Arg::with_name("quiet")
                .long("quiet")
                .short("q")
                .help("Do not print progress"),
            Arg::with_name("depth")
                .long("depth")
                .takes_value(true)
                .default_value("3")
                .validator(|s| s.parse::<u32>()
                    .map(|_| ())
                    .map_err(|e| format!("Invalid number {}", e)))
                .help("Maximal path length to store in the dictionary."),
            Arg::with_name("window-width")
                .long("window-width")
                .takes_value(true)
                .default_value("32")
                .validator(|s| s.parse::<u32>()
                    .map(|_| ())
                    .map_err(|e| format!("Invalid number {}", e)))
                .help("String window width."),
            Arg::with_name("threshold")
                .long("threshold")
                .takes_value(true)
                .default_value("2")
                .validator(|s| s.parse::<u32>()
                    .map(|_| ())
                    .map_err(|e| format!("Invalid number {}", e)))
                .help("Prune from the dictionary all user-extensible values that appear in at most [threshold] files"),
        ])
        .get_matches();

    let sources: Vec<_> = matches
        .values_of("in")
        .map_or_else(|| Vec::new(), |input| input.map(Path::new).map(Path::to_path_buf).collect());

    let dest = Path::new(matches.value_of("out").unwrap());

    let quiet = matches.is_present("quiet");

    let lazification =
        str::parse(matches.value_of("lazify").expect("Missing lazify")).expect("Invalid number");

    let depth = str::parse(matches.value_of("depth").unwrap()).expect("Invalid number");

    let width = str::parse(matches.value_of("window-width").unwrap()).expect("Invalid number");

    let threshold: usize =
        str::parse(matches.value_of("threshold").unwrap()).expect("Invalid number");

    let parser = Shift::try_new().expect("Could not launch Shift");
    let options = generate_dictionary::Options {
        depth,
        width,
        lazification,
        threshold: threshold.into(),
        parser: &parser,
        quiet,
    };

    // Extract dictionary.
    let dictionary = generate_dictionary::generate_dictionary(&sources, options)
        .expect("Could not extract dictionary");

    // Write dictionary to disk.
    DirBuilder::new()
        .recursive(true)
        .create(dest)
        .expect("Could not create directory");

    let dest_dictionary = dest.join("dict.entropy");

    let file_dictionary =
        File::create(dest_dictionary).unwrap_or_else(|e| panic!("Could not create file: {:?}", e));
    bincode::serialize_into(file_dictionary, &dictionary)
        .expect("Could not serialize entropy dictionary");
}