//! A small tool to look at the streams extracted by a previous experiment and compare
//! compression with brotli.
//!
//! Example output (CSV)
//! ```csv
//! File,                             raw (b),       brotli-compressed (b)
//!
//! binjs,                            75044,         75049
//! js,                               168517,        51589
//! main.entropy,                     33924,         33928
//! floats.content,                   1451,          414
//! identifier_names.content,         44480,         14801
//! list_lengths.content,             13574,         3863
//! property_keys.content,            8474,          5604
//! string_literals.content,          5360,          3828
//! unsigned_longs.content,           3736,          720
//! floats.prelude,                   90,            94
//! identifier_names.prelude,         1017,          604
//! identifier_names_len.prelude,     460,           80
//! list_lengths.prelude,             39,            43
//! property_keys.prelude,            8711,          3457
//! property_keys_len.prelude,        885,           517
//! string_literals.prelude,          13261,         5935
//! string_literals_len.prelude,      1288,          803
//! unsigned_longs.prelude,           11,            15
//!```



extern crate clap;
extern crate glob;
extern crate itertools;

use clap::*;

use itertools::Itertools;

use std::collections::HashMap;
use std::process::Command;

/// The sizes observed for a stream.
#[derive(Default, PartialOrd, PartialEq, Eq, Ord)]
struct Sizes {
    /// Raw size.
    raw: u64,

    /// Brotli-compressed size.
    brotli: u64,
}

fn main() {
    let matches = App::new("Stream investigator")
        .author("David Teller, <dteller@mozilla.com>")
        .about("Examine the output of a `binjs_encode --split-streams advanced entropy`. Mostly useful to examine the result of compressing many files at once.")
        .args(&[
            Arg::with_name("path")
                .index(1)
                .help("Root of the directory where binjs_encode created its file.")
                .required(true)
        ])
        .get_matches();

    let root = matches.value_of("path").unwrap(); // Checked by clap.
    let glob = format!("{}/**/*", root); // Quick and dirty walk through subdirectories.

    let mut number_of_files = 0;
    let mut stats = HashMap::new();
    for path in glob::glob(&glob).unwrap_or_else(|e| panic!("Cannot walk directory {}: {:?}", root, e)) {
        let path = path.expect("Error while walking");
        let meta_data = std::fs::metadata(&path).unwrap();
        if meta_data.is_dir() {
            continue;
        }

        number_of_files += 1;

        let extension = path.extension().unwrap().to_str().unwrap().to_string();
        match extension.as_str() {
            "binjs" | "js" | "entropy" => {
                // binjs_decode doesn't output a brotli-compressed version of these
                // files, so we call `brotli` manually to obtain one and evaluate
                // the file size..
                let brotli_size = {
                    let out = Command::new("brotli")
                        .arg("--best")
                        .arg("--keep")
                        .arg("--stdout")
                        .arg(&path)
                        .output()
                        .expect("Error during brotli");
                    assert!(out.status.success());
                    assert!(out.stdout.len() != 0);
                    out.stdout.len()
                } as u64;

                let name = match extension.as_str() {
                    // For `binjs` and `js` files, we only care about the extension.
                    "binjs" | "js" => extension.clone(),
                    _ => path.file_name().unwrap().to_str().unwrap().to_string(),
                };

                let info = stats
                    .entry(extension.clone())
                    .or_insert_with(|| HashMap::new())
                    .entry(name)
                    .or_insert_with(|| Sizes::default());
                info.raw += meta_data.len();
                info.brotli += brotli_size as u64;
            }
            // Brotli files, output by binjs_encode.
            // Remove the `bro` from the name, store in `Sizes::brotli`.
            "bro" => {
                let path = path.as_path().with_extension("");
                let name = path.file_name().unwrap().to_str().unwrap().to_string();
                let extension = path.extension().unwrap().to_str().unwrap().to_string();
                let info = stats
                    .entry(extension.clone())
                    .or_insert_with(|| HashMap::new())
                    .entry(name)
                    .or_insert_with(|| Sizes::default());
                info.brotli += meta_data.len();
            }
            _ => {
                let name = path.file_name().unwrap().to_str().unwrap().to_string();
                let info = stats
                    .entry(extension.clone())
                    .or_insert_with(|| HashMap::new())
                    .entry(name)
                    .or_insert_with(|| Sizes::default());
                info.raw += meta_data.len();
            }
        };
    }

    eprintln!("Walked {} files", number_of_files);

    // Output results as CSV.
    // File name / Number of bytes (uncompressed) / Number of bytes (compressed with brotli)
    println!("{:40}, {:10}, {:20}", "File", "raw (b)", "brotli-compressed (b)");
    let print = |in_extension: &HashMap<String, Sizes>| {
        for (name, info) in in_extension.iter().sorted().into_iter() {
            println!("{:40}, {:10}, {:20}", name, info.raw, info.brotli);
        }
    };

    // Output `js` and `binjs` first.
    if let Some(in_extension) = stats.remove("js") {
        print(&in_extension);
    }
    if let Some(in_extension) = stats.remove("binjs") {
        print(&in_extension);
    }
    // Output the rest by alphabetical order.
    for extension in stats.keys().sorted().into_iter() {
        let in_extension = stats.get(extension).unwrap();
        print(in_extension);
    }
}
