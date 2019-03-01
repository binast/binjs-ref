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

use std::collections::BTreeMap;
use std::path::Path;
use std::process::Command;

/// The sizes observed for a stream.
#[derive(Default, PartialOrd, PartialEq, Eq, Ord)]
struct Compressions<T> {
    /// Raw.
    raw: T,

    /// Brotli-compressed.
    brotli: T,

    /// Bzip2-compressed.
    bzip: T,

    /// Lzma-compressed.
    lzma: T,
}

struct Options {
    compare_to_bzip: bool,
    compare_to_lzma: bool,
}

/// Call brotli to compute the compressed size of a file.
///
/// Uses best compression options available.
fn get_brotli_size(path: &Path) -> u64 {
    let out = Command::new("brotli")
        .arg("--best")
        .arg("--keep")
        .arg("--stdout")
        .arg(&path)
        .output()
        .expect("Error during brotli");
    assert!(out.status.success());
    assert!(out.stdout.len() != 0);
    out.stdout.len() as u64
}

/// Call bzip2 to compute the compressed size of a file.
///
/// Uses best compression options available.
fn get_bzip2_size(path: &Path) -> u64 {
    let out = Command::new("bzip2")
        .arg("--compress")
        .arg("--best")
        .arg("--keep")
        .arg("--stdout")
        .arg(&path)
        .output()
        .expect("Error during bzip2");
    assert!(out.status.success());
    assert!(out.stdout.len() != 0);
    out.stdout.len() as u64
}

/// Call lzma to compute the compressed size of a file.
///
/// Uses best compression options available (although they increase decompression CPU usage).
fn get_lzma_size(path: &Path) -> u64 {
    let out = Command::new("lzma")
        .arg("--compress")
        .arg("-9") // Note: This increases decompression CPU usage.
        .arg("--keep")
        .arg("--extreme")
        .arg("--stdout")
        .arg("--threads=0")
        .arg(&path)
        .output()
        .expect("Error during lzma");
    assert!(out.status.success());
    assert!(out.stdout.len() != 0);
    out.stdout.len() as u64
}

/// Print the contents of a single column.
fn print_column<T: std::fmt::Display>(options: &Options, name: &str, data: &Compressions<T>) {
    println!(
        "{name:40}{raw:>10}{brotli}{bzip}{lzma}",
        name = format!("{},", name),
        raw = format!("{},", data.raw),
        brotli = format!("{:>12},", data.brotli),
        bzip = if options.compare_to_bzip {
            format!("{:>12},", data.bzip)
        } else {
            "".to_string()
        },
        lzma = if options.compare_to_lzma {
            format!("{:>12},", data.lzma)
        } else {
            "".to_string()
        },
    );
}

fn main() {
    let matches = App::new("Stream investigator")
        .author("David Teller, <dteller@mozilla.com>")
        .about("Examine the output of a `binjs_encode --split-streams advanced entropy`. Mostly useful to examine the result of compressing many files at once.")
        .args(&[
            Arg::with_name("path")
                .index(1)
                .help("Root of the directory where binjs_encode created its file.")
                .required(true),
            Arg::with_name("bzip")
                .long("--compare-to-bzip")
                .takes_value(false)
                .help("If specified, compress files with bzip2 for comparison."),
            Arg::with_name("lzma")
                .long("--compare-to-lzma")
                .takes_value(false)
                .help("If specified, compress files with lzma for comparison."),
            Arg::with_name("rank-by")
                .long("--rank-by")
                .possible_values(&["alpha", "brotli"])
                .default_value("alpha")
                .help("Specify sorting order for individual streams"),
        ])
        .get_matches();

    enum Ranking {
        /// Sort by alphanumerical order.
        Alpha,

        /// Sort by compressed size.
        BrotliCompressedSize,
    };
    let ranking = match matches.value_of("rank-by").unwrap() {
        "alpha" => Ranking::Alpha,
        "brotli" => Ranking::BrotliCompressedSize,
        _ => panic!(),
    };

    let root = matches.value_of("path").unwrap(); // Checked by clap.
    let glob = format!("{}/**/*", root); // Quick and dirty walk through subdirectories.
    let options = Options {
        compare_to_bzip: matches.is_present("bzip"),
        compare_to_lzma: matches.is_present("lzma"),
    };

    let mut number_of_files = 0;
    let mut stats: BTreeMap<String, BTreeMap<String, Compressions<u64>>> = BTreeMap::new();
    for path in
        glob::glob(&glob).unwrap_or_else(|e| panic!("Cannot walk directory {}: {:?}", root, e))
    {
        let path = path.expect("Error while walking");
        eprintln!("Visiting {:?}", path);
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
                // the file size.
                let brotli_size = get_brotli_size(&path);

                let name = match extension.as_str() {
                    // For `binjs` and `js` files, we only care about the extension.
                    "binjs" | "js" => extension.clone(),
                    _ => path.file_name().unwrap().to_str().unwrap().to_string(),
                };

                let info = stats
                    .entry(extension.clone())
                    .or_default()
                    .entry(name)
                    .or_default();
                info.raw += meta_data.len();
                info.brotli += brotli_size as u64;

                if options.compare_to_bzip {
                    info.bzip += get_bzip2_size(&path);
                };
                if options.compare_to_lzma {
                    info.lzma += get_lzma_size(&path);
                };
            }
            // Brotli files, output by binjs_encode.
            // Remove the `bro` from the name, store in `Compressions::brotli`.
            "bro" => {
                let path = path.as_path().with_extension("");
                let name = path.file_name().unwrap().to_str().unwrap().to_string();
                let extension = path.extension().unwrap().to_str().unwrap().to_string();
                let info = stats
                    .entry(extension.clone())
                    .or_default()
                    .entry(name)
                    .or_default();
                info.brotli += meta_data.len();
            }
            _ => {
                let name = path.file_name().unwrap().to_str().unwrap().to_string();
                let info = stats
                    .entry(extension.clone())
                    .or_default()
                    .entry(name)
                    .or_default();
                info.raw += meta_data.len();
                if options.compare_to_bzip {
                    info.bzip += get_bzip2_size(&path);
                };
                if options.compare_to_lzma {
                    info.lzma += get_lzma_size(&path);
                };
            }
        };
    }

    eprintln!("Walked {} files", number_of_files);

    // Output results as CSV.
    // File name / Number of bytes (uncompressed) / Number of bytes (compressed with brotli)
    print_column(
        &options,
        "File",
        &Compressions {
            raw: "raw (b)",
            brotli: "brotli (b)",
            bzip: "bzip2 (b)",
            lzma: "lzma (b)",
        },
    );

    // Output `js` and `binjs` first.
    if let Some(mut in_extension) = stats.remove("js") {
        print_column(&options, "js", &in_extension.remove("js").unwrap());
    }
    if let Some(mut in_extension) = stats.remove("binjs") {
        print_column(&options, "binjs", &in_extension.remove("binjs").unwrap());
    }
    println!("");

    // Output the rest by `ranking` order.
    let all_values = stats.into_iter().flat_map(|(_, value)| value.into_iter());
    let sorted = match ranking {
        Ranking::Alpha => all_values.sorted(),
        Ranking::BrotliCompressedSize => all_values.sorted_by_key(|x| x.1.brotli),
    };
    for (name, info) in sorted {
        print_column(&options, &name, &info)
    }
}
