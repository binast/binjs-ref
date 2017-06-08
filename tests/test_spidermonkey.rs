extern crate binjs;
extern crate glob;
extern crate serde_json;

use serde_json::Value as JSON;

use std::fs::File;
use std::io::Read;
use std::path::*;

#[test]
fn test_spidermonkey() {
    println!("Setting up SpiderMonkey");
    let path_to_spidermonkey = std::env::var("BINJS_SPIDERMONKEY")
        .expect("To run this test, please set BINJS_SPIDERMONKEY to the path of the spidermonkey `js` binary.");
    let spidermonkey = binjs::source::SpiderMonkey::new(path_to_spidermonkey);

    println!("Setting up whitelist");
    // The following files have different behavior in Esprima and SpiderMonkey.
    let whitelist = [
        Path::new("ES6/binding-pattern/array-pattern/for-let-let.tree.json"),
        Path::new("ES6/binding-pattern/object-pattern/for-let-let.tree.json")
    ];

    println!("Locating source files");
    let mut path = PathBuf::new();
    for item in &[env!("CARGO_MANIFEST_DIR"), "tests", "data", "esprima", "test", "fixtures"] {
        path.push(item);
    }
    let path = path;
    let glob_format = format!("{}/**/*.tree.json", path.to_str().expect("Could not convert path to string"));

    let mut failures = vec![];
    'test_file: for metadata_path in glob::glob(&glob_format).expect("Could not glob path") {
        if let Ok(path) = metadata_path {
            println!("Analyzing source file metadata {:?}.", path);
            const SUFFIX : &'static str = ".tree.json";

            for white in &whitelist {
                if path.ends_with(white) {
                    println!("Skipping whitelisted file");
                    continue 'test_file;
                }
            }

            let mut metadata_file = File::open(path.clone())
                .expect("Could not open metadata file");
            let mut metadata_source = String::new();
            metadata_file
                .read_to_string(&mut metadata_source)
                .expect("Could not read metadata file");

            let metadata : JSON = serde_json::from_str(&metadata_source)
                .expect("Could not parse metadata");
            let metadata = metadata.as_object().unwrap();

            let path = path.to_str()
                .expect("Could not convert path to string");
            let (prefix, _) = path.split_at(path.len() - SUFFIX.len());

            let source_path = format!("{}.js", prefix);

            println!("Parsing source file {}", source_path);
            let parse_result = spidermonkey.parse_file(&source_path);

            if metadata.get("errors").is_some() {
                println!("Expecting parse failure.");
                // FIXME: TODO
            } else {
                println!("Expecting parse success.");
                if parse_result.is_err() {
                    println!("Parse failed.");
                    failures.push(path.to_owned())
                }
            }
        }

        println!("Failures: {:?}", failures);
    }
}