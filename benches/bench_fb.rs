//! Encode a BinJS, then decode it, ensure that we obtain the same AST.

#[macro_use]
extern crate bencher;
extern crate binjs;
extern crate env_logger;
extern crate glob;
extern crate itertools;

use binjs::generic::*;
use binjs::source::*;

use itertools::Itertools;

use std::thread;

const PATHS: [&'static str; 1] = ["tests/data/facebook/single/**/*.js"];
const NUMBER_OF_SAMPLES: usize = 3;

fn bench_parsing_one_parser_per_run(bencher: &mut bencher::Bencher) {
    let bencher = bencher.clone();
    thread::Builder::new()
        .name("Large stack dedicated thread".to_string())
        .stack_size(20 * 1024 * 1024)
        .spawn(move || {
            println!("Benchmark: one parser per run");
            bench_parsing_aux(None, bencher);
        })
        .expect("Could not launch dedicated thread")
        .join()
        .expect("Error in dedicated thread");
}

fn bench_parsing_reuse_parser(bencher: &mut bencher::Bencher) {
    let bencher = bencher.clone();
    thread::Builder::new()
        .name("Large stack dedicated thread".to_string())
        .stack_size(20 * 1024 * 1024)
        .spawn(move || {
            println!("Benchmark: reuse parser");
            let mut parser = Shift::new();
            bench_parsing_aux(Some(&mut parser), bencher);
        })
        .expect("Could not launch dedicated thread")
        .join()
        .expect("Error in dedicated thread");
}

fn bench_parsing_aux(parser: Option<&mut Shift>, mut bencher: bencher::Bencher) {
    let entries = PATHS
        .iter()
        .map(|path_suffix| format!("{}/{}", env!("CARGO_MANIFEST_DIR"), path_suffix))
        .flat_map(|path| glob::glob(&path).expect("Invalid path"))
        .map(|entry| entry.expect("Invalid entry"))
        .sorted();
    let paths: Vec<_> = entries.into_iter().take(NUMBER_OF_SAMPLES).collect();
    for path in &paths {
        print!("\n{:?}.", path);
        bencher.iter(|| {
            print!(".");
            let json = match parser {
                None => Shift::new()
                    .parse_file(path.clone())
                    .expect("Could not parse source"),
                Some(ref parser) => parser
                    .parse_file(path.clone())
                    .expect("Could not parse source"),
            };

            let _ =
                binjs::specialized::es6::ast::Script::import(&json).expect("Could not import AST");
        });
    }
}

benchmark_group!(
    bench,
    bench_parsing_one_parser_per_run,
    bench_parsing_reuse_parser
);
benchmark_main!(bench);
