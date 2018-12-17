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

const PATHS: [&'static str; 1] = ["benches/test.js"];
const NUMBER_OF_SAMPLES: usize = 3;

fn bench_parsing_one_parser_per_run(bencher: &mut bencher::Bencher) {
    bench_parsing_aux(None, bencher);
}

fn bench_parsing_reuse_parser(bencher: &mut bencher::Bencher) {
    bench_parsing_aux(Some(&mut Shift::new()), bencher);
}

fn bench_parsing_aux(parser: Option<&mut Shift>, bencher: &mut bencher::Bencher) {
    let entries = PATHS
        .iter()
        .map(|path_suffix| format!("{}/{}", env!("CARGO_MANIFEST_DIR"), path_suffix))
        .flat_map(|path| glob::glob(&path).expect("Invalid path"))
        .map(|entry| entry.expect("Invalid entry"))
        .sorted();
    let paths: Vec<_> = entries.into_iter().take(NUMBER_OF_SAMPLES).collect();
    for path in &paths {
        bencher.iter(|| {
            let json = match parser {
                None => Shift::new()
                    .parse_file(path)
                    .expect("Could not parse source"),
                Some(ref parser) => parser.parse_file(path).expect("Could not parse source"),
            };

            binjs::specialized::es6::ast::Script::import(&json).expect("Could not import AST")
        });
    }
}

benchmark_group!(
    bench,
    bench_parsing_one_parser_per_run,
    bench_parsing_reuse_parser
);
benchmark_main!(bench);
