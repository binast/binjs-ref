extern crate binjs;
extern crate clap;
extern crate env_logger;

use binjs::ast::library;
use clap:: { App, Arg };

fn main() {
    env_logger::init();

    let matches = App::new("BinJS AST documentation")
        .author("David Teller <dteller@mozilla.com")
        .args(&[
            Arg::with_name("level")
                .long("level")
                .takes_value(true)
                .possible_values(&["es6"])
                .help("JavaScript level to use. If unspecified, default to es6."),
        ]).get_matches();

    let level = match matches.value_of("level") {
        None | Some("es6") => library::Level::ES6,
        Some(other) => panic!("Unknown level `{}`", other)
    };

    let syntax = library::syntax(level.clone());
    println!(" // # Documentation of the JavaScript AST, level {level}\n\n\n{pretty}",
        level=level,
        pretty=syntax.pretty("    "));
}