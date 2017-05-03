extern crate easter;
extern crate esprit;

use std::env;
use std::fs::File;
use std::io::prelude::*;

mod to_bin;

fn main() {
    for source_path in env::args().skip(1) {
        println!("Reading {}", source_path);
        let mut source_text = String::new();
        let mut file = File::open(source_path).expect("Could not open file.");
        file.read_to_string(&mut source_text).expect("Could not read file.");

        println!("Parsing...");
        let script = esprit::script(&source_text).expect("Could not parse file.");

        println!("Compiling...");
        to_bin::compile(&script);
    }
}