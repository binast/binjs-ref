#![feature(const_fn)]

extern crate easter;
extern crate esprit;

use std::fs::File;
use std::io::prelude::*;

// mod esprima;
mod atoms;
mod serialize;
mod varnum;

fn main() {
    let args : Vec<_> = std::env::args().collect();
    if args.len() < 3 {
        println!("Expected use: {} source dest", args[0]);
        std::process::exit(1);
    }

    let source_path = args[1].clone();
    let dest_path = args[2].clone();

    println!("Reading {}", source_path);
    let mut source_text = String::new();
    let mut file = File::open(source_path).expect("Could not open source file.");
    file.read_to_string(&mut source_text).expect("Could not read file.");

    println!("Parsing...");
    let script = esprit::script(&source_text).expect("Could not parse file.");

    let mut out = File::create(dest_path).expect("Could not open destination file.");
    println!("Compiling...");
    serialize::compile(&script, &mut out).unwrap();
}