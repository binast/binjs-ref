extern crate binjs;

fn main() {
    // Usage: `spidermonkey path/to/spidermonkey path/to/source.js`
    let mut args = std::env::args().skip(1);

    let path_to_spidermonkey = args.next()
        .expect("Expected path to spidermonkey.");

    let path_to_source = args.next()
        .expect("Expected path to .js text source.");

    let spidermonkey = binjs::source::SpiderMonkey::new(path_to_spidermonkey);
    let json = spidermonkey.parse_file(path_to_source)
        .expect("Could not parse file.");

    println!("Parse result {}", json)
}