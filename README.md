![Travis status](https://travis-ci.org/Yoric/binjs-ref.svg?branch=master)

# About the JavaScript Binary AST

As websites become more sophisticated, the amount of JavaScript source code keeps
increasing. By itself, this is not a problem. However, with the amount of code
loaded by large websites such as Facebook's chat, it is now common to witness
page loads during which both the loading and the parsing of JS code can take
several seconds each – this is assuming a fast connection, and taking into
account that the code is both compressed and optimized for loading and parsing
speed.

There is no reason to believe that the size of JS code will decrease or will
even stop increasing, nor that every large webdev team has the means to profile
loading and parsing speed of all their code.

This repo offers a (WIP) reference implementation for BinJs, a vendor-neutral
JavaScript format designed to optimize parsing speed and, when possible,
loading speed.

The JavaScript Binary AST format is not designed to be read or written by developers. Rather,
we expect that the toolchain, both server-side and browser-side, will generate
(respectively parse) .binjs files, letting .binjs files be used transparently
for communication between the browser and the server.

## Testing it

1. Install dependencies (you will need `npm`, `rustup`)
```
npm install
rustup install nightly
```
2. Pull the code.
```
git clone https://github.com/binast/binjs-ref
```
3. Compress/decompress.
```
cargo run --bin binjs_encode -- --help
cargo run --bin binjs_decode -- --help
```

## Compatibility with JavaScript source code

The JavaScript Binary AST format is designed to preserve the semantics for all syntactically
correct files.

Not preserved:
- actual semantics of syntax errors;
- source code positions;
- formatting (including whitespaces and semicolumns);
- comments (including source maps).

## Expected benefits

The BinJs format is designed so that the VM can start parsing the file
as soon as the first few bytes are received.

The BinJs format is designed to be generally faster to parse than JS source,
thanks to a syntax that requires no backtracking, strings that do not need
interning more than once, etc.

Furthermore, parsing a JS source is specified for a specific encoding, which
means that many encodings need to be transcoded before they can be parsed
(or, at best, while parsing), which slows down parsing. As BinJs is
a binary format, it does not need any form of transcoding.

Finally, most modern JavaScript VMs support a form of lazy parsing, which
performs faster parsing without most memory allocations. The BinJs format is
designed to make lazy parsing more efficient, by letting parsers jump over
subtrees (e.g. functions) in a single operation.


## Benchmarks

**WARNING** These benchmarks have been done with an early prototype of BinJs.
Actual implementations may end up with entirely different results.

These benchmarks do not attempt to measure the impact of early parsing.

### Firefox DevTools snapshot

Size (gzipped): -72%
Total parsing time:


### Facebook chat snapshot

(total size/time for 152 files)

Size (gzipped): 2541kb => 2403kb (-5%)
Total parsing time:  -72% without skipping, -94% skipping all functions

# Specifications

All (WIP) specifications may be found in the modules that implement them.
See [the documentation](https://binast.github.io/binjs-ref/binjs/index.html).
