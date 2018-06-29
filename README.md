![Travis status](https://travis-ci.org/binjs/binjs-ref.svg?branch=master)

# About the JavaScript Binary AST


As websites become more sophisticated, the amount of JavaScript source code keeps
increasing. While depending upon a large JavaScript codebase won't prevent a website
from working, it will cause websites to
start slowly – often [unacceptably slow](https://medium.com/reloading/javascript-start-up-performance-69200f43b201).
This is due to two bottlenecks: parsing and bytecode compiling JavaScript.
Unfortunately, browsers have pretty much
reached efficiency peak for both operations.

We (Mozilla, Bloomberg, Facebook, CloudFlare) are currently working on a
domain-specific encoding for JavaScript, called "BinAST" (short for
"JavaScript Binary AST"). The JavaScript Binary AST is designed to
break the bottleneck. Current
advanced prototypes already show JS [parsing improvements of 30%-50%](https://gist.github.com/Yoric/1d41cdf3715815d39032f0dbce31ed42) on
all the most common frameworks, just by changing the format,
and we believe that we can increase
this improvement much further. The encoding can be built as part of a
webdev toolchain, or injected by a
proxy or CDN, hence automatically improving the performance of end users
without change to the original website.

This encoding is currently in the JavaScript TC39 standardization process [3].
It can be used alongside existing compression techniques (gzip, brotli, etc.)

## Testing it

1. Install dependencies (you will need `npm`, `rustup`)
```
npm install
rustup install nightly
rustup default nightly
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
**Note** The JS parser may choke on very large JS source files. If so, you'll need to set the environment variable `NODE_MAX_OLD_SPACE_SIZE=xxxx`. This will instruct the Node-based parser to allocate more memory. The default value is 2048 (Mb). This is equivalent to passing `--max_old_space_size` to the Node process.

4. Dump tree structure.
```
cargo run --bin binjs_dump -- --help
```
**Note** `binjs_dump` supports only `multipart` format.

## Compatibility with JavaScript source code

Preserved:
- semantics of well-formed programs;
- variable and function names.

Not preserved:
- actual semantics of syntax errors;
- source code positions;
- formatting (including whitespaces and semicolumns);
- comments (including source maps).

## Expected benefits


The Binary AST format is designed to be generally faster to parse than JS source,
thanks to a syntax that requires no backtracking, strings that do not need
interning more than once, etc.

The Binary AST format is designed so that the VM can start parsing the file
as soon as the first few bytes are received (streaming parsing) and can
start compiling the file to bytecode soon after that (streaming bytecode compilation).

Furthermore, parsing a JS source is specified for a specific encoding, which
means that many encodings need to be transcoded before they can be parsed
(or, at best, while parsing), which slows down parsing. As BinAST is
a binary format, it does not need any form of transcoding.

Finally, most modern JavaScript VMs support a form of lazy parsing, which
performs faster parsing without most memory allocations. The BinAST format is
designed to make lazy parsing more efficient, if required,
by letting parsers jump over (functions) in a single operation.

# Specifications

- The semantics are specified [here](https://binast.github.io/ecmascript-binary-ast/).
- The binary format is specified [here](https://binast.github.io/binjs-ref/binjs/io/multipart.html).
