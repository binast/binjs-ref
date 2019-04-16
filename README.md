# Facebook Semi-structured Data Compression (FBSSDC)

This is a prototype compressor for typed, semi-structured data with an
example application: compressing JavaScript abstract syntax trees with
additional data about captured variables, scopes with eval, etc. In
addition the format supports random access to "lazy" fields.

The type of the data being compressed is specified in IDL derived from
https://github.com/binast/binjs-ref/blob/master/spec/es6.webidl

## Getting Started

Prerequisites: Set up [Brotli](https://github.com/google/brotli) and
[binjs-ref](https://github.com/binast/binjs-ref). binjs-ref's
binjs_encode is used to invoke the Shift JavaScript parser, annotate
the abstract syntax tree, and dump it as JSON.

To encode a file:

````
# Parse JavaScript and produce AST JSON
$ binjs_encode --show-ast -i file.js -o /dev/null > file.ast

# Rewrite integer literals which should be floating point numbers
$ ./bpy.py fix-types file.ast file-typed.ast

# Build a dictionary of string contents.
# Dictionaries do not need to exhaustively cover a file; in fact
# you can use a file containing astdict\0 for an empty dictionary.
$ ./bpy.py make-dict file-typed.ast dict.bin

# Encode and compress
$ ./bpy.py encode-ast dict.bin file-typed.ast file.bin
$ brotli file.bin
$ ls -ald file.bin.br
````

The Brotli step is necessary to compress metadata in the file which is
not natively compressed yet.

Decode a file:

```
$ bpy.py decode-ast dict.bin file.bin file-out.ast
$ diff file-typed.ast file-out.ast
```

If diff produces no output, the files are identical. The encoder and
decoder worked.

The files are self-contained, you should be able to take a file
encoded on one system and decode it on another system.

bpy has various other functions, use `bpy.py --help`.

## Tests

To run the tests:

````
$ ./test.py  # unit tests
$ ./test.sh  # integration test
lazified 939 functions
test passed
````

Files are tested with doctest and can also be run directly on the
command line.

## Design

### Type-directed encoding

Where the IDL enforces a specific structure, the encoder and decoder
use that metadata to make concurrent steps without any encoding cost.
For example an interface has a fixed set of fields; if a field is a
fixed type, the type tag for the member doesn't need to be encoded;
etc.

Probability models can be grouped by type and only things permitted by
that type are modelled. For example, ArrayBinding's elements member is
FrozenArray<(Binding or BindingWithInitializer)?>, so that array's
elements' type tags only need to encompass the ones for Binding
(ObjectBinding, ArrayBinding or BindingIdentifier),
BindingWithInitializer, or None; the probability model for
UnaryOperator can use symbols 0 .. 6 and they will be interpreted as
enum members.

Non-zero entries in the probability models indicate other probability
models are necessary. For example if the probability model for Script
or Module has no weight for Module, then it follows that models which
mention imports or exports are not possible.

The probability model is total and static; there's no escapes like
PPM*. As a consequence ASTs is (should be) small and quick to decode.

Some attempt is made to encode probability tables compactly. They may
be encoded as a single symbol, for a probability table with one entry;
a list of canonical Huffman code lengths (without symbols) in symbol
order for dense tables; or symbol, length pairs. For some kinds of
symbols such as float constants enumerating the symbols is necessary.

### Probability Table Construction

First, the input is sharded by type, symbol occurences examined and
canonical Huffman tables are constructed. Then there's a second pass
to encode using the tables of the relevant type.

The streams are not separated in the file but written in order,
interleaved.

The tables are written in the preamble of the file. Which tables are
serialized is guided by a type-directed exploration of the grammar.

### Lazy attributes

`[Lazy]` members are encoded recursively with a table that provides
random access to them, provided the lazy member has been decoded.
References to lazy members are encountered in order, so they are
encoded with zero size.

### Lists

For a FrozenArray field, the field context is used to encode the list
length. Length being 0 is used for type pruning.

The values follow, in the usual way--type tags only for polymorphic
static types. The context is the list type itself.

### Strings

Strings are the most important values. JavaScript programs use a lot
of them. The values are irregular and can be large.

The format has two string tables: the first is encoded in the file
itself; the second is in a separate file. The format relies on an
external compressor for these tables.

Strings are encoded as indices into the concatenated table.

### Other values

Doubles are encoded in eight bytes; integers in four bytes.

## Known Issues

In general, grep for TODO/FIXME to see places to improve.

The format doesn't have a lot of fit-and-finish features, like a
signature, compressed probability tables (in particular this should
probably just use FSE like zstd), or a sanity check that the type of
the start symbol the decoder requested matches.

There are some assumptions that es6.idl uses a limited set of
features. For example primitive types, other than string, don't appear
as optional; tagged types are only alternated with other tagged types
(for example `Script or Module` is OK; `Script or string` will not
work. These are not systemic issues, just implementation details which
need to be fleshed out if they are to be used.

The format could be improved for more robust decoding by limiting the
length of Huffman table entries. Different limits would be appropriate
depending on the type of the field, for example IIRC zstd uses a 10?
bit limit but that is probably too small for the number of JavaScript
string literals in a large program.

## Future Directions in Compression

Here are some ideas to make JavaScript ASTs smaller. These are in
order of ease and likely impact.

Use ANS instead of Huffman for fields where that is beneficial. These
are fields with probabilities that diverge from sums of 1/2^n terms.
There are typically some boolean-valued fields which occur frequently
enough to make this have significant impact.

Experiment with context in more detail. For example list lengths are
encoded in the context of their referring field, but for list values
the context is simply the list type. Investigate transmitting context,
like a field, to the values too (at the cost of more tables.)

Use MRUs, deltas into the string stream. Separate strings which only
appear once in the file (for some files this is 40% of strings;
usually it is around 5% of strings.) Shard the string stream by type
(property, literal, etc.)

Design the schema for compression. For example, identifiers which
refer to bound names can be encoded as a scope index, slot index pair.
Scope and slot indices can refer to Asserted*Scope members of
enclosing scopes which have captures/are captured.

Share similar models. There is one hard-coded example of model sharing
in model.py.

Implement multiple models and switch between them like Brotli.

Implement a copy mechanism by memoizing subtrees, using TreeRePair, or
encoding repeats.

Tighten probabilities as fields are exhausted.

## Contributing

See the [CONTRIBUTING](CONTRIBUTING.md) file for how to help out.

## License

FBSSDC is MIT licensed, as found in the LICENSE file.
