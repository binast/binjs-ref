# Conventions

## Relationship with EcmaScript grammar

A BinJs file is meant to convey the exact same AST as the EcmaScript grammar,
with a different concrete syntax, optimized for being parsed by a machine,
rather than a human being.

A guideline is that a conversion from a syntactically valid js file to a BinJs
file and back to a syntactically valid js file should always be possible and
will only ever lose
- comments (including sourcemaps);
- formatting (including whitespace and semicolumns).

Everything else should be preserved.

## Variable-length numbers

(aka `Varnum`)

TBD


## Self-describing node kinds

There is no doubt that the JavaScript AST will evolve with time. New kinds of
nodes will be added (e.g. arrow functions, generators). Other kinds of nodes
that by definition could only have arity `n` may find themselves accepting
additional optional children. Nodes that could only have one kind of children
may find themselves accepting other children. While it is possible that large
enough changes will require introducing an entirely new parser,
the BinJs format is designed to simplify the life of developers of parsers
and transpilers, by making the format extensible enough that most changes in
the JavaScript grammar and/or AST will not require breaking changes in a parser
or transpiler.

The BinJs prepares itself for such changes by tagging all nodes of the AST
with a *node kind* and *expecting that the set of node kinds will change as
new versions of JavaScript appear*. Node kinds are simply strings, extracted
from the specifications of JavaScript and representing the role of a node in
an AST (e.g. `ArrayLiteral`, `AssignmentExpression`). The node kind lets the
BinJs parser lookup in its internal representation of the grammar to find out
the arity of the node, the kind of children it accepts, etc.

Should a new version of JavaScript introduce new AST nodes, the specifications
of the new nodes will be published for use by parser/transpiler
developers and will not affect existing nodes. Older BinJs
files will remain readable by updated parsers, while updated transpilers
transpiling older source code will still be able to produce BinJs files that
can be read by older parsers.

Should a new version of JavaScript change the details of an existing AST node,
e.g. by adding optional children, this new node with added optional children
MUST be considered a new node and specified as such. For instance, consider a
hypothetical scenario in which the `ArrayLiteral` gains an optional hint
informing the VM that the array will be used for left-append, right-append
or no-append. Rather than just patching the definition of `ArrayLiteral`,
the specifications of JavaScript will need to rename `ArrayLiteral`
`ArrayLiteralWithAppendHint`, with its own specifications. Updated parsers
will treat legacy `ArrayLiteral` nodes as a shorthand for
`ArrayLiteralWithAppendHint` with an empty hint.

For efficiency, node kinds are gathered in string table, defined in the header
of the file, and the rest of the file only references them through a `Varnum`
index in this table. In most cases, a node kind therefore occupies a single byte
in the file. Unused node kinds will not occupy any space in the file. A quick
lookup to the string table will be able to tell whether the file only uses
node kinds that the parser understands.

## Annotations on nodes

TBD

## String table

TBD

# Contents of a .BinJs file

## Format header

Rationale: Being sure that we're reading a .BinJs file.

Format: The characters `BINJS` followed by a version number as a VARNUM, currently 0.

## Table of atoms

Rationale: Putting all atoms in the same place makes it easier to start parsing
before having received the entire file. Also, makes the file smaller.

Format: Table of strings.

TBD

## Table of node kinds

Rationale: See the section on Self-describing node kinds.

Format: Table of strings.

TBD

## AST