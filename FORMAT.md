# About the BinJS format

A BinJs file is meant to convey the exact same AST as the EcmaScript grammar,
with a different concrete syntax, optimized for being parsed by a machine,
rather than a human being.

A guideline is that a conversion from a syntactically valid js file to a BinJs
file and back to a syntactically valid js file should always be possible and
will only ever lose
- comments (including sourcemaps);
- formatting (including whitespace and semicolumns).

Everything else should be preserved.

# Design choices

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

## `Varnum` (variable-length numbers)

All metadata numbers (e.g. node kinds, string lengths, ...) are represented
as variable-length 32-bit integers. This ensures that short values (which
experiments confirm are by a huge margin the most common) take up less binary
space than rarer longer values. This also ensures that unused node kinds
do not waste any space in a file.

TBD

## Atoms

TBD

# Reading a BinJS

To simplify specifications, reading from/writing to BinJS format is specified
as a series of independent steps. Actual implementations may decide to adopt
implementation strategies that merge steps, parallelize them, delay them
until the result is needed, etc.

1. Read the format header.
2. Read the table of atoms.
3. Read the table of node kinds.
4. Read the intermediate tree.
5. Decode the intermediate tree to an AST.

## 0. Global structure

```
BinJSFile ::= FormatHeader TableOfAtoms TableOfNodeKinds LabelledTree
```

## 1. Read the format header.

Rationale: The format header helps identify that the binary file is indeed
a BinJS and, if the BinJS format needs to evolve, which version of the format
is used.

```
FormatHeader ::= 'BINJS' FormatVersion
FormatVersion ::= VarNum
```

For the current version of this specifications, the `FormatVersion` is always 0.

## 2. Read the table of atoms.

Rationale: The table of atoms regroups atoms (e.g. literal strings, string
templates, identifiers, labels) in a single place, ensuring that the BinJS
file can reference each atom with a single `VarNum`
and that the parser can
pre-parse atoms without having to re-hash them at a later stage.

```
TableOfAtoms ::= NumberOfEntries ListOfLengths ListOfString
NumberOfEntries ::= VarNum
ListOfLengths ::= VarNum*
ListOfStrings ::= String*
String ::= Byte*
```

`NumberOfEntries` represent the total number of atoms in the tree.

Both `ListOfLengths` and `ListOfStrings` contain exactly `NumberOfEntries`.

For each `i` in `[0,NumberOfEntries[`, `ListOfStrings[i]` contains exactly
`ListOfLengths[i]` bytes.

### 2.1 FIXME
If we decide to head towards maximal sharing, `ListOfStrings` is replaced
taken as an index in a table
containing `IntermediateTree`.

## 3. Read the table of kinds.

Rationale: The table of node kinds regroups node kinds (e.g. `ArrayExpression`,
`Regexp`, ...) in a single place, ensuring that the BinJS
file can reference each kind with a single `VarNum` and that the parser can
pre-parse atoms without having to re-hash them at a later stage.

```
TableOfNodeKinds ::= NumberOfEntries ListOfLengths ListOfString
```

`NumberOfEntries` represent the total number of atoms in the tree.

Both `ListOfLengths` and `ListOfStrings` contain exactly `NumberOfEntries`.

For each `i` in `[0,NumberOfEntries[`, `ListOfStrings[i]` contains exactly
`ListOfLengths[i]` bytes.

For each `i` in `[0,NumberOfEntries[`, `ListOfStrings[i]` MUST be a node kind
understood by the parser.

## 4. Read the intermediate tree

Rationale: Specifying the encoding/decoding of a trivial tree that is neutral
with respect to the version of JavaScript is simpler than specifying the
encoding/decoding of a specific JavaScript grammar.

```
IntermediateTree ::= UnlabelledTree
                  |  LabelledTree
LabelledTree     ::= NodeKind UnlabelledTree
UnlabelledTree   ::= RawFloat64
                  |  RawByte
                  |  Atom
                  |  Tuple
                  |  List
RawFloat64       ::= (one IEEE-754 double-precision floating-point)
RawByte          ::= (a single byte)
Atom             ::= VarNum
Tuple            ::= IntermediateTree*
List             ::= ByteLength NumberOfEntries IntermediateTree*
ByteLength       ::= VarNum
NodeKind         ::= VarNum
```

**Caveat** This grammar is presented as a BNF but it doesn't contain
any terminal. Consequently, this BNF is not sufficient to determine
whether a stream of bytes represents an `UnlabelledTree` or a `LabelledTree`,
or whether an `UnlabelledTree` is a `RawFloat64`, a `RawByte`, an `Atom`
a `Tuple` or a `List`. This is by design. All this information is meant
to be extracted by interpreting `NodeKind` using the existing EcmaScript
grammar. See Step 5 for the details.

Productions `RawFloat`, `RawByte` and `Atom` are considered short
and are not prefixed by their length. Productions `Tuple` and `List` are
considered more complex and are prefixed by their byte length. The role
of the byte length is double: it helps detect incorrect files and it lets
parsers delay parsing subtrees or delegate it to background tasks.

Both `Tuple` and `List` represent several intermediate trees. The only
difference is that `Tuple` is designed for constructions that have a fixed
number of children (e.g. `IfStatement` always has three children, even if
some may be undefined) while `List` is designed for constructions that have
a variable number of children (e.g. `CaseClauses` may contain an arbitrary
number of clauses).

In `List`, `ByteLength` represents the total length of the
rest of the production (covering both `NumberOfEntries` and `IntermediateTree`).

The specifications of `RawFloat64` may be found here: https://en.wikipedia.org/wiki/Double-precision_floating-point_format

The value of `Atom` is taken as an index in table `ListOfStrings` read in step 2.

The value of `NodeKind` is taken as an index in table `ListOfStrings` read in step 3.

### 4.1 FIXME
If we decide to head towards maximal sharing, we need to make the following
changes:

- `Atom` is replaced with a production `String`;
- `UnlabelledTree` gains a production `SharedTree`;
- `SharedTree` is an index in a table of shared trees;
- we need to distinguish a special `NodeKind` to indicate to the parser that
  it should read an `UnlabelledTree` as a `SharedTree`.

## 5. Extracting to an ESTree grammar

For the time being, we use the [ESTree](https://github.com/estree/estree)
specification as a target for decoding JavaScript.

**This section is very much in progress.**

**FIXME** Specify amendments to ESTree:
- specify order of fields;
- introduce interfaces `String`, `Boolean`, ... and use them instead of sum types `string | boolean | ...`;
- introduce interfaces `Get`, `Set`, ... and use them instead of enums `"get" | "set" | ...`;
- introduce interface `Null` and use it instead of all the `| null`;
- extend functions with BinJS metadata;
- we don't care about locations;
- packing booleans?;
- implicit interface for `null`;
- ... ?

```
parse():
  return parse_node(Program) // `Program` is defined by ESTree

parse_node(esTreeType):
  if esTreeType is an interface:
    type <- read NodeKind
    return parse_node_with_type(type, esTreeType)
  else if esTreeType is boolean:
    // FIXME: Finish
  else if esTreeType is String:
    // FIXME: Finish
  else if esTreeType is [esTreeType2]:
    // FIXME: Finish
  else if esTreeType is {field1: type1, ..., fieldN: typeN}
    return parse_object(esTreeType)
  else if esTreeType is type1 | ... | typeN:
    // FIXME: Finish (handle as if it was a super-interface)
  // ...

parse_node_with_type(type, esTreeType):
    if esTreeType.type is defined: // Example: interface VariableDeclarator
      if type == Foo.type:
        return parse_object(esTreeType) 
      else:
        throw BadKind
    else: // Example: interface Expression
      if there is a subinterface esTreeType2 of esTreeType such that esTreeType2.type == type:
        return parse_object(esTreeType2)
      else:
        throw BadKind

parse_object(esTreeType):
  for (fieldName, fieldType) <- each MEANINGFUL field in the interface esTreeType: // FIXME: Specify order, "meaningful"
    fieldContent <- parse_node(fieldType)
  // FIXME: Finish


```

Note that this algorithm may does not need backtracking.

// FIXME: Specify the step in which we replace `Atom` and `NodeKind` by the
// corresponding values taken from their table.

