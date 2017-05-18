# BinJS containers

This file specifies the binary format used to carry AST in BinJS. These
specifications make no assumption about the content of the AST and could,
in theory, be used for formats outside of EcmaScript.

For this document, we expect an AST data structure to be specified using the
notations of [ESTree](https://github.com/estree/estree).  See [AST.md](AST.md)
for more details about the AST data structure.

# Reading a BinJS

To simplify specifications, reading from/writing to BinJS format is specified
as a series of independent steps. Actual implementations may decide to adopt
implementation strategies that merge steps, parallelize them, delay them
until the result is needed, etc.

1. Read the format header.
2. Read the syntax specification.
3. Read the table of atoms.
4. Read the table of fragments.
5. Produce the AST

This file does not attempt to specify how the AST maps to the EcmaScript language.

## 0. Global structure

```
BinJSFile ::= FormatHeader TableOfAtoms TableOfNodeKinds LabelledTree
```

```rust
// Pseudo-code
fn parse_js_file(input: &mut InputStream) -> Result<AST> {
    parse_format_header()?;
    let syntax    = parse_syntax_specifications(input)?;
    let atoms     = parse_table_of_atoms(input)?;
    let fragments = parse_fragments(input, &syntax, &atoms)?;
    return Ok(fragments[fragments.len() - 1])
}
```

## 1. Read the format header.

**Rationale** The format header helps identify that the binary file is indeed
a BinJS and, should the BinJS format needs to evolve, which version of the format
is used.

```
FormatHeader ::= 'BINJS' FormatVersion
FormatVersion ::= VarNum
```

The format version, as all metadata numbers (e.g. node kinds, string lengths,
...) is represented as a variable-length unsigned 32-bit integers. This ensures that
short values (which experiments confirm are by a huge margin the most common)
take up less binary space than rarer longer values. This also ensures that
unused node kinds do not waste any space in a file.

Specifications for `VarNum` may be found [here](https://en.wikipedia.org/wiki/Variable-length_quantity).

For the current version of this specifications, the `FormatVersion` is always 0.

```rust
// Pseudo-code
fn parse_header(input: &mut InputStream) -> Result<u32> {
    const EXPECTED_FORMAT: String = "BINJS";
    let actual_format = input.read_string(EXPECTED_FORMAT.len())?;
    if actual_format != EXPECTED {
        return Err(InvalidFormatHeader)
    }

    const EXPECTED_VERSION : u32 = 0;
    let actual_version = input.read_varnum()?;
    if actual_version != EXPECTED_VERSION {
        return Err(InvalidVersion)
    }

    return Ok(actual_version)
}
```

## 2. Read the syntax

**Rationale** There is no doubt that the JavaScript AST will evolve with time.
New kinds of
nodes will be added (e.g. arrow functions, generators have been introduced).
Other kinds of nodes will need to convey more information (e.g. variable may now
be declared with `var`, `let` or `const`), ... Similarly, certain kinds of
nodes will be progressively disappear to be replaced with more modern versions
(e.g. E4X nodes or `for each` loops).

For instance, until ES5 included, functions were
specified entirely by the following data:
- an optional identifier;
- a list of parameters;
- a body.

Using the EStree notation:

```js
interface Function <: Node {
    id: Identifier | null;
    params: [ Pattern ];
    body: BlockStatement;
}
```

In successive versions of EcmaScript, this definition was extended as follows:

```js
extend interface Function {
    generator: boolean;
}
```

```js
extend interface Function {
    async: boolean;
}
```

While it is possible that large enough changes will require introducing an
entirely new encoder/decoder, the BinJS format is designed to avoid this as
often as possible. For this purpose, the format is kept extensible enough
that most changes in the JavaScript grammar and/or AST will not require
breaking changes to encoders/parsers and that support for new node kinds may be
added with fine granularity.

For this reason, each BinJS file contains as part of its header a summary
of the syntax it contains.

```
TableOfNodeKinds ::= NumberOfEntries ListOfLengths NodeKinds*
NumberOfEntries  ::= Length
ListOfLengths    ::= ByteLength*
Length           ::= VarNum
ByteLength       ::= VarNum
NodeKind         ::= NodeType FieldName*
NodeType         ::= Length String
FieldName        ::= Length String
String           ::= Byte*
```

For instance, a header may specify `Function [id, params, body]` (matching ES5),
or `Function [id, params, body, generator]` (to add field `generator`) or
`Function [id, params, body, async]` or `Function [id, params, body, async, generator]`.
The table of nodes does not attempt to specify the *contents* of each field.
Rather, the decoder is expected to know these contents.

A special node `BinJS.Fragment [id]` may be specified as part the table of
nodes. If specified, this distinguished node will be used while resolving
fragments (see below).

**Size note** At the time of this writing, The table of syntax for a file using
all the features of JavaScript is expected to contain about 100 entries for a
total of about 200 fields, which may all be represented within ~3kb. For a file
restricting itself to JSON, the table of syntax is expected to contain only
2 entries and fit within a few bytes.

// FIXME: We need to specify the root of the tree, right?

## 3. Read the table of atoms.

**Rationale** The table of atoms regroups atoms (e.g. literal strings, string
templates, identifiers, labels) in a single place, ensuring that the BinJS
file can reference each atom with a single `VarNum` and that the parser can
pre-parse atoms without having to re-hash them at a later stage.

```
TableOfAtoms ::= NumberOfEntries ListOfLengths String*
```



## 4. Read the table of fragments.

**Rationale** The AST may be represented as one or more fragments. Simple files
will typically be represented as a single fragment, while complex and/or
repetitive data can be optimized by sharing repeated fragments.

```
TableOfFragments ::= NumberOfEntries IndexOfRoot ListOfLengths ListOfFragments
ListOfFragments  ::= Fragment*
Fragment         ::= LabelledTree
LabelledTree     ::= NodeKindIndex UnlabelledTree
UnlabelledTree   ::= RawFloat64
                  |  RawByte
                  |  AtomIndex
                  |  FragmentIndex
                  |  Tuple
                  |  List
RawFloat64       ::= (one IEEE-754 double-precision floating-point)
RawByte          ::= (a single byte)
AtomIndex        ::= VarNum
FragmentIndex    ::= VarNum
Tuple            ::= IntermediateTree*
List             ::= ByteLength Length AnyTree*
AnyTree          ::= UnlabelledTree
                  |  LabelledTree
NodeKindIndex    ::= VarNum
```

**Caveat** This grammar is presented as a BNF but it doesn't contain
any terminal. In particular, this BNF is not sufficient to determine
whether a stream of bytes represents an `UnlabelledTree` or a `LabelledTree`,
or whether an `UnlabelledTree` is a `RawFloat64`, a `RawByte`, ...

This is by design. All this information is meant to be extracted based on a
specific AST data structure and by interpreting the data extracted from the
`TableOfNodeKinds`. See [AST.md](AST.md) for more details about the AST
data structure.

Productions `RawFloat`, `RawByte` and `Atom` are considered short
and are not prefixed by their length. Productions `Tuple` and `List` are
considered more complex and are prefixed by their byte length. The role
of the byte length is double: it helps detect incorrect files and it lets
parsers delay parsing subtrees or delegate it to background tasks.

Both `Tuple` and `List` represent several intermediate trees. The
difference is that `Tuple` is designed for constructions that have a fixed
number of children (e.g. `IfStatement` always has three children, even if
some may be undefined) while `List` is designed for constructions that have
a variable number of children (e.g. `SwitchStatement` may contain an arbitrary
number of clauses).

In `List`, `ByteLength` represents the total length of the
rest of the production (covering both `NumberOfEntries` and `IntermediateTree`).

The specifications of `RawFloat64` may be found [here](https://en.wikipedia.org/wiki/Double-precision_floating-point_format).

The value of `AtomIndex` is taken as an index in table `TableOfAtoms` read in step 2.

The value of `NodeKindIndex` is taken as an index in table `TableOfNodeKinds` read in step 3.


```rust
// Pseudo-code

struct Parser {
    input: InputStream,
    syntax: Syntax,
    atoms: Atoms,
}

impl Parser {
    /// Parse from the root.
    fn parse_ast(&mut self) -> Result<Fragment> {
      return self.parse_node(input, syntax, atoms, &syntax.root)
    }

    fn parse_node(&mut self, expect: &Type) -> Result<Fragment> {
        match expect.kind() {
            Interface(ref interface) => {
                let node_kind = self.parse_node_kind()?;
                return self.parse_node_with_type(interface, node_kind)
            }
            Boolean => {
                // FIXME: TODO
            }
            String => {
                // FIXME: TODO
            }
            Array(ref expect2) => {
                // FIXME: TODO
            }
            Structure(ref structure) => {
                return self.parse_tuple(structure)
            }
            Or(ref list) => {
                // FIXME: Finish (handle as if it was a super-interface without a `type`)
            }
            // ... FIXME: Finish
        }
    }

    fn parse_node_with_type(&mut self, interface: &Interface, kind: &String) -> Result<Fragment> {
        if let Some(ref type) = interface.type() {
            // Example: interface VariableDeclarator
            return self.parse_tuple(interface.structure())?;
        }
        // Example: interface Expression
        if let Some(ref refined_interface) = interface.find_subinterface(kind) {
            return self.parse_tuple(refined_interface.structure())?;
        }
        return Err(BadKind(kind))
    }

    fn parse_tuple(&mut self, structure: &Structure) -> Result<Fragment> {
        let mut result = Fragment::new();
        for (ref field_name, ref field_node) in structure {
            let field_content = self.parse_node(field_node)?;
            result.insert(field_name, field_content);
        }
        return Ok(result)
    }

    // ...
}
```

Note that this algorithm does not need backtracking.

Also note that this algorithm may be amended for concurrency or for lazy loading of functions or fragments.

// FIXME: Specify the step in which we replace `Atom` and `NodeKind` by the
// corresponding values taken from their table.


## 5. Extracting the AST




