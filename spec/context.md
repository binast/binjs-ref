# About this file

This is an attempt to document the context-0.2 compression format.

WARNING: the content is almost just a copy of entropy.md and doesn't reflect
the actual format.  They're about to be changed so much once we fixed the
format.  The current content is here to clarify the difference between entropy
in the future changes.

# ASTs

This document specifies how to compress an Abstract Syntax Tree (or AST) in a given **Grammar**.

## Grammar

TBD

## Nodes

An AST is a **Node**, as defined below in pseudo-code:

```typescript
type Node = LiteralString  // A literal string in the language.
          | F64            // 64-bit floating-point number
          | U32            // 32-bit unsigned integer
          | Bool
          | PropertyKey    // A property key (aka "field name") in the language.
          | IdentifierName // An identifier in the language.
          | InterfaceNode
          | Array
          | Null           // An empty node.
          
class Null { }

class LiteralString {
  value: String
}

class PropertyKey {
  value: String
}

class IdentifierName {
  value: String
}

class InterfaceNode {
  type: InterfaceName;
  fields: [Field]; // Order of fields is specified in the grammar
}

class Field {
  name: FieldName;
  value: Node;
}

class InterfaceName {
  value: String; // The list of valid values is specified in the grammar
}

class FieldName {
  value: String;
}

class Array {
  values: [Node];
}

class Bool {
  value: bool;
}
```

## Paths

Whenever we walk the AST, each node is assigned a Path, as follows:

```typescript
class Path {
  items: [PathItem]
  function append(interface: InterfaceName, field: FieldName) -> Path {
    let copy = this.items.slice();
    copy.push(new PathItem(interface, field));
    return new Path(item);
  }
}
class PathItem {
  interface: InterfaceName,
  field: FieldName,  
}

function walk_root(node: Node, walker: Walker) {
  return walk_node(node, new Path());
}
function walk_node(node: Node, path: Path) {
   if (node instanceof LiteralString) {
     walker.visit_literal_string(path, node);
   } else if (node instanceof F64) {
     walker.visit_f64(path, node);
   } else if (node instanceof U32) {
     walker.visit_u32(path, node);
   } else if (node instanceof PropertyKey) {
     walker.visit_property_key(path, node);
   } else if (node instanceof IdentifierName) {
     walker.visit_identifier_name(path, node);
   } else if (node instanceof InterfaceNode) {
     walker.visit_interface_node(path, node);
     for (let field of node.fields) {
       let sub_path = path.append(node.type, field.name.value);
       walk_node(field.value, sub_path);
     }
   } else if (node instanceof Null) {
     walker.visit_null(path, node);
   } else if (node instanceof Bool) {
     walker.visit_bool(path, node);
   } else if (node instanceof Array) {
     // Arrays do not influence paths.
     for (let sub_node of node.values) {
       walk_node(sub_node, path);
     }
   }
}
```

Note that, within a grammar, there is a simple bijection between the set of valid `(InterfaceName, FieldName)` and the set of `(InterfaceName, position)` where `position` is the index of the field named `FieldName` in the interface named `InterfaceName`. Similarly, if we assume that instances of `InterfaceName` are sorted by some well-known order, they can be represented as integers. Consequently, a `path` may easily be represented as an even-sized vector of integers.

# Global structure

```
Stream ::= GlobalHeaders Chunk+
```

# Global Headers

Headers identify the file, the version of the format used and the shared dictionary used, if any.

The shared dictionary provides:

- a family of context-based prediction tables;
- well-known literal strings;
- well-known floating-point numbers;
- well-known unsigned long numbers;
- well-known property keys;
- well-known identifier names;
- well-known list lengths.

```
GlobalHeaders ::= MagicNumber FormatVersionNumber LinkToSharedDictionary
```

## Magic number

```
MagicNumber ::= "\x89BJS\r\n\0\n"
```


## Version number

Not specified/implemented yet.

```
VersionNumber ::= TBD
```

## Link to shared dictionary

Not specified/implemented yet.

The shared dictionary is optional.

```
LinkToSharedDictionary ::= TBD
```

# Chunk

A file may contain one or more **Chunks**. Typically, the first **Chunk** will contain the code
known to be executed during the startup, additional chunks may contain the definition of lazy
functions. Additionally, if several source files are concatenated into one, there may be several
**Chunks**, each with its own eager code.

```
Chunk  ::= ChunkHeader Prelude? Content? Main Footers?
ChunkHeader ::= "CHNK" ByteLen ChunkIdentifier
ChunkIdentifier ::= "EAGR" ChunkNum
                 |  "LAZY" ChunkNum LazyNum
ChunkNum ::= var_u32
LazyNum ::= var_u32
```

A `ChunkIdentifier` may be:

- `EAGR`, in which case the chunk contains toplevel code, meant to be parsed and executed immediately;
- `LAZY`, in which case the chunk contains lazy function definitions, meant to be parsed/executed by-need.

It is a syntax error for an `EAGR` chunk to appear after a `LAZY` chunk.

A `ChunkNum` is an arbitrary number, used to identify that two chunks belong to the same source file. Two
`ChunkIdentifier`s with the same `ChunkNum` belong to the same source file.

A `LazyNum` is an arbitrary number, valid within a file, and used to identify the lazy function definitions.
It is a syntax error for the same `"LAZY" ChunkNum LazyNum` value to appear more than once.

# Prelude

The **Prelude** extends the shared dictionary with additional strings, numbers, keys, names, ...
Each **Chunk** may contain its own **Prelude**.

Data structures are separated even when they have the same underlying representation, to improve
compression.

```
Prelude ::= "PREL" NamedPreludeDictionary*
NamedPreludeDictionary ::= "FLOA" PreludeStream<f64?>
                         | "ULON" PreludeStream<u32?>
                         | "LIST" PreludeStream<u32?>
                         | "PROC" PreludeStream<String>
                         | "IDEC" PreludeStream<String>
                         | "STRC" PreludeStream<String>
                         | "PROL" PreludeStream<StringLen>
                         | "IDEL" PreludeStream<StringLen>
                         | "STRL" PreludeStream<StringLen>
```

| name | content |
|---|---|
| `FLOA` | the list of float values |
| `ULON` | the list of unsigned long values |
| `LIST` | the list of the length of list values |
| `PROC` | the list of property key strings |
| `IDEC` | the list of identifier name strings |
| `STRC` | the list of string values |
| `PROL` | the list of the length of property key string |
| `IDEL` | the list of the length of identifier name strings |
| `STRL` | the list of the list of string values |

If the same prelude name (`"FLOA"`, `"ULON"`, etc.) appears more than once, this
is a syntax error.

If `"PROC"` exists and is not empty, `"PROL"` must exist and be non-empty,
otherwise, this is a syntax error.

If `"IDEC"` exists and is not empty, `"IDEL"` must exist and be non-empty,
otherwise, this is a syntax error.

If `"STRC"` exists and is not empty, `"STRL"` must exist and be non-empty,
otherwise, this is a syntax error.

## Internal compression

All prelude streams have the same structure

```
PreludeStream<T> ::= CompressionFormat ByteLen CompressedByteStream<T>
CompressionFormat ::= "BROT" // The only compression format supported so far.
ByteLen := var_u32
```

Where:

- the `CompressionFormat` is the name of the compression to use to decompress the `CompressedByteStream`;
- the `ByteLen` is the number of bytes in the `CompressedByteStream`;

Let us call `ExpandedByteStream<T>` the result of decompressing the `ByteLen` bytes of
a `CompressedByteStream<T>` with the specified `CompressionFormat`.

## Stream interpretation

### Integers

```
ExpandedByteStream<u32?> ::= ExpandedByteStreamItem<u32?>*
ExpandedByteStreamItem<u32?> ::= var_u32         // Some(value)
                               | var_u32_invalid_1  // None
```

- `var_u32_invalid_1` is interpreted as the `null` value for unsigned numbers;
- All valid `var_u32` values are interpreted as numbers.


### Floating-point

```
ExpandedByteStream<f64?> ::= ExpandedByteStreamItem<f64?>
ExpandedByteStreamItem<f64?> ::= var_i32            // Some(integer value)
                               | var_i32_invalid_1 f64 // Some(floating-point value)
                               | var_i32_invalid_2     // None
```

- All valid `var_i32` values are interpreted as `i32` numbers, which are then converted to `f64`. This has the advantage that
    most numbers actually used in practice fit within `i32`.
- `var_i32_invalid_1` is used as a prefix for floating-point values that do not fit in `i32`;
- `var_i32_invalid_2` is used to represent the `null` value for floating-point numbers.

### Strings

To interpret a string stream, it is necessary to interpret both the `PreludeStream<String>` and the `PreludeStream<StringLen>`.

If the `PreludeStream<String>` and the `PreludeStream<StringLen>` do not have the same number of elements, this is a syntax error.

```
ExpandedByteStream<StringLen> ::= varu32?
ExpandedByteStream<String> ::= [char; len_0], [char; len_1], ... [char; len_k]
                             // where len_0, len_1, ..., len_k are the values of the ExpandedByteStream<StringLen>
```

If `len_0 + len_1 + ... + len_k` is not equal to the `ByteLen` attached to the `ExpandedByteStream<String>`, this is a syntax error.

Strings are interpreted as WTF-8 (NOT UTF-8).

# Content

The **content** defines streams of references to the dictionaries (both the prelude dictionary and the shared dictionary,
if specified). These values are pulled from the content streams while reading the **main stream**.

```
Content ::= "CONT" NamedContentStream*
NamedContentStream ::= "FLOA" ContentStream<f64?>
                     | "ULON" ContentStream<u32?>
                     | "LIST" ContentStream<u32?>
                     | "PROC" ContentStream<String>
                     | "IDEC" ContentStream<String>
                     | "STRC" ContentStream<String>
```

If the same content name (`"FLOA"`, `"ULON"`, etc.) appears more than once, this
is a syntax error.

## Internal compression

All content streams have the same structure

```
ContentStream<T> ::= CompressionFormat ByteLen CompressedContentStream<T>
```

Where:

- the `CompressionFormat` is the name of the compression to use to decompress the `CompressedByteStream`;
- the `ByteLen` is the number of bytes in the `CompressedByteStream`;

Let us call `ExpandedContentStream<T>` the result of decompressing the `ByteLen` bytes of
a `CompressedContentStream<T>` with the specified `CompressionFormat`.

## Stream interpretation

All expanded content streams have the same structure

```
ExpandedContentStream<T> ::= WindowLen ExpandedContentReferences<T>
WindowLen ::= var_u32
```

Where `WindowLen` is the number of bytes in the LRU cache used to interpret `ExpandedContentReferences`. It may be 0.

```
ExpandedContentReferences<T> ::= ContentReferenceBytes<T>*
ContentReferenceBytes<T> ::= var_u32
```

Interpreting a `ContentReferenceBytes<T>` requires the following information:
- a per-stream value `prelude_latest`, initially set to `-1`;
- a per-stream value `imported_len`, initially set to `0`.

A `ContentReferenceBytes<T>` is interpreted into a `ContentReference<T>` through
the following algorithm:

```python
prelude_latest = -1
imported = []
window = LRU(WindowLen)
prelude_len = # Length of the matching Prelude stream
def interpret(u32):
    # 0 means "next in prelude"
    if u32 == 0:
        prelude_latest += 1
        return PreludeReference(prelude_latest)
    # [1, WindowLen] is a LRU cache
    if 1 <= u32 and u32 <= len(window):
        result = window.fetch(u32 - 1)
        if result.isinstance(PreludeReference):
            prelude_latest = result.index
        return result
    # [WindowLen + 1, WindowLen + PreludeLen] is a reference to the prelude
    # dictionary.
    if len(window) + 1 <= u32 and u32 <= len(window) + len(prelude):
        prelude_latest = u32 - len(window) - 1
        result = PreludeReference(prelude_latest)
        window.insert(result)
        return result
    # [WindowLen + PreludeLen + 1, WindowLen + PreludeLen + len(imported)] is a reference
    # to an already encountered value from the shared dictionary.
    if len(window) + len(prelude) + 1 <= u32 and u32 <= len(window) + len(prelude) + len(imported):
        result = imported[u32 - len(window) - len(prelude) - 1]
        window.insert(result)
        return result
    # Any value above is a first-time reference into the shared dictionary.
    if len(window) + len(prelude) + len(imported) < u32:
        result = SharedReference(u32 - len(window) - len(prelude) - len(imported))
        window.insert(result)
        return result

```


# Main

TBD


# Footers

Not specified/implemented yet.

# Primitive values

## Variable-length numbers (unsigned)

We use `var_u32` to denote a variable-length representation of 32 bit unsigned integers.

```
var_u32 ::= var_u32_non_terminal_byte* var_u32_terminal_byte
var_u32_non_terminal_byte ::= Any byte b such that b & 1 == 1
var_u32_terminal_byte     ::= Any byte b such that b & 1 == 0
var_u32_invalid_1 ::= 0x00000001 0x00000000
var_u32_invalid_2 ::= 0x00000001 0x00000001 0x00000000
var_u32_invalid_3 ::= 0x00000001 0x00000001 0x00000001 0x00000000
```

TBD


As an exception, the following sequence of `var_u32_non_terminal_byte* var_non_terminal_byte` is *not* a `var_u32`:
- it consists in at least two bytes;
- all `var_u32_non_terminal_byte` are `1`;
- the `var_u32_terminal_byte` is `0`.

Such sequences are used to represent exceptional cases (e.g. `null` values).

## Variable-length numbers (signed)

```
var_i32 ::= var_u32
var_i32_invalid_1 ::= 0x00000001 0x00000000 // Aliased for convenience
var_i32_invalid_2 ::= 0x00000001 0x00000001 0x00000000 // Aliased for convenience
var_i32_invalid_3 ::= 0x00000001 0x00000001 0x00000001 0x00000000 // Aliased for convenience
```

Signed variable-length numbers are stored as unsigned, using the following algorithm:
- if `signed_value` is a `var_u32` used to store a `var_i32`
    - if `signed_value % 2` == 0
        - the unsigned value is `signed_value >> 1`
    - else
        - the unsigned value is `-((signed_value - 1) >> 1)`


TBD

## Floating-point

We use `f64` to denote 64-bit floating point numbers.

```
f64 ::= IEEE754 64 Floating Point encoding
```

## Raw data
```
[char; N] ::= the next N bytes in the stream
```

