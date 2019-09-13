# Context 0.1 Format (née "fbssdc")

- Dominic Cooney (dpc) 2019-06-27
- Reformated for markdown David Teller (yoric) 2019-09-02


# Overview

BinAST is a Work In Progress format for JavaScript programs designed for compact transfer and fast, streaming startup. This document describes the "Context 0.1" format of files on disk.

The goals of this iteration of the BinAST format are to:
 -  Represent JavaScript syntax trees and metadata about variable capture, etc.
 -  Be compact when compressed with Brotli.
 -  Support random access to the content of lazy functions within the decompressed stream.

The syntax trees and metadata are described in this IDL file: https://github.com/binast/binjs-ref/spec/es6.webidl and can be produced using the `binjs_encode` tool’s `--show-ast` option; see https://github.com/binast/binjs-ref/. You should have a copy of the IDL handy when reading this document.

This document describes the structure of the files on disk at the bit level. This document includes commentary about design choices in the format, but not in the AST/IDL structure.
The Context 0.1 format consists of the following items, in order:

1. A signature, the bytes `89h`, `42h`, `4ah`, `53h` (these three bytes `"BJS"`) `dh` `ah` `0h` `ah` `2h` (NOTE: Currently not implemented!)
2. A table of strings.
3. A set of tables for constructing codes used to decode the AST
4. An encoded Script AST node

The subsequent sections describe each of these items in detail.

To ease skimming, the format is summarized in BNF in green. The previous list can be written this way:


```
File ::= Signature StringTable
         CodeTables Node
```

Repeated items will be written between square brackets with braces indicating the number of items. Literal bytes will be written in hexadecimal with a trailing `h`.

# Signature

The file starts with these bytes: `89h`, `42h`, `4ah`, `53h` (these three bytes `"BJS"`) `dh` `ah` `0h` `ah` followed by a version number.

```
Signature ::= 89h 42h 4ah 53h 0dh 0ah 00h 0ah VersionNumber
VersionNumber ::= 02h
```

# String Table

JavaScript programs contain a lot of text data, not only in string literals, but also in property names, variable names, etc. The format lifts string data into a header section and refers to strings by index.

This has a couple of benefits: First, property names can be relatively long and are referred to repetitively, so using an index is more succinct. Second, there is a lot of redundancy between strings and Brotli compresses these more effectively if they are close together (although the benefit only scales ~logarithmically.)

## String Table Format

The string table consists of:
 -  The number of strings, n, in varuint encoding.
 -  n strings.


Each string consists of the literal bytes of string content, with a null terminator. Because strings may contain embedded nulls, nulls are escaped as `01h 00h`, which means `01h` bytes must be escaped too; they’re escaped as `01h 01h`.

The format does not do any semantically impactful string mangling, for example, if a string literal has an escape sequence, the escape sequence will be represented in the string data.

```
StringTable ::= StringCount [String]{StringCount}
StringCount ::= VarUint
String ::= [StringByte] 00h
StringBytes ::= 02h .. ffh   (a literal byte)
              | 01 00h       (the byte 00h)
              | 01 01h       (the byte 01h)
```

## External String Tables

The format can optionally refer to strings from an external string table. External string tables make it possible to produce very small files because strings can often be cached, reused, and shared between files (for example a library function and its caller will both refer to that method’s name.) These strings are assigned indices starting after the last intrinsic string.
The external string table is specified out-of-band and a decoder has limited capacity to detect a missing external string table -- when it encounters an out-of-bounds string table reference.

External string tables start with the ASCII bytes `astdict`. The following bytes are in the same format as the intrinsic string table.

```
StringFile ::= StringSignature StringTable
StringSignature ::= 61h 73h 74h 64h 69h 63h 74h
```

# Code Tables

In general the AST is serialized by traversing the AST writing the symbols encountered into the file. (There is an exception to this supporting random access to lazy function bodies described later.) Because the content of files can vary a lot, the format does not use a fixed set of codes but instead the file contains tables of codes used in the file.

## Symbols

ASTs contain the following kinds of symbols:

 -  Primitive values: Booleans, unsigned integers, JavaScript numbers (IEEE 754 64-bit floating point numbers), strings, and optional strings.
 -  Enumeration values. (These are described using strings in es6.webidl, but the format treats enums as a distinct kind of value.)
 -  Interface type tags. For example, Script and Block are interface type tags. Note that typedefs in es6.webidl are for convenience when reading but the format deals with desugared interface definitions without typedefs.
 -  None, a special symbol which represents an absent interface.

Symbols can appear in the file in two ways: literally; or encoded with a code. The format makes it clear when each form is used and the two forms are never mixed.

## Symbol Context Modeling

The Context 0.1 format saves space by using different symbol → code mappings depending on the part of the AST being encoded. For example, when encoding a Boolean value, only two symbols are valid so the format can use a code which is at most 1 bit long, even though that overlaps with codes in other contexts.

There are two kinds of contexts:

1. Fields, for example `AssertedBoundName.isCaptured`.
2. Array lengths of a given array type, for example a `FrozenArray<(Spread or Expression)>` length

The set of symbols which are allowed depend on the type associated with the context:

 -  For a field with an array type, the associated type is the element type of the array.
 -  For other fields, the associated type is the type of the field.
 -  For an array length, the associated type is unsigned long.

Given an associated type, the allowable symbols are:
 -  For a primitive type, the allowed symbols are values of that type.
 -  For an enum type, the allowed symbols are the members of the enum.
 -  For a set of interface type tags, possibly including `None`, the allowed symbols are those type tags, and `None` if applicable.

Each context which appears in the AST will have a code table, with two exceptions:

1. Fields with a monomorphic interface type or a `FrozenArray` of a monomorphic interface element type must always produce that interface type tag. It is redundant to produce tables for these and the format prevents that.

2. The `StaticMemberAssignmentTarget.property` field and `StaticMemberExpression.property` field are treated as the same context. These fields usually have large tables of property names with similar lengths, so they are combined to save space. Algorithms which talk about enumerating the fields of an interface should treat these two interfaces as sharing one property field.

## Code Table Encoding
Each code table does not specify the codes directly, but instead specifies the length of each symbol’s code. How codes are assigned based on length is described later.

Code tables can be encoded multiple ways:

```
CodeTable ::= UnitCodeTable
            | MultiCodeTableImplicit
            | MultiCodeTableExplicit
            | EmptyCodeTable

UnitCodeTable ::= 00h LiteralSymbol
MultiCodeTableExplicit ::= 01h CodeCount [CodeLength]{CodeCount} [LiteralSymbol]{CodeCount}
CodeLength ::= 00h .. 14h
MultiCodeTableExplicit ::= 01h [CodeLength]{SymbolCount}
EmptyCodeTable ::= 02h
```

The `UnitCodeTable` is a table with a single symbol. This symbol is assigned given a zero-length code. As a consequence when that table is used to encode a symbol in the tree, it generates no bits in the output.

Because modern minified JavaScript tends to be very regular `UnitCodeTables` occur a lot in practice and are a large contributor to the size reduction of BinAST encoded tree sizes. For example, BinAST tracks whether each scope uses `eval` (see `hasDirectEval` in `es6.webidl`.) However modern JavaScript practice avoids using `eval`. Encoding a unit code table for `false` takes two bytes, `00h 00h`, so the format can represent an unlimited number of block scope’s lack of `eval` with a fixed overhead of two bytes -- fewer after Brotli compression.

Note that a unit code table is the only way to produce a zero-length code. `MultiCodeTableImplicit` may mention symbols with zero length, but these are skipped during code assignment.
The `MultiCodeTable` is a table with multiple symbols. There are two variants of the encoding depending on the type of symbol being encoded.

The implicit table is for symbols which form a small, closed set. The lengths of all symbols are enumerated. Unused symbols are denoted with zero length and are skipped during code assignment.

The explicit table is for symbols which form an effectively open set. The table includes a symbol count, then the lengths of all symbols, then the literal symbols.

Type                         | Table Type | Ordering
-----------------------------|------------|------------------
Boolean                      | Implicit   | `false`, `true`
Unsigned integers            | Explicit   | Encoder determined
JavaScript numbers           | Explicit   | Encoder determined
Strings                      | Explicit   | Encoder determined
Optional strings             | Explicit   | Encoder determined
Enumeration values           | Implicit   | IDL declaration order
Interface type tags & `None` | Implicit   | `None` first; then lexical order of interface names


Finally, the `EmptyCodeTable` represents a set of unused codes. How empty code tables arise is described later.

## Symbol Literal Encoding

Symbols appear literally in the `UnitCodeTable` and `MultiCodeTableExplicit`. When a symbol appears literally, it is encoded this way:

Type                         | Literal Encoding
-----------------------------|-----------------
Booleans                     | One byte. `false`, `00h`; `true`, `01h`.
Unsigned integers            | 4-byte big-endian integer
JavaScript numbers           | 8-byte big-endian IEEE-754
Strings                      | `Varuint` string table index
Optional string              | If `None`, `00h`, otherwise `Varuint` 1 + string table index
Enumeration value            | Varuint index of IDL declaration order
Interface type tag & `None`  | Given a set of relevant interface type tags, and None if relevant, sorted by symbol ordering (see below), `varuint` index of the symbol in the set


## Code Assignment: From Symbol Lengths to Codes

To encode or decode a file, each symbol must be assigned not just a code length but a specific sequence of bits. The format uses a canonical Huffman code for this purpose.

Given a set `S` of length, symbol pairs, canonical Huffman codes can be assigned this way:

1. Sort `S` by length. For pairs with the same length, sort them by the symbol. (Symbol ordering is defined below.)
2. Initialize `code = 0`, `last_length = 0`
3. For i ∈ 1...|S|
   1. `next_length` = length associated with `S[i]`
   2. Assign code of length `next_length` to symbol `S[i]`
   3. `code = (code + 1) × 2^(next_length - last_length)`
   4. last_length = next_length

The codes constructed by this algorithm begin in the high-order bit, so when these codes are written into the file the high-order bit must appear first. That is, codes are written as bitwise big-endian, variable-length numbers.

A decoder should check codes fit in their prescribed length; a file which produced codes longer than their length is invalid. Codes have a maximum length of 20 bits, which in practice limits JavaScript files to roughly a million strings and a million numbers.

## Symbol Ordering for Code Assignment


To briefly recap, files contain tables specifying the code lengths (but not the codes themselves) of symbols which appear in the serialized AST. The encoder and decoder must agree on the order different symbols with the same code length are assigned codes using the algorithm above. Here is the order:

Type                         | Order for Code Assignment
-----------------------------|-------------------------------
Boolean                      | `false`, `true`
Unsigned integers            | Order literals appear in the code table
JavaScript numbers           | Order literals appear in the code table
Strings                      | String table index
Optional strings             | `None`, then string table index
Enumeration values           | Lexicographic order of IDL string constant
Interface type tags & `None` | `None`, then lexicographic order of name

Note, this order is for assigning codes. This ordering is not related to the ordering values like strings and numbers might usually have, although that situation could arise coincidentally as in implementation detail of an encoder. As you have seen, enumeration values use a different ordering in enumerating lengths in `MultiCodeImplicitTables`. (This was an oversight but a harmless one.)

## Code Table Set Encoding

The format can model hundreds of contexts. Most files use far fewer contexts than this. To keep the file compact, the code table set only encodes tables used by the file. It does this by doing a depth-first walk through the fields of the IDL, starting with the fields of the Script interface. The walk is pruned in two ways: when it encounters array length contexts with code tables which only contain the symbol zero; when interface type tags are not present in a set of allowable symbols.

To decode models:

1. Initialize the set of visited contexts to empty
2. Initialize the set of empty array types to empty
3. Push all of the fields of the Script interface
4. While the stack of fields is not empty
  1. Pop a field
  2. Decode a code table for the effective type of the field.
  3. For each of the code table’s symbols that are interface type tags:
      1. Push all of the fields of the interface

To push a field:

1. If the field is in the set of visited contexts, stop
2. Add the field to the set of visited fields
3. If the field has a FrozenArray type
  1. Determine if the array type is always empty
  2. If so, stop
4. If the effective type is a monomorphic interface, push all of the interface’s fields
5. Otherwise, push the field onto the stack

To determine a field’s effective type:

1. If the field’s type is an array type, the effective type is the array element type. Stop.
2. Otherwise, the effective type is the field’s type. Stop.

To determine if an array type is always empty:

1. If the array type is in the set of empty arrays types, it is always empty. Stop.
2. If the array type is in the set of visited contexts, it is not always empty. Stop.
3. Decode a code table for unsigned longs. This is the code table for this array type.
4. Add the array type to the set of visited contexts.
5. If the code table has any non-zero symbols, the array type is not always empty. Stop.
6. Otherwise, add the array type to the set of empty array types. It is always empty. Stop.

The sharing of array length models by array type, instead of specific fields, explains the need for `EmptyCodeTables`: A situation can arise where an array length model includes zero and non-zero values, but within the context of a specific field the array length is always zero in practice.

## Node Encoding

With a complete string table and set of codes in hand, encoding and decoding nodes is straightforward:

```
Node ::= [Symbol] <pad to byte boundary> LazyItemCount [LazyItemLength]{LazyItemCount} [Node]{LazyItemCount}
LazyItemCount ::= varuint
LazyItemLength ::= varuint
```

To deserialize an interface I:
1. For each field f in I:
  1. If f’s type is an array:
    1. Decode an array length, n, using the code table for the array type consuming the prefix of the input bit stream.
    2. For 1 ... n:
      1. Deserialize a value in context f.
  2. Deserialize a value in context f.

To deserialize a value in a field context f:
1. If f has a Lazy attribute, add the effective type of f to the lazy item list. (Currently, these are always monomorphic interface types.)
2. If f’s effective type is a monomorphic interface type, deserialize an interface of f’s effective type. Note, this does not consume the bit stream or consult any code table.
3. If f’s effective type includes interface type tags:
  1. Decode an interface type tag J, or None, using the code table for f consuming the prefix of the input bitstream.
  2. If J is not none, deserialize an interface J.
4. Otherwise, decode a symbol using the code table for f consuming the prefix of the input bitstream.

An AST subtree may contain lazy sections; these are tagged `[Lazy]` in es6.webidl. A tree or subtree has postamble 0-padding to a byte boundary, a redundant count of lazy items, and a list of byte lengths which refer to the following serialized lazy nodes. This arrangement supports random access to lazy fields, once the subtree containing the field has been deserialized to discover the types of those fields.

The encoding is recursive -- the script’s lazy nodes may themselves have lazy nodes which are written at the end of that node’s serialization.

# Variable-length Unsigned Integers (varuint)
## Encoding

Unsigned integers appear in various places in the format. These are encoded using a byte-oriented format of 7 bits of little-endian data and the high order bit indicating continuation. These should not exceed 5 bytes containing up to 32 bits and 3 zero bits.

```
Varuint ::= [1nnnnnnn]{0..4} 0nnnnnnn
```
