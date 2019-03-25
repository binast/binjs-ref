# About this file

This is an attempt to document the Entropy 0.4 compression format.

# Global structure

```
Stream ::= Headers Prelude? Content? Main Footers?
```

# Headers

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
Headers ::= MagicNumber FormatVersionNumber LinkToSharedDictionary
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


# Prelude

The **prelude** extends the shared dictionary with additional strings, numbers, keys, names, ...

Data structures are separated even when they have the same underlying representation, to improve
compression.

```
Prelude ::= "[[prelude]]" NamedPreludeDictionary*
NamedPreludeDictionary ::= "[floats]" PreludeStream<f64?>
                         |  "[unsigned_longs]" PreludeStream<u32?>
                         |  "[list_lengths]" PreludeStream<u32?>
                         |  "[property_keys]" PreludeStream<String>
                         |  "[identifier_names]" PreludeStream<String>
                         |  "[string_literals]" PreludeStream<String>
                         |  "[property_keys_len]" PreludeStream<StringLen>
                         |  "[identifier_names_len]" PreludeStream<StringLen>
                         |  "[string_literals_len]" PreludeStream<StringLen>
```

If the same prelude name (`"[floats]"`, `"[unsigned_longs]"`, etc.) appears more than once, this
is a syntax error.

If `["property_keys"]` exists and is not empty, `'["property_keys_len"]` must exist and be non-empty,
otherwise, this is a syntax error.

If `["identifier_names"]` exists and is not empty, `'["identifier_names"]` must exist and be non-empty,
otherwise, this is a syntax error.

If `["string_literals"]` exists and is not empty, `'["string_literals_len"]` must exist and be non-empty,
otherwise, this is a syntax error.

## Internal compression

All prelude streams have the same structure

```
PreludeStream<T> ::= CompressionFormat ";" ByteLen CompressedByteStream<T>
CompressionFormat ::= "brotli" // The only compression format supported so far.
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
Content ::= "[[content]]" NamedContentStream*
NamedContentStream ::= "[floats]" ContentStream<f64?>
                     | "[unsigned_longs]" ContentStream<u32?>
                     | "[list_lengths]" ContentStream<u32?>
                     | "[property_keys]" ContentStream<String>
                     | "[identifier_names]" ContentStream<String>
                     | "[string_literals]" ContentStream<String>
```

If the same content name (`"[floats]"`, `"[unsigned_longs]"`, etc.) appears more than once, this
is a syntax error.

## Internal compression

All content streams have the same structure

```
ContentStream<T> ::= CompressionFormat ";" ByteLen CompressedContentStream<T>
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