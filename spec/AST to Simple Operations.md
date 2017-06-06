**FIXME** This needs to be specified.

**FIXME** Specify what's a Simple Operations writer and what "driving" one means.

**FIXME** Specify what's a Simple Operations reader and what "driving" one means.

# Quick Overview

*Simple Operations* are a simple algebra designed to be expressive enough to represent any AST matching the conventions of `AST conventions.md` and simple enough that it can be efficiently stored and parsed.

This document specifies how we can go from an AST and an AST grammar both
matching the conventions of `AST conventions.md` and use it to drive a Simple Operations writer, presumably to produce a stream of bytes.

This document specifies how we can go from an AST grammar matching the
conventions of `AST conventions.md` and use it to drive a Simple Operations
reader, presumably reading a stream of bytes, to finally produce an AST
also matching the conventions of `AST conventions.md`.
