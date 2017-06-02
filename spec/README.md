# Overview of BinJS

**FIXME** TODO

# Files in this directory

- `AST conventions.md` specifies the AST conventions used for BinJS.
- `AST to Simple Operations.md` specifies
  - the algorithm used to drive a Simple Operations writer to with an AST
    from a specific AST grammar matching our AST conventions;
  - the algorithm used to drive a Simple Operations reader using a specific AST
    grammar matching our AST conventions and use it to build an AST.
- `Simple Operations to Binary.md` specifies
  - the algorithm used to how to implement a Simple Operations writer into
    a compressed stream of bytes;
  - the algorithm used to implement a Simple Operations writer from a
    compressed stream of bytes.