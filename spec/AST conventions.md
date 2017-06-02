**FIXME** This needs to be specified.

**FIXME** Specify the conventions of our AST grammar (e.g. subtyping, extending nodes, ...)

**FIXME** Specify what is an AST.

# Quick overview

We use a slight variant on the conventions of ESTree.

Each AST node is a dictionary. Each node has a distinguished property `type`
that defines the role of the node in the AST (e.g. `ArrayExpression`). The
`type` dictates the set of properties for the node. However, due to
evolutions of JavaScript, it is possible that two nodes in two different
files bearing the same `type` will not have the exact same set of properties.

It is expected that actual implementations of the AST will not use dictionaries
but some more optimized data structures.