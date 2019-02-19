// Extensions to the webidl.
//
// These extensions are not part of the AST spec, but of the compression mechanism.

[ExtendsTypeSum=Expression]
// An extension to `Expression` used to locally change probability tables.
//
// Instances of `BinASTExpressionWithProbabilityTable` are designed to
// be inserted during compression and erased during decompression.
//
// In terms of semantics, an instance of
// `BinASTExpressionWithProbabilityTable { table: _, expression: e }`
// is equivalent to `e`.
interface BinASTExpressionWithProbabilityTable {
    // The name of the probability table to use (e.g. `main`, `json`).
    //
    // The list of probability tables is not specified as part of the grammar.
    // Rather, individual compression dictionaries may define separate probability
    // tables.
    //
    // Attempting to (de)compress with a probability table that does not
    // appear in the dictionary is a (de)compression error.
    attribute string table;

    // The actual expression.
    attribute Expression expression;
};
