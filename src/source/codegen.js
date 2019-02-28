/**
 * This CLI "daemon" expects Shift AST objects (in JSON format) on the stdin
 * and will return a generated JS (as JSON strings) on stdout.
 */

'use strict';

const codegen = require('shift-codegen').default;
const startJSONStream = require('./start-json-stream');

startJSONStream({
    // Mirrors the Rust side of generic value transformations.
    //
    // This one takes an AST from the Rust side as an input and applies
    // a codegen as a transform, returning generated JavaScript back.
    transform: ast => codegen(ast)
});
