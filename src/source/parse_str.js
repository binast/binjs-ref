/**
 * This CLI "daemon" expects JSON strings containing JavaScript code on the
 * stdin and will parse them and produce BinaryAST objects as JSON on stdout.
 */

'use strict';

const { parseScript } = require('shift-parser');
const startJSONStream = require('./start-json-stream');

startJSONStream({
    // Mirrors the Rust side of generic value transformations.
    //
    // This one takes a JavaScript source code from the Rust side as an input
    // and parses it into a Shift AST.
    transform: code => parseScript(code, { earlyErrors: false }),

    // `toJSON` is a `JSON.stringify` replacer callback that converts Shift AST
    // to a BinaryAST JSON format on the fly.
    toJSON: require('./from-shift')
});
