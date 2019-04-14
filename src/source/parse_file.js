/**
 * This CLI "daemon" expects file paths to JavaScript files as JSON strings
 * on the stdin and will parse them and produce BinaryAST objects as JSON
 * on stdout.
 */

'use strict';

const { readFileSync } = require('fs');
const { parseScript } = require('shift-parser');
const startJSONStream = require('./start-json-stream');

startJSONStream({
    // Mirrors the Rust side of generic value transformations.
    //
    // This one takes a file path from the Rust side as an input, reads its
    // contents and parses into a Shift AST.
    transform: filename => {
        let code = readFileSync(filename, 'utf-8');
        return parseScript(code, { earlyErrors: false });
    },

    // `toJSON` is a `JSON.stringify` replacer callback that converts Shift AST
    // to a BinaryAST JSON format on the fly.
    toJSON: require('./from-shift')
});
