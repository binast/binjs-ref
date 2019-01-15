/**
 * This CLI "daemon" expects file paths to JavaScript files as JSON strings
 * on the stdin and will parse them and produce Shift AST objects as JSON
 * on stdout.
 */

 'use strict';

const { readFileSync } = require('fs');
const { parseScript } = require('shift-parser');
const startJSONStream = require('./start-json-stream');

startJSONStream(filename => {
	let code = readFileSync(filename, 'utf-8');
	return parseScript(code, { earlyErrors: false });
});
