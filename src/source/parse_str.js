/**
 * This CLI "daemon" expects JSON strings containing JavaScript code on the
 * stdin and will parse them and produce Shift AST objects as JSON on stdout.
 */

'use strict';

const { parseScript } = require('shift-parser');
const startJSONStream = require('./start-json-stream');

startJSONStream(code => parseScript(code, { earlyErrors: false }));
