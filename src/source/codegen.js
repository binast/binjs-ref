/**
 * This CLI "daemon" expects Shift AST objects (in JSON format) on the stdin
 * and will return a generated JS (as JSON strings) on stdout.
 */

'use strict';

const codegen = require('shift-codegen').default;
const startJSONStream = require('./start-json-stream');

startJSONStream(ast => codegen(ast));
