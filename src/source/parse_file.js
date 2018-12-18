'use strict';

const { readFileSync } = require('fs');
const { parseScript } = require('shift-parser');
const startJSONStream = require('./start-json-stream');

startJSONStream(filename => {
	let code = readFileSync(filename, 'utf-8');
	return parseScript(code, { earlyErrors: false });
});
