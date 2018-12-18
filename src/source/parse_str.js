'use strict';

const { parseScript } = require('shift-parser');
const startJSONStream = require('./start-json-stream');

startJSONStream(code => parseScript(code, { earlyErrors: false }));
