'use strict';

const codegen = require('shift-codegen').default;
const startJSONStream = require('./start-json-stream');

startJSONStream(ast => codegen(ast));
