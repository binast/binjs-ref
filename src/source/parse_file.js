var parseScript = require('shift-parser').parseScript;
var getStdin = require('get-stdin');
var escape = require('./escape_wtf8.js');
var readFileSync = require('fs').readFileSync;

getStdin().then(filename => {
	var data = readFileSync(filename, 'utf-8');
	var parsed = parseScript(data, { earlyErrors: false });
	process.stdout.write(escape(JSON.stringify(parsed)));
});
