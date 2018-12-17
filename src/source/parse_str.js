var parseScript = require('shift-parser').parseScript;
var getStdin = require('get-stdin');
var escape = require('./escape_wtf8.js');

getStdin().then(data => {
	var parsed = parseScript(data, { earlyErrors: false });
	process.stdout.write(escape(JSON.stringify(parsed)));
});
