var codegen = require('shift-codegen').default;
var getStdin = require('get-stdin');

getStdin().then(ast => {
	var parsed = JSON.parse(ast);
	process.stdout.write(codegen(parsed));
});
