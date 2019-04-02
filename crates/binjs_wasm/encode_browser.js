'use strict';

import fromShift from '../../src/source/from-shift';
import { parseScript } from 'shift-parser';
import wasm_bindgen_init, { encodeMultipart } from './pkg/binjs_wasm';

const WASM = wasm_bindgen_init('./pkg/binjs_wasm_bg.wasm');

const input = document.getElementById('input');
const output = document.getElementById('output');

input.addEventListener('change', async () => {
	let file = input.files[0];

	let js = await new Promise((resolve, reject) => {
		let reader = new FileReader();
		reader.onload = () => resolve(reader.result);
		reader.onerror = () => reject(reader.error);
		reader.readAsText(file);
	});

	let shiftAST = parseScript(js, { earlyErrors: false });
	let bAST = JSON.parse(JSON.stringify(shiftAST, fromShift));

	if (output.src) {
		URL.revokeObjectURL(output.src);
		output.src = '';
	}

	await WASM;
	let encoded = encodeMultipart(bAST);

	output.src = URL.createObjectURL(new Blob([encoded], { type: 'application/javascript-binast' }));
});
