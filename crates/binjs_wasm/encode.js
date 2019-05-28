'use strict';

import fromShift from '../../src/source/from-shift';
import { parseScript } from 'shift-parser';
import initWASM, { encodeMultipart } from './pkg';

const initialisedWASM = initWASM(BINJS_WASM);

const CONTENT_TYPE = 'application/javascript-binast';
const CONTENT_TYPE_RE = /^application\/javascript-binast(?:,|$)/;
const MAX_JS_SIZE = 1 << 20; // 1 MB = 2 ^ 20 bytes
const VERSION = 'binjs-19';

function transformLikeToJSON(obj, callback) {
	return (function transform(obj, k, v) {
		obj[k] = v = callback.call(obj, k, v);

		if (typeof v === 'object' && v !== null) {
			for (let k in v) {
				transform(v, k, v[k]);
			}
		}

		return v;
	})({ '': obj }, '', obj);
}

addEventListener('fetch', event => {
	if (event.request.method !== 'GET') return;

	let url = new URL(event.request.url);
	if (!url.pathname.endsWith('.js')) return;

	let accept = event.request.headers.get('Accept') || '';
	if (!CONTENT_TYPE_RE.test(accept)) return;

	let contentLength = event.request.headers.get('Content-Length') | 0;
	if (!contentLength || contentLength > MAX_JS_SIZE) return;

	event.passThroughOnException();
	event.respondWith(handleBinJS(event));
});

async function handleBinJS(event) {
	const req = event.request;

	function log(...args) {
		args.unshift(req.url);

		console.log(...args);

		// event.waitUntil(
		//   fetch('...', {
		//     method: 'POST',
		//     body: args.join(' ')
		//   })
		// );
	}

	try {
		const cache = await caches.open(VERSION);

		{
			let cacheRes = await cache.match(req);
			log('cache match', cacheRes ? `Content-Type: ${cacheRes.headers.get('Content-Type')}` : false);
			if (cacheRes) return cacheRes;
		}

		let origRes;
		{
			let origReq = new Request(req);
			origReq.headers.set('Accept', '*/*');
			origRes = await fetch(origReq);
			log('original response status', origRes.statusText || origRes.status);
			if (!origRes.ok) return origRes;
			event.waitUntil(cache.put(req, origRes.clone()).then(() => {
				log('cached original response');
			}));
		}

		event.waitUntil(
			(async function transformAndCache() {
				try {
					let js = await origRes.clone().text();
					log('original text length', js.length);

					const shiftAST = parseScript(js, { earlyErrors: false });
					log('parsed');

					const bAST = transformLikeToJSON(shiftAST, fromShift);
					log('transformed');

					await initialisedWASM;
					let encoded = encodeMultipart(bAST);
					log('encoded', encoded.length);

					const bastRes = new Response(encoded, origRes);
					bastRes.headers.delete('Content-Encoding');
					bastRes.headers.set('Content-Type', CONTENT_TYPE);
					bastRes.headers.append('Vary', 'Accept');
					log('created bast response');

					await cache.put(req, bastRes);
					log('cached bast');
				} catch (e) {
					log('error', e.stack);
				}
			})()
		);

		return origRes;
	} catch (e) {
		log('error', e.stack);
		throw e;
	}
}
