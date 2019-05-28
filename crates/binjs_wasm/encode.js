'use strict';

import fromShift from '../../src/source/from-shift';
import { parseScript } from 'shift-parser';
import initWASM, { encodeMultipart } from './pkg';

const initialisedWASM = initWASM(BINJS_WASM);

const CONTENT_TYPE = 'application/javascript-binast';
const CONTENT_TYPE_RE = /^application\/javascript-binast(?:,|$)/; // strict check for the following char
const JS_CONTENT_TYPE_RE = /^(?:text|application)\/javascript(?:;|$)/;
const MAX_JS_SIZE = 1 << 20; // 1 MB = 2 ^ 20 bytes
const VERSION = 'binjs-21'; // cache buster

// Performs recursive transformation using the same strategy as JSON.stringify.
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
			// Request to the origin shouldn't ask for BinaryAST.
			origReq.headers.set('Accept', '*/*');
			origRes = await fetch(origReq);
			log('original response', `Status: ${origRes.status} ${origRes.statusText}; Content-Type: ${origRes.headers.get('Content-Type')}; Content-Length: ${origRes.headers.get('Content-Length')}`);
			if (!origRes.ok) return origRes;

			// Make sure we don't accidentally perform mime sniffing on non-JS responses.
			let contentType = origRes.headers.get('Content-Type') || '';
			if (!JS_CONTENT_TYPE_RE.test(contentType)) return;

			// Check Content-Length if it exists.
			let contentLength = +origRes.headers.get('Content-Length');
			if (contentLength > MAX_JS_SIZE) return;

			// First, store the JS into the cache. If transformation fails
			// or takes a long time, other requests will take and return this
			// unmodified responses instead of attempting to spawn new
			// transformations for the same resource.
			event.waitUntil(cache.put(req, origRes.clone()).then(() => {
				log('cached original response');
			}));
		}

		event.waitUntil(
			(async function transformAndCache() {
				try {
					let js = await origRes.clone().text();
					log('original text length', js.length);

					// In case we didn't have Content-Length, check the actual size too.
					if (js.length > MAX_JS_SIZE) return;

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
