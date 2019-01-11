/**
 * This module is the primary entry point that takes care of communication
 * with the Rust side over stdin and stdout pipes.
 *
 * Each pipe is expected to be an ND-JSON (newline-delimited JSON) message.
 * Because we're not doing anything asynchronously, it's expected that after
 * each input being sent over the stdin as a separate line, JS will produce
 * an answer on stdout right after, also as a JSON value on a separate line.
 */

'use strict';

const split = require('split');

// See crates/binjs_io/src/escaped_wtf8.rs
function escapeWTF8(s) {
  return s.replace(/[\u007F\uD800-\uDFFF]/gu, m => {
    if (m == '\u007F') {
      {
        return '\u007F007F';
      }
    }
    return '\u007F' + m.charCodeAt(0).toString(16);
  });
}

/**
 * This API should be called with a callback which simply accepts a parsed input
 * and either returns an output value or throws.
 *
 * JSON parsing, serialisation and error handling will be taken care of.
 */
module.exports = mapper =>
  process.stdin
    .pipe(split(line => {
        line = JSON.parse(line);
        try {
          line = { type: 'Ok', value: mapper(line) };
        } catch (e) {
          line = { type: 'Err', value: e.message };
        }
        line = escapeWTF8(JSON.stringify(line));
        return line + '\n';
      }, null, { trailing: false }))
    .pipe(process.stdout);
