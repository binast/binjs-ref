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
