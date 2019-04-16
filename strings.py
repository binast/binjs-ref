#!/usr/bin/env python3

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import doctest

import ast
import bits
import idl
import re


DICT_SIGNATURE = b'astdict'


class StringCollector(ast.AstVisitor):
  def __init__(self, types):
    super().__init__(types)
    self.strings = set()

  def visit_primitive(self, ty, value):
    if ty == idl.TY_STRING:
      self.strings.add(value)


def prepare_dict(types, sources):
  '''
  Builds and returns a dictionary.

  Args:
    types: idl.IdlTypeResolver for ES6.
    sources: iterable of idl.Ty, object pairs to extract strings from.

  Returns:
    A dictionary suitable for encoding those sources.
  '''
  collector = StringCollector(types)
  for ty, source in sources:
    collector.visit(ty, source)
  strings = list(sorted(collector.strings))
  return strings


def write_dict(out, strings, with_signature):
  '''Writes a dictionary from prepare_dict to byte-oriented output.'''
  if with_signature:
    out.write(DICT_SIGNATURE)
  bits.write_varint(out, len(strings))
  for s in strings:
    encoded = s.encode('utf-8')
    encoded = re.sub(b'([\x00\x01])', b'\x01\\1', encoded)
    out.write(encoded)
    out.write(b'\x00')


def read_dict(inp, with_signature):
  '''Reads a dictionary from byte-oriented input.

  >>> import ast, idl, io
  >>> types = idl.parse_es6_idl()
  >>> tree = ast.load_test_ast('y5R7cnYctJv.js.dump')
  >>> strings = prepare_dict(types, [(types.interfaces['Script'], tree)])
  >>> buf = io.BytesIO()
  >>> write_dict(buf, strings, True)
  >>> buf.seek(0)
  0
  >>> back = read_dict(buf, True)
  >>> strings == back
  True
  '''
  if with_signature:
    signature = inp.read(len(DICT_SIGNATURE))
    assert signature == DICT_SIGNATURE, 'signature mismatch: ' + str(signature)
  n_strings = bits.read_varint(inp)
  strings = []
  for _ in range(n_strings):
    buf = bytearray()
    while True:
      b = inp.read(1)
      if b == b'\x01':
        b = inp.read(1)
      elif b == b'\x00':
        break
      buf.extend(b)
    s = buf.decode('utf-8')
    strings.append(s)
  return strings


if __name__ == '__main__':
  doctest.testmod()
