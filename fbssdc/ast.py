#!/usr/bin/env python3

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import doctest
import json
import os
import subprocess

import idl

BINJS_SIGNATURE = b'BINJS\x02'

def load_ast(filename):
  with open(filename) as inp:
    return json.loads(inp.read())

def load_test_ast(filename):
  return load_ast(os.path.join(os.path.dirname(__file__), 'test-data', filename))

# Map ASTs to something which includes all of the data.
# Values:
# - tagged object (with "type")
# - int
# - float
# - string
# - bool
# - None
# - list

# TODO: Add a "specials" for objects added by AST mutilations, like LazyIOUs.
class AstVisitor(object):
  def __init__(self, types):
    self.types = types

  def visit(self, ty, value):
    '''Visits value with declared type ty.

    Declared types may be much broader than the actual type of value.
    The encoder must narrow this uncertainty for the decoder to make
    coordinated decisions.
    '''
    if type(value) in [bool, float, int, str, type(None)]:
      self.visit_primitive(ty, value)
    elif type(value) is list:
      # FIXME: When es6 IDL uses ... or FrozenArray<...>, unpack and
      # narrow the type to this value.
      assert isinstance(ty, idl.TyFrozenArray), str(ty)
      self.visit_list(ty, value)
    elif type(value) is dict:
      actual_ty = self.types.interfaces[value['type']]
      self.visit_struct(ty, actual_ty, value)
    else:
      assert False, f'unreachable: {type(value)}'

  def visit_list(self, ty, xs):
    for i, x in enumerate(xs):
      self.visit_list_item(ty.element_ty, i, x)

  def visit_list_item(self, ty, i, x):
    self.visit(ty, x)

  def visit_struct(self, declared_ty, actual_ty, obj):
    for i, attr in enumerate(actual_ty.attributes()):
      self.visit_field(actual_ty, obj, i, attr)

  def visit_field(self, struct_ty, obj, i, attr):
    self.visit(attr.resolved_ty, obj[attr.name])

  def visit_primitive(self, ty, value):
    pass


# This type is not used but it is useful as a unit test for AstVisitor.
class AstStringIndexer(AstVisitor):
  '''
  >>> types = idl.parse_es6_idl()
  >>> tree = load_test_ast('y5R7cnYctJv.js.dump')
  >>> visitor = AstStringIndexer(types)
  >>> visitor.visit(types.interfaces['Script'], tree)
  >>> len(visitor.strings)
  1330
  >>> visitor.strings[10:14]
  ['IdentifierExpression', 'CavalryLogger', 'start_js', 'ArrayExpression']
  '''
  def __init__(self, types):
    super().__init__(types)
    self.strings = list()

  def visit_primitive(self, ty, value):
    super().visit_primitive(ty, value)
    if type(value) is str:
      self.strings.append(value)


# When a lazy node is deported, this breadcrumb is dropped in its
# place. References to lazy functions are deserialized in order so
# there is no need to add indexes to them.
# TODO: Make this a singleton.
class LazyIOU(object):
  pass


if __name__ == '__main__':
  doctest.testmod()
