#!/usr/bin/env python3

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import doctest
import json

import idl

class FloatFixer(object):
  '''
  Shift writes some doubles as ints; cheers, JavaScript.

  >>> tys = idl.parse_es6_idl()
  >>> import ast
  >>> a = ast.load_test_ast('y5R7cnYctJv.js.dump')
  >>> checker = TypeChecker(tys)
  >>> checker.check(a)
  Traceback (most recent call last):
  ...
  Exception: expected float but got 0
  >>> floater = FloatFixer(tys)
  >>> floater.rewrite(tys.interfaces['Script'], a)
  >>> checker.check(a)
  '''
  def __init__(self, tys):
    self.tys = tys

  def rewrite(self, ty, value):
    if type(ty) is idl.TyInterface:
      # Interfaces
      self.interface(ty, value)
    elif type(ty) is idl.Alt:
      if type(value) is dict:
        self.rewrite(self.tys.interfaces[value['type']], value)
      else:
        # FIXME: If floats start showing up as optional implement
        # unpacking those types.
        return value
    elif type(ty) is idl.TyFrozenArray:
      if ty.element_ty is idl.TY_DOUBLE:
        # FIXME: If doubles start showing up in type alternatives, do more here
        for i, v in enumerate(value):
          if type(v) is int:
            value[i] = float(v)
      else:
        for v in value:
          self.rewrite(ty.element_ty, v)
        return
    elif ty == idl.TY_DOUBLE and type(value) is int:
      assert False, 'unreachable: should have been handled at the containing site'

  def interface(self, ty, node):
    if ty.name != node['type']:
      raise Exception(f'expected {i.name} but got {node["type"]}')
    for attr in ty.attrs:
      if attr.resolved_ty == idl.TY_DOUBLE and type(node[attr.name]) is int:
        node[attr.name] = float(node[attr.name])
      else:
        self.rewrite(attr.resolved_ty, node[attr.name])


class TypeChecker(object):
  '''
  >>> tys = idl.parse_es6_idl()
  >>> checker = TypeChecker(tys)
  >>> a = {'type': 'Foo'}
  >>> checker.check(a)
  Traceback (most recent call last):
      ...
  Exception: expected Script but got Foo
  >>> import ast
  >>> a = ast.load_test_ast('yRA0kDZHvwL.js.dump')
  >>> FloatFixer(tys).rewrite(tys.interfaces['Script'], a)
  >>> checker.check(a)

  Now let's break the type of something; make an if statement test
  a string instead of a node.

  >>> a['statements'][0]['test'] = 'boogers'
  >>> checker.check(a) # doctest: +ELLIPSIS
  Traceback (most recent call last):
      ...
  Exception: no types ['interface Array...
  '''
  def __init__(self, tys):
    self.tys = tys

  def check(self, root):
    '''Checks that root is a script.

    If this does not throw an exception the check passed.
    '''
    # TODO: These could be modules, too.
    ty_script = self.tys.interfaces['Script']
    self.interface(ty_script, root)

  def check_any(self, ty, value):
    if type(ty) is idl.TyInterface:
      # Interfaces
      self.interface(ty, value)
      return
    if type(ty) is idl.TyEnum and value not in ty.values:
      # Enums
      raise Exception(f'{value} is not a {ty} value ({ty.values})')
    if type(ty) is idl.TyNone and value is not None:
      raise Exception(f'{value} should be None')
    if type(ty) is idl.Alt:
      for sub_ty in ty.tys:
        try:
          self.check_any(sub_ty, value)
          return
        except Exception:
          # TODO: Make this a specific exception type.
          if type(sub_ty) is idl.TyInterface and type(value) is dict and sub_ty.name == value.get('type'):
            # Propagate this because the type field matches and there
            # is probably a more specific type error buried in here.
            raise
          pass
      raise Exception(f'no types {list(sorted(map(str, ty.tys)))} matched {json.dumps(value, indent=2)}')
    if type(ty) is idl.TyFrozenArray:
      if type(value) is not list:
        raise Exception(f'expected list representing FrozenArray, was {value}')
      for item in value:
        self.check_any(ty.element_ty, item)
    if ty == idl.TY_BOOLEAN and type(value) is not bool:
      raise Exception(f'expected bool but got {value}')
    if ty == idl.TY_DOUBLE and type(value) != float:
      raise Exception(f'expected float but got {value}')
    if (ty == idl.TY_LONG or
          ty == idl.TY_UNSIGNED_LONG) and type(value) is not int:
      raise Exception(f'expected int but got {value}')
    if ty == idl.TY_STRING and type(value) is not str:
      raise Exception(f'expected string but got {value}')
    if ty == idl.TY_TYPE and value not in self.tys.interfaces:
      raise Exception(f'unknown type {value}')
    # Pass the check

  def interface(self, i, node):
    if i.name != node['type']:
      raise Exception(f'expected {i.name} but got {node["type"]}')
    # Get the names the interface expects
    expected = set()
    j = i
    while j:
      expected.update({attr.name for attr in j.attrs})
      j = j.resolved_extends
    # AST JSON encodes 'type' for each AST node, even ones which
    # aren't polymorphic such as AssertedBlockScope.
    expected.add('type')
    # Get the names the node has
    actual = set(node.keys())
    extra = actual - expected
    if extra:
      raise Exception(f'unexpected properties {extra} in instance of {i.name} {node}')
    missing = expected - actual
    if missing:
      raise Exception(f'missing properties {missing} in instance of {i.name} {node}')
    j = i
    while j:
      for attr in j.attrs:
        value = node[attr.name]
        self.check_any(attr.resolved_ty, value)
      j = j.resolved_extends


if __name__ == '__main__':
  doctest.testmod()
