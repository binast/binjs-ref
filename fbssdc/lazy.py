#!/usr/bin/env python3

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Replaces lazy members with placeholders.

import doctest

import ast
import idl

class LazyMemberReplacer(object):
  def __init__(self, types):
    self.types = types

  def replace(self, ty, n):
    if type(n) is dict:
      return self.replace_iface(self.types.interfaces[n['type']], n)
    if type(n) is list:
      assert type(ty) is idl.TyFrozenArray, ty
      return list(map(lambda m: self.replace(ty.element_ty, m), n))
    return n

  def replace_iface(self, ty, n):
    # TODO: Should stop passing types around and just look up the type here.
    # If a lazy field had a compound type, passing this type isn't going to
    # work anyway.
    m = {'type': n['type']}
    for attr in ty.attrs:
      if attr.lazy:
        m[attr.name] = self.replacement(ty, attr, n)
      else:
        m[attr.name] = self.replace(attr.resolved_ty, n[attr.name])
    return m


class LazyMemberExtractor(LazyMemberReplacer):
  '''Collects lazy field values, replacing them with LazyIOUs.

  This does not recursively process the extracted lazy members; use
  another extractor to do that.

  >>> types = idl.parse_es6_idl()
  >>> ty_script = types.interfaces['Script']
  >>> a = ast.load_test_ast('y5R7cnYctJv.js.dump')
  >>> import tycheck, opt
  >>> tycheck.FloatFixer(types).rewrite(ty_script, a)
  >>> a = opt.optimize(a)
  lazified 1 functions
  >>> ex = LazyMemberExtractor(types)
  >>> _ = ex.replace(ty_script, a)
  >>> len(ex.lazies)
  1

  '''
  def __init__(self, types):
    super().__init__(types)
    self.lazies = []

  def replacement(self, ty, attr, n):
    val = n[attr.name]
    index = len(self.lazies)
    self.lazies.append((ty, attr, val))
    return ast.LazyIOU()


class LazyMemberRestorer(LazyMemberReplacer):
  '''
  >>> types = idl.parse_es6_idl()
  >>> ty_script = types.interfaces['Script']
  >>> a = ast.load_test_ast('y5R7cnYctJv.js.dump')
  >>> import tycheck, opt
  >>> tycheck.FloatFixer(types).rewrite(ty_script, a)
  >>> a = opt.optimize(a)
  lazified 1 functions
  >>> ex = LazyMemberExtractor(types)
  >>> replacement = ex.replace(types.interfaces['Script'], a)
  >>> def get_lazy(ty, attr, index):
  ...   assert ex.lazies[index][0] is ty
  ...   assert ex.lazies[index][1] is attr
  ...   return ex.lazies[index][2]
  >>> restorer = LazyMemberRestorer(types, get_lazy)
  >>> b = restorer.replace(types.interfaces['Script'], replacement)
  >>> import json
  >>> assert json.dumps(a) == json.dumps(b)
  '''
  def __init__(self, types, lazy_materializer):
    super().__init__(types)
    self.lazy_materializer = lazy_materializer
    self.i = 0

  def replacement(self, ty, attr, n):
    self.i += 1
    return self.lazy_materializer(ty, attr, self.i - 1)


if __name__ == '__main__':
  doctest.testmod()
