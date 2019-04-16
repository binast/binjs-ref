#!/usr/bin/env python3

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import collections
import doctest
import io
import json
import struct

import ast
import bits
import idl
import model

# To write probability tables into the file, we need to discover the
# tables we need in a consistent order. This does that by following
# the IDL.
class ModelExplorer(object):
  def __init__(self, types):
    self.types = types
    self.queue = []
    self.visited = set()
    self.tables = {}
    self.nonempty_arrays = set()

  def roam(self, root_ty):
    self.enqueue_fields(root_ty)
    while self.queue:
      #for i, item in enumerate(self.queue):
      #  print(i, item)
      k = self.queue.pop()
      self.processed(k, self.process(k))

  def enqueue_sym(self, sym):
    if type(sym) is idl.TyInterface:
      self.enqueue_fields(sym)
    # Primitives, arrays, none, etc. are keys for fields' probability
    # tables; we don't need tables for them specifically.

  def enqueue_fields(self, struct_ty):
    for attr in struct_ty.attrs:
      self.enqueue((struct_ty, attr.name))

  def enqueue(self, k):
    k = model.map_model_key(self.types, k)
    if k in self.visited:
      return
    assert k[1] != 'list-length'
    ty = k[0].type_of(k[1])
    if type(ty) is idl.TyFrozenArray:
      length_k = (ty, 'list-length')
      if length_k not in self.visited:
        self.visited.add(length_k)
        # We eagerly get this to see if we need to include the field
        #print(f'processing length model immediately for {k}')
        length_m = self.process(length_k)
        self.processed(length_k, length_m)
      else:
        #print(f'already have length model for {k}')
        length_m = self.tables[length_k]
      if sum(length_m.symbol_to_code.keys()) == 0:
        # All of these are empty.
        #print(f'all symbols empty for {k}')
        return
      ty = ty.element_ty
    self.visited.add(k)
    if type(ty) is idl.TyInterface:
      self.processed(k, self.trivial(k, ty))
    else:
      self.queue.append(k)

  def processed(self, k, m):
    assert k not in self.tables, 'FIXME: may need to loosen this to allow equal trivial models'
    self.tables[k] = m
    if k[1] == 'list-length':
      return
    # Now we only explore things with non-zero probability
    for sym in m.in_use_syms():
      self.enqueue_sym(sym)

  def trivial(self, k, ty):
    return model.TrivialModel(ty)

  def process(self, k):
    '''Reads (writes) a model and returns it.'''
    assert False, 'implement process in a subtype'


class TestModelExplorer(ModelExplorer):
  '''
  >>> types = idl.parse_es6_idl()
  >>> ty_script = types.interfaces['Script']
  >>> tree = ast.load_test_ast('y5R7cnYctJv.js.dump')
  >>> target = model.model_tree(types, ty_script, tree)
  >>> explorer = TestModelExplorer(types, target)
  >>> explorer.roam(ty_script)
  >>> for k in set(explorer.tables.keys()) - set(target.keys()): # extra models
  ...   assert type(explorer.tables[k]) in [model.TrivialModel, model.UnreachableModel]
  >>> for k in set(target.keys()) - set(explorer.tables.keys()): # missing models
  ...   print(k)
  '''
  def __init__(self, types, target):
    super().__init__(types)
    self.target = target

  def process(self, k):
    if k[1] != 'list-length':
      ty = k[0].type_of(k[1])
      if type(ty) is idl.TyFrozenArray and k not in self.target:
        return model.UnreachableModel()
    assert k in self.target, k
    return self.target[k]


class ModelWriter(ModelExplorer):
  def __init__(self, types, dictionary, out):
    super().__init__(types)
    self.encoder = ModelEncoder(dictionary, out)

  def write(self, root_ty, target):
    self.target = target
    self.roam(root_ty)

  def process(self, k):
    if k[1] == 'list-length':
      ty = idl.TY_LONG
      m = self.target[k]
    else:
      ty = k[0].type_of(k[1])
      if type(ty) is idl.TyFrozenArray and k not in self.target:
        m = model.UnreachableModel()
      else:
        m = self.target[k]
    assert type(m) is not model.TrivialModel, 'should be handled by trivial'
    self.encoder.encode_model(ty, m)
    return m


class ModelReader(ModelExplorer):
  '''
  >>> types = idl.parse_es6_idl()
  >>> ty_script = types.interfaces['Script']
  >>> tree = ast.load_test_ast('y5R7cnYctJv.js.dump')
  >>> #tree = ast.load_test_ast('three.min.js.dump')
  >>> target = model.model_tree(types, ty_script, tree)
  >>> import strings; string_dict = strings.prepare_dict(types, [(ty_script, tree)])
  >>> buf = io.BytesIO()
  >>> writer = ModelWriter(types, string_dict, buf)
  >>> writer.write(ty_script, target)
  >>> buf.tell()
  1408
  >>> buf.seek(0)
  0
  >>> reader = ModelReader(types, string_dict, buf)
  >>> read = reader.read(ty_script)
  >>> buf.tell()
  1408
  >>> for k in set(read.keys()) - set(target.keys()): # extra models
  ...   assert type(read[k]) in [model.TrivialModel, model.UnreachableModel]
  >>> len(target)
  96
  >>> len(read)
  99
  >>> for k in set(target.keys()) - set(read.keys()): # missing models
  ...   print(k)
  >>> for k in target.keys():
  ...   m = target[k]
  ...   n = read[k]
  ...   assert set(m.in_use_syms()) == set(n.in_use_syms()), f'{k} {set(m.in_use_syms())} {set(n.in_use_syms())}'
  ...   for sym in m.in_use_syms():
  ...     assert m.symbol_to_code[sym] == n.symbol_to_code[sym]
  '''
  def __init__(self, types, dictionary, inp):
    super().__init__(types)
    self.decoder = ModelDecoder(dictionary, inp)

  def read(self, root_ty):
    self.roam(root_ty)
    return self.tables

  def process(self, k):
    if k[1] == 'list-length':
      ty = idl.TY_LONG
    else:
      ty = k[0].type_of(k[1])
    return self.decoder.decode_model(ty)


# A naive model encoder which just dumps bytes for Brotli to chew on.
# tables = model.model_tree(types, ty_script, tree)
# buf = io.BytesIO(); e = encode.ModelEncoder(dictionary, buf); e.encode_models(tables); buf.tell()
class ModelEncoder(object):
  def __init__(self, dictionary, out):
    self.out = out
    self.dictionary = {s: i for i, s in enumerate(dictionary)}

  def encode_models(self, models):
    for k, v in models.items():
      if k[1] == 'list-length':
        ty = idl.TY_LONG
      else:
        ty = k[0].type_of(k[1])
      #print(k, ty, type(v), 'size', len(v.code_to_symbol))
      self.encode_model(ty, v)

  def encode_model(self, ty, m):
    if type(m) is model.TrivialModel:
      return
    # print('E', ty, m, self.out.tell())
    # if type(m) is model.UnreachableModel:
    #   print('***unreachable model***')
    # elif len(m.code_to_symbol) < 30:
    #   print(m.code_to_symbol)

    if type(m) is model.UnreachableModel:
      self.out.write(b'\x02')
    elif len(m.symbol_to_code) is 1:
      #print('single')
      self.out.write(b'\x00')
      sym = list(m.code_to_symbol.values())[0]
      if type(m) is model.ExplicitSymbolModel:
        assert not model.is_indexed_type(ty)
        self.encode_symbol(ty, sym)
        return
      assert type(m) is model.IndexedSymbolModel
      assert model.is_indexed_type(ty)
      self.encode_index(m.index[sym])
    elif type(m) is model.ExplicitSymbolModel:
      #print('multiple, explicit')
      assert not model.is_indexed_type(ty)
      # These are not enumerable, we just need to dump the symbols and their lengths.
      length_sym = list(sorted([(code[1], type(sym) is idl.TyNone and 1 or 2, sym) for code, sym in m.code_to_symbol.items()]))
      self.out.write(b'\x01')
      bits.write_varint(self.out, len(length_sym))
      for length, _, _ in length_sym:
        # TODO: In practice lengths are < 32 and we could pack these, etc.
        assert length < 256
        self.out.write(length.to_bytes(1, byteorder='big'))
      for _, _, sym in length_sym:
        self.encode_symbol(ty, sym)
    elif type(m) is model.IndexedSymbolModel:
      #print('multiple, indexed')
      self.out.write(b'\x01')
      assert type(m) is model.IndexedSymbolModel
      assert model.is_indexed_type(ty)
      # These are enumerable
      for i, sym in enumerate(m.symbols):
        code_length = m.symbol_to_code.get(sym)
        length = code_length and code_length[1] or 0
        assert length < 256
        self.out.write(length.to_bytes(1, byteorder='big'))
    else:
      assert False, 'unreachable'

  def encode_index(self, i):
    bits.write_varint(self.out, i)

  def encode_symbol(self, ty, sym):
    if ty == idl.TY_STRING:
      assert type(sym) is str
      bits.write_varint(self.out, self.dictionary[sym])
    elif ty == idl.TY_DOUBLE:
      self.out.write(struct.pack('!d', sym))
    elif ty == idl.TY_LONG:
      self.out.write(struct.pack('!l', sym))
    elif ty == idl.TY_UNSIGNED_LONG:
      self.out.write(struct.pack('!L', sym))
    elif ty == idl.TY_BOOLEAN:
      self.out.write(int(sym).to_bytes(1, byteorder='big'))
    elif type(ty) is idl.TyFrozenArray:
      self.encode_symbol(ty.element_ty, sym)
    elif type(ty) is idl.Alt and ty.ty_set == set([idl.TyNone(), idl.TY_STRING]):
      if sym == idl.TyNone():
        bits.write_varint(self.out, 0)
      else:
        bits.write_varint(self.out, self.dictionary[sym] + 1)
    else:
      assert False, f'unreachable (type should be indexed?) {ty}: {sym}'


class ModelDecoder(object):
  '''
  Encode a multiple long model

  >>> buf = io.BytesIO()
  >>> encoder = ModelEncoder([], buf)
  >>> encoder.encode_model(idl.TY_LONG, model.ExplicitSymbolModel().from_values([0, 0, 1, 2]))
  >>> buf.tell()
  17
  >>> buf.seek(0)
  0
  >>> decoder = ModelDecoder([], buf)
  >>> m = decoder.decode_model(idl.TY_LONG)
  >>> buf.tell()
  17
  >>> type(m)
  <class 'model.ExplicitSymbolModel'>
  >>> m.code_to_symbol
  {(0, 1): 0, (2, 2): 1, (3, 2): 2}

  Encode a single indexed model

  >>> tys = [idl.TyInterface('A', None, []), idl.TyInterface('B', None, [])]
  >>> ty = idl.Alt(tys)
  >>> buf.seek(0)
  0
  >>> encoder.encode_model(ty, model.IndexedSymbolModel(tys).from_values([tys[1]]))
  >>> buf.tell()
  2
  >>> buf.seek(0)
  0
  >>> m = decoder.decode_model(ty)
  >>> buf.tell()
  2
  >>> type(m)
  <class 'model.IndexedSymbolModel'>
  >>> m.code_to_symbol
  {(0, 0): interface B...}

  Encode a bunch of interfaces

  >>> ty = idl.Alt([idl.TyNone(), idl.TyInterface('Foo', None, [])])
  >>> buf.seek(0)
  0
  >>> encoder.encode_model(ty, model.IndexedSymbolModel(ty.tys).from_values([ty.tys[0], ty.tys[1]]))
  >>> buf.tell()
  3
  >>> buf.seek(0)
  0
  >>> m = decoder.decode_model(ty)
  >>> buf.tell()
  3
  >>> type(m)
  <class 'model.IndexedSymbolModel'>
  >>> m.code_to_symbol
  {(0, 1): none, (1, 1): interface Foo...}

  Encode a FrozenArray<A or B>

  >>> tys = [idl.TyInterface('A', None, []), idl.TyInterface('B', None, [])]
  >>> ty = idl.TyFrozenArray(idl.Alt(tys))
  >>> buf.seek(0)
  0
  >>> encoder.encode_model(ty, model.IndexedSymbolModel(tys).from_values([tys[1], tys[0]]))
  >>> buf.tell()
  3
  >>> buf.seek(0)
  0
  >>> m = decoder.decode_model(ty)
  >>> buf.tell()
  3
  >>> type(m)
  <class 'model.IndexedSymbolModel'>
  >>> m.code_to_symbol
  {(0, 1): interface A..., (1, 1): interface B...}

  Encode an unreachable model

  >>> buf.seek(0)
  0
  >>> buf.truncate()
  0
  >>> ty = idl.TyFrozenArray(idl.TyInterface('A', None, []))
  >>> encoder.encode_model(ty, model.UnreachableModel())
  >>> buf.tell()
  1
  >>> bytes(buf.getbuffer())
  b'\\x02'
  >>> buf.seek(0)
  0
  >>> m = decoder.decode_model(ty)
  >>> type(m)
  <class 'model.UnreachableModel'>
  '''
  def __init__(self, dictionary, inp):
    self.inp = inp
    self.dictionary = dictionary

  def decode_model(self, ty):
    #print('D', ty, self.inp.tell())
    m = self._decode_model(ty)
    # if type(m) is model.UnreachableModel:
    #   print('***unreachable model***')
    # elif len(m.code_to_symbol) < 30:
    #   print(m.code_to_symbol)
    return m

  def _decode_model(self, ty):
    kind = self.inp.read(1)[0]
    #print('_decode_model', kind)
    if kind == 0:
      #print('single')
      if model.is_indexed_type(ty):
        syms = model.symbols_for_indexed_type(ty)
        sym = self.decode_index(ty)
        return model.IndexedSymbolModel(syms).from_values([sym])
      else:
        sym = self.decode_symbol(ty)
        return model.ExplicitSymbolModel().from_values([sym])
    elif kind == 1:
      if not model.is_indexed_type(ty):
        #print('multiple, explicit')
        # These are not enumerable, we just need to suck in the symbols and their lengths.
        num_syms = bits.read_varint(self.inp)
        lengths = []
        for _ in range(num_syms):
          lengths.append(self.inp.read(1)[0])
        syms = []
        for _ in range(num_syms):
          syms.append(self.decode_symbol(ty))
        length_symbol = list(zip(lengths, [0] * len(syms), syms))
        m = model.ExplicitSymbolModel()
        m.code_to_symbol, m.symbol_to_code = model.huffman_assign_order(length_symbol)
        return m
      else:
        #print('multiple, indexed')
        # These are enumerable
        length_symbol = []
        syms = model.symbols_for_indexed_type(ty)
        for i, sym in enumerate(syms):
          length = self.inp.read(1)[0]
          length_symbol.append((length, type(sym) is idl.TyNone and 1 or 2, sym))
        m = model.IndexedSymbolModel(syms)
        #print(length_symbol)
        m.code_to_symbol, m.symbol_to_code = model.huffman_assign_order(list(sorted(length_symbol)))
        return m
    elif kind == 2:
      assert type(ty) is idl.TyFrozenArray
      return model.UnreachableModel()

  def decode_index(self, ty):
    i = bits.read_varint(self.inp)
    return model.symbols_for_indexed_type(ty)[i]

  def decode_symbol(self, ty):
    if ty == idl.TY_STRING:
      return self.dictionary[bits.read_varint(self.inp)]
    elif ty == idl.TY_DOUBLE:
      return struct.unpack('!d', self.inp.read(struct.calcsize('!d')))[0]
    elif ty == idl.TY_LONG:
      return struct.unpack('!l', self.inp.read(struct.calcsize('!l')))[0]
    elif ty == idl.TY_UNSIGNED_LONG:
      return struct.unpack('!L', self.inp.read(struct.calcsize('!L')))[0]
    elif ty == idl.TY_BOOLEAN:
      return bool(self.inp.read(1)[0])
    elif type(ty) is idl.TyFrozenArray:
      self.decode_symbol(ty.element_ty)
    elif type(ty) is idl.Alt and ty.ty_set == set([idl.TyNone(), idl.TY_STRING]):
      n = bits.read_varint(self.inp)
      if n == 0:
        return idl.TyNone()
      else:
        return self.dictionary[n-1]
    else:
      assert False, f'unreachable (type should be indexed?) {ty}: {sym}'


class TreeEncoder(ast.AstVisitor):
  def __init__(self, types, tables, out):
    super().__init__(types)
    self.tables = tables
    self.out = bits.BitsIO(out)
    self.field = []
    self.log = []

  def visit_list(self, ty, xs):
    # TODO: These magic keys need to be kept in sync with model.TreeSharder
    k = (ty, 'list-length')
    self._write(k, len(xs))
    super().visit_list(ty, xs)

  def visit_struct(self, declared_ty, actual_ty, obj):
    if len(self.field) == 0:
      # This is the root struct, so we don't need to output a tag.
      # FIXME: When the serialized root is not a struct, insinuate a starting model for it.
      pass
    else:
      self._write(self.field[-1], actual_ty)
    super().visit_struct(declared_ty, actual_ty, obj)

  def visit_field(self, struct_ty, obj, i, attr):
    if attr.lazy:
      # Halt the walk here; this will appear in the lazy stream.
      # FIXME: When the ES6 IDL makes these types non-trivial, we may want to
      # write the type tag here and decode a specific type later.
      assert type(attr.resolved_ty) is idl.TyInterface, 'NYI'
      return
    self.field.append(model.map_model_key(self.types, (struct_ty, attr.name)))
    super().visit_field(struct_ty, obj, i, attr)
    self.field.pop()

  def visit_primitive(self, ty, value):
    if ty is idl.TY_TYPE:
      return
    if value is None:
      value = idl.TyNone()
    self._write(self.field[-1], value)

  def _write(self, k, value):
    effective_key = model.map_model_key(self.types, k)
    m = self.tables[effective_key]
    code, length = m.symbol_to_code[value]
    #print(f'E{len(self.log)}', k, f'{code:b}', length, list(m.symbol_to_code.items()), value)
    self.log.append((effective_key, value))
    for i in range(length):
      self.out.write(1, (code >> (length - i - 1)) & 1)

debug = {}

def debug_dump():
   print('encoder log length:', len(debug['encoder'].log))
   print('decoder log length:', len(debug['decoder'].log))
   for i, (w, r) in enumerate(zip(debug['encoder'].log, debug['decoder'].log)):
     print(i, w, r)
     if w != r:
       print('inconsistent'); break


def encode(types, tables, buf, ty, tree):
  encoder = TreeEncoder(types, tables, buf)
  debug['encoder'] = encoder
  encoder.visit(ty, tree)
  encoder.out.flush()


class TreeDecoder(object):
  def __init__(self, types, tables, inp):
    self.types = types
    self.tables = tables
    self.inp = bits.BitsIO(inp)
    self.field = []
    self.log = []

  def decode(self, ty):
    assert type(ty) is idl.TyInterface
    return self._struct(ty)

  def _struct(self, ty):
    if ty == idl.TyNone():
      return None
    obj = {'type': ty.name}
    for attr in ty.attrs:
      if attr.lazy:
        obj[attr.name] = ast.LazyIOU()
      else:
        obj[attr.name] = self._field(ty, attr.name, attr.resolved_ty)
    return obj

  # This is needlessly tricky, but we reenter here for
  # list items with the attr_ty flipped to the element
  # type.
  def _field(self, struct_ty, attr_name, attr_ty):
    k = (struct_ty, attr_name)
    if type(attr_ty) in [idl.TyPrimitive, idl.TyEnum]:
      return self._primitive(k)
    if type(attr_ty) is idl.TyFrozenArray:
      return self._list(struct_ty, attr_name, attr_ty)
    child = self._read(k)
    # FIXME: As more primitive? appears in IDL expand this set.
    if type(attr_ty) is idl.Alt and attr_ty.ty_set == set([idl.TyNone(), idl.TY_STRING]):
      assert type(child) in [idl.TyNone, str], child
      if child == idl.TyNone():
        return None
      else:
        return child
    # It's a type-tagged thing.
    # TODO: Check that monomorphic FrozenArrays aren't emitting type tags.
    assert isinstance(child, idl.Ty), f'{struct_ty} {attr_name} {attr_ty}: {child}'
    return self._struct(child)

  def _primitive(self, k):
    return self._read(k)

  def _list(self, struct_ty, attr_name, attr_ty):
    n_items = self._read((attr_ty, 'list-length'))
    result = []
    for _ in range(n_items):
      result.append(self._field(struct_ty, attr_name, attr_ty.element_ty))
    return result

  def _read(self, key):
    effective_key = model.map_model_key(self.types, key)
    m = self.tables[effective_key]
    n_bits = 0
    code = 0
    while True:
      k = (code, n_bits)
      #print(f'D{len(self.log)}', key, f'{code:b}', k, list(m.code_to_symbol.items()))
      s = m.code_to_symbol.get(k)
      #print('symbol?', s)
      if s != None:
        self.log.append((effective_key, s))
        return s
      code <<= 1
      code |= self.inp.read(1)
      n_bits += 1

def decode(types, tables, ty, inp):
  '''
  >>> types = idl.parse_es6_idl()
  >>> ty_script = types.interfaces['Script']
  >>> #tree = ast.load_test_ast('y5R7cnYctJv.js.dump')
  >>> tree = ast.load_test_ast('three.min.js.dump')
  >>> import tycheck; tycheck.FloatFixer(types).rewrite(ty_script, tree)
  >>> import opt; tree = opt.optimize(tree) # doctest:+ELLIPSIS
  lazified ... functions
  >>> tables = model.model_tree(types, ty_script, tree)
  >>> buf = io.BytesIO()
  >>> encode(types, tables, buf, ty_script, tree)
  >>> debug['encoder'] is not None
  True
  >>> buf.tell()
  27586
  >>> buf.seek(0)
  0
  >>> try:
  ...   ki = decode(types, tables, ty_script, buf)
  ... except Exception:
  ...   debug_dump()
  ...   raise

  We can't directly compare JSON, since encode/decode skips lazy fields,
  so we have to do a similar transform ourselves:

  >>> import lazy
  >>> lazy_tree = lazy.LazyMemberExtractor(types).replace(ty_script, tree)
  >>> restorer = lazy.LazyMemberRestorer(types, lambda ty, attr, index: attr.name)
  >>> lazy_tree = restorer.replace(ty_script, lazy_tree)
  >>> ki = restorer.replace(ty_script, ki)
  >>> json.dumps(lazy_tree) == json.dumps(ki)
  True
  >>> for i, (s_in, s_out) in enumerate(zip(json.dumps(lazy_tree, indent=2).split('\\n'), json.dumps(ki, indent=2).split('\\n'))):
  ...   if s_in != s_out:
  ...     print('!!!', s_out)
  ...     break
  '''
  decoder = TreeDecoder(types, tables, inp)
  debug['decoder'] = decoder
  return decoder.decode(ty)


if __name__ == '__main__':
  doctest.testmod()
