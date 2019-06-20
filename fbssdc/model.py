#!/usr/bin/env python3

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

'''
Splits a syntax tree by type and models the data within each type.
'''

import collections
import doctest
import heapq
import io
import itertools
import math
import operator

import ast
import idl

# TODO: This is a SWAG. Maybe do this iteratively and refine the estimate.
BLOCK_SWITCH_COST_ESTIMATE = 4

class Block(object):
  def __init__(self, universe_size, histo, start, end):
    self.histo = histo
    model = huffman(histo)
    self.cost = sum(map(lambda p: p[1] * model[p[0]], histo.items()))
    # Models are free, for now
    # TODO: Model cost is complicated--there's no notion of reuse
    #self.cost += huffman_model_cost(model, universe_size)
    self.cost += math.log2(end - start) # Distance cost
    self.cost += BLOCK_SWITCH_COST_ESTIMATE
    self.pred = self.succ = None
    self.cost_delta = 0
    self.start = start
    self.end = end

  def __repr__(self):
    return f'Block({self.start}-{self.end}, {self.histo})'

  def __lt__(self, other):
    return self.cost_delta < other.cost_delta

# TODO: Rescale combined histograms
def combine_histo(l, r):
  lr = collections.defaultdict(int)
  for k, v in itertools.chain(l.items(), r.items()):
    lr[k] += v
  return lr

class MergeBlock(Block):
  def __init__(self, universe_size, left, right):
    '''Creates a proposal to merge two blocks.

    The caller must wire up the pred and succ properties of this block.'''
    super().__init__(universe_size, combine_histo(left.histo, right.histo), left.start, right.end)
    assert left.succ is right
    assert right.pred is left
    self.left = left
    self.right = right
    self.cost_delta = self.cost - (left.cost + right.cost)
    self.committed = False # This is just a proposal

  def __repr__(self):
    return f'MergeBlock({self.left},{self.right},{self.cost_delta})'

  def accept(self, universe_size, heap):
    # This block is furniture now, we can't "apply" it again
    self.cost_delta = 0
    self.committed = True

    # There's three things going on here:

    # 1. Remove the things which merged with the old left and right block.
    if self.pred:
      old_pred = self.pred.pred
      heap.remove(self.pred)
    else:
      old_pred = None
    if self.succ:
      old_succ = self.succ.succ
      heap.remove(self.succ)
    else:
      old_succ = None

    # 2. Wire up the committed block's predecessor, successor pointers, for producing the text again.
    self.pred = self.left.pred
    if self.pred:
      self.pred.succ = self
    self.succ = self.right.succ
    if self.succ:
      self.succ.pred = self

    # Will be reheapified later.
    heap.remove(self.left)
    heap.remove(self.right)
    heap.append(self) # We were popped before accept

    # 3. Add things which merge with the new block
    if self.pred:
      a = MergeBlock(universe_size, self.pred, self)
      a.pred = old_pred
      if old_pred:
        old_pred.succ = a
      heap.append(a)
    else:
      a = None
    if self.succ:
      c = MergeBlock(universe_size, self, self.succ)
      c.succ = old_succ
      if old_succ:
        old_succ.pred = c
      heap.append(c)
    else:
      c = None

    if a and c:
      a.succ = c
      c.pred = a

    # TODO: This would be cheaper if replacement was in-place and bubble-up
    heapq.heapify(heap)


def split_consistency_check(heap):
  for val in heap:
    #print(val)
    #print('pred:', val.pred)
    #print('succ:', val.succ)
    assert val.pred is None or val.pred.succ is val
    assert val.succ is None or val.succ.pred is val
    if type(val) is MergeBlock:
      #print('left:', val.left)
      #print('right:', val.right)
      assert (val.left in heap) != val.committed
      assert (val.right in heap) != val.committed

def split(universe_size, values):
  '''Breaks a sequence of values up into parts encoded with different models.

  >>> u = Block(26, {'a': 1}, 0, 1)
  >>> u.cost > 0
  True
  >>> v = Block(26, {'a': 1}, 1, 2)
  >>> u.succ = v
  >>> v.pred = u
  >>> w = MergeBlock(26, u, v)
  >>> w.start
  0
  >>> w.end
  2
  >>> w.cost_delta < 0
  True
  >>> vals = list('aaaaaa')
  >>> c = split(26, vals)
  >>> while c:
  ...   print(c.histo)
  ...   c = c.succ
  defaultdict(<class 'int'>, {'a': 6})
  >>> vals = list('a man aa plaaan a canal panama')
  >>> c = split(26, vals)
  >>> cost = 0
  >>> while c:
  ...   cost += c.cost
  ...   print(c.start, c.end, dict(c.histo))
  ...   c = c.succ
  0 8 {'a': 4, ' ': 2, 'm': 1, 'n': 1}
  8 11 {' ': 1, 'p': 1, 'l': 1}
  11 18 {'a': 4, 'n': 1, ' ': 2}
  18 19 {'c': 1}
  19 23 {'a': 2, 'n': 1, 'l': 1}
  23 25 {' ': 1, 'p': 1}
  25 30 {'a': 3, 'n': 1, 'm': 1}
  >>> print(int(cost))
  84
  >>> hist = collections.defaultdict(int)
  >>> for v in vals: hist[v] += 1
  >>> huff = huffman(hist)
  >>> print(int(huffman_encode_cost(huff, 26, vals)))
  115
  '''
  # Make a sequence of leaf blocks and proposals for merging them
  blocks = [Block(universe_size, {sym: 1}, i, i+1) for i, sym in enumerate(values)]
  proposed_merges = []
  for u, v in zip(blocks, blocks[1:]):
    u.succ = v; v.pred = u
    proposed_merges.append(MergeBlock(universe_size, u, v))

  for m, n in zip(proposed_merges, proposed_merges[1:]):
    m.succ = n
    n.pred = m

  # TODO: It's lazy to have these blocks here, keep track of them separately
  proposed_merges += blocks
  split_consistency_check(proposed_merges)
  heapq.heapify(proposed_merges)

  while True:
    #print('-' * 80)
    #for i in proposed_merges:
    #  print(i)
    candidate = heapq.heappop(proposed_merges)
    #print(f'candidate={candidate}')
    if candidate.cost_delta < 0:
      candidate.accept(universe_size, proposed_merges)
      split_consistency_check(proposed_merges)
    else:
      break
  if type(candidate) is MergeBlock and not candidate.committed:
    candidate = candidate.left
  while candidate.pred:
    candidate = candidate.pred
  return candidate


def huffman_encode_cost(table, universe_size, values):
  '''Given a table of code lengths, the cost of encoding it and values with it.

  >>> table = huffman({'a': 2, 'b': 1, 'c': 0})
  >>> huffman_encode_cost(table, 3, ['a', 'b'])
  5.0
  '''
  return huffman_model_cost(table, universe_size) + sum(map(lambda v: table[v], values))


def huffman_model_cost(table, universe_size):
  '''Estimates the cost of encoding a Huffman table.

  >>> huffman_model_cost({'a': 1}, 1024)
  10.0
  >>> huffman_model_cost({'a': 1, 'b': 1}, 1024)
  20.0
  >>> huffman_model_cost({'a': 1, 'b': 2, 'c': 2}, 1024)
  32.0
  '''
  if len(table) == 1:
    # Just encode the symbol
    return math.log2(universe_size)
  if len(table) <= universe_size // 2:
    # Say we'd encode each of the members and their code length
    return math.log2(universe_size) * len(table) + sum(map(math.log2, table.values()))
  # We'd just encode the lengths and expect 0s to compress well.
  return sum(map(math.log2, table.values()))


def huffman(histogram):
  '''Builds a Huffman tree and returns a dictionary of key to code length.

  >>> huffman({'a': 47})
  {'a': 0}
  >>> huffman({'a': 20, 'b': 3, 'c': 1})
  {'c': 2, 'b': 2, 'a': 1}
  '''
  # Make a heap of (count, n, key); wrap keys in one-element tuples
  # to distinguish them as leaves.
  #
  # n is a unique number which means we don't compare keys during heap
  # operations. Without n, this comparison would happen if two symbols
  # had the same count.
  h = list(map(lambda p: (p[1][1], p[0], (p[1][0],)), enumerate(histogram.items())))
  heapq.heapify(h)
  # Merge items to build the tree
  while len(h) > 1:
    fst = heapq.heappop(h)
    snd = heapq.heappop(h)
    heapq.heappush(h, (fst[0] + snd[0], fst[1], (fst[2], snd[2])))
  table = {}
  def visit(cost, node):
    if len(node) == 1:
      table[node[0]] = cost
    else:
      visit(cost + 1, node[0])
      visit(cost + 1, node[1])
  visit(0, h[0][2])
  return table

def universe_size_for_key(k, vs):
  ty, field_name = k
  if ty is idl.TY_TYPE:
    # TY_TYPE is used to mark the root; we expect the decoder to know that type
    return 1
  if field_name == 'list-length':
    # This, and string/double/etc. are imperfect; they assume a dictionary we're indexing into.
    return len(set(vs))
  attr_ty = ty.type_of(field_name)
  return universe_size_for_type(attr_ty, vs)

def universe_size_for_type(ty, vs):
  if type(ty) in [idl.TyInterface, idl.TyNone, idl.TyFrozenArray]:
    return 1
  if type(ty) is idl.TyEnum:
    return len(ty.values)
  if type(ty) is idl.TyRef:
    assert False, 'unreachable'
  if type(ty) is idl.Alt:
    return sum(map(lambda t: universe_size_for_type(t, []), ty.tys))
  if ty is idl.TY_BOOLEAN:
    return 2
  if ty in [idl.TY_DOUBLE, idl.TY_LONG, idl.TY_STRING, idl.TY_UNSIGNED_LONG]:
    return len(set(vs))

# To dump stats for a file:
#
# types = idl.parse_es6_idl()
# tree = ast.load_ast(...)
# shard = model.TreeSharder(types); shard.visit(types.interfaces['Script'], tree); model.total_stats(shard.group)
def total_stats(groups):
  by_size = []
  for k, v in groups.items():
    by_size.append(group_stats(v, universe_size_for_key(k, v)) + k)
  by_size = list(sorted(by_size, key=lambda p: p[1]))
  total_size_bits = sum(map(operator.itemgetter(0), by_size))
  total_size_huff_bits = sum(map(operator.itemgetter(1), by_size))
  for _, _, message, ty, name in by_size:
    print(str(ty), name)
    print(message)
    print()
  print('-' * 10)
  print(len(groups), 'groups')
  print(total_size_bits / 8, 'bytes')
  print('huffman:', total_size_huff_bits / 8, 'bytes')
  print('note: does not include string data')

def entropy(values):
  '''Computes entropy, in bits, of a set of frequency values.
  >>> entropy([100])
  0.0
  >>> entropy([1, 1])
  1.0
  '''
  total = sum(values)
  entropy = 0.0
  for value in values:
    p = value / total
    entropy -= p * math.log2(p)
  return entropy

def group_stats(group, universe_size):
  hist = collections.defaultdict(int)
  for item in group:
    hist[item] += 1
  msg = []
  msg.append(f'{len(hist.keys())} symbols')
  size = len(group)
  msg.append(f'{size} instances')
  entropy_bits = entropy(list(hist.values()))
  msg.append(f'{entropy_bits} bits per symbol')
  msg.append(f'~= {entropy_bits * size / 8} bytes')

  huff_table = huffman(hist)
  huff_cost = huffman_encode_cost(huff_table, universe_size, group)
  msg.append(f'Huffman {huff_cost / len(group)} bits per symbol')
  msg.append(f'Huffman encoding ~= {huff_cost / 8} bytes')

  return size * entropy_bits, huff_cost, '\n'.join(msg)


# This collapses some fields with similar function to save model space.
def map_model_key(types, k):
  if k[0] == types.interfaces['StaticMemberAssignmentTarget'] and k[1] == 'property':
    return (types.interfaces['StaticMemberExpression'], 'property')
  return k


class TreeSharder(ast.AstVisitor):
  '''
  Traverses a tree, grouping items by declared type.

  >>> types = idl.parse_es6_idl()
  >>> tree = ast.load_test_ast('y5R7cnYctJv.js.dump')
  >>> sharder = TreeSharder(types)
  >>> sharder.visit(types.interfaces['Script'], tree)
  >>> k = (types.interfaces['AssertedDeclaredName'], 'isCaptured')
  >>> len(sharder.group[k])
  31
  >>> sharder.group[k][0:10]
  [True, False, False, False, False, False, False, False, False, False]
  '''

  def __init__(self, types):
    super().__init__(types)
    self.group = collections.defaultdict(list)
    # Not really a type, but we need to record the kick-off record
    self.field = [(idl.TY_TYPE, 'init')]
#    self.array_field = []

  def visit_list(self, ty, xs):
    self.group[(ty, 'list-length')].append(len(xs))
#    self.array_field.append(self.field[-1])
    super().visit_list(ty, xs)
#    self.array_field.pop()

#  def visit_list_item(self, ty, i, x):
#    self.field.append((self.array_field[-1], i % 8))
#    super().visit_list_item(ty, i, x)
#    self.field.pop()

  def visit_struct(self, declared_ty, actual_ty, obj):
    # Record types here, and not in the type field, because
    # the type narrowing happens when decoding the struct.
    # Note, this probably records some fields with fixed types
    # which need no encoding.
    self.group[self.field[-1]].append(actual_ty)
    super().visit_struct(declared_ty, actual_ty, obj)

  def visit_field(self, struct_ty, obj, i, attr):
    self.field.append(map_model_key(self.types, (struct_ty, attr.name)))
    super().visit_field(struct_ty, obj, i, attr)
    self.field.pop()

  def visit_primitive(self, ty, value):
    if ty is idl.TY_TYPE:
      return
    if value is None:
      value = idl.TyNone()
    self.group[self.field[-1]].append(value)


def model_tree(types, ty, tree):
  '''Builds per-field probability models for a tree of a given type.'''
  sharder = TreeSharder(types)
  sharder.visit(ty, tree)
  # We skip TY_TYPE because it marks the first item.
  tables = {k: make_model(k[0], k[1], v) for k, v in sharder.group.items() if k[0] != idl.TY_TYPE}
  for k, v in tables.items():
    assert v is not None, (k, make_model(k[0], k[1], sharder.group[k]))
  return tables


def make_huffman_codebook(values):
  '''Makes a canonical Huffman code modeling values.

  Symbols must be comparable to sort them for the canonical code.

  Returns:
    A code, code length -> symbol map, and symbol -> code, code_length map.

  >>> c, s = make_huffman_codebook(list('appl'))
  >>> c[(0, 1)]
  'p'
  >>> c[(0b10, 2)]
  'a'
  >>> c[(0b11, 2)]
  'l'
  >>> s['p']
  (0, 1)
  '''
  hist = collections.defaultdict(int)
  for value in values:
    hist[value] += 1
  lengths = huffman(hist)
  # Now assign canonical codes
  return huffman_assign_order(list(sorted(map(lambda s: (lengths[s], type(s) is idl.TyNone and 1 or 2, s), hist.keys()))))

def huffman_assign_order(length_order_code):
  if len(length_order_code) == 1:
    assert length_order_code[0][0] == 0
    code = (0, 0)
    sym = length_order_code[0][2]
    return {code: sym}, {sym: code}
  assign_order = list(filter(lambda p: p[0] > 0, length_order_code)) + [(20, None, None)]
  code = 0
  codes = {}
  symbols = {}
  for (code_length, _, symbol), (next_code_length, _, _) in zip(assign_order, assign_order[1:]):
    k = (code, code_length)
    codes[k] = symbol
    symbols[symbol] = k
    assert next_code_length >= code_length,  f'{k}, {next_code_length}, {code_length}'
    code = (code + 1) << ((next_code_length - code_length))
  return codes, symbols

# The model also has to produce the symbol values
class ExplicitSymbolModel(object):
  def from_values(self, values):
    self.code_to_symbol, self.symbol_to_code = make_huffman_codebook(values)
    return self

  def in_use_syms(self):
    yield from self.symbol_to_code.keys()


# The encoder can use symbol indices to produce values
class IndexedSymbolModel(object):
  def __init__(self, symbols):
    self.symbols = symbols
    self.index = {s: i for i, s in enumerate(self.symbols)}
    assert len(self.index) == len(self.symbols), 'must be bijection'

  def from_values(self, values):
    self.code_to_symbol, self.symbol_to_code = make_huffman_codebook(values)
    return self

  def in_use_syms(self):
    yield from self.symbol_to_code.keys()


# A model which must exist because a given array type instantiates it,
# although there are no values at runtime. This arises because array
# lengths are shared by array type and are not specific to a field.
class UnreachableModel(object):
  @property
  def symbol_to_code(self):
    assert False, 'unreachable'

  @property
  def code_to_symbol(self):
    assert False, 'unreachable'

  def in_use_syms(self):
    return []


# There is statically only one value. These models should not be written to the file.
class TrivialModel(object):
  def __init__(self, ty):
    self.ty = ty
    self.symbol_to_code = {ty: (0, 0)}
    self.code_to_symbol = {(0, 0): ty}

  def in_use_syms(self):
    yield self.ty


# Makes a model for a field, including a synthetic field list-length
def make_model(ty, field_name, group):
  assert ty != idl.TY_TYPE
  assert None not in group, (str(ty), field_name)
  if field_name == 'list-length':
    return ExplicitSymbolModel().from_values(group)
  ty = ty.type_of(field_name)
  return make_model_for_type(ty, group)


# This must be in sync with is_indexed_type
def is_indexed_type(ty):
  if type(ty) is idl.Alt:
    return all(map(lambda t: is_indexed_type(t) or type(t) is idl.TyInterface, ty.tys))
  else:
    return type(ty) in [idl.TyNone, idl.TyEnum] or ty == idl.TY_BOOLEAN or (type(ty) is idl.TyFrozenArray and is_indexed_type(ty.element_ty))

def symbols_for_indexed_type(ty):
  if type(ty) is idl.TyFrozenArray:
    return symbols_for_indexed_type(ty.element_ty)
  if type(ty) is idl.Alt:
    assert is_indexed_type(ty)
    return ty.tys
  if type(ty) is idl.TyEnum:
    return ty.values
  if ty is idl.TY_BOOLEAN:
    return [False, True]

# Makes a model where the values are a specific type.
def make_model_for_type(ty, group):
  if type(ty) is idl.TyInterface:
    assert all(map(lambda x: x == ty, group)), (ty, group)
    return TrivialModel(ty)
  if type(ty) is idl.TyFrozenArray:
    return make_model_for_type(ty.element_ty, group)
  if type(ty) is idl.Alt:
    if is_indexed_type(ty):
      return IndexedSymbolModel(ty.tys).from_values(group)
    else:
      return ExplicitSymbolModel().from_values(group)
  if type(ty) is idl.TyEnum:
    return IndexedSymbolModel(ty.values).from_values(group)
  if ty is idl.TY_BOOLEAN:
    return IndexedSymbolModel([False, True]).from_values(group)
  if type(ty) is idl.TyPrimitive:
    return ExplicitSymbolModel().from_values(group)
  assert False, ('unreachable', ty, group)



if __name__ == '__main__':
  doctest.testmod()
