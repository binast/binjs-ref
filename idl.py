#!/usr/bin/env python3

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# This isn't a full WebIDL parser by a long shot. It's just designed
# to grok the tiny subset which BinAST's spec uses.

import doctest
import itertools
import os

IDENTIFIER = '*ID*'
STRING = '*STRING*'


def lex(s):
  '''Generates tokens from a string of IDL syntax.

  >>> def t(g):
  ...   return list(map(lambda p: p[1], g))
  >>> t(lex('// comments\\nattribute { are } the "best";'))
  ['attribute', '{', ('*ID*', 'are'), '}', ('*ID*', 'the'), ('*STRING*', 'best'), ';']
  '''
  line = 0
  col = 0
  bol = 0
  # Ending with a newline makes lexing trailing comments easier.
  s += '\n'
  keywords = set([
    'attribute',
    'enum',
    'interface',
    'or',
    'readonly',
    'typedef',
    'unsigned',
  ])
  punctuation = set([
    '[', ']', ',', '(', ')', '{', '}', ';', ':', '<', '>', '?'
  ])
  while col < len(s):
    if s[col:col+2] == '//':
      # Skip comments
      newline = s.index('\n', col)
      line += 1
      col = newline + 1
      bol = col
      continue
    if s[col] == '\n':
      # Skip newlines
      line += 1
      col += 1
      bol = col
      continue
    if s[col] in ' \t\r\v\f':
      # Skip whitespace
      col += 1
      continue
    if s[col] in punctuation:
      # All the punctuation is one character.
      yield ((line, bol, col, col + 1), s[col])
      col += 1
      continue
    if '_' == s[col] or 'a' <= s[col] <= 'z' or 'A' <= s[col] <= 'Z':
      # Identifiers
      ident_begin = col
      col += 1
      while col < len(s) and ('_' == s[col] or 'a' <= s[col] <= 'z' or 'A' <= s[col] <= 'Z' or '0' <= s[col] <= '9'):
        col += 1
      ident = s[ident_begin:col]
      if ident not in keywords:
        ident = (IDENTIFIER, ident)
      yield ((line, bol, ident_begin, col), ident)
      continue
    if s[col] == '"':
      # Strings
      string_begin = col
      col += 1
      while col < len(s) and s[col] not in '"\n':
        col += 1
      if col == len(s) or s[col] == '\n':
        print('unterminated string literal at ({},{})'.format(line, string_begin - bol))
        print(s[bol:s.index('\n', bol)])
        print(' ' * (string_begin - bol - 1), '^', sep='')
        raise Exception('lexical error')
      col += 1 # skip past the closing "
      yield ((line, bol, string_begin, col), (STRING, s[string_begin+1:col-1]))
      continue
    print('lolwut at ({},{})'.format(line, col - bol))
    print(s[bol:s.index('\n', bol)])
    print(' ' * (col - bol - 1), '^', sep='')
    raise Exception('lexical error')


class Parser(object):
  '''Consumes a sequence of tokens and drives callbacks on an IdlEater.'''
  def parse(self, source, eater):
    '''
    >>> p = Parser()
    >>> try:
    ...   p.parse('// here we go\\nhello world', IdlEater())
    ... except Exception:
    ...   pass
    (1,0) expected declaration
    hello world
    ^^^^^
    >>> _ = p.parse('enum Foo { "++", "typeof" }; enum Bar {};', IdlLogger())
    enum Foo ['++', 'typeof']
    enum Bar []

    >>> _ = p.parse('typedef Foo? Bar;', IdlLogger())
    tyref Foo
    optional Foo
    typedef Foo? Bar

    >>> _ = p.parse('typedef (((A))) C;', IdlLogger())
    tyref A
    typedef A C

    >>> _ = p.parse('typedef (A)? C;', IdlLogger())
    tyref A
    optional A
    typedef A? C

    >>> _ = p.parse('typedef (A?) C;', IdlLogger())
    tyref A
    optional A
    typedef A? C

    >>> _ = p.parse('typedef (A or B) C;', IdlLogger())
    tyref A
    tyref B
    alt A B
    typedef (A|B) C

    >>> _ = p.parse('typedef X<(A or B?)?>? Wut;', IdlLogger())
    tyref A
    tyref B
    optional B
    alt A B?
    optional (A|B?)
    instance X ['(A|B?)?']
    optional X<['(A|B?)?']>
    typedef X<['(A|B?)?']>? Wut

    >>> _ = p.parse('interface A : Node {};', IdlLogger())
    interface A Node []

    >>> _ = p.parse('interface A : Node {attribute Node x; attribute Node? y;};', IdlLogger())
    tyref Node
    attribute set() Node x
    tyref Node
    optional Node
    attribute set() Node? y
    interface A Node ['A(Node,x)', 'A(Node?,y)']

    >>> _ = p.parse('interface A : Node {[Foo] attribute Node x;};', IdlLogger())
    tyref Node
    attribute {'Foo'} Node x
    interface A Node ['A(Node,x)']

    >>> _ = p.parse('typedef unsigned long Foo;', IdlLogger())
    tyref long
    unsigned long
    typedef unsigned(long) Foo
    '''
    self.source = source + '\n'
    self.pos = (0, 0, 0, 0)
    self.tokens = lex(source)
    self.eater = eater
    return self._decls()

  def error(self, msg, *fmt, **kwargs):
    line, bol, start, end = self.pos
    print('({},{})'.format(line, start - bol), msg.format(*fmt, **kwargs))
    print(self.source[bol:self.source.index('\n', bol)])
    print(' ' * (start - bol), '^' * (end - start), sep='')
    raise Exception('parse error')

  def _peek(self):
    old_pos = self.pos
    try:
      tok = self._read()
    except StopIteration:
      return None
    self.tokens = itertools.chain([(self.pos, tok)], self.tokens)
    self.pos = old_pos
    return tok

  def _read(self):
    self.pos, tok = next(self.tokens)
    return tok

  def _eat(self, symbol):
    tok = self._read()
    if tok != symbol:
      return self.error('expected {}', symbol)

  def _ident(self, what):
    tok = self._read()
    if type(tok) is not tuple or tok[0] is not IDENTIFIER:
      return self.error('expected {}', what)
    return tok[1]

  def _decls(self):
    decls = []
    while self._peek():
      decl = self._decl()
      decls.append(decl)
    return decls

  def _decl(self):
    tok = self._read()
    if tok == 'typedef':
      d = self._typedef()
    elif tok == 'interface':
      d = self._interface()
    elif tok == 'enum':
      d = self._enum()
    else:
      return self.error('expected declaration')
    self._eat(';')
    return d

  def _ty(self):
    tok = self._read()
    if tok == '(':
      # Parenthesized type.
      ty = self._ty()
      self._eat(')')
    elif tok == 'unsigned':
      # TODO: split this up to avoid "unsigned unsigned" etc.
      ty = self._ty()
      ty = self.eater.unsigned(ty)
    elif type(tok) is tuple and tok[0] is IDENTIFIER:
      # Type instance or named type.
      ty = self._instance(tok[1])
    else:
      return self.error('expected start of type: ( or type name')

    # Now modifiers.
    if self._peek() == '?':
      # Optional; this binds tightly to the preceding thing.
      self._eat('?')
      ty = self.eater.optional(ty)
    if self._peek() == 'or':
      # Alternate.
      self._eat('or')
      next_ty = self._ty()
      ty = self.eater.alt(ty, next_ty)
    return ty

  # May produce an instance or a simple name.
  def _instance(self, name):
    if self._peek() != '<':
      # Just a simple type ref.
      return self.eater.tyref(name)
    self._eat('<')
    params = []
    next_tok = self._peek()
    while next_tok and next_tok != '>':
      # Remarkably all the type instances are FrozenArray<Thing>
      # so parsing commas, etc. here is NYI
      param = self._ty()
      params.append(param)
      next_tok = self._peek()
    if not next_tok:
      return self.error('unterminated type instantiation')
    self._eat('>')
    return self.eater.instance(name, params)

  def _typedef(self):
    ty = self._ty()
    name = self._ident('type name')
    return self.eater.typedef(ty, name)

  def _enum(self):
    name = self._ident('type name')
    self._eat('{')
    values = []
    tok = self._read()
    while tok != '}':
      if type(tok) is not tuple or tok[0] is not STRING:
        return self.error('expected string enum member')
      values.append(tok[1])
      tok = self._read()
      if tok == ',':
        tok = self._read()
        continue
      if tok == '}':
        break
      return self.error('expected , or } after enum member')
    return self.eater.enum(name, values)

  def _interface(self):
    name = self._ident('interface name')
    if self._peek() == ':':
      self._eat(':')
      parent_name = self._ident('base interface name')
    else:
      parent_name = None
    self._eat('{')
    attrs = []
    next_tok = self._peek()
    while next_tok and next_tok != '}':
      attr = self._attr()
      attrs.append(attr)
      next_tok = self._peek()
    self._eat('}')
    return self.eater.interface(name, parent_name, attrs)

  def _attr(self):
    extended_attrs = []
    if self._peek() == '[':
      tok = self._read()
      while tok != ']':
        tok = self._read()
        if tok is not ']':
          if tok[0] is not IDENTIFIER:
            return self.error('expected identifier of extended attribute')
          extended_attrs.append(tok[1])
    # We don't care whether attributes are readonly.
    if self._peek() == 'readonly':
      self._eat('readonly')
    self._eat('attribute')
    ty = self._ty()
    name = self._ident('attribute name')
    if name == '_object':
      # es6.idl calls object _object, maybe because object
      # is a WebIDL built-in type.
      name = 'object'
    self._eat(';')
    return self.eater.attribute(set(extended_attrs), ty, name)


class IdlEater(object):
  def tyref(self, name):
    '''A reference to a type by name.'''
    pass

  def instance(self, name, params):
    '''An instantiation of a generic type.'''
    pass

  def alt(self, ty_a, ty_b):
    '''An alternation between two types.'''
    pass

  def optional(self, ty):
    '''An optional type.'''
    pass

  def unsigned(self, ty):
    '''An unsigned type.

    These should only be the built-in numerics but the parser does not
    check for this and applies "unsigned" as a modifier like optional.
    '''
    pass

  def typedef(self, ty, name):

    '''An alias of a type.'''
    pass

  def enum(self, name, values):
    '''An enumeration.'''
    pass

  def interface(self, name, extends, attrs):
    '''An interface declaration.'''
    pass

  def attribute(self, extended_attrs, ty, name):
    '''An interface attribute.'''
    pass

  def extended_attr(self, name):
    '''An extended metadata attribute.'''
    pass


class IdlLogger(IdlEater):
  '''Dumps the IDL parsed for debugging.'''
  def tyref(self, name):
    print('tyref', name)
    return name

  def instance(self, name, params):
    print('instance', name, params)
    return '{}<{}>'.format(name, params)

  def alt(self, ty_a, ty_b):
    print('alt', ty_a, ty_b)
    return '({}|{})'.format(ty_a, ty_b)

  def optional(self, ty):
    print('optional', ty)
    return '{}?'.format(ty)

  def unsigned(self, ty):
    print('unsigned', ty)
    return 'unsigned({})'.format(ty)

  def typedef(self, ty, name):
    print('typedef', ty, name)
    return '{} <==> {}'.format(ty, name)

  def enum(self, name, values):
    print('enum', name, values)
    return 'E({},{})'.format(name, values)

  def interface(self, name, extends, attrs):
    print('interface', name, extends, attrs)
    return 'I({},...)'.format(name)

  def attribute(self, extended_attrs, ty, name):
    print('attribute', extended_attrs, ty, name)
    return 'A({},{})'.format(ty, name)

  def extended_attr(self, name):
    print('extended attr', name)
    return '@{}'.format(name)


# IDL types

class Ty(object):
  def concrete_types(self):
    '''Enumerates the possible concrete types matching this type.'''
    yield self

  # It's convenient for types to be ordered so that we can assign them symbols
  def __lt__(self, other):
    if not isinstance(other, Ty):
      raise Exception(f'cannot compare unrelated type {type(other)}')

    self_ty = type(self)
    other_ty = type(other)

    if self_ty is TyNone:
      return other_ty is not TyNone
    if other_ty is TyNone:
      return False

    if self_ty is TyPrimitive:
      return other_ty is not TyPrimitive or self.name < other.name
    if other_ty is TyPrimitive:
      return False

    if self_ty is Alt:
      return other_ty is not Alt or self.tys < other.tys
    if other_ty is Alt:
      return False

    if self_ty is TyFrozenArray:
      if other_ty is not TyFrozenArray:
        return True
      return self.element_ty < other.element_ty
    if other_ty is TyFrozenArray:
      return False

    if self_ty is TyEnum:
      return other_ty is not TyEnum or self.name < other.name
    if other_ty is TyEnum:
      return False

    if self_ty is TyInterface:
      return other_ty is not TyInterface or self.name < other.name
    if other_ty is TyInterface:
      return False

    if self_ty is TyRef:
      return other_ty is not TyRef or self.name < other.name
    assert other_ty is TyRef
    assert self == other
    return False

  def __repr__(self):
    return str(self)


class TyPrimitive(Ty):
  def __init__(self, name):
    super().__init__()
    self.name = name

  def __str__(self):
    return self.name

TY_BOOLEAN = TyPrimitive('boolean')
TY_DOUBLE = TyPrimitive('double')
TY_LONG = TyPrimitive('long')
TY_STRING = TyPrimitive('DOMString')
TY_UNSIGNED_LONG = TyPrimitive('unsigned long')

# Pretty sure this *isn't* a WebIDL type, but it's convenient to make
# it a distinct type since this field is not serialized like the
# others but is part of the structure of the AST.
TY_TYPE = TyPrimitive('Type')

builtins = {
  'boolean': TY_BOOLEAN,
  'double': TY_DOUBLE,
  'unsigned long': TY_UNSIGNED_LONG,
  'long': TY_LONG,
  'DOMString': TY_STRING,
  'Type': TY_TYPE,
}


class TyFrozenArray(Ty):
  def __init__(self, element_ty):
    super().__init__()
    self.element_ty = element_ty

  def __str__(self):
    return f'FrozenArray<{self.element_ty}>'

  def __eq__(self, other):
    return type(other) is TyFrozenArray and self.element_ty == other.element_ty

  def __hash__(self):
    return 37 * hash(self.element_ty)


class TyNone(Ty):
  def __eq__(self, other):
    return type(other) is TyNone

  def __str__(self):
    return 'none'

  def __hash__(self):
    return 17 * hash(TyNone)


class Alt(Ty):
  def __init__(self, tys):
    super().__init__()
    assert len(tys) > 1
    self.tys = tuple(sorted(tys))
    self.ty_set = frozenset(tys)

  def __str__(self):
    return '({})'.format(' or '.join(map(str, self.tys)))

  def __eq__(self, other):
    return type(other) is Alt and other.tys == self.tys

  def __hash__(self):
    return 257 * hash(self.tys)

  def concrete_types(self):
    '''
    >>> t1 = TyInterface('A', None, [])
    >>> t2 = TyInterface('B', None, [])
    >>> t1_or_t2 = Alt([t1, t2])
    >>> array_t1_or_t2 = TyFrozenArray(t1_or_t2)
    >>> optional_array = Alt([array_t1_or_t2, t1, TyNone()])
    >>> concretes = set(optional_array.concrete_types())
    >>> len(concretes)
    3
    >>> array_t1_or_t2 in concretes
    True
    >>> t1 in concretes
    True
    >>> TyNone() in concretes
    True
    '''
    yield from itertools.chain.from_iterable(map(lambda t: t.concrete_types(), self.tys))


class TyInterface(Ty):
  def __init__(self, name, extends, attrs):
    self.name = name
    self.extends = extends
    self.attrs = attrs
    self.prop_ty_map = None

  def type_of(self, prop):
    '''Gets the resolved type of an attribute named prop.

    This must be called after types are resolved.
    '''
    if not self.prop_ty_map:
      self.prop_ty_map = {attr.name: attr.resolved_ty for attr in self.attrs}
    return self.prop_ty_map[prop]

  def type_at(self, index):
    '''Gets the resolved type at the i-th attribute.

    This must be called after types are resolved.
    '''
    return self.attrs[index].resolved_ty

  def attributes(self):
    '''Gets the attributes and inherited attributes of this interface.'''
    if self.resolved_extends:
      yield from self.resolved_extends.attributes()
    yield from self.attrs

  def __str__(self):
    return 'interface {}...'.format(self.name)


class TyEnum(Ty):
  def __init__(self, name, values):
    self.name = name
    self.values = values

  def __str__(self):
    return 'enum {}...'.format(self.name)


class TyRef(Ty):
  '''An unresolved reference to a type.'''
  def __init__(self, name):
    self.name = name

  def __str__(self):
    return '??{}??'.format(self.name)

class Attribute(object):
  '''An interface attribute.'''
  def __init__(self, extended_attrs, ty, name):
    self.extended_attrs = extended_attrs
    self.ty = ty
    self.name = name

  def __str__(self):
    return 'attribute {} {}'.format(ty, name)

  @property
  def lazy(self):
    return 'Lazy' in self.extended_attrs


class IdlTypeResolver(IdlEater):
  '''Records types in the es6.webidl file.'''
  def __init__(self):
    self.typedefs = {}
    self.enums = {}
    self.interfaces = {}

  def tyref(self, name):
    '''A reference to a type by name.'''
    builtin = builtins.get(name)
    if builtin:
      return builtin
    return TyRef(name)

  def instance(self, name, params):
    '''An instantiation of a generic type.'''
    # The only such type es6.webidl uses is FrozenArray
    assert name == 'FrozenArray'
    return TyFrozenArray(params[0])

  def alt(self, ty_a, ty_b):
    '''An alternation between two types.'''
    return Alt([ty_a, ty_b])

  def optional(self, ty):
    '''An optional type.'''
    return Alt([ty, TyNone()])

  def unsigned(self, ty):
    '''An unsigned type.'''
    assert ty is TY_LONG
    return TY_UNSIGNED_LONG

  def typedef(self, ty, name):
    '''An alias of a type.'''
    if name in self.typedefs:
      raise Exception('duplicate typedef {}'.format(name))
    self.typedefs[name] = ty

  def enum(self, name, values):
    '''An enumeration.'''
    if name in self.enums:
      raise Exception('duplicate enum definition {}'.format(name))
    self.enums[name] = TyEnum(name, values)

  def interface(self, name, extends, attrs):
    '''An interface declaration.'''
    if name in self.interfaces:
      raise Exception('duplicate interface definition: {}'.format(name))
    self.interfaces[name] = TyInterface(name, extends, attrs)

  def attribute(self, extended_attrs, ty, name):
    '''An interface attribute.'''
    return Attribute(extended_attrs, ty, name)

  def extended_attr(self, name):
    '''An extended metadata attribute.'''
    pass # Don't care about these for now.

  def resolve_types(self):
    for ty in self.interfaces.values():
      ty.resolved_extends = ty.extends and self.resolve(TyRef(ty.extends))
      for attr in ty.attrs:
        attr.resolved_ty = self.resolve(attr.ty)

    # Check that if there's an EagerX, the LazyX lines up with it.
    for ty in self.interfaces.values():
      if ty.name.startswith('Eager'):
        lazy_ty = self.interfaces['Lazy' + ty.name[len('Eager'):]]
        assert len(ty.attrs) == len(lazy_ty.attrs)
        for e1, z1 in zip(ty.attrs, lazy_ty.attrs):
          assert e1.name == z1.name, f'{ty.name} {e1.name}; {lazy_ty.name} {z1.name}'
          # TODO: When types support equality checks, test that the resolved
          # types are the same too

  def resolve(self, ty):
    if type(ty) is TyPrimitive:
      return ty
    if type(ty) is TyFrozenArray:
      return TyFrozenArray(self.resolve(ty.element_ty))
    if type(ty) is Alt:
      tys = set()
      for sub_ty in map(self.resolve, ty.tys):
        if type(sub_ty) is Alt:
          tys.update(sub_ty.ty_set)
        else:
          tys.add(sub_ty)
      return Alt(tys)
    if type(ty) in [TyInterface, TyEnum, TyNone]:
      return ty
    if type(ty) is TyRef:
      iface = self.interfaces.get(ty.name)
      if iface:
        return iface
      alias = self.typedefs.get(ty.name)
      if alias:
        return self.resolve(alias)
      enum = self.enums.get(ty.name)
      if enum:
        return enum
      raise Exception(f'can not resolve type name {ty.name}')
    assert False, f'unreachable: {type(ty)}, {ty}'


def es6_webidl_source():
  '''Reads and returns the es6 webidl source text.'''
  with open(os.path.join(os.path.dirname(__file__), 'es6.webidl'), 'r') as inp:
    return inp.read()


def parse_es6_idl():
  '''
  >>> resolver = parse_es6_idl()
  >>> len(resolver.interfaces)
  120
  >>> for i, ty in zip(range(3), resolver.interfaces.values()):
  ...   print(ty)
  interface AssertedDeclaredName...
  interface AssertedPositionalParameterName...
  interface AssertedRestParameterName...
  >>> for i, ty in zip(range(3), resolver.enums.values()):
  ...   print(ty)
  enum VariableDeclarationKind...
  enum CompoundAssignmentOperator...
  enum BinaryOperator...
  >>> resolver.resolve_types()
  >>> a = next(filter(lambda a: a.name == 'paramNames', resolver.interfaces['AssertedParameterScope'].attrs))
  >>> print(a.ty)
  FrozenArray<??AssertedMaybePositionalParameterName??>
  >>> print(a.resolved_ty)
  FrozenArray<(interface AssertedParameterName... or interface AssertedPositionalParameterName... or interface AssertedRestParameterName...)>
  >>> print(resolver.resolve(Alt([Alt([resolver.tyref('double'), TyNone()]), Alt([TyNone(), resolver.tyref('long')])])))
  (none or double or long)
  >>> print(resolver.interfaces['Script'].type_of('scope'))
  interface AssertedScriptGlobalScope...
  '''
  p = Parser()
  source = es6_webidl_source()
  resolver = IdlTypeResolver()
  p.parse(source, resolver)
  resolver.resolve_types()
  return resolver


if __name__ == '__main__':
  doctest.testmod()
