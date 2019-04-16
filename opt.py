#!/usr/bin/env python3

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# This file transforms ASTs in ways outside the scope of file format
# and compression. There are no BinAST minifiers yet, so for now it's
# convenient to integrate it into the encoder/decoder tool.

import doctest

import ast
import idl
import types
import tycheck

# Test helpers.

resolver = idl.parse_es6_idl()
resolver.resolve_types()
checker = tycheck.TypeChecker(resolver)


def check_ty(ty_name, obj):
  '''Checks whether obj matches the type named ty_name.'''
  ty = resolver.resolve(idl.TyRef(ty_name))
  checker.check_any(ty, obj)


def make_iffe():
  '''Makes an AST fragment which is an immediately-invoked function expression.

  >>> check_ty('Expression', make_iffe())
  '''
  return {
      'type': 'CallExpression',
      'callee': make_function_expr(),
      'arguments': []
    }

def make_function_expr(name=None, stmts=None):
  '''Return an AST fragment for an eager function expression.

  >>> check_ty('EagerFunctionExpression', make_function_expr())
  >>> check_ty('EagerFunctionExpression', make_function_expr('print'))
  '''
  if name:
    name = {
        'type': 'BindingIdentifier',
        'name': name
      }
  return {
      'type': 'EagerFunctionExpression',
      'isAsync': False,
      'isGenerator': False,
      'name': name,
      'length': 0,
      'directives': [],
      'contents': {
        'type': 'FunctionExpressionContents',
        'isFunctionNameCaptured': False,
        'isThisCaptured': False,
        'parameterScope': {
            'type': 'AssertedParameterScope',
            'paramNames': [],
            'hasDirectEval': False,
            'isSimpleParameterList': True
          },
        'params': {
            'type': 'FormalParameters',
            'items': [],
            'rest': None
          },
        'bodyScope': {
            'type': 'AssertedVarScope',
            'declaredNames': [],
            'hasDirectEval': False
          },
        'body': stmts or []
      }
    }

def make_function_decl(name, stmts=None):
  '''Makes a named function declaration.
  
  >>> check_ty('EagerFunctionDeclaration', make_function_decl('g'))
  '''
  return {
      'type': 'EagerFunctionDeclaration',
      'isAsync': False,
      'isGenerator': False,
      'name': {
          'type': 'BindingIdentifier',
          'name': name
        },
      'length': 0,
      'directives': [],
      'contents': {
        'type': 'FunctionOrMethodContents',
        'isThisCaptured': False,
        'parameterScope': {
            'type': 'AssertedParameterScope',
            'paramNames': [],
            'hasDirectEval': False,
            'isSimpleParameterList': True
          },
        'params': {
            'type': 'FormalParameters',
            'items': [],
            'rest': None
          },
        'bodyScope': {
            'type': 'AssertedVarScope',
            'declaredNames': [],
            'hasDirectEval': False
          },
        'body': stmts or []
      }
    }

def make_expression_statement(expr):
  '''
  >>> check_ty('ExpressionStatement', make_expression_statement(make_iffe()))
  '''
  return {
      'type': 'ExpressionStatement',
      'expression': expr
    }

def make_script(statements):
  '''Makes a top-level script.

  >>> check_ty('Script', make_script([make_expression_statement(make_iffe())]))
  '''
  return {
      'type': 'Script',
      'scope': {
          'type': 'AssertedScriptGlobalScope',
          # TODO: This probably isn't correct; grovel over the statements.
          'declaredNames': [],
          'hasDirectEval': False,
        },
      'directives': [],
      'statements': statements,
    }

def make_n_statements(n):
  return [{'type': 'EmptyStatement'}] * n

class FunctionLazifier(object):
  '''Converts eager functions matching a heuristic into lazy ones.

  Eager:
  - IIFEs
  - "Small" functions
  - Top-level functions

  Not implemented yet implemented, but should be eager:
  - Immediately bound function expressions
  
  Lazy: everything else.

  Set up an AST:

  >>> f = make_function_decl('f') # Top-level function => eager
  >>> g = make_iffe()             # IFFE => eager
  >>> h = make_function_decl('h', make_n_statements(3)) # Nested function decl => lazy
  >>> i = make_function_expr('i', make_n_statements(4)) # Nested function expr => lazy
  >>> j = make_function_decl('j', make_n_statements(2)) # "Small" nested function => eager
  >>> f['contents']['body'] = [make_expression_statement(g), h, make_expression_statement(i), j]
  >>> s = make_script([f])
  >>> check_ty('Script', s)

  Rewrite it:

  >>> lazifier = FunctionLazifier()
  >>> t = lazifier.lazify(s)
  >>> import json
  >>> print(json.dumps(t, indent=2)) # doctest: +ELLIPSIS
  {
    "type": "Script",
    "scope": {
      ...
    },
    "directives": [],
    "statements": [
      {
        "type": "EagerFunctionDeclaration",
        ...
        "name": {
          "type": "BindingIdentifier",
          "name": "f"
        },
        ...
        "contents": {
          ...
          "body": [
            {
              "type": "ExpressionStatement",
              "expression": {
                "type": "CallExpression",
                "callee": {
                  "type": "EagerFunctionExpression",
                  ...
                  "name": null,
                  ...
                },
                "arguments": []
              }
            },
            {
              "type": "LazyFunctionDeclaration",
              ...
              "name": {
                "type": "BindingIdentifier",
                "name": "h"
              },
              ...
            },
            {
              "type": "ExpressionStatement",
              "expression": {
                "type": "LazyFunctionExpression",
                ...
                "name": {
                  "type": "BindingIdentifier",
                  "name": "i"
                },
                ...
              }
            },
            {
              "type": "EagerFunctionDeclaration",
              ...
              "name": {
                "type": "BindingIdentifier",
                "name": "j"
              },
              ...
            }
          ]
        }
      }
    ]
  }
  '''

  def __init__(self):
    self.depth = 0
    self.lazified = 0

  def lazify(self, n):
    if type(n) in [bool, str, int, float, type(None)]:
      return n
    if type(n) is list:
      return list(map(self.lazify, n))
    if is_functionesque(n):
      lazify = self.depth > 0 and functionesque_length(n) > 2
      self.lazified += lazify
      m = {'type': lazify_type_name(n['type'], lazify)}
      self.depth += 1
      for k, v in n.items():
        if k == 'type':
          continue
        m[k] = self.lazify(v)
      self.depth -= 1
      return m
    if type(n) is dict:
      m = {'type': n['type']}
      for k, v in n.items():
        if k == 'type':
          continue
        m[k] = self.lazify(v)
      return m
    assert False, f'unreachable: {type(n)}'


def lazify_type_name(name, lazify):
  prefix = lazify and 'Lazy' or 'Eager'
  if name.startswith('Lazy'):
    return prefix + name[len('Lazy'):]
  elif name.startswith('Eager'):
    return prefix + name[len('Eager'):]
  assert False, f'unreachable: {name}'
  

# TODO: Handle arrow functions when the corpus uses arrow functions.
def is_functionesque(n):
  return type(n) is dict and n['type'] in [
      'EagerFunctionExpression',
      'LazyFunctionExpression',
      'EagerFunctionDeclaration',
      'LazyFunctionDeclaration'
    ]

def cases_length(cs):
  return sum(map(statement_length, itertools.chain.from_iterable(map(lambda case: case.consequent, cs))))

# Naive heuristic for statement complexity.
def statement_length(s):
  if s is None:
    return 0
  if s['type'] in ['DoWhileStatement', 'ForInStatement', 'ForOfStatement', 'ForStatement',
                   'WhileStatement', 'LabelledStatement',]:
    return 1 + statement_length(s['body'])
  if s['type'] == 'Block':
    return sum(map(statement_length, s['statements']))
  if s['type'] == 'IfStatement':
    return statement_length(s['consequent']) + statement_length(s['alternate'])
  if s['type'] == 'SwitchStatement':
    return 1 + cases_length(s['cases'])
  if s['type'] == 'SwithStatementWithDefault':
    return 1 + cases_length(s['preDefaultCases'] + [s['defaultCase']] + s['postDefaultCases'])
  # TODO: Do something accurate for try/catch/finally
  if s['type'] == 'TryCatchStatement':
    return 1e10
  if s['type'] == 'TryFinallyStatement':
    return 1e10
  if s['type'] == 'WithStatement':
    raise Exception('lolwut')
  return 1

def functionesque_length(obj):
  return len(obj['contents']['body'])

def optimize(a):
  lazifier = FunctionLazifier()
  checker.check(a)
  b = lazifier.lazify(a)
  print(f'lazified {lazifier.lazified} functions')
  checker.check(b)
  return b

if __name__ == '__main__':
  doctest.testmod()
