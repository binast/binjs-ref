#!/usr/bin/env python3

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import format
import idl
import opt
import strings
import tycheck
import types

import argparse
import json
import sys


def encode(dict_file, in_file, out_file):
  types = idl.parse_es6_idl()
  ty_script = types.interfaces['Script']
  string_dict = strings.read_dict(dict_file, with_signature=True)
  proggy = json.loads(in_file.read())
  format.write(types, string_dict, ty_script, proggy, out_file)


def decode(dict_file, in_file, out_file):
  types = idl.parse_es6_idl()
  ty_script = types.interfaces['Script']
  string_dict = strings.read_dict(dict_file, with_signature=True)
  proggy = format.read(types, string_dict, ty_script, in_file)
  json.dump(proggy, out_file)


def optimize(in_file, out_file):
  proggy = json.loads(in_file.read())
  proggy = opt.optimize(proggy)
  json.dump(proggy, out_file)


def make_dict(in_files, out_file):
  types = idl.parse_es6_idl()
  ty_script = types.interfaces['Script']
  sources = []
  for in_file in in_files:
    proggy = json.loads(in_file.read())
    tycheck.FloatFixer(types).rewrite(ty_script, proggy)
    tycheck.TypeChecker(types).check_any(ty_script, proggy)
    sources.append((ty_script, proggy))
  string_dict = strings.prepare_dict(types, sources)
  strings.write_dict(out_file, string_dict, with_signature=True)


def pretty_json(in_file):
  json.dump(sys.stdout, json.loads(in_file.read()), indent=2)


def type_check(in_files):
  types = idl.parse_es6_idl()
  ty_script = types.interfaces['Script']
  for in_file in in_files:
    proggy = json.loads(in_file.read())
    print(f'checking {in_file.name}... ', end='')
    try:
      tycheck.TypeChecker(types).check_any(ty_script, proggy)
    except Exception as e:
      # FIXME: Make this a specific exception type, do error recovery, etc.
      print(f'NG, {e!s}')
      continue
    print('OK')


def fix_types(in_file):
  types = idl.parse_es6_idl()
  ty_script = types.interfaces['Script']
  proggy = json.loads(in_file.read())
  tycheck.FloatFixer(types).rewrite(ty_script, proggy)
  json.dump(proggy, sys.stdout)


def main():
  parser = argparse.ArgumentParser()
  parser.set_defaults(func=lambda args: print('use --help to see commands'))

  subs = parser.add_subparsers(title='subcommands')

  encode_parser = subs.add_parser('encode-ast', help='AST JSON to binary.',
                                  description='''Caveats:
(1) dictionary misses are not supported yet
(2) not all file sections are compressed natively yet;
    the output should be compressed with Brotli
''')
  encode_parser.add_argument('dictionary', type=argparse.FileType('rb'),
                             help='the dictionary file to read from')
  encode_parser.add_argument('input', type=argparse.FileType('r'),
                             help='the AST JSON file read from')
  encode_parser.add_argument('output', type=argparse.FileType('wb'),
                             help='the binary file to write to')
  encode_parser.set_defaults(func=lambda args: encode(args.dictionary, args.input, args.output))

  decode_parser = subs.add_parser('decode-ast', help='Binary to AST JSON.',
                                  description='''
Caveat: dictionary identity is not checked yet; use the same dictionary as encoding.
''')
  decode_parser.add_argument('dictionary', type=argparse.FileType('rb'),
                             help='the dictionary file to read from')
  decode_parser.add_argument('input', type=argparse.FileType('rb'),
                             help='the binary file read from')
  decode_parser.add_argument('output', type=argparse.FileType('w'),
                             help='the JSON file to write AST to')
  decode_parser.set_defaults(func=lambda args: decode(args.dictionary, args.input, args.output))

  opt_parser = subs.add_parser('optimize-ast',
                               help='Adds laziness annotations to an AST.')
  opt_parser.add_argument('input', type=argparse.FileType('r'),
                             help='the AST JSON file read from')
  opt_parser.add_argument('output', type=argparse.FileType('w'),
                             help='the AST JSON file to write to')
  opt_parser.set_defaults(func=lambda args: optimize(args.input, args.output))

  make_dict_parser = subs.add_parser('make-dict',
                                     help='Makes a string dictionary from AST JSON files.')
  make_dict_parser.add_argument('input', type=argparse.FileType('r'), nargs='+',
                                help='the AST JSON files to read from')
  make_dict_parser.add_argument('output', type=argparse.FileType('wb'),
                                help='the binary file to write to')
  make_dict_parser.set_defaults(func=lambda args: make_dict(args.input, args.output))

  pretty_json_parser = subs.add_parser('pretty-json',
                                       help='Pretty-prints JSON, which is useful for AST diffs.')
  pretty_json_parser.add_argument('input', type=argparse.FileType('r'), help='the JSON file to read from')
  pretty_json_parser.set_defaults(func=lambda args: pretty_json(args.input))

  type_check_parser = subs.add_parser('type-check',
                                      help='Checks AST JSON conforms to ES6 IDL.')
  type_check_parser.add_argument('input', type=argparse.FileType('r'), nargs='+',
                                 help='the AST JSON file to read from')
  type_check_parser.set_defaults(func=lambda args: type_check(args.input))
  
  fix_types_parser = subs.add_parser('fix-types',
                                     help='Repairs AST JSON which has ints for doubles.')
  fix_types_parser.add_argument('input', type=argparse.FileType('r'),
                                help='the AST JSON file to read from')
  fix_types_parser.set_defaults(func=lambda args: fix_types(args.input))
  
  args = parser.parse_args()
  args.func(args)


if __name__ == '__main__':
  main()
