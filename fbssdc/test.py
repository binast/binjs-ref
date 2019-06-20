#!/usr/bin/env python3

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import doctest

import ast
import bits
import encode
import format
import idl
import lazy
import model
import opt
import strings
import tycheck

if __name__ == '__main__':
  for mod in [bits, strings, idl, ast, tycheck, opt, lazy, model, encode, format]:
    doctest.testmod(mod)
