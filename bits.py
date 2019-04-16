#!/usr/bin/env python3

# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import doctest
import io

class BitsIO(object):
  '''A bit-oriented reader.

  When writing multiple bits across multiple bytes, the low-order bits appear first.
  '''
  def __init__(self, s):
    self.s = s
    self.buf = 0
    self.used_bits = 0

  def write(self, count, value):
    '''
    >>> s = io.BytesIO()
    >>> w = BitsIO(s)
    >>> w.write(3, 7)
    >>> w.write(8, 0xaa)
    >>> w.flush()
    >>> bytes(s.getbuffer())
    b'W\\x05'
    >>> s.getbuffer()[0] == 0b01010_111
    True
    >>> s.getbuffer()[1] == 0b00000_101
    True
    '''
    assert 0 <= value < (1 << count)
    while count > 0:
      free_bits = 8 - self.used_bits
      self.buf = self.buf | (value << self.used_bits) & 0xff
      self.used_bits += min(count, free_bits)
      value >>= free_bits
      count -= free_bits
      if self.used_bits == 8:
        self.s.write(self.buf.to_bytes(1, byteorder='big'))
        self.buf = 0
        self.used_bits = 0
    assert self.used_bits < 8, self.used_bits

  def flush(self):
    if self.used_bits:
      self.s.write(self.buf.to_bytes(1, byteorder='big'))
    self.buf = 0
    self.used_bits = 0

  def read(self, count):
    '''
    >>> s = io.BytesIO()
    >>> w = BitsIO(s)
    >>> w.write(1, 0)
    >>> w.write(1, 1)
    >>> w.write(32, 0xdeadbeef)
    >>> w.write(64, 0xcafef00d13)
    >>> w.flush()
    >>> s.seek(0)
    0
    >>> r = BitsIO(s)
    >>> r.read(1)
    0
    >>> r.read(1)
    1
    >>> '{:x}'.format(r.read(32))
    'deadbeef'
    >>> '{:x}'.format(r.read(64))
    'cafef00d13'
    '''
    value = 0
    read_bits = 0
    while count != read_bits:
      if self.used_bits == 0:
        self.used_bits = 8
        self.buf = self.s.read(1)[0]
      read_this_time = min(count - read_bits, self.used_bits)
      value |= (self.buf & ((1 << read_this_time) - 1)) << read_bits
      read_bits += read_this_time
      self.buf >>= read_this_time
      self.used_bits -= read_this_time
    return value


def write_varint(bs, n):
  while n > 0x7f:
    bs.write((0x80 | (n & 0x7f)).to_bytes(1, byteorder='big'))
    n >>= 7
  bs.write(n.to_bytes(1, byteorder='big'))


def read_varint(bs):
  '''
  >>> bs = io.BytesIO()
  >>> write_varint(bs, 7)
  >>> write_varint(bs, 0)
  >>> write_varint(bs, 7000)
  >>> bs.seek(0)
  0
  >>> read_varint(bs)
  7
  >>> read_varint(bs)
  0
  >>> read_varint(bs)
  7000
  >>> assert bs.tell() == bs.seek(0, io.SEEK_END)
  '''
  b = 0xff
  n = 0
  shift = 0
  while b & 0x80:
    b = bs.read(1)[0]
    n |= (b & 0x7f) << shift
    shift += 7
  return n


if __name__ == '__main__':
  doctest.testmod()
