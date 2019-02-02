#!/usr/bin/env python3

import argparse
import concurrent.futures
import glob
import itertools
import io
import os
import subprocess


# binjs_generate_prediction_tables takes a dictionary output path
# whereas binjs_encode takes an input dictionary file; we just work
# with paths and hard-code the filename the same way as
# binjs_generate_prediction_tables.
def dict_path_to_filename(path):
  return os.path.join(path, 'dict.entropy')


def generate_dictionary(args):
  input_files = args.input_files
  output_dictionary = args.dictionary
  cmd = ['../../target/release/binjs_generate_prediction_tables',
         '-o', output_dictionary, '-i'] + [f.name for f in input_files]
  subprocess.run(cmd)
  with open(dict_path_to_filename(output_dictionary), 'rb') as d:
    dict_size = brotli_compressed_size(d)
  print('brotli compressed dictionary size:', dict_size)


def binjs_compressed_size(input_file, dictionary):
  if not os.path.exists(dictionary):
    raise Exception('dictionary file {} does not exist'.format(dictionary))
  with subprocess.Popen(
      ['../../target/release/binjs_encode', 'advanced', 'entropy',
       '--dictionary', dictionary],
      stdin=subprocess.PIPE,
      stdout=subprocess.PIPE) as proc:
    stdout, stderr = proc.communicate(input_file.read())
    if stderr:
      raise Exception(stderr)
    return len(stdout)


def brotli_compressed_size(inp):
  br_bin = 'brotli'
  with subprocess.Popen(
      [br_bin, '-c', '-q', '11', '-w', '20', '-'],
      stdin=subprocess.PIPE, stdout=subprocess.PIPE) as proc:
    out, err = proc.communicate(inp.read())
    return len(out)


def measure(input_file, dictionary):
  input_file.seek(0, 2)
  file_size = input_file.tell()
  input_file.seek(0)
  br = brotli_compressed_size(input_file)
  input_file.seek(0)
  binjs_size = binjs_compressed_size(input_file, dictionary)
  return (
    input_file.name,
    file_size,
    br,
    binjs_size)


def print_sizes(args):
  dictionary = dict_path_to_filename(args.dictionary)
  input_files = args.input_files
  with concurrent.futures.ThreadPoolExecutor(max_workers=16) as executor:
    futures = [executor.submit(measure, f, dictionary) for f in input_files]
    results = map(lambda f: f.result(),
                  concurrent.futures.as_completed(futures))
    print('filename,size,brotli,binast')
    for filename, size, br_size, binast_size in results:
      print('{},{},{},{}'.format(
        filename,
        size,
        br_size,
        binast_size))


def aging_measure(group, filename, dictionary):
  with open(filename, 'rb') as f:
    return (group, measure(f, dictionary))


def aging(args):
  dictionary = dict_path_to_filename(args.dictionary)
  dirs = args.dirs
  with concurrent.futures.ThreadPoolExecutor(max_workers=16) as executor:
    futures = []
    for d in dirs:
      for filename in glob.iglob(d, recursive=True):
        futures.append(executor.submit(aging_measure, d, filename, dictionary))
    dir_sizes = {}
    print('dir,filename,size,brotli,binast')
    for d, g in itertools.groupby(map(lambda f: f.result(), futures),
                                  key=lambda r: r[0]):
      dir_size = {
        'size': 0,
        'brotli': 0,
        'binast': 0,
      }
      dir_sizes[d] = dir_size
      for _, (filename, size, brotli, binast) in g:
        print('{},{},{},{},{}'.format(d, filename, size, brotli, binast))
        dir_size['size'] += size
        dir_size['brotli'] += brotli
        dir_size['binast'] += binast
    print(8 * '-', 'Totals', 8 * '-')
    print('dir,size,brotli,binast')
    for d, sizes in dir_sizes.items():
      print('{0},{size},{brotli},{binast}'.format(d, **sizes))


def main():
  parser = argparse.ArgumentParser(
    fromfile_prefix_chars='@',
    description='''Measures binjs_encode sizes and Brotli+JS sizes.
Has to be run from a binjs-ref repo root. Build a binjs_encode binary
with `cargo build --bin binjs_encode --bin binjs_generate_prediction_tables
--release`. `brotli` must be available on PATH.
''')
  subs = parser.add_subparsers(title='command', dest='command')

  gendict_parser = subs.add_parser('gendict',
                                   help='generate a dictionary')
  gendict_parser.add_argument('-d', '--dictionary', required=True,
                              help='dictionary dir to generate in')
  gendict_parser.add_argument('input_files', nargs='+',
                              type=argparse.FileType('rb'),
                              help='JS files to sample to generate dictionary')

  measure_parser = subs.add_parser('measure',
                                   help='measure compressed file sizes')
  measure_parser.add_argument('-d', '--dictionary', required=True,
                              help='dictionary dir to read dict.entropy from')
  measure_parser.add_argument('input_files', nargs='+',
                              type=argparse.FileType('rb'),
                              help='JS files to compress')

  aging_parser = subs.add_parser('aging',
                                 help='measure dictionary aging across dirs')
  aging_parser.add_argument('-d', '--dictionary', required=True,
                            help='dictionary dir to read dict.entropy from')
  aging_parser.add_argument('dirs', nargs='+',
                            help='globs of JavaScript files, eg \'a/**/*.js\'')

  args = parser.parse_args()
  if args.command == 'gendict':
    generate_dictionary(args)
  elif args.command == 'aging':
    aging(args)
  else:
    print_sizes(args)


if __name__ == '__main__':
  main()
