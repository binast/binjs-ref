#!/usr/bin/env python3

import argparse
import concurrent.futures
import glob
import itertools
import io
import os
import subprocess

def cargo_boilerplate(binary):
    return ['cargo', 'run', '--bin', binary, '--release', '--']

# binjs_generate_prediction_tables takes a dictionary output path
# whereas binjs_encode takes an input dictionary file; we just work
# with paths and hard-code the filename the same way as
# binjs_generate_prediction_tables.
def dict_path_to_filename(path):
  return os.path.join(path, 'dict.entropy')

# Generates static dictionary from input files
def generate_dictionary(args):
  input_files = args.input_files
  output_dictionary = args.dictionary
  cmd = cargo_boilerplate('binjs_generate_prediction_tables') + \
    ['-o', output_dictionary, '-i'] + [f.name for f in input_files]
  subprocess.run(cmd)
  with open(dict_path_to_filename(output_dictionary), 'rb') as d:
    dict_size = brotli_compressed_size(d)
  print('brotli compressed dictionary size:', dict_size)

# Returns a JS file's BinAST-encoded size
def binjs_compressed_size(input_file, dictionary):
  if not os.path.exists(dictionary):
    raise Exception('dictionary file {} does not exist'.format(dictionary))
  with subprocess.Popen(
      cargo_boilerplate('binjs_encode') +
        ['advanced', 'entropy', '--dictionary', dictionary],
      stdin=subprocess.PIPE,
      stdout=subprocess.PIPE) as proc:
    stdout, stderr = proc.communicate(input_file.read())
    if stderr:
      raise Exception(stderr)
    return len(stdout)

# Returns a JS file's Brotli-compressed size
def brotli_compressed_size(inp):
  br_bin = 'brotli'
  with subprocess.Popen(
      [br_bin, '-c', '-q', '11', '-w', '20', '-'],
      stdin=subprocess.PIPE, stdout=subprocess.PIPE) as proc:
    out, err = proc.communicate(inp.read())
    return len(out)

# Returns a JS file's gzip-compressed size
def gzip_compressed_size(inp):
  gz_bin = 'gzip'
  with subprocess.Popen(
      [gz_bin, '-c', '-9', '-'],
      stdin=subprocess.PIPE, stdout=subprocess.PIPE) as proc:
    out, err = proc.communicate(inp.read())
    return len(out)

# Returns a JS file's Brotli-compressed + BinAST-encoded size
def measure(input_file, dictionary, use_gzip):
  input_file.seek(0, 2)
  file_size = input_file.tell()
  input_file.seek(0)
  if use_gzip:
    compressed_size = gzip_compressed_size(input_file)
  else:
    compressed_size = brotli_compressed_size(input_file)
  input_file.seek(0)
  binjs_size = binjs_compressed_size(input_file, dictionary)
  return (
    input_file.name,
    file_size,
    compressed_size,
    binjs_size)

# Reports size bloat or size reduction from encoding input JS files to BinAST
# format vs compressing with Brotli 
def print_sizes(args):
  dictionary = dict_path_to_filename(args.dictionary)
  input_files = args.input_files
  use_gzip = args.gzip
  with concurrent.futures.ThreadPoolExecutor(max_workers=16) as executor:
    futures = [executor.submit(measure, f, dictionary, use_gzip) for f in input_files]
    results = map(lambda f: f.result(),
                  concurrent.futures.as_completed(futures))
    print('filename,size,' + \
      ('gzip' if use_gzip else 'br') + \
      ',binast,ratio')
    for filename, size, br_size, binast_size in results:
      print('{},{},{},{},{:.3f}'.format(
        filename,
        size,
        br_size,
        binast_size,
        binast_size/br_size))


def aging_measure(group, filename, dictionary, use_gzip):
  with open(filename, 'rb') as f:
    return (group, measure(f, dictionary, use_gzip))


# Report encoded-size ratios from using a single dictionary trained on one 
# directory but used to encode all directories.
# When each directory represents a snapshot from a different day, this shows
# the impact of dictionary aging on BinAST file size
def aging(args):
  dictionary = dict_path_to_filename(args.dictionary)
  dirs = args.dirs
  use_gzip = args.gzip
  with concurrent.futures.ThreadPoolExecutor(max_workers=16) as executor:
    futures = []
    for d in dirs:
      for filename in glob.iglob(d, recursive=True):
        futures.append(executor.submit(aging_measure, d, filename, dictionary, use_gzip))
    dir_sizes = {}
    print('dir,filename,size,' + \
      ('gzip' if use_gzip else 'br') + \
      ',binast,ratio')
    for d, g in itertools.groupby(map(lambda f: f.result(), futures),
                                  key=lambda r: r[0]):
      dir_size = {
        'size': 0,
        'brotli': 0,
        'binast': 0,
      }
      dir_sizes[d] = dir_size
      for _, (filename, size, brotli, binast) in g:
        print('{},{},{},{},{},{:.3f}'.format(d, filename, size, brotli, binast, binast/brotli))
        dir_size['size'] += size
        dir_size['brotli'] += brotli
        dir_size['binast'] += binast
    print(8 * '-', 'Totals', 8 * '-')
    print('dir,size,' + \
      ('gzip' if use_gzip else 'br') + \
      ',binast,ratio')
    for d, sizes in dir_sizes.items():
      print('{0},{size},{brotli},{binast},'.format(d, **sizes) +
        '{:.3f}'.format(sizes['binast']/sizes['brotli']))


def main():
  os.environ['NODE_MAX_OLD_SPACE_SIZE'] = '9999'

  parser = argparse.ArgumentParser(
    fromfile_prefix_chars='@',
    description='''Measures binjs_encode sizes and Brotli+JS sizes.
    `brotli` must be available on PATH.
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
  measure_parser.add_argument('--gzip', action='store_true',
                              help='compare vs gzip (default is brotli)')
  measure_parser.add_argument('-d', '--dictionary', required=True,
                              help='dictionary dir to read dict.entropy from')
  measure_parser.add_argument('input_files', nargs='+',
                              type=argparse.FileType('rb'),
                              help='JS files to compress')

  aging_parser = subs.add_parser('aging',
                                 help='measure dictionary aging across dirs')
  aging_parser.add_argument('--gzip', action='store_true',
                              help='compare vs gzip (default is brotli)')
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
