This directory contains the reference JS corpuses against which we optimize the BinAST encoder.

Each subdirectory contains a different corpus. Use the sizes.py script in this directory to measure compression quality. The script depends on the brotli commandline encoder, so e.g. ```brew install brotli``` and make sure it's in the PATH

Example of measuring compression ratios on a single JS input file:

```
./sizes.py gendict -d /tmp/fb_bench_concat/ ./fb_bench_files/concat.js
./sizes.py measure -d /tmp/fb_bench_concat/ ./fb_bench_files/concat.js
```

Example of measuring dictionary aging:

```
./sizes.py gendict -d /tmp/ef20190102 -- ./earlyflush/ef-2019-01-02/*.js
./sizes.py aging -d /tmp/ef20190102 './earlyflush/ef-2019-01-02/*.js' './earlyflush/ef-2019-01-03/*.js' './earlyflush/ef-2019-01-04/*.js' './earlyflush/ef-2019-01-10/*.js' './earlyflush/ef-2019-01-16/*.js' './earlyflush/ef-2019-02-01/*.js'
```
