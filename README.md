# hookaml

VCS scripting

## Usage

**NB:** For the most up-to-date usage examples, see the cram tests in [usage/](usage/)

### gitst

`gitst w.sexp w.out` reads a 'workspace' description in `w.sexp` and outputs the corresponding status of your git repository in `w.out`

An intended usage is something like:

`watch -n 1 'gitst w.sexp w.out'`, which will continuously update `w.out` with the current status

For developers: `watch -n 1 'dune exec ./gitst.exe w.sexp w.out'` ensures to update with the latest status of your code for a nice feedback loop

## Development

- run the tests: `make test`
- update the tests' expectations: `make test-promote`
- format source code: `make fmt`

## Installation

Run `make install-gitst` to build and copy `gitst` to your system. Installation folder is specified with the variable `INSTALL_ROOT` (default to `~/bin`), e.g.

`make install INSTALL_ROOT=/usr/custom/bin`
