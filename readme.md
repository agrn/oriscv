# oriscv

A RISC-V 32 instruction decoder/statistics generator written in OCaml.  It can
be compiled with OCaml 4.05.0 (the version distributed in Debian).

This also contains a web version of the decoder.  Only `decoder.ml` is transpiled
in JS with `js_of_ocaml`.

## Dependencies

Only the stdlib and Dune for building.  A `Makefile` could be made to do the
same thing as this software is so simple.

For the web version, `js_of_ocaml-compiler` is required.  Build it in release
mode or the resulting file will be humongous.

## Usage

To build and run `oriscv`, use the following commands:

```sh
$ dune build
$ dune exec src/oriscv.exe <instruction file>
```

`oriscv` reads hex instructions following their addresses, so you have to dump
them first like this:

```sh
$ riscv32-unknown-elf-objdump -d <exe> | awk '/^[0-9a-f]+:/ {print $1, $2}' >out.dis
```

## Warning

`oriscv` only supports a subset of RV32I for now (all of them except fences and
CPSR-related instructions).  Unrecognised instructions won't crash it (they will
show up as illegal instructions once decoded).

## Current features

 * Number of instructions
 * Proportion of branches
 * Proportions of jumps
 * Proportions of various pseudo-instructions (`lla`, `la`, far calls, `li`)
 * Count chains of `rd` (ie. how many time a register is written to in an
   instruction window of size 8 by default).
