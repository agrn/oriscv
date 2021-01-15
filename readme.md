# oriscv

A RISC-V 32 instruction decoder/statistics generator written in OCaml.  It can
be compiled with OCaml 4.05.0 (the version distributed in Debian).

## Usage

To build and run `oriscv`, use the following commands:

```sh
$ dune build
$ dune exec src/oriscv.exe <instruction file>
```

`oriscv` reads hex instructions, so you have to dump them first like this:

```sh
$ riscv32-unknown-elf-objdump -d <exe> | awk '/^[0-9a-f]+:/ {print $2} >out.dis'
```

## Warning

As `oriscv` stores read bytes in native OCaml integers (as they can store
`n - 1` bits of information), it does not work on 32-bits machines.  To work on
those, the program could be changed to use `Int32` instead.

`oriscv` only supports a subset of RV32I for now (all of them except fences and
CPSR-related instructions).  Unrecognised instructions won't crash it (they will
show up as illegal instructions once decoded).
