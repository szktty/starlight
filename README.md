# Starlight

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

Starlight is a yet another Erlang implementation in Rust.

This project is experimental.

## System Requirements

- OS
  - macOS 10.14+
- Compiler
  - OCaml 4.07.1+
  - OMake 0.9.8.6
  - (Optional) OPAM 2.0.1+
  - core
  - yojson
  - ppx_let
  - menhir
- VM
  - Rust 1.31+
  - libdispatch

## Building Starlight

```
$ make
```

## Tools

``strl``: Compiler

``strlrun``: VM

## Usage

```
# build
$ ./strl build hello.erl

# build and run
$ ./strl run hello.erl

# run compiled code
$ ./strlrun hello.strlc
```
