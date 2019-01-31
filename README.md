# Tiger

[![CircleCI](https://circleci.com/gh/arucil/tiger.svg?style=svg)](https://circleci.com/gh/arucil/tiger)

Tiger is a compiler for the Tiger language described in *Modern Compiler Implementation in ML*.

# Building

This project is built with OCaml and Dune.

One can use OPAM 2.0 to install dependencies automatically, as follows,

```bash
opam install --deps-only .
```

Currently there is only a dummy executable for ensuring that all modules can be built successfully. The executable can be built with the following command,

```bash
dune build bin/tiger.exe
```

The extension `.exe` is a convention of Dune for building native executables, which is platform irrelevant.

After building successfully, run the executable with the following command,

```bash
_build/default/bin/tiger.exe
```

One can also use Dune's `exec` command to build and execute in one step,

```bash
dune exec bin/tiger.exe
```

# Testing

The unit tests is built and run with Dune.

```bash
dune runtest               # Run all tests
dune runtest lib/lex       # Run lexer tests
dune runtest lib/parse     # Run parser tests
dune runtest lib/semantic  # Run semantic analyzer tests
```

# License

This project is licensed under the MIT License.