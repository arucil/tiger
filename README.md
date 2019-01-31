# Tiger

Tiger is a compiler for the Tiger language described in *Modern Compiler Implementation in ML*.

# Building

This project is built with OCaml and Dune.

One can use OPAM 2.0 to install dependencies automatically, as follows,

```bash
opam install --deps-only .
```

Currently there is no executables so the building part is over.

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