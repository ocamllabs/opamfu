opamfu
======

**opamfu** is a library of utility functions operating over an OPAM
 universe.

### Dependencies

- opam [github.com/OCamlPro/opam](https://github.com/OCamlPro/opam)
- uri [github.com/avsm/ocaml-uri](https://github.com/avsm/ocaml-uri)

If you have opam installed:
```bash
opam install opam-lib uri
```

If you have [cmdliner](http://erratique.ch/software/cmdliner) installed,
the findlib package `opamfu.cli` will be installed which contains
standard **cmdliner** terms for predicate and repository definitions. You
can install **cmdliner** with opam:
```bash
opam install cmdliner
```
