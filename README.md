opamfu
======

**opamfu** is a library of utility functions operating over an OPAM
 universe.

The latest release of **opamfu** is available via
[opam](http://opam.ocaml.org). Simply run `opam install opamfu` to install.

### Dependencies

- opam [github.com/ocaml/opam](https://github.com/ocaml/opam) 1.1.1 or later
- uri [github.com/avsm/ocaml-uri](https://github.com/avsm/ocaml-uri) 1.3.11 or later

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
