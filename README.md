# unionfind - Union-find (disjoint-set) data structure

*Author:* Langston Barrett <langston.barrett@gmail.com><br>
*Version:* 0.1<br>

This library provides a union-find (disjoint set) data structure in Emacs
Lisp.

## API

The following operations are exposed:

- `unionfind-of`: Create a new union-find data structure containing given values
- `unionfind-count`: Count the number of elements in this union-find structure
- `unionfind-insert`: Add a new value to the union-find structure
- `unionfind-find`: Get the canonical representative of a given value
- `unionfind-equiv-p`: Check if two values share a canonical representative
- `unionfind-merge`: Merge the equivalence classes of two values
- `unionfind-merge-canonical`: Merge the equivalence classes of two canonical
  representatives
- `unionfind-to-hash-set`: Create a two-level nested hash set representing the
  partition/equivalence classes.

## Performance

This library implements an imperative (pointer-based) union-find structure,
the complexity of the operations are as follows:

- `unionfind-of`: O(n) in the number of values
- `unionfind-insert`: O(1) (amortized)
- `unionfind-count`: O(1) (amortized)
- `unionfind-find`: O(1) (amortized)
- `unionfind-equiv-p`: O(1) (amortized)
- `unionfind-merge`: O(1) (amortized)
- `unionfind-to-hash-set`: O(n^2)

## Dependencies

This library has no dependencies, but will use contract.el if it's available.

## Development

PRs and issues welcome! Develop using Cask. Here are a few Makefile targets:

    make .cask/
    make build
    make test

Tests are provided using doctest, buttercup, and propcheck.


---
Converted from `unionfind.el` by [*el2markdown*](https://github.com/Lindydancer/el2markdown).
