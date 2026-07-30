# PGM2 2.0.1 — CRAN submission comments

## Note on the 2.0.0 submission

Version 2.0.0 was submitted a short time ago. Before it was processed we
found a defect in `Uniform()` and are replacing that submission with
2.0.1. Please discard 2.0.0.

`Uniform()` extracted parallel classes by a first-fit greedy scan, which
could fail on a valid resolvable design presented in an unfavourable row
order: of 200 random row permutations of one design shipped in the
package vignette, 178 raised an error. The resolution is now found by an
exact-cover search with backtracking, the function validates its input,
and regression tests cover row-permutation invariance. Designs produced
by the package's own constructors are unchanged.

## Summary of changes

Major update of a package first released in 2014 (last update 1.2,
2025-05-27). The construction is generalised from GF(2) to GF(p) for any
prime p, with a `p` argument added to `BIB()` and `Steps()` (default
`p = 2`). A new function `Qn()` builds the reduced resolvable designs and
their uniform designs directly. The release also adds a testthat suite, a
vignette reproducing the underlying 2013 paper, argument validation, and
a faster subspace enumeration.

Two user-visible changes relative to 1.2, both documented in NEWS.md:

* `Steps()` returns a named list with one element per stage instead of
  the flat, partially unnamed list of previous versions, and `BIB()`
  gains a `Lambda` element. The design matrices returned for `p = 2` are
  unchanged, which is enforced by a regression test against the 1.2
  algorithm.
* `Gen()`, `Resolvable()` and `Steps()` now validate the block index and
  report the admissible range. Previously an out-of-range index failed
  with `subscript out of bounds`; in `Steps()` this affected every index
  greater than p^2 + p + 1, since the same index is used at each stage
  while the number of blocks decreases along the recursion.

## Test environments

* Local Linux, R 4.3.3
* GitHub Actions: ubuntu-latest (R release and R devel), windows-latest,
  macos-latest — all passing.

## R CMD check results

0 errors | 0 warnings | 2 notes

Both notes are properties of the local check environment rather than the
package, and neither appears on the GitHub Actions runs:

* "unable to verify current time" (no network clock in the local
  sandbox);
* "Skipping checking HTML validation: no command 'tidy' found" and
  "package 'V8' unavailable" (optional HTML-manual validation tools are
  not installed locally).

## Reverse dependencies

None: no packages on CRAN depend on, import or suggest PGM2.
