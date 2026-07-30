# PGM2 2.0.0 — CRAN submission comments

## Summary of changes

Major update of a package first released in 2014 (last update 1.2,
2025-05-27): the construction is generalised from GF(2) to GF(p) for any
prime p, with a `p` argument added to `BIB()` and `Steps()` (default
`p = 2`, fully backward compatible — enforced by a regression test).
Adds a test suite (testthat), a vignette reproducing the underlying 2013
paper, and input validation.

## Test environments

* Local Linux, R 4.3.3
* GitHub Actions: ubuntu-latest (R release, devel), windows-latest,
  macos-latest

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

None (checked with CRAN's revdep list: no packages depend on PGM2).
