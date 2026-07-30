# PGM2 2.0.1 — CRAN submission comments

## Reason for this release

This is a bug-fix release following 2.0.0, published a few days ago. It
corrects a defect in `Uniform()` that we found after that release.

`Uniform()` extracted the parallel classes of a resolvable design by a
first-fit greedy scan. That procedure can become trapped even when a
resolution exists, so the function was dependent on the order of the rows
of its input: of 200 random row permutations of one design shipped in the
package vignette, 178 failed with `replacement has length zero`. The
resolution is now found by an exact-cover search with backtracking over
the whole partition; all 200 permutations succeed and give the same
design up to a permutation of the factors and a relabelling of levels,
and the function validates its input rather than returning a partial
design. Regression tests cover row-permutation invariance, a resolvable
design from outside the package, and non-resolvable input.

Designs produced by the package's own constructors are unchanged, and
`BIB()`, `Gen()`, `Resolvable()`, `Qn()` and `Steps()` are unaffected.

Also in this release: `Steps()` rejects unknown or empty `stage` values
instead of silently returning an empty list; the `Qn()` complexity note
covers the labelling step as well as the subspace enumeration; the
Plackett-Burman identification on the `Qn()` help page is stated up to
row, column and level equivalence; and the now-unused `stats` import has
been dropped from Imports.

## Test environments

* Local Linux (Ubuntu 24.04), R 4.3.3
* GitHub Actions: ubuntu-latest (R release and R devel), windows-latest,
  macos-latest

## R CMD check results

0 errors | 0 warnings | 3 notes

The notes are:

* "Days since last update: N", because 2.0.0 was released very recently.
  This release exists to fix the defect described above; we are glad to
  delay it if the CRAN team prefers.
* "unable to verify current time" — no network clock in the local
  sandbox.
* "Skipping checking HTML validation: no command 'tidy' found" and
  "package 'V8' unavailable" — the optional HTML-manual validation tools
  are not installed locally.

None of the three appears on the GitHub Actions runs.

## Tests

The testthat suite contains 24 blocks and 485 expectations, all passing,
including the new row-permutation invariance tests for `Uniform()`.

## Reverse dependencies

None: no packages on CRAN depend on, import or suggest PGM2.
