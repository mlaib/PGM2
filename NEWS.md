# PGM2 2.0.0

Major generalisation: the whole construction now works over GF(p) for
**any prime p**, as in the theory of Boudraa, Gheribi-Aoulmi & Laib (2013).
Versions <= 1.2 implemented only p = 2.

## New features

* New function `Qn(m, n, p = 2)`: builds the reduced resolvable design
  Q*_n of stage n directly, with its associated uniform design
  U(p^m, (p^n)^r) — the design of the 2013 paper's Example 3, previously
  constructible only by hand. Any two runs coincide in a constant number
  of factors (equidistance), so every stage attains the
  discrete-discrepancy lower bound of Fang et al. (2004), and levels
  refine across stages.
* `BIB(m, p = 2)` and `Steps(m, n, stage, p = 2)` gain a `p` argument
  (prime; checked). `Gen()` and `Resolvable()` are now order-agnostic:
  block sizes are derived from the data instead of binary-only formulas.
* `BIB()` additionally returns `Lambda`, the concurrence parameter.
* A test suite validates every construction against the parameters
  published in Tables 1 (p = 2) and 2 (p = 3) of the 2013 paper, checks
  balance (constant r and lambda over all pairs), actual resolvability
  (partition into parallel classes) and level balance of the uniform
  designs.
* A vignette reproduces all worked examples and both tables of the paper.

## Backward compatibility

* For p = 2 (the default), `BIB()`, `Gen()`, `Resolvable()` and
  `Uniform()` return exactly the same designs as version 1.2 (this is
  enforced by a regression test against the 1.2 algorithm).
* `Steps()` now returns a tidy named list (`BIB1`, `BIBg`, `Resolvables`,
  `UDs`, each stage a proper sub-list) instead of the flat, partially
  unnamed splice of previous versions.
* `Uniform()` no longer emits recycling warnings (an internal placeholder
  row was removed); results are unchanged.
* Input validation: `BIB()` rejects non-prime `p` and `m < 2` with clear
  error messages.

# PGM2 1.2

* Last GF(2)-only release (CRAN, 2025-05-27).
