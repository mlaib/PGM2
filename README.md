# PGM2

<!-- badges: start -->
[![Downloads from the RStudio CRAN mirror](http://cranlogs.r-pkg.org/badges/grand-total/PGM2)](https://cran.r-project.org/package=PGM2)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1161718.svg)](https://doi.org/10.5281/zenodo.1161718)
<!-- badges: end -->

Recursive construction of **nested resolvable designs** and their
**associated uniform designs** from finite projective geometries
PG(m, p) over a Galois field GF(p) of **any prime order p**.

The package implements and generalises the method of:

> A. Boudraa, Z. Gheribi-Aoulmi and M. Laib (2013). *Recursive method for
> construction of resolvable nested designs and uniform designs
> associated.* International Journal of Research and Reviews in Applied
> Sciences, 17(2), 167–176.

The paper states its theory for any prime p; versions ≤ 1.2 of the package
implemented only p = 2. Since version 2.0 the whole chain

```
PG(m, p)  →  BIB  →  next generations (Gen)  →  RBIB (Resolvable)  →  UD (Uniform)
```

works for every prime p, with results validated against the parameter
tables of the paper (p = 2 and p = 3) by the test suite.

## Installation

```r
# from CRAN
install.packages("PGM2")

# development version
# remotes::install_github("mlaib/PGM2")
```

## Quick start

```r
library(PGM2)

## Over GF(2) (identical to versions <= 1.2):
X <- BIB(4)                  # BIBD (31, 15, 7) from PG(4, 2)
Y <- Resolvable(1, X$BIB)    # RBIBD (16, 30, 15, 8, 7)
Uniform(Y$RBIB)$UD           # uniform design U(16, 2^15)

## Over GF(3):
X3 <- BIB(2, p = 3)          # BIBD (13, 4, 1): projective plane of order 3
Y3 <- Resolvable(1, X3$BIB)  # RBIBD (9, 12, 4, 3, 1): AG(2, 3)
Uniform(Y3$RBIB)$UD          # uniform design U(9, 3^4)

## Every stage of the recursion at once:
s <- Steps(4, 1)             # PG(4, 2): stages S1..S4
s3 <- Steps(3, 1, p = 3)     # PG(3, 3)
```

See the vignette, `vignette("PGM2-paper")`, which reproduces the worked
examples of the 2013 paper together with the first-generation BIBD, RBIBD
and uniform-design rows of both of its parameter tables, and selected
reduced-design rows.

License: GPL-3
