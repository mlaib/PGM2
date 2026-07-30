# Q*_n parameters from the 2013 paper's Tables 1 (p = 2) and 2 (p = 3),
# plus equidistance, level balance and the refinement property.

qn_cases <- list(
  list(m = 3, n = 2, p = 2, pars = c(8, 28, 7, 2, 1)),
  list(m = 4, n = 2, p = 2, pars = c(16, 140, 35, 4, 7)),
  list(m = 4, n = 3, p = 2, pars = c(16, 120, 15, 2, 1)),
  list(m = 3, n = 2, p = 3, pars = c(27, 117, 13, 3, 1))
)

test_that("Qn reproduces the Q*_n parameters of the paper's tables", {
  for (cs in qn_cases) {
    Q <- Qn(cs$m, cs$n, cs$p)
    expect_equal(c(Q$V, Q$B, Q$R, Q$K, Q$Lambda), cs$pars,
                 label = sprintf("PG(%d,%d) n=%d", cs$m, cs$p, cs$n))
    expect_equal(Q$B, Q$R * Q$Levels)
    expect_equal(dim(Q$UD), c(Q$V, Q$R))
  }
})

test_that("Qn uniform designs are level-balanced and equidistant", {
  for (cs in qn_cases[c(1, 4)]) {
    Q <- Qn(cs$m, cs$n, cs$p)
    for (j in seq_len(Q$R)) {
      tab <- table(Q$UD[, j])
      expect_length(tab, Q$Levels)
      expect_length(unique(as.vector(tab)), 1)
    }
    co <- c()
    for (i in seq_len(Q$V - 1)) for (k in (i + 1):Q$V)
      co <- c(co, sum(Q$UD[i, ] == Q$UD[k, ]))
    expect_equal(unique(co), Q$Lambda)
  }
})

test_that("Qn stage 1 is the stage-1 design of Resolvable + Uniform", {
  for (cfg in list(c(3, 2), c(2, 3))) {
    m <- cfg[1]; p <- cfg[2]
    Q <- Qn(m, 1, p)
    U <- Uniform(Resolvable(1, BIB(m, p)$BIB)$RBIB)
    expect_equal(Q$V, U$n)
    expect_equal(Q$R, U$F)
    # same coincidence profile (designs equal up to column/level relabel)
    coinc <- function(D) {
      out <- c()
      for (i in seq_len(nrow(D) - 1)) for (k in (i + 1):nrow(D))
        out <- c(out, sum(D[i, ] == D[k, ]))
      sort(out)
    }
    expect_equal(coinc(Q$UD), coinc(U$UD))
  }
})

test_that("levels refine across stages with the exact multiplicities (PG(3,2))", {
  Q1 <- Qn(3, 1); Q2 <- Qn(3, 2)
  refines <- function(fine, coarse)
    all(rowSums(table(fine, coarse) > 0) == 1)
  # each stage-1 factor (dim-2 subspace W) is refined by exactly
  # (2^2 - 1)/(2 - 1) = 3 stage-2 factors (dim-1 subspaces W' of W)
  for (j1 in seq_len(Q1$R)) {
    hits <- sum(vapply(seq_len(Q2$R), function(j2)
      refines(Q2$UD[, j2], Q1$UD[, j1]), logical(1)))
    expect_equal(hits, 3, label = sprintf("stage-1 factor %d refiners", j1))
  }
  # each stage-2 factor refines exactly (2^(3-2+1) - 1)/(2 - 1) = 3
  # stage-1 factors (dim-2 subspaces containing W')
  for (j2 in seq_len(Q2$R)) {
    hits <- sum(vapply(seq_len(Q1$R), function(j1)
      refines(Q2$UD[, j2], Q1$UD[, j1]), logical(1)))
    expect_equal(hits, 3, label = sprintf("stage-2 factor %d coarsenings", j2))
  }
})

test_that("Qn validates its arguments", {
  expect_error(Qn(3, 3), "n")
  expect_error(Qn(3, 0), "n")
  expect_error(Qn(1, 1), "m")
  expect_error(Qn(3, 1, p = 4), "prime")
})
