# Full validation of the equidistance / discrete-discrepancy-bound and
# refinement claims, on ALL nine configurations tabulated in the
# accompanying paper (all stages of PG(3,2), PG(4,2), PG(3,3), plus
# PG(2,3) and PG(2,5)).

nine_configs <- list(
  c(3, 1, 2), c(3, 2, 2),
  c(4, 1, 2), c(4, 2, 2), c(4, 3, 2),
  c(2, 1, 3), c(3, 1, 3), c(3, 2, 3),
  c(2, 1, 5)
)

test_that("all nine tabulated Qn designs are equidistant with the closed-form delta", {
  for (cs in nine_configs) {
    m <- cs[1]; n <- cs[2]; p <- cs[3]
    Q <- Qn(m, n, p)
    co <- c()
    for (i in seq_len(Q$V - 1)) for (k in (i + 1):Q$V)
      co <- c(co, sum(Q$UD[i, ] == Q$UD[k, ]))
    expect_equal(unique(co), Q$Lambda,
                 label = sprintf("PG(%d,%d) n=%d delta", m, p, n))
    expect_equal(Q$Lambda, PGM2:::gaussian_binom(m - 1, m - n - 1, p))
  }
})

test_that("all nine tabulated Qn designs attain the discrete-discrepancy lower bound", {
  for (cs in nine_configs) {
    m <- cs[1]; n <- cs[2]; p <- cs[3]
    Q <- Qn(m, n, p)
    expect_equal(dd2(Q$UD, Q$Levels), dd2_lb(Q$V, Q$R, Q$Levels),
                 label = sprintf("PG(%d,%d) n=%d DD bound", m, p, n))
  }
})

test_that("exhaustive refinement: PG(4,2) stages 2 -> 3 (all subspace pairs)", {
  q2 <- Qn(4, 2); q3 <- Qn(4, 3)
  refines <- function(fine, coarse)
    all(rowSums(table(fine, coarse) > 0) == 1)
  # every stage-2 factor must be refined by at least one stage-3 factor,
  # and the refinement counts must match Proposition 2:
  # each dim-2 W contains (2^2-1)/(2-1) = 3 dim-1 subspaces W'.
  for (j2 in seq_len(q2$R)) {
    hits <- sum(vapply(seq_len(q3$R), function(j3)
      refines(q3$UD[, j3], q2$UD[, j2]), logical(1)))
    expect_equal(hits, 3, label = sprintf("stage-2 factor %d refiners", j2))
  }
  # each dim-1 W' is contained in (2^(4-2+1)-1)/(2-1) = 7 dim-2 subspaces
  for (j3 in seq_len(q3$R)) {
    hits <- sum(vapply(seq_len(q2$R), function(j2)
      refines(q3$UD[, j3], q2$UD[, j2]), logical(1)))
    expect_equal(hits, 7, label = sprintf("stage-3 factor %d coarsenings", j3))
  }
})
