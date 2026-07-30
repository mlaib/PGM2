# Every parameter below is taken from Tables 1 and 2 of
# Boudraa, Gheribi-Aoulmi & Laib (2013), IJRRAS 17(2), 167-176.

paper_cases <- list(
  list(m = 2, p = 2, bib = c(7, 3, 1),    rbib = c(4, 6, 3, 2, 1)),
  list(m = 3, p = 2, bib = c(15, 7, 3),   rbib = c(8, 14, 7, 4, 3)),
  list(m = 4, p = 2, bib = c(31, 15, 7),  rbib = c(16, 30, 15, 8, 7)),
  list(m = 2, p = 3, bib = c(13, 4, 1),   rbib = c(9, 12, 4, 3, 1)),
  list(m = 3, p = 3, bib = c(40, 13, 4),  rbib = c(27, 39, 13, 9, 4)),
  list(m = 4, p = 3, bib = c(121, 40, 13), rbib = c(81, 120, 40, 27, 13))
)

test_that("BIB reproduces the (v, k, lambda) of the paper's tables", {
  for (cs in paper_cases) {
    X <- BIB(cs$m, cs$p)
    expect_equal(c(X$V, X$K, X$Lambda), cs$bib,
                 label = sprintf("PG(%d,%d)", cs$m, cs$p))
    expect_equal(X$B, X$V)  # symmetric
    expect_equal(X$R, X$K)
  }
})

test_that("BIB output is a valid BIBD (constant lambda and r over all pairs)", {
  for (cs in paper_cases[1:5]) {  # skip the largest case for speed
    X <- BIB(cs$m, cs$p)
    expect_length(replications(X$BIB), 1)
    expect_equal(pair_lambdas(X$BIB), cs$bib[3])
  }
})

test_that("Resolvable reproduces the (v, b, r, k, lambda) of the paper's tables", {
  for (cs in paper_cases) {
    Y <- Resolvable(1, BIB(cs$m, cs$p)$BIB)
    expect_equal(c(Y$V, Y$B, Y$R, Y$K), cs$rbib[1:4],
                 label = sprintf("PG(%d,%d)", cs$m, cs$p))
  }
})

test_that("Resolvable output is balanced and actually resolvable", {
  for (cs in paper_cases[1:5]) {
    Y <- Resolvable(1, BIB(cs$m, cs$p)$BIB)
    expect_length(replications(Y$RBIB), 1)
    expect_equal(pair_lambdas(Y$RBIB), cs$rbib[5])
    classes <- is_resolvable(Y$RBIB)
    expect_equal(classes, Y$R)  # r parallel classes of p blocks each
  }
})

test_that("Gen extracts the BIBD of PG(m-1, p)", {
  for (cs in paper_cases[c(2, 3, 5)]) {  # m >= 3
    G <- Gen(1, BIB(cs$m, cs$p)$BIB)
    prev <- BIB(cs$m - 1, cs$p)
    expect_equal(c(G$V, G$B, G$R, G$K), c(prev$V, prev$B, prev$R, prev$K))
    expect_equal(pair_lambdas(G$BIB2), prev$Lambda)
  }
})

test_that("Uniform builds a U(p^m, p^r): balanced levels in every column", {
  for (cs in paper_cases[1:5]) {
    Y <- Resolvable(1, BIB(cs$m, cs$p)$BIB)
    ud <- Uniform(Y$RBIB)
    expect_equal(ud$n, cs$p^cs$m)
    expect_equal(ud$F, Y$R)
    for (j in seq_len(ud$F)) {
      tab <- table(ud$UD[, j])
      expect_equal(sort(as.integer(names(tab))), seq_len(cs$p))
      expect_length(unique(as.vector(tab)), 1)  # each level equally often
    }
  }
})

test_that("Steps returns all stages with consistent structure", {
  s <- Steps(4, 1, p = 2)
  expect_named(s, c("BIB1", "BIBg", "Resolvables", "UDs"))
  expect_length(s$Resolvables, 3)  # stages n = 1, 2, 3
  expect_length(s$UDs, 3)
  expect_length(s$BIBg, 2)
  # chain: each generation is the BIB of the geometry one dimension lower
  expect_equal(s$BIBg[[1]]$V, BIB(3)$V)
  expect_equal(s$BIBg[[2]]$V, BIB(2)$V)

  s3 <- Steps(3, 1, p = 3)
  expect_length(s3$Resolvables, 2)
  expect_equal(s3$UDs[[1]]$n, 27)
  expect_equal(s3$UDs[[2]]$n, 9)

  onlyS4 <- Steps(3, 1, "S4")
  expect_named(onlyS4, "UDs")
})

test_that("p = 2 reproduces PGM2 1.2 exactly (backward compatibility)", {
  for (m in 2:4) {
    expect_identical(unname(BIB(m)$BIB), unname(bib_v12(m)),
                     label = sprintf("BIB(%d)", m))
  }
})

test_that("non-prime and invalid orders are rejected", {
  expect_error(BIB(3, p = 4), "prime")
  expect_error(BIB(3, p = 6), "prime")
  expect_error(BIB(3, p = 1), "prime")
  expect_error(Steps(3, 1, p = 9), "prime")
  expect_error(BIB(1), "m")
})

test_that("block indices are validated with informative messages", {
  bib <- BIB(3)$BIB                       # 15 blocks
  expect_error(Gen(99, bib), "between 1 and 15")
  expect_error(Resolvable(99, bib), "between 1 and 15")
  expect_error(Resolvable(0, bib), "between 1 and 15")
  expect_error(Resolvable(-1, bib), "between 1 and 15")
  expect_error(Gen(2.5, bib), "between 1 and 15")
  expect_error(Gen(c(1, 2), bib), "between 1 and 15")
  expect_error(Gen(NA, bib), "between 1 and 15")
  expect_error(Resolvable(1, bib[1, ]), "matrix of at least two blocks")
  # valid indices other than 1 work
  expect_equal(Resolvable(15, bib)$V, 8)
  expect_equal(Gen(15, bib)$V, 7)
})

test_that("Steps rejects block indices larger than the last stage", {
  # PG(4,2) chain has 31 -> 15 -> 7 blocks; the same index is used throughout
  expect_error(Steps(4, 8), "between 1 and 7")
  expect_error(Steps(4, 31), "between 1 and 7")
  expect_error(Steps(3, 0), "between 1 and 7")
  expect_error(Steps(3, 1, p = 3)[[1]], NA)          # 13 allowed for p = 3
  expect_error(Steps(3, 13, p = 3), NA)
  expect_error(Steps(3, 14, p = 3), "between 1 and 13")
  # every admissible index yields the full chain
  for (n in 1:7) expect_length(Steps(4, n)$UDs, 3)
})

test_that("Uniform() does not depend on the order of the blocks", {
  Y <- Resolvable(1, BIB(3)$BIB)
  base <- Uniform(Y$RBIB)
  coinc <- function(D) {
    o <- c()
    for (i in seq_len(nrow(D) - 1)) for (k in (i + 1):nrow(D))
      o <- c(o, sum(D[i, ] == D[k, ]))
    sort(o)
  }
  for (s in 1:20) {
    set.seed(s)
    P <- Y$RBIB[sample(nrow(Y$RBIB)), ]
    U <- Uniform(P)
    expect_equal(U$n, base$n)
    expect_equal(U$F, base$F)
    expect_equal(coinc(U$UD), coinc(base$UD))
  }
})

test_that("Uniform() resolves designs a first-fit greedy cannot", {
  # the 28-block design of the 2013 paper's Example 3, row-permuted
  bib <- BIB(3)$BIB
  mat <- NULL
  for (i in 1:15) mat[[i]] <- Gen(i, bib)$BIB2
  x <- Reduce("rbind", mat)
  v <- bib[1, ]
  for (i in seq_len(nrow(x))) for (j in seq_len(ncol(x)))
    if (any(x[i, j] == v)) x[i, j] <- 0
  for (i in nrow(x):1) if (all(x[i, ] == 0)) x <- x[-i, ]
  s0 <- x[1, ]; s0 <- s0[s0 > 0]
  x1 <- matrix(nrow = nrow(x), ncol = length(s0))
  for (i in seq_len(nrow(x))) x1[i, ] <- x[i, ][x[i, ] > 0]
  A <- unique(x1)
  expect_equal(dim(A), c(28L, 2L))
  for (s in 1:20) {
    set.seed(s)
    U <- Uniform(A[sample(nrow(A)), ])
    expect_equal(U$F, 7)
    expect_equal(U$n, 8)
  }
  # a classical resolvable design from outside the package: 1-factorisation of K6
  U6 <- Uniform(t(utils::combn(6, 2)))
  expect_equal(U6$n, 6)
  expect_equal(U6$F, 5)
  expect_true(all(vapply(seq_len(U6$F),
    function(j) length(unique(table(U6$UD[, j]))) == 1L, logical(1))))
})

test_that("Uniform() rejects designs that cannot be resolvable", {
  expect_error(Uniform(t(utils::combn(5, 2))), "cannot be resolvable")
  # 4 treatments, block size 2 (so divisibility passes), but block 1 repeats
  expect_error(Uniform(matrix(c(1, 1, 2, 3, 4, 2), nrow = 3, byrow = TRUE)),
               "repeated treatment")
})

test_that("Steps validates the stage argument", {
  expect_error(Steps(3, 1, stage = "not-a-stage"), "stage")
  expect_error(Steps(3, 1, stage = character(0)), "stage")
  expect_error(Steps(3, 1, stage = c("S1", "S9")), "stage")
  expect_named(Steps(3, 1, stage = c("S1", "S1")), "BIB1")
})
