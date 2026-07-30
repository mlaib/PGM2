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
