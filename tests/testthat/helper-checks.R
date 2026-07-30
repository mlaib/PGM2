# Combinatorial validators used across tests.

# lambda over all treatment pairs; a valid BIBD has a single value.
pair_lambdas <- function(mat) {
  v <- sort(unique(as.vector(mat)))
  unique(as.vector(utils::combn(v, 2, function(pr)
    sum(apply(mat, 1, function(b) all(pr %in% b))))))
}

replications <- function(mat) {
  v <- sort(unique(as.vector(mat)))
  unique(sapply(v, function(x) sum(mat == x)))
}

# Greedy parallel-class extraction; TRUE iff blocks partition into classes.
is_resolvable <- function(mat) {
  v <- sort(unique(as.vector(mat)))
  W <- mat
  classes <- 0
  while (nrow(W) > 0) {
    vv <- v
    repeat {
      cand <- which(apply(W, 1, function(b) all(b %in% vv)))
      if (!length(cand)) break
      vv <- setdiff(vv, W[cand[1], ])
      W <- W[-cand[1], , drop = FALSE]
    }
    if (length(vv)) return(FALSE)
    classes <- classes + 1
  }
  classes
}

# The BIB algorithm of PGM2 1.2, verbatim (p = 2 only), as the
# backward-compatibility reference.
bib_v12 <- function(m) {
  p <- 2
  lin <- p^(m + 1)
  coll <- m + 1
  prg <- matrix(nrow = lin, ncol = coll)
  for (j in 1:coll) prg[, j] <- c(rep(0, lin / (p^j)), rep(1, lin / (p^j)))
  prg <- prg[-1, ]
  t <- dim(prg)[1]
  fill <- function(i, mat) {
    x <- c(); mm <- c(); w <- which(mat[i, ] == 1)
    for (j in 1:t) {
      for (k in w) x <- c(x, prg[j, k])
      if (sum(x) %% 2 == 0) mm <- c(mm, j)
      x <- c()
    }
    mm
  }
  k <- (t - 1) / 2
  bie <- matrix(nrow = t, ncol = k)
  for (i in 1:t) bie[i, ] <- fill(i, prg)
  bie
}
