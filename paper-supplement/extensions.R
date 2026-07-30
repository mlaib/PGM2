# Round-3 extensions: independent verification and computation of
#  A1: E(f_NOD) — direct vs closed form for every stage; margins vs the
#      archived optimised comparators; DD^2-excess of comparators (C1).
#  A2: spread sub-selection example for q=2, m=4, d=2: U(16; 4^5).
#  A3: exact CD2 closed form at q=2, stage 1 (and its coincidence-only form).
# Also: catalogue data. Run from track2/ AFTER symmetric-cd2.R.
.libPaths(c(Sys.glob("~/R/library"), .libPaths()))
library(PGM2)

gaussb <- function(m, k, p) PGM2:::gaussian_binom(m, k, p)
lv2u <- function(D, q) (D - 0.5) / q
cd2 <- function(D, q) {
  U <- lv2u(D, q); n <- nrow(U); s <- ncol(U)
  t2 <- sum(apply(U, 1, function(u) prod(1 + 0.5 * abs(u - 0.5) - 0.5 * (u - 0.5)^2)))
  t3 <- 0
  for (i in seq_len(n)) { ai <- abs(U[i, ] - 0.5)
    for (k in seq_len(n)) { ak <- abs(U[k, ] - 0.5)
      t3 <- t3 + prod(1 + 0.5 * ai + 0.5 * ak - 0.5 * abs(U[i, ] - U[k, ])) } }
  sqrt((13 / 12)^s - 2 / n * t2 + t3 / n^2)
}
coinc_counts <- function(D) {   # off-diagonal coincidence vector (ordered pairs /2)
  n <- nrow(D); out <- integer(n * (n - 1) / 2); idx <- 0
  for (i in seq_len(n - 1)) for (k in (i + 1):n) {
    idx <- idx + 1; out[idx] <- sum(D[i, ] == D[k, ]) }
  out
}
dd2 <- function(D, l, a = 1, b = 0.5) {
  n <- nrow(D); s <- ncol(D); co <- coinc_counts(D)
  -((a + (l - 1) * b) / l)^s + (n * a^s + 2 * sum(a^co * b^(s - co))) / n^2
}
dd2_lb <- function(n, s, l, a = 1, b = 0.5) {
  lam <- s * (n / l - 1) / (n - 1)
  -((a + (l - 1) * b) / l)^s + a^s / n + (n - 1) / n * a^lam * b^(s - lam)
}
# E(f_NOD): average over unordered column pairs of sum_(a,b) (n_ab - N/l^2)^2
enod_direct <- function(D, l) {
  s <- ncol(D); N <- nrow(D); tot <- 0
  for (c1 in seq_len(s - 1)) for (c2 in (c1 + 1):s) {
    tab <- table(factor(D[, c1], 1:l), factor(D[, c2], 1:l))
    tot <- tot + sum((tab - N / l^2)^2)
  }
  tot / (s * (s - 1) / 2)
}
# closed form via coincidence distribution (derived independently):
# sum_{c!=d} sum_ab n_ab^2 = sum_{i!=k} delta(delta-1) + N s(s-1)
enod_from_coinc <- function(co, N, s, l) {
  (2 * sum(co * (co - 1)) + N * s * (s - 1) - s * (s - 1) * N^2 / l^2) / (s * (s - 1))
}
enod_equidistant <- function(N, s, l, lam)
  (N * (N - 1) * lam * (lam - 1) + N * s * (s - 1) - s * (s - 1) * N^2 / l^2) / (s * (s - 1))

configs <- list(c(3,1,2), c(3,2,2), c(4,1,2), c(4,2,2), c(4,3,2),
                c(5,1,2), c(5,2,2), c(5,3,2), c(5,4,2),
                c(2,1,3), c(3,1,3), c(3,2,3), c(2,1,5))

cat("=========== A1: E(f_NOD) + C1: DD^2 excess ===========\n")
res <- NULL
for (cs in configs) {
  m <- cs[1]; n <- cs[2]; p <- cs[3]
  Q <- Qn(m, n, p); l <- Q$Levels; N <- Q$V; s <- Q$R
  lam <- Q$Lambda
  e_direct <- enod_direct(Q$UD, l)
  e_closed <- enod_equidistant(N, s, l, lam)
  stopifnot(abs(e_direct - e_closed) < 1e-8)
  rds <- sprintf("optimized-designs/pg%d_%d_n%d.rds", m, p, n)
  ex <- readRDS(rds)
  lb <- dd2_lb(N, s, l)
  cdd <- vapply(ex$comparator_optimized, function(D) dd2(D, l), numeric(1))
  ddexc <- (cdd - lb) / abs(lb) * 100
  cenod <- vapply(ex$comparator_optimized, function(D) {
    co <- coinc_counts(D); enod_from_coinc(co, N, s, l) }, numeric(1))
  emargin_min <- (min(cenod) - e_closed) / e_closed * 100
  emargin_med <- (median(cenod) - e_closed) / e_closed * 100
  cat(sprintf("PG(%d,%d) n=%d: E(fNOD)=%.4f (closed=direct OK) | comp margin min=%.1f%% med=%.1f%% | DDexc min=%.3g%% med=%.3g%%\n",
      m, p, n, e_closed, emargin_min, emargin_med, min(ddexc), median(ddexc)))
  res <- rbind(res, data.frame(m=m, p=p, n=n, N=N, s=s, l=l, lambda=lam,
    enod=e_closed, enod_margin_min=emargin_min, enod_margin_med=emargin_med,
    ddexc_min=min(ddexc), ddexc_med=median(ddexc)))
}
write.csv(res, "enod-ddexc-results.csv", row.names = FALSE)

cat("\n=========== A3: exact CD2 at q=2, stage 1 ===========\n")
for (m in 3:5) {
  Q <- Qn(m, 1, 2); N <- Q$V; s <- Q$R; lam <- Q$Lambda
  closed <- sqrt((13/12)^s - 2*(35/32)^s + (5/4)^s/N + (1 - 1/N)*(5/4)^lam)
  direct <- cd2(Q$UD, 2)
  cat(sprintf("m=%d: closed=%.12f direct=%.12f agree=%s\n",
      m, closed, direct, isTRUE(all.equal(closed, direct))))
  # empirical check that no random U-type design goes below the closed form
  set.seed(99)
  best <- Inf
  for (i in 1:500) {
    D <- vapply(seq_len(s), function(j) sample(rep(1:2, each = N/2)), integer(N))
    best <- min(best, cd2(D, 2))
  }
  cat(sprintf("   best of 500 random U(%d;2^%d): %.6f (>= closed bound: %s)\n",
      N, s, best, best >= closed - 1e-12))
}

cat("\n=========== A2: spread sub-selection U(16; 4^5) ===========\n")
Ws <- PGM2:::subspaces(4, 2, 2)                 # order matches Qn(4,2) columns
keys <- lapply(Ws, function(W) {
  W <- W[rowSums(W) > 0, , drop = FALSE]
  sort(apply(W, 1, paste, collapse = ","))
})
# backtracking search for a spread: 5 subspaces partitioning the 15 nonzero pts
allpts <- sort(unique(unlist(keys)))
found <- NULL
search <- function(chosen, used) {
  if (length(chosen) == 5) { found <<- chosen; return(TRUE) }
  start <- if (length(chosen)) max(chosen) + 1 else 1
  for (j in start:length(keys)) {
    if (!any(keys[[j]] %in% used)) {
      if (search(c(chosen, j), c(used, keys[[j]]))) return(TRUE)
    }
  }
  FALSE
}
search(integer(0), character(0))
cat("spread columns:", found, "\n")
Q42 <- Qn(4, 2)
Dsub <- Q42$UD[, found]
co <- coinc_counts(Dsub)
cat(sprintf("sub-design U(%d; %d^%d): coincidences unique = {%s} (expect {1})\n",
    nrow(Dsub), Q42$Levels, ncol(Dsub), paste(unique(co), collapse = ",")))
cat(sprintf("DD2 = %.10f | bound = %.10f | attained = %s\n",
    dd2(Dsub, 4), dd2_lb(16, 5, 4), isTRUE(all.equal(dd2(Dsub, 4), dd2_lb(16, 5, 4)))))
cat(sprintf("E(fNOD) sub = %.4f | closed(lam=1) = %.4f\n",
    enod_direct(Dsub, 4), enod_equidistant(16, 5, 4, 1)))
# random 5-column subsets generally lose equidistance:
set.seed(7); bad <- 0
for (i in 1:50) {
  cols <- sample(35, 5)
  if (length(unique(coinc_counts(Q42$UD[, cols]))) > 1) bad <- bad + 1
}
cat(sprintf("random 5-column subsets non-equidistant: %d/50\n", bad))

cat("\n=========== catalogue data ===========\n")
cata <- NULL
for (p in c(2, 3, 5)) for (m in 2:5) {
  if (p^m > 300) next
  for (n in seq_len(m - 1)) {
    s <- gaussb(m, m - n, p); lam <- gaussb(m - 1, m - n - 1, p)
    cata <- rbind(cata, data.frame(q = p, m = m, n = n, N = p^m, s = s,
      levels = p^n, lambda = lam,
      enod = enod_equidistant(p^m, s, p^n, lam)))
  }
}
print(cata, row.names = FALSE)
write.csv(cata, "catalogue.csv", row.names = FALSE)
cat("done\n")
