# partial-spreads.R -- Proposition 2 and Corollary 4 (factor selection) of
#   M. Laib, "Affine-geometry uniform designs: coincidence optimality,
#   projection spectra and factor selection" (submitted to JSPI, 2026).
#
# Stage D_2 of PG(4, 2) is U(16; 4^35) with d = 2 = m/2.  This script
#   1. finds a 2-spread of GF(2)^4 among the 35 factors (five pairwise
#      orthogonal columns);
#   2. for k = 2, ..., 5 columns of that spread, checks Corollary 4: the
#      coincidence counts lie in {0, 1}, the sub-array is an
#      OA(16, k, 4, 2) with E(f_NOD) = 0, and DD^2 (a = 1, b = 1/2) equals
#      the two-value bound of Corollary 1 (equality for k <= 4 in the
#      nonintegral regime, exact equidistance for the full spread k = 5);
#   3. checks Proposition 2 exhaustively on all choose(35, 3) = 6545
#      three-column subsets: the two-valued ones ({0, 1}) are exactly the
#      partial spreads, and none of them is a subspace 1-design;
#   4. checks that 50 random five-column subsets fail equidistance, as
#      reported in Section 5 of the paper.
# It writes partial-spreads-results.csv and prints a summary.  Run from
# this directory:  Rscript partial-spreads.R      (under a minute)

.libPaths(c(Sys.glob("~/R/library"), .libPaths()))
suppressMessages(library(PGM2))

coinc <- function(M) {                  # off-diagonal coincidence counts
  C <- matrix(0, nrow(M), nrow(M))
  for (j in seq_len(ncol(M))) C <- C + outer(M[, j], M[, j], "==")
  C[upper.tri(C) | lower.tri(C)]
}
orthogonal_pair <- function(a, b, N, l) all(table(a, b) == N / l^2)
is_oa2 <- function(M, l) {              # orthogonal array of strength 2
  k <- ncol(M); N <- nrow(M)
  for (a in seq_len(k - 1)) for (b in (a + 1):k)
    if (!orthogonal_pair(M[, a], M[, b], N, l)) return(FALSE)
  TRUE
}
dd2 <- function(M, l, a = 1, b = 1/2) { # discrete discrepancy, eq. (3)
  N <- nrow(M); s <- ncol(M); off <- coinc(M)
  -((a + (l - 1) * b) / l)^s + (N * a^s + sum(a^off * b^(s - off))) / N^2
}
dd2_bound <- function(N, s, l, lam, a = 1, b = 1/2) {  # Corollary 1, two-value form
  th <- floor(lam); f <- lam - th
  -((a + (l - 1) * b) / l)^s + a^s / N +
    (N - 1) / N * ((1 - f) * a^th * b^(s - th) + f * a^(th + 1) * b^(s - th - 1))
}
enod <- function(M, l) {                # E(f_NOD), definition in Section 3.4
  N <- nrow(M); s <- ncol(M); tot <- 0
  for (c1 in seq_len(s - 1)) for (c2 in (c1 + 1):s) {
    tab <- table(factor(M[, c1], levels = 1:l), factor(M[, c2], levels = 1:l))
    tot <- tot + sum((tab - N / l^2)^2)
  }
  tot / choose(s, 2)
}

m <- 4; q <- 2; n <- 2; d <- m - n
D <- Qn(m, n, q)$UD; N <- nrow(D); s <- ncol(D); l <- q^n
stopifnot(N == 16, s == 35, l == 4)

# 1. a spread: greedily grow a set of pairwise orthogonal columns to size q^d + 1
disj <- outer(seq_len(s), seq_len(s), Vectorize(function(i, j)
  i != j && orthogonal_pair(D[, i], D[, j], N, l)))
spread <- NULL
for (c1 in seq_len(s)) {
  cand <- c1
  for (c2 in seq_len(s)) if (all(disj[c2, cand])) cand <- c(cand, c2)
  if (length(cand) >= q^d + 1) { spread <- cand[seq_len(q^d + 1)]; break }
}
stopifnot(!is.null(spread))
cat("Spread found: columns", paste(spread, collapse = " "), "\n\n")

# 2. Corollary 4 along the spread
rows <- list()
for (k in 2:(q^d + 1)) {
  S <- D[, spread[seq_len(k)]]; dl <- coinc(S)
  lam <- k * (q^d - 1) / (q^m - 1)
  two_valued <- all(dl %in% c(floor(lam), ceiling(lam)))
  rows[[k]] <- data.frame(k = k, coincidence_values = paste(sort(unique(dl)), collapse = "/"),
                          mean = lam, two_valued = two_valued, equidistant = length(unique(dl)) == 1,
                          OA_strength2 = is_oa2(S, l), efnod = enod(S, l),
                          dd2 = dd2(S, l), dd2_bound = dd2_bound(N, k, l, lam),
                          attains_bound = abs(dd2(S, l) - dd2_bound(N, k, l, lam)) < 1e-12)
  with(rows[[k]], cat(sprintf("k=%d: coincidences {%s}, mean %.3f, two-valued %s, equidistant %s, OA(16,%d,4,2) %s, E(fNOD)=%g, DD2=%.8f = bound %s\n",
                              k, coincidence_values, mean, two_valued, equidistant, k, OA_strength2, efnod, dd2, attains_bound)))
}
res <- do.call(rbind, rows)
write.csv(res, "partial-spreads-results.csv", row.names = FALSE)

# 3. Proposition 2 on every 3-subset: two-valued {0,1}  <=>  partial spread
cmb <- combn(s, 3); n_two <- 0; n_ps <- 0; n_both <- 0; n_1design <- 0
for (i in seq_len(ncol(cmb))) {
  S <- D[, cmb[, i]]; dl <- coinc(S)
  two <- all(dl %in% c(0, 1)); ps <- is_oa2(S, l)
  n_two <- n_two + two; n_ps <- n_ps + ps; n_both <- n_both + (two && ps)
  n_1design <- n_1design + (length(unique(dl)) == 1)
}
cat(sprintf("\n3-subsets: %d total; two-valued {0,1}: %d; partial spreads: %d; both: %d; equidistant (subspace 1-designs): %d\n",
            ncol(cmb), n_two, n_ps, n_both, n_1design))
stopifnot(n_two == n_ps, n_both == n_two, n_1design == 0)

# 4. random 5-subsets are generally not equidistant (Section 5)
set.seed(20260827); fails <- 0
for (t in 1:50) { S <- D[, sample(s, 5)]; if (length(unique(coinc(S))) > 1) fails <- fails + 1 }
cat(sprintf("Random 5-column subsets failing equidistance: %d / 50\n", fails))

cat("\nAll checks passed:", all(res$two_valued & res$OA_strength2 & res$attains_bound & res$efnod == 0) &&
      res$equidistant[nrow(res)] && !any(res$equidistant[-nrow(res)]), "\n")
cat("Written: partial-spreads-results.csv\n")
