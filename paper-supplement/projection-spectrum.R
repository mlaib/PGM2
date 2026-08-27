# projection-spectrum.R -- Theorem 1 (two-factor projection spectrum) of
#   M. Laib, "Affine-geometry uniform designs: coincidence optimality,
#   projection spectra and factor selection" (submitted to JSPI, 2026).
#
# For every configuration of Table 1 of the paper, and for every pair of
# factors W, W' of the stage design D_n = Qn(m, n, q)$UD, this script
# checks against the design itself that
#   (i)   the joint level table has exactly q^(m-r) occupied cells, each
#         equal to q^r, where r = dim(W \cap W');
#   (ii)  the pair's contribution Phi(W, W') = sum_{a,b} (n_ab - N/l^2)^2
#         equals q^(m+r) - q^(2d), d = m - n;
#   (iii) the pair is orthogonal iff 2d >= m and r = 2d - m;
#   (iv)  the number of unordered pairs with intersection dimension r is
#         M_r = (s_n / 2) q^((d-r)^2) [d, r]_q [m-d, d-r]_q  (eq. (2)),
#         the Grassmann-graph valency, and sum_r M_r = choose(s_n, 2);
#   (v)   the spectrum average sum_r M_r (q^(m+r) - q^(2d)) / choose(s_n,2)
#         equals the closed-form E(f_NOD) of Corollary 2.
# It writes projection-spectrum-results.csv (one row per configuration
# and intersection dimension) and prints a summary.  Run from this
# directory:  Rscript projection-spectrum.R      (a few seconds)

.libPaths(c(Sys.glob("~/R/library"), .libPaths()))
suppressMessages(library(PGM2))

gauss <- function(m, k, q) {           # Gaussian binomial [m, k]_q
  if (k < 0 || k > m) return(0)
  if (k == 0) return(1)
  v <- 1
  for (i in seq_len(k)) v <- v * (q^(m - i + 1) - 1) / (q^i - 1)
  v
}
enod_closed <- function(N, s, l, lam)  # Corollary 2, integral case
  (N * (N - 1) * lam * (lam - 1) + N * s * (s - 1) - s * (s - 1) * N^2 / l^2) /
    (s * (s - 1))

configs <- rbind(c(3, 2, 1), c(3, 2, 2), c(4, 2, 1), c(4, 2, 2), c(4, 2, 3),
                 c(5, 2, 1), c(5, 2, 2), c(5, 2, 3), c(5, 2, 4),
                 c(2, 3, 1), c(3, 3, 1), c(3, 3, 2), c(2, 5, 1))
colnames(configs) <- c("m", "q", "n")

rows <- list()
for (i in seq_len(nrow(configs))) {
  m <- configs[i, "m"]; q <- configs[i, "q"]; n <- configs[i, "n"]; d <- m - n
  D <- Qn(m, n, q)$UD
  N <- nrow(D); s <- ncol(D); l <- q^n
  stopifnot(N == q^m, s == gauss(m, d, q))

  r_of_pair <- integer(0); ok_cells <- TRUE; ok_phi <- TRUE; ok_orth <- TRUE
  for (c1 in seq_len(s - 1)) for (c2 in (c1 + 1):s) {
    tab <- table(factor(D[, c1], levels = 1:l), factor(D[, c2], levels = 1:l))
    occ <- sum(tab > 0)
    r <- round(m - log(occ, q))                       # (i): occupied cells = q^(m-r)
    if (occ != q^(m - r) || !all(tab[tab > 0] == q^r)) ok_cells <- FALSE
    phi <- sum((tab - N / l^2)^2)
    if (abs(phi - (q^(m + r) - q^(2 * d))) > 1e-9) ok_phi <- FALSE      # (ii)
    orth <- all(tab == N / l^2)
    if (orth != (2 * d >= m && r == 2 * d - m)) ok_orth <- FALSE        # (iii)
    r_of_pair <- c(r_of_pair, r)
  }
  r_range <- max(0, 2 * d - m):(d - 1)
  M_emp <- as.integer(table(factor(r_of_pair, levels = r_range)))
  M_thm <- sapply(r_range, function(r) s / 2 * q^((d - r)^2) * gauss(d, r, q) * gauss(m - d, d - r, q))
  ok_mult <- all(M_emp == M_thm) && sum(M_emp) == choose(s, 2)          # (iv)
  avg <- sum(M_thm * (q^(m + r_range) - q^(2 * d))) / choose(s, 2)
  ok_avg <- abs(avg - enod_closed(N, s, l, gauss(m - 1, d - 1, q))) < 1e-9   # (v)

  for (j in seq_along(r_range))
    rows[[length(rows) + 1]] <- data.frame(
      m = m, q = q, n = n, N = N, s = s, levels = l, r = r_range[j],
      M_r = M_thm[j], M_r_observed = M_emp[j],
      phi = q^(m + r_range[j]) - q^(2 * d),
      orthogonal = (2 * d >= m && r_range[j] == 2 * d - m),
      cells_ok = ok_cells, phi_ok = ok_phi, orth_ok = ok_orth,
      mult_ok = ok_mult, avg_efnod = avg, avg_ok = ok_avg)
  cat(sprintf("PG(%d,%d) n=%d: s=%d pairs=%d | cells %s phi %s orth %s mult %s | E(fNOD) %.6f %s\n",
              m, q, n, s, choose(s, 2), ok_cells, ok_phi, ok_orth, ok_mult, avg, ok_avg))
}
res <- do.call(rbind, rows)
write.csv(res, "projection-spectrum-results.csv", row.names = FALSE)
cat("\nAll checks passed:", all(res$cells_ok & res$phi_ok & res$orth_ok & res$mult_ok & res$avg_ok), "\n")
cat("Written: projection-spectrum-results.csv (", nrow(res), "rows )\n")
