# SYMMETRIC CD2 experiment (revision of the asymmetric one criticised in
# PAPER_REVIEW.md Finding 4).
# For each configuration:
#   - our design: canonical CD2 and CD2 after label optimisation;
#   - 20 independent random U-type designs of identical (N, q^s) structure,
#     EACH optimised with the identical optimiser and budget; min and
#     median of the 20 optimised values reported;
#   - best of 300 unoptimised random LHDs (external reference only).
# Optimiser: coordinate descent over factors; per factor, all q!
# permutations when q <= 5, FIRST-IMPROVEMENT transposition descent
# otherwise; sweeps to convergence; 8 restarts (restart 1 = incoming
# labels, restarts 2..8 random). All optimised design matrices are saved.
# Seeds: master seed 20260730; per-design seeds derived deterministically.
.libPaths(c(Sys.glob("~/R/library"), .libPaths()))
library(PGM2)

lv2u <- function(D, q) (D - 0.5) / q
f2 <- function(u) 1 + 0.5 * abs(u - 0.5) - 0.5 * (u - 0.5)^2
g3 <- function(u) {
  a <- abs(u - 0.5)
  1 + 0.5 * outer(a, a, "+") - 0.5 * abs(outer(u, u, "-"))
}
cd2_state <- function(U) {
  n <- nrow(U); M2 <- rep(1, n); M3 <- matrix(1, n, n)
  for (j in seq_len(ncol(U))) { M2 <- M2 * f2(U[, j]); M3 <- M3 * g3(U[, j]) }
  list(M2 = M2, M3 = M3, n = n, s = ncol(U))
}
cd2_from_state <- function(st)
  sqrt((13 / 12)^st$s - 2 / st$n * sum(st$M2) + sum(st$M3) / st$n^2)
cd2_full <- function(D, q) cd2_from_state(cd2_state(lv2u(D, q)))

perms <- function(v) {
  if (length(v) == 1) return(list(v))
  out <- list()
  for (i in seq_along(v)) for (p in perms(v[-i])) out[[length(out) + 1]] <- c(v[i], p)
  out
}
best_col <- function(lv, q, Q2, Q3, st, exhaustive) {
  eval_perm <- function(pm) {
    u <- (pm[lv] - 0.5) / q
    M2 <- Q2 * f2(u); M3 <- Q3 * g3(u)
    (13 / 12)^st$s - 2 / st$n * sum(M2) + sum(M3) / st$n^2
  }
  cur <- seq_len(q); curval <- eval_perm(cur)
  if (exhaustive) {
    for (pm in perms(seq_len(q))) {
      v <- eval_perm(pm)
      if (v < curval - 1e-14) { cur <- pm; curval <- v }
    }
  } else {
    repeat {
      improved <- FALSE
      for (a in 1:(q - 1)) for (b in (a + 1):q) {
        pm <- cur; pm[c(a, b)] <- pm[c(b, a)]
        v <- eval_perm(pm)
        if (v < curval - 1e-14) { cur <- pm; curval <- v; improved <- TRUE }
      }
      if (!improved) break
    }
  }
  list(perm = cur, val = curval)
}
optimise_labels <- function(D, q, restarts = 8, seed = 1) {
  set.seed(seed)
  s <- ncol(D)
  bestD <- D; bestcd <- cd2_full(D, q)
  exhaustive <- q <= 5
  for (r in seq_len(restarts)) {
    Dr <- D
    if (r > 1) for (j in seq_len(s)) Dr[, j] <- sample(q)[Dr[, j]]
    U <- lv2u(Dr, q); st <- cd2_state(U)
    repeat {
      changed <- FALSE
      for (j in seq_len(s)) {
        Q2 <- st$M2 / f2(U[, j]); Q3 <- st$M3 / g3(U[, j])
        res <- best_col(Dr[, j], q, Q2, Q3, st, exhaustive)
        newcol <- res$perm[Dr[, j]]
        if (!all(newcol == Dr[, j])) {
          Dr[, j] <- newcol; U[, j] <- (newcol - 0.5) / q
          st$M2 <- Q2 * f2(U[, j]); st$M3 <- Q3 * g3(U[, j])
          changed <- TRUE
        }
      }
      if (!changed) break
    }
    cd <- cd2_full(Dr, q)
    if (cd < bestcd) { bestcd <- cd; bestD <- Dr }
  }
  list(cd = bestcd, D = bestD)
}
col_perms <- function(before, after, q) {
  lapply(seq_len(ncol(before)), function(j) {
    pm <- integer(q)
    for (l in seq_len(q)) pm[l] <- unique(after[before[, j] == l, j])
    pm
  })
}
rand_utype <- function(n, s, q) {
  base <- rep(seq_len(q), each = n / q)
  vapply(seq_len(s), function(j) sample(base), integer(n))
}

configs <- list(c(3, 1, 2), c(3, 2, 2), c(4, 1, 2), c(4, 2, 2), c(4, 3, 2),
                c(5, 1, 2), c(5, 2, 2), c(5, 3, 2), c(5, 4, 2),
                c(2, 1, 3), c(3, 1, 3), c(3, 2, 3), c(2, 1, 5))
NCOMP <- 20
# Run from the directory containing this script; outputs are relative.
out_dir <- Sys.getenv("SYMCD2_OUT", "optimized-designs")
dir.create(out_dir, showWarnings = FALSE)

out <- NULL; t_all <- Sys.time()
for (ci in seq_along(configs)) {
  cs <- configs[[ci]]
  m <- cs[1]; n <- cs[2]; p <- cs[3]
  Q <- Qn(m, n, p); q <- Q$Levels; N <- Q$V; s <- Q$R
  t0 <- Sys.time()

  can <- cd2_full(Q$UD, q)
  ours <- optimise_labels(Q$UD, q, seed = 20260730 + ci)

  comp <- numeric(NCOMP)
  comp_init <- comp_opt <- comp_perms <- vector("list", NCOMP)
  for (b in seq_len(NCOMP)) {
    set.seed(20260730 + 1000 * ci + b)
    R0 <- rand_utype(N, s, q)
    ob <- optimise_labels(R0, q, seed = 20260730 + 1000 * ci + b)
    comp[b] <- ob$cd
    comp_init[[b]] <- R0
    comp_opt[[b]] <- ob$D
    comp_perms[[b]] <- col_perms(R0, ob$D, q)
  }

  set.seed(20260730 + 500 + ci)
  lhd <- min(vapply(seq_len(300), function(i)
    cd2_full(vapply(seq_len(s), function(j) sample(N), integer(N)), N),
    numeric(1)))

  saveRDS(list(config = c(m = m, n = n, p = p),
               canonical = Q$UD,
               optimized = ours$D,
               optimized_perms = col_perms(Q$UD, ours$D, q),
               comparator_cd = comp,
               comparator_initial = comp_init,
               comparator_optimized = comp_opt,
               comparator_perms = comp_perms),
          file.path(out_dir, sprintf("pg%d_%d_n%d.rds", m, p, n)))

  el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("PG(%d,%d) n=%d: can=%.4f opt=%.4f | comp opt min=%.4f med=%.4f max=%.4f | LHD=%.4f (%.0fs)\n",
      m, p, n, can, ours$cd, min(comp), median(comp), max(comp), lhd, el))
  out <- rbind(out, data.frame(m = m, p = p, n = n, N = N, s = s, q = q,
    cd2_canonical = can, cd2_opt = ours$cd,
    comp_opt_min = min(comp), comp_opt_median = median(comp),
    comp_opt_max = max(comp), lhd_best300 = lhd, seconds = el))
}
write.csv(out, "symmetric-cd2-results.csv", row.names = FALSE)
cat(sprintf("TOTAL %.0fs\nR version: %s | PGM2 %s\n",
    as.numeric(difftime(Sys.time(), t_all, units = "secs")),
    R.version.string, as.character(packageVersion("PGM2"))))
