#' Internal arithmetic helpers for GF(p), p prime
#'
#' @description Small utilities used by the design constructors: primality
#' check and enumeration of the points of the projective geometry PG(m, p).
#'
#' @details Points of PG(m, p) are the nonzero vectors of GF(p)^(m+1) up to
#' multiplication by a nonzero scalar. One canonical representative is kept
#' per point (the one whose first nonzero coordinate equals 1), giving
#' (p^(m+1) - 1)/(p - 1) points. Vectors are enumerated in base-p counting
#' order with the first coordinate as the most significant digit, which for
#' p = 2 reproduces exactly the point (and hence treatment) numbering of
#' PGM2 <= 1.2.
#'
#' @name gf-internal
#' @keywords internal
NULL

is_prime <- function(p) {
  if (length(p) != 1L || is.na(p) || p != round(p) || p < 2) return(FALSE)
  if (p %in% c(2, 3)) return(TRUE)
  if (p %% 2 == 0) return(FALSE)
  d <- 3
  while (d * d <= p) {
    if (p %% d == 0) return(FALSE)
    d <- d + 2
  }
  TRUE
}

check_prime <- function(p) {
  if (!is_prime(p))
    stop("'p' must be a prime number: Galois fields GF(p^s) of prime-power ",
         "order are not supported yet (got p = ", p, ").", call. = FALSE)
  as.integer(p)
}

proj_points <- function(m, p) {
  grid <- as.matrix(expand.grid(rep(list(0:(p - 1)), m + 1)))[, (m + 1):1,
                                                              drop = FALSE]
  dimnames(grid) <- NULL
  keep <- apply(grid, 1, function(x) any(x != 0) && x[which(x != 0)[1]] == 1)
  grid[keep, , drop = FALSE]
}

gf_vectors <- function(m, p) {
  g <- as.matrix(expand.grid(rep(list(0:(p - 1)), m)))
  dimnames(g) <- NULL
  g
}

gaussian_binom <- function(m, k, p) {
  if (k < 0 || k > m) return(0)
  if (k == 0) return(1)
  num <- prod(vapply(seq_len(k), function(i) p^(m - i + 1) - 1, numeric(1)))
  den <- prod(vapply(seq_len(k), function(i) p^i - 1, numeric(1)))
  round(num / den)
}

subspaces <- function(m, p, d) {
  # All d-dimensional linear subspaces of GF(p)^m, each returned as the
  # matrix of its p^d vectors. Enumeration is by canonical reduced row
  # echelon basis: every subspace has exactly one RREF basis, determined by
  # its d pivot columns together with the free entries to the right of each
  # pivot in non-pivot columns. This generates each subspace exactly once,
  # so the cost is proportional to the Gaussian binomial [m, d]_p rather
  # than to the number of d-subsets of the p^m - 1 nonzero vectors.
  if (d <= 0L) return(list(matrix(0L, nrow = 1L, ncol = m)))
  co <- as.matrix(expand.grid(rep(list(0:(p - 1)), d)))
  dimnames(co) <- NULL
  out <- vector("list", gaussian_binom(m, d, p))
  cnt <- 0L
  for (piv in utils::combn(m, d, simplify = FALSE)) {
    free <- lapply(seq_len(d), function(i) {
      cc <- seq_len(m)
      cc[cc > piv[i] & !(cc %in% piv)]
    })
    nfree <- sum(lengths(free))
    fills <- if (nfree == 0L) matrix(0L, nrow = 1L, ncol = 0L) else {
      f <- as.matrix(expand.grid(rep(list(0:(p - 1)), nfree)))
      dimnames(f) <- NULL
      f
    }
    for (r in seq_len(nrow(fills))) {
      G <- matrix(0L, nrow = d, ncol = m)
      for (i in seq_len(d)) G[i, piv[i]] <- 1L
      if (nfree > 0L) {
        k <- 0L
        for (i in seq_len(d)) for (cc in free[[i]]) {
          k <- k + 1L
          G[i, cc] <- fills[r, k]
        }
      }
      cnt <- cnt + 1L
      out[[cnt]] <- (co %*% G) %% p
    }
  }
  out[seq_len(cnt)]
}

check_design_matrix <- function(mat) {
  if (!is.matrix(mat) || nrow(mat) < 2L || ncol(mat) < 1L)
    stop("'mat' must be a matrix of at least two blocks (rows), such as the ",
         "'BIB' component returned by BIB().", call. = FALSE)
  invisible(TRUE)
}

check_block_index <- function(n, nb) {
  if (length(n) != 1L || is.na(n) || !is.numeric(n) || n != round(n) ||
      n < 1 || n > nb)
    stop("'n' must be a single block index between 1 and ", nb,
         " (the number of blocks of 'mat'); got ",
         paste(format(n), collapse = ", "), ".", call. = FALSE)
  as.integer(n)
}
