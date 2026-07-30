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
