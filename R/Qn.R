#' Reduced Resolvable Design of Stage n and its Uniform Design
#'
#' Builds the reduced resolvable design \eqn{Q^*_n} of Boudraa et al.
#' (2013) directly, together with its associated uniform design
#' \eqn{U(p^m, (p^n)^{r^{**}_n})}: the treatments are the \eqn{p^m} points
#' of the affine geometry AG(m, p), each factor corresponds to one
#' \eqn{(m-n)}-dimensional linear subspace W of GF(p)^m (one parallel
#' class of affine flats), and the level of a treatment x on factor W is
#' the coset x + W.
#'
#' @param m Dimension of the projective geometry (an integer, \code{m >= 2}).
#' @param n Stage of the recursion, an integer with \code{1 <= n <= m - 1}.
#' @param p Order of the Galois field GF(p); must be prime. Defaults to
#'   \code{p = 2}.
#' @return A list with components:
#' \describe{
#'  \item{\code{V}}{Number of treatments (runs), \eqn{p^m}.}
#'  \item{\code{B}}{Number of blocks of \eqn{Q^*_n}.}
#'  \item{\code{R}}{Replication of each treatment (= number of factors).}
#'  \item{\code{K}}{Block size, \eqn{p^{m-n}}.}
#'  \item{\code{Lambda}}{Concurrence parameter (constant, see Details).}
#'  \item{\code{Levels}}{Number of levels of each factor, \eqn{p^n}.}
#'  \item{\code{UD}}{The uniform design: a \eqn{p^m \times R} matrix of
#'    levels \code{1..Levels}.}
#' }
#' @details The number of factors is the Gaussian binomial coefficient
#' \eqn{\binom{m}{m-n}_p}. Any two distinct runs coincide in exactly
#' \eqn{\binom{m-1}{m-n-1}_p} factors (the number of \eqn{(m-n)}-dimensional
#' subspaces containing a fixed nonzero vector), so the design is
#' equidistant; consequently it attains the discrete-discrepancy lower
#' bound of Fang et al. (2004). Across stages the levels refine: if
#' \eqn{W' \subset W} then the level partition induced by W is a
#' coarsening of the one induced by W'.
#'
#' For \eqn{n = 1} the design is the Rao-Hamming orthogonal array
#' \eqn{OA(p^m, (p^m-1)/(p-1), p, 2)}; for \eqn{p = 2, n = 1} the
#' Plackett-Burman design of order \eqn{2^m}.
#'
#' Complexity grows quickly with \eqn{p^m}; intended for moderate sizes
#' (\eqn{p^m \le 128}).
#' @author Mohamed Laib, Abla Boudraa and Zebida Gheribi-Aoulmi
#' @references
#' A. Boudraa, Z. Gheribi-Aoulmi and M. Laib (2013). Recursive method for
#' construction of resolvable nested designs and uniform designs associated.
#' \emph{International Journal of Research and Reviews in Applied Sciences},
#' 17(2), 167--176.
#'
#' K.T. Fang, X. Lu, Y. Tang and J. Yin (2004). Constructions of uniform
#' designs by using resolvable packings and coverings. \emph{Discrete
#' Mathematics}, 274, 25--40.
#' @examples
#' Q <- Qn(3, 2)            # stage 1 of PG(3,2): U(8, 2^7), Plackett-Burman
#' Q$UD
#'
#' Q2 <- Qn(3, 2, p = 2)    # same as above
#' Q32 <- Qn(3, 1, p = 3)   # U(27, 3^13)
#' c(Q32$V, Q32$B, Q32$R, Q32$K, Q32$Lambda)
#'
#' Qn(4, 2)$R               # 35 four-level factors on 16 runs
#' @export
Qn <- function(m, n, p = 2) {
  p <- check_prime(p)
  if (length(m) != 1L || is.na(m) || m != round(m) || m < 2)
    stop("'m' must be an integer >= 2.", call. = FALSE)
  if (length(n) != 1L || is.na(n) || n != round(n) || n < 1 || n > m - 1)
    stop("'n' must be an integer with 1 <= n <= m - 1.", call. = FALSE)
  d <- m - n
  runs <- gf_vectors(m, p)
  Ws <- subspaces(m, p, d)
  UD <- vapply(Ws, function(W) {
    labs <- apply(runs, 1, function(x)
      min(apply(sweep(W, 2, x, "+") %% p, 1, paste, collapse = ",")))
    as.integer(factor(labs))
  }, integer(nrow(runs)))
  s <- length(Ws)
  list(V = p^m, B = s * p^n, R = s, K = p^d,
       Lambda = gaussian_binom(m - 1, d - 1, p),
       Levels = p^n, UD = UD)
}
