#' Balanced Incomplete Block Design from PG(m, p)
#'
#' Builds the symmetric balanced incomplete block design (BIBD) whose
#' treatments are the points of the projective geometry PG(m, p) over the
#' Galois field GF(p) and whose blocks are its hyperplanes.
#'
#' @param m Dimension of the projective geometry (an integer, \code{m >= 2}).
#' @param p Order of the Galois field GF(p); must be prime. Defaults to
#'   \code{p = 2}, which reproduces the designs of PGM2 <= 1.2 exactly.
#' @return A list with components:
#' \describe{
#'  \item{\code{V}}{Number of treatments, \eqn{(p^{m+1}-1)/(p-1)}.}
#'  \item{\code{B}}{Number of blocks (equal to \code{V}: the design is
#'    symmetric).}
#'  \item{\code{R}}{Replication of each treatment, \eqn{(p^m-1)/(p-1)}.}
#'  \item{\code{K}}{Block size (equal to \code{R}).}
#'  \item{\code{Lambda}}{Concurrence parameter, \eqn{(p^{m-1}-1)/(p-1)}.}
#'  \item{\code{BIB}}{The design: a \code{B} x \code{K} matrix whose rows
#'    are the blocks, containing treatment labels.}
#' }
#' @details Treatments are numbered by enumerating the canonical
#' representatives of the projective points in base-p counting order. Block
#' \code{i} consists of the points \code{x} lying on the hyperplane
#' \code{a_i . x = 0 (mod p)}, where \code{a_i} is the \code{i}-th point
#' (point-hyperplane duality).
#' @author Mohamed Laib, Abla Boudraa and Zebida Gheribi-Aoulmi
#' @references
#' A. Boudraa, Z. Gheribi-Aoulmi and M. Laib (2013). Recursive method for
#' construction of resolvable nested designs and uniform designs associated.
#' \emph{International Journal of Research and Reviews in Applied Sciences},
#' 17(2), 167--176.
#'
#' D. Dugue (1958). \emph{Traite de statistique theorique et appliquee}.
#' Masson et Cie, Paris.
#' @examples
#' X <- BIB(4)        # BIBD (31, 15, 7) from PG(4, 2)
#' X$V; X$K; X$Lambda
#'
#' Y <- BIB(2, p = 3) # BIBD (13, 4, 1): the projective plane of order 3
#' Y$BIB
#' @export
BIB <- function(m, p = 2) {
  p <- check_prime(p)
  if (length(m) != 1L || is.na(m) || m != round(m) || m < 2)
    stop("'m' must be an integer >= 2.", call. = FALSE)
  pts <- proj_points(m, p)
  bie <- t(apply(pts, 1, function(a) which((pts %*% a) %% p == 0)))
  v <- sort(unique(as.vector(bie)))
  V <- length(v)
  R <- length(which(bie[1, 1] == bie))
  list(V = V, B = nrow(bie), R = R, K = ncol(bie),
       Lambda = (p^(m - 1) - 1) %/% (p - 1), BIB = bie)
}
