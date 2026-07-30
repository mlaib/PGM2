#' @description
#' Recursive construction of balanced incomplete block designs (BIBDs),
#' their successive generations, resolvable BIBDs (RBIBDs) and associated
#' uniform designs (UDs), derived from finite projective geometries
#' PG(m, p) over a Galois field GF(p) of any prime order p.
#'
#' Version 2.0 generalises the whole package from GF(2) to GF(p) for any
#' prime p, as described in the underlying paper (Boudraa et al., 2013),
#' whose theory is stated for general p even though versions <= 1.2 of the
#' package implemented only p = 2.
#'
#' @references
#' A. Boudraa, Z. Gheribi-Aoulmi and M. Laib (2013). Recursive method for
#' construction of resolvable nested designs and uniform designs associated.
#' \emph{International Journal of Research and Reviews in Applied Sciences},
#' 17(2), 167--176.
#'
#' D. Dugue (1958). \emph{Traite de statistique theorique et appliquee}.
#' Masson et Cie, Paris.
#'
#' Z. Gheribi-Aoulmi and M. Bousseboua (2005). Recursive methods for
#' construction of balanced n-ary block designs. \emph{Serdica Mathematical
#' Journal}, 31, 189--200.
#'
#' K.T. Fang, X. Lu, Y. Tang and J. Yin (2004). Constructions of uniform
#' designs by using resolvable packings and coverings. \emph{Discrete
#' Mathematics}, 274, 25--40.
#'
#' K.T. Fang, G.N. Ge, M.Q. Liu and H. Qin (2004). Construction of uniform
#' designs via super-simple resolvable t-designs. \emph{Utilitas
#' Mathematica}, 66, 15--32.
#'
#' @examples
#' # The chain of designs of PG(3, 2):
#' X <- BIB(3)                  # BIBD (15, 7, 3)
#' Y <- Resolvable(1, X$BIB)    # RBIBD (8, 14, 7, 4, 3)
#' Uniform(Y$RBIB)$UD           # U(8, 2^7)
#'
#' # The same chain over GF(3):
#' X3 <- BIB(2, p = 3)          # BIBD (13, 4, 1)
#' Y3 <- Resolvable(1, X3$BIB)  # RBIBD (9, 12, 4, 3, 1)
#' Uniform(Y3$RBIB)$UD          # U(9, 3^4)
#' @keywords internal
"_PACKAGE"
