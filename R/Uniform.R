#' Uniform Design from a Resolvable BIBD
#'
#' Builds a symmetric uniform design (UD) from a resolvable balanced
#' incomplete block design, following the correspondence of Fang et al.:
#' the blocks are partitioned greedily into parallel classes, each class
#' becomes a factor (column), and the level of a treatment on a factor is
#' the index of the block containing it within that class.
#'
#' @param mat The matrix of the RBIBD (rows are blocks), e.g. the
#'   \code{RBIB} component returned by \code{\link{Resolvable}}.
#' @return A list with components:
#' \describe{
#'  \item{\code{n}}{Number of experiments (runs) of the uniform design.}
#'  \item{\code{F}}{Number of factors (one per parallel class).}
#'  \item{\code{UD}}{The design: an \code{n} x \code{F} matrix of levels.}
#' }
#' @details Works for any order p (the parallel-class extraction is purely
#' combinatorial). For the RBIBD residual to a block of the BIBD of
#' PG(m, p), the result is a uniform design with \eqn{p^m} runs and p
#' levels per factor.
#' @author Mohamed Laib, Abla Boudraa and Zebida Gheribi-Aoulmi
#' @references
#' K.T. Fang, X. Lu, Y. Tang and J. Yin (2004). Constructions of uniform
#' designs by using resolvable packings and coverings. \emph{Discrete
#' Mathematics}, 274, 25--40.
#'
#' K.T. Fang, G.N. Ge, M.Q. Liu and H. Qin (2004). Construction of uniform
#' designs via super-simple resolvable t-designs. \emph{Utilitas
#' Mathematica}, 66, 15--32.
#' @examples
#' X <- BIB(3)                    # BIBD (15, 7, 3) from PG(3, 2)
#' Y <- Resolvable(1, X$BIB)      # RBIBD (8, 14, 7, 4, 3)
#' Uniform(Y$RBIB)$UD             # U(8, 2^7)
#'
#' Z <- BIB(2, p = 3)             # BIBD (13, 4, 1) from PG(2, 3)
#' W <- Resolvable(1, Z$BIB)      # RBIBD (9, 12, 4, 3, 1)
#' Uniform(W$RBIB)$UD             # U(9, 3^4)
#' @importFrom stats na.omit
#' @export
Uniform <- function(mat) {
  w <- mat
  v <- sort(unique(as.vector(w)))
  W <- w
  malist <- NULL
  TRI <- function(a, b) all(a %in% b)

  k <- 1
  while (TRUE) {
    vv <- v
    malist[k] <- list(NULL)
    while (TRUE) {
      bool <- apply(W, 1, TRI, vv)
      if (!any(bool)) {
        break
      } else {
        ind <- which(bool)[1]
        u <- W[ind, ]
        vv <- vv[!(vv %in% u)]
        W <- W[-ind, , drop = FALSE]
        malist[[k]] <- rbind(malist[[k]], u)
      }
    }
    if (!all(as.vector(W) %in% v) | length(W) == 0) break
    k <- k + 1
  }
  x <- Reduce("rbind", malist)

  a <- max(x)
  b <- length(malist)
  c <- dim(x)[1]
  lev <- c / b
  UD <- matrix(nrow = a, ncol = b)
  v <- sort(unique(as.vector(x)))
  for (i in 1:b) {
    q <- malist[[i]]
    e <- c()
    for (j in v) {
      e[j] <- which(q == j)
      if (e[j] > lev) e[j] <- e[j] %% lev
      if (e[j] == 0) e[j] <- lev
    }
    UD[, i] <- e
  }
  ud <- na.omit(UD)
  attr(ud, "na.action") <- NULL
  list(n = dim(ud)[1], F = dim(ud)[2], UD = ud)
}
