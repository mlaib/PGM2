#' Resolvable Balanced Incomplete Block Design
#'
#' Extracts the resolvable balanced incomplete block design (RBIBD) residual
#' to block \code{n} of a BIBD constructed from a projective geometry: block
#' \code{n} is deleted together with all its treatments, and the surviving
#' parts of the remaining blocks form a resolvable design (for a BIBD from
#' PG(m, p) this is the design of the affine geometry AG(m, p)).
#'
#' @param n Index of the block (sub-variety) to be deleted; an integer
#'   between 1 and \code{nrow(mat)}.
#' @param mat The matrix of the BIBD (rows are blocks), e.g. the \code{BIB}
#'   component returned by \code{\link{BIB}}.
#' @return A list with components:
#' \describe{
#'  \item{\code{V}}{Number of treatments of the RBIBD, \eqn{p^m}.}
#'  \item{\code{B}}{Number of blocks.}
#'  \item{\code{R}}{Replication of each treatment.}
#'  \item{\code{K}}{Size of each block, \eqn{p^{m-1}}.}
#'  \item{\code{RBIB}}{The design: a matrix whose rows are the blocks.}
#' }
#' @details Works for any order p: residual block sizes are derived from the
#' data instead of the binary-only formula used in PGM2 <= 1.2 (every
#' remaining block loses exactly \code{Lambda} treatments to the deleted
#' block, so all residual blocks have size \code{K - Lambda}). The blocks
#' partition into \code{R} parallel classes of \code{p} blocks each.
#' @author Mohamed Laib, Abla Boudraa and Zebida Gheribi-Aoulmi
#' @references
#' A. Boudraa, Z. Gheribi-Aoulmi and M. Laib (2013). Recursive method for
#' construction of resolvable nested designs and uniform designs associated.
#' \emph{International Journal of Research and Reviews in Applied Sciences},
#' 17(2), 167--176.
#'
#' R.C. Bose (1942). A note on the resolvability of balanced incomplete
#' block designs. \emph{Sankhya}, 6, 105--110.
#' @examples
#' X <- BIB(4)                    # BIBD (31, 15, 7) from PG(4, 2)
#' Y <- Resolvable(1, X$BIB)      # RBIBD (16, 30, 15, 8, 7)
#' Y$V; Y$B; Y$K
#'
#' Z <- BIB(2, p = 3)             # BIBD (13, 4, 1) from PG(2, 3)
#' W <- Resolvable(1, Z$BIB)      # RBIBD (9, 12, 4, 3, 1): AG(2, 3)
#' W$RBIB
#' @export
Resolvable <- function(n, mat) {
  check_design_matrix(mat)
  n <- check_block_index(n, nrow(mat))
  C <- mat[n, ]
  B <- mat[-n, , drop = FALSE]
  rows <- lapply(seq_len(nrow(B)), function(i) B[i, ][!(B[i, ] %in% C)])
  len <- unique(lengths(rows))
  if (length(len) != 1L)
    stop("blocks of 'mat' do not intersect block ", n, " in a constant ",
         "number of treatments: 'mat' is not a symmetric BIBD.",
         call. = FALSE)
  X <- do.call(rbind, rows)
  v <- sort(unique(as.vector(X)))
  R <- length(which(X[1, 1] == X))
  list(V = length(v), B = nrow(X), R = R, K = ncol(X), RBIB = X)
}
