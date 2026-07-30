#' BIBD of the Next Generation
#'
#' Extracts the balanced incomplete block design of the next generation by
#' deleting block \code{n} of a BIBD built from a projective geometry: each
#' remaining block is intersected with the deleted one, so the deleted block
#' plays the role of a lower-dimensional projective geometry and the
#' intersections are its hyperplanes.
#'
#' @param n Index of the block (sub-variety) to be deleted; an integer
#'   between 1 and \code{nrow(mat)}.
#' @param mat The matrix of the BIBD (rows are blocks), e.g. the \code{BIB}
#'   component returned by \code{\link{BIB}}.
#' @return A list with components:
#' \describe{
#'  \item{\code{V}}{Number of treatments of the new BIBD.}
#'  \item{\code{B}}{Number of blocks.}
#'  \item{\code{R}}{Replication of each treatment.}
#'  \item{\code{K}}{Size of each block.}
#'  \item{\code{BIB2}}{The design: a matrix whose rows are the blocks.}
#' }
#' @details Works for any order p: block sizes are derived from the data
#' instead of the binary-only formula used in PGM2 <= 1.2 (for a symmetric
#' BIBD every remaining block meets the deleted block in exactly
#' \code{Lambda} treatments). For a BIBD from PG(m, p), the result is the
#' BIBD of PG(m - 1, p) on the treatments of the deleted block.
#' @author Mohamed Laib, Abla Boudraa and Zebida Gheribi-Aoulmi
#' @references
#' A. Boudraa, Z. Gheribi-Aoulmi and M. Laib (2013). Recursive method for
#' construction of resolvable nested designs and uniform designs associated.
#' \emph{International Journal of Research and Reviews in Applied Sciences},
#' 17(2), 167--176.
#' @examples
#' X <- BIB(4)              # BIBD (31, 15, 7) from PG(4, 2)
#' X2 <- Gen(1, X$BIB)      # second generation: BIBD (15, 7, 3)
#' X2$V; X2$K
#'
#' Y <- BIB(3, p = 3)       # BIBD (40, 13, 4) from PG(3, 3)
#' Y2 <- Gen(1, Y$BIB)      # second generation: BIBD (13, 4, 1)
#' Y2$V; Y2$K
#' @export
Gen <- function(n, mat) {
  check_design_matrix(mat)
  n <- check_block_index(n, nrow(mat))
  A <- mat[n, ]
  B <- mat[-n, , drop = FALSE]
  rows <- lapply(seq_len(nrow(B)), function(i) B[i, ][B[i, ] %in% A])
  len <- unique(lengths(rows))
  if (length(len) != 1L)
    stop("blocks of 'mat' do not intersect block ", n, " in a constant ",
         "number of treatments: 'mat' is not a symmetric BIBD.",
         call. = FALSE)
  x1 <- unique(do.call(rbind, rows))
  v <- sort(unique(as.vector(x1)))
  R <- length(which(x1[1, 1] == x1))
  list(V = length(v), B = nrow(x1), R = R, K = ncol(x1), BIB2 = x1)
}
