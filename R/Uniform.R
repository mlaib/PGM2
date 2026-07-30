#' Uniform Design from a Resolvable BIBD
#'
#' Builds a symmetric uniform design (UD) from a resolvable balanced
#' incomplete block design, following the correspondence of Fang et al.:
#' the blocks are partitioned into parallel classes, each class becomes a
#' factor (column), and the level of a treatment on a factor is the index
#' of the block of that class containing it.
#'
#' @param mat The matrix of the RBIBD (rows are blocks), e.g. the
#'   \code{RBIB} component returned by \code{\link{Resolvable}}.
#' @return A list with components:
#' \describe{
#'  \item{\code{n}}{Number of experiments (runs) of the uniform design.}
#'  \item{\code{F}}{Number of factors (one per parallel class).}
#'  \item{\code{UD}}{The design: an \code{n} x \code{F} matrix of levels.}
#'  \item{\code{classes}}{A list giving, for each factor, the row indices
#'    of \code{mat} forming that parallel class.}
#' }
#' @details The resolution is found by exact-cover search with
#' backtracking, so the result does not depend on the order of the rows of
#' \code{mat}: any row permutation yields the same design up to a
#' permutation of the factors and a relabelling of levels. Earlier
#' versions used a first-fit greedy extraction, which could fail on a
#' valid resolvable design presented in an unfavourable row order.
#'
#' If \code{mat} admits no resolution, or the search budget is exhausted,
#' an informative error is raised rather than a partial design returned.
#'
#' Works for any order p (the parallel-class extraction is purely
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
#'
#' # the result does not depend on the order of the blocks
#' set.seed(1)
#' P <- Y$RBIB[sample(nrow(Y$RBIB)), ]
#' dim(Uniform(P)$UD)
#' @export
Uniform <- function(mat) {
  check_design_matrix(mat)
  v <- sort(unique(as.vector(mat)))
  nv <- length(v)
  nb <- nrow(mat)
  k <- ncol(mat)
  if (nv %% k != 0L)
    stop("'mat' cannot be resolvable: the block size (", k, ") does not ",
         "divide the number of treatments (", nv, ").", call. = FALSE)
  if (any(apply(mat, 1, anyDuplicated) > 0))
    stop("'mat' has a repeated treatment within a block.", call. = FALSE)

  blocks <- lapply(seq_len(nb), function(i) match(mat[i, ], v))
  # which blocks contain each treatment
  holders <- vector("list", nv)
  for (i in seq_len(nb)) for (t in blocks[[i]])
    holders[[t]] <- c(holders[[t]], i)

  budget <- 2e6L
  spend <- function() {
    budget <<- budget - 1L
    if (budget < 0L)
      stop("search budget exhausted while resolving 'mat': the design may ",
           "be too large, or it may not be resolvable.", call. = FALSE)
  }

  # All parallel classes that contain block b0, by exact cover of the
  # treatments not already covered by b0, using currently unused blocks.
  classes_containing <- function(b0, used, cap = 20000L) {
    out <- list()
    covered <- logical(nv)
    covered[blocks[[b0]]] <- TRUE
    chosen <- b0
    rec <- function() {
      spend()
      if (length(out) >= cap) return(invisible(NULL))
      if (all(covered)) {
        out[[length(out) + 1L]] <<- chosen
        return(invisible(NULL))
      }
      t <- which(!covered)[1L]
      for (i in holders[[t]]) {
        if (used[i]) next
        b <- blocks[[i]]
        if (any(covered[b])) next
        covered[b] <<- TRUE
        chosen <<- c(chosen, i)
        rec()
        covered[b] <<- FALSE
        chosen <<- chosen[-length(chosen)]
      }
      invisible(NULL)
    }
    rec()
    out
  }

  # Backtracking over the whole resolution. The lowest-indexed unused
  # block must lie in some class, so we branch only on the classes that
  # contain it; this is canonical and prunes the search heavily.
  solve_resolution <- function(used) {
    if (!any(!used)) return(list())
    b0 <- which(!used)[1L]
    for (cl in classes_containing(b0, used)) {
      u2 <- used
      u2[cl] <- TRUE
      rest <- solve_resolution(u2)
      if (!is.null(rest)) return(c(list(cl), rest))
    }
    NULL
  }

  classes <- solve_resolution(logical(nb))
  if (is.null(classes))
    stop("'mat' is not resolvable: its blocks cannot be partitioned into ",
         "parallel classes.", call. = FALSE)

  nf <- length(classes)
  UD <- matrix(NA_integer_, nrow = nv, ncol = nf)
  for (j in seq_len(nf)) {
    cl <- classes[[j]]
    for (l in seq_along(cl)) UD[blocks[[cl[l]]], j] <- l
  }
  if (anyNA(UD))
    stop("internal error: the extracted classes do not cover every ",
         "treatment.", call. = FALSE)

  list(n = nv, F = nf, UD = UD, classes = classes)
}
