#' All Stages of the Recursive Construction
#'
#' Runs the whole recursion of Boudraa et al. (2013) starting from the
#' projective geometry PG(m, p): at each stage the current BIBD yields a
#' resolvable design (via \code{\link{Resolvable}}) and its associated
#' uniform design (via \code{\link{Uniform}}), and the next-generation BIBD
#' is extracted (via \code{\link{Gen}}) until the geometry is exhausted.
#'
#' @param m Dimension of the projective geometry (an integer, \code{m >= 2}).
#' @param n Index of the block (sub-variety) to be deleted at each stage.
#'   The same index is used at every stage, and the number of blocks shrinks
#'   along the recursion, so \code{n} must not exceed the block count of the
#'   last stage, \eqn{p^2 + p + 1} (the design of PG(2, p)).
#' @param stage Stages wanted, a character vector (default \code{"all"}):
#'   \describe{
#'    \item{\code{'S1'}}{The first-generation BIBD.}
#'    \item{\code{'S2'}}{The BIBDs of the following generations.}
#'    \item{\code{'S3'}}{The resolvable designs of every stage.}
#'    \item{\code{'S4'}}{The uniform designs associated with every stage.}
#'   }
#' @param p Order of the Galois field GF(p); must be prime. Defaults to
#'   \code{p = 2}, which reproduces the designs of PGM2 <= 1.2.
#' @return A named list with (depending on \code{stage}) components
#'   \code{BIB1} (the first-generation BIBD as returned by
#'   \code{\link{BIB}}), \code{BIBg} (a list of the next-generation BIBDs),
#'   \code{Resolvables} (a list of the resolvable designs of every stage)
#'   and \code{UDs} (a list of the associated uniform designs).
#' @note The return structure is tidier than in PGM2 <= 1.2, where the
#'   stages were spliced into one flat, partially unnamed list: each stage
#'   is now a proper sub-list.
#' @author Mohamed Laib, Abla Boudraa and Zebida Gheribi-Aoulmi
#' @references
#' A. Boudraa, Z. Gheribi-Aoulmi and M. Laib (2013). Recursive method for
#' construction of resolvable nested designs and uniform designs associated.
#' \emph{International Journal of Research and Reviews in Applied Sciences},
#' 17(2), 167--176.
#'
#' Z. Gheribi-Aoulmi and M. Bousseboua (2005). Recursive methods for
#' construction of balanced n-ary block designs. \emph{Serdica Mathematical
#' Journal}, 31, 189--200.
#' @examples
#' s <- Steps(3, 1)               # all stages of PG(3, 2)
#' names(s)
#' s$UDs[[1]]$UD                  # U(8, 2^7)
#'
#' s3 <- Steps(3, 1, p = 3)       # all stages of PG(3, 3)
#' s3$UDs[[1]]$UD                 # U(27, 3^13)
#'
#' Steps(4, 1, c('S1', 'S4'))     # first and last stage only, PG(4, 2)
#' @export
Steps <- function(m, n, stage = "all", p = 2) {
  p <- check_prime(p)
  nb_min <- p^2 + p + 1                 # blocks of PG(2, p), the last stage
  if (length(n) != 1L || is.na(n) || !is.numeric(n) || n != round(n) ||
      n < 1 || n > nb_min)
    stop("'n' must be a single block index between 1 and ", nb_min,
         ": the same block index is deleted at every stage, and the last ",
         "stage is the design of PG(2, ", p, "), which has only ", nb_min,
         " blocks. Got ", paste(format(n), collapse = ", "), ".",
         call. = FALSE)
  n <- as.integer(n)
  A <- BIB(m, p)
  s <- A$BIB
  gens <- list()
  resolvables <- list()
  uds <- list()
  while (m >= 2) {
    d <- dim(s)[2]
    reso <- Resolvable(n, s)
    resolvables[[length(resolvables) + 1]] <- reso
    uds[[length(uds) + 1]] <- Uniform(reso$RBIB)
    if (d > p + 1) {
      ss <- Gen(n, s)
      s <- ss$BIB2
      gens[[length(gens) + 1]] <- ss
    }
    m <- m - 1
  }

  if (length(stage) == 1 && stage == "all") stage <- c("S1", "S2", "S3", "S4")
  lst <- list()
  if ("S1" %in% stage) lst$BIB1 <- A
  if ("S2" %in% stage) lst$BIBg <- gens
  if ("S3" %in% stage) lst$Resolvables <- resolvables
  if ("S4" %in% stage) lst$UDs <- uds
  lst
}
