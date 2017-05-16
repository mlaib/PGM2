#' Resolvable Balanced Incomplete Block Design RBIBD
#'
#' Extracts the Resolvable Balanced Incomplete Block Design (RBIBD) of
#' any BIBD constructed from a projective geometry.
#'
#' @usage Resolvable(n, mat)
#' @param n The sub-variety of the block to be deleted.
#' @param mat The matrix of the BIB.
#' @return A LIST of:
#' \enumerate{
#'  \item \code{V }{Number of treatments in the RBIBD.}
#'  \item \code{B }{Number of blocks of the RBIBD.}
#'  \item \code{R }{Repetition of each treatment.}
#'  \item \code{K }{Size of each block.}
#'  \item \code{BIB }{The configuration of the RBIBD.}
#' }
#' @author Mohamed Laib, Abla Boudraa and Zebida Gheribi-Aoulmi
#' @references D. Dugué Traité de statistique théorique et appliquée, Masson et Cie, 1958.
#' @examples
#' \dontrun{
#' m<-4
#' X<-BIB(m)   #BIBD from PG(4,2)
#'
#' n<-1
#' mat<-X$BIB
#' Y<-Resolvable(n,mat) #Extracts the RBIBD
#'
#'
#' }
#' @export
Resolvable <-function(n,mat) {
  B<-mat
  C<-B[n,]
  B<-B[-n,]
  a<-dim(B)[1]
  c<-dim(B)[2]
  e<-(c+1)/2
  for (j in 1:a) {for (i in 1:c) {if (any (B[j,i]==C)) {B[j,i]<-0}}}
  X<-matrix(nrow=a, ncol=e)
  for (i in 1:a) {X[i,]<-B[i,][B[i,]>0]}
  v <- sort(unique(as.vector(X)))
  V<-length(v)
  T<-X[1,1]
  R<-length(which(T==X))
  B<-dim(X)[1]
  K<-dim(X)[2]
return(list(V=V,B=B,R=R,K=K,RBIB=X))}
