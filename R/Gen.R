#' Balanced Incomplete binary Blocks Designs of second Generation.
#'
#' Gives the configuration of a Balanced Incomplete binary Blocks
#' Designs of seconde generation.
#'
#' @usage Gen(n, mat)
#' @param n The sub-variety of the block to be deleted.
#' @param mat The matrix of the BIB.
#' @return A LIST of:
#' \enumerate{
#'  \item \code{V }{Number of treatments in the BIBD.}
#'  \item \code{B }{Number of blocks of the BIBD.}
#'  \item \code{R }{Repetition of each treatment.}
#'  \item \code{K }{Size of each block.}
#'  \item \code{BIB }{The configuration of the BIBD.}
#' }
#' @author Mohamed Laib, Abla Boudraa and Zebida Gheribi-Aoulmi
#' @references D. Dugué Traité de statistique théorique et appliquée, Masson et Cie, 1958.
#' @examples
#' \dontrun{
#' m<-4
#' X<-BIB(m)   #BIB from PG(4,2)
#'
#' n<-1
#' mat<-X$BIB
#' X2<-Gen(n,mat) #Extracts the BIB second generation
#' }
#' @export
Gen <-function(n,mat){
  A<-mat[n,]
  B<-mat[-n,]
  a<-dim(B)[1]
  b<-dim(B)[2]
  e<-(b-1)/2
  x1<-matrix(nrow=a, ncol=e)
  for (j in 1:a) {for (i in 1:b) {if (all (B[j,i]!=A)) {B[j,i]<-0}}}
  for (i in 1:a) {x1[i,]<-B[i,][B[i,]>0]}
  x1<-unique(x1)
  v <- sort(unique(as.vector(x1)))
  V<-length(v)
  T<-x1[1,1]
  R<-length(which(T==x1))
  B<-dim(x1)[1]
  K<-dim(x1)[2]
return(list(V=V,B=B,R=R,K=K,BIB2=x1))}
