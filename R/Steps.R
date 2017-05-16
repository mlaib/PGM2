#' Nested Resolvable Designs and associated Uniform Designs in different stages.
#'
#' Gives the different stages of nested design begening from
#' a projective geometry, BIBD, BIBD seconde generation, RBIBD
#' and associated uniform designs.
#'
#' @usage Steps(m, n, stage = "all")
#' @param m Dimension of the projective geometry defined on GF(2).
#' @param n The sub-variety of the block to be deleted.
#' @param stage Stage of recurrence wanted (single value or vector) :
#' 'all' Print all designs.
#' 'S1' Print the configurations of BIB of first generation.
#' 'S2' Print the configurations of BIB of seconde generation.
#' 'S3' Print the configurations of RBIB.
#' 'S4' Print the configurations of the uniform designs associate to each nested resolvable.
#'
#' @return A LIST, with the parametrs and the configurations on each stages of recurrence.
#' @author Mohamed Laib, Abla Boudraa and Zebida Gheribi-Aoulmi
#' @references Gheribi-Aoulmi. Z and M. Bousseboua Recursive methods for construction of balanced n-ary block designs. Serdica Math.J (31), 2005,189-200
#' @examples
#' \dontrun{
#' list1<-Steps(4,1) #Get all stages : of the PG(4,2)
#'
#' list2<-Steps(4,1,c('S1','S2')) #Get the 2 first stages : of the PG(4,2)
#'
#' list3<-Steps(4,1,c('S1','S4')) #Get the first & the last stage : of the PG(4,2)
#'
#' list4<-Steps(4,1,'S4') #Get the last stage : of the PG(4,2)
#' }
#' @export
Steps <-function(m,n,stage="all"){
  lst<-NULL
  V<-NULL
  WW<-NULL
  W<-NULL
  A<-BIB(m)
  s<-A$BIB
  while (m >= 2) {
    d<-dim(s)[2]
    reso<-Resolvable(1,s)
    U<-reso$RBIB
    UD<-Uniform(U)
    if (d > 3) {
    ss<-Gen(n,s)
    s<-ss$BIB2
    WW<-list(WW,ss)}
    W<-c(W,reso)
    V<-c(V,UD)
    m<-m-1
  }

BIB<-FALSE
BIBg<-FALSE
RBIB<-FALSE
UDs<-FALSE
if (length(stage) == 1 && stage == "all") {
stage <- c("S1", "S2", "S3", "S4")}
for (i in 1:length(stage)) {
stage_ <- stage[i]
        switch(stage_, S1 = {
            BIB <- TRUE
        }, S2 = {
            BIBg <- TRUE
        }, S3 = {
            RBIB <- TRUE
        }, S4 = {
            UDs <- TRUE
        })
}
if (BIB==TRUE) {
lst<-c(lst,BIB1=A)}
if (BIBg==TRUE){
lst<-c(lst,BIBg=WW)}
if (RBIB==TRUE) {
lst<-c(lst,Resolvables=W)}
if (UDs==TRUE){
lst<-c(lst,V)}
return(lst)}
