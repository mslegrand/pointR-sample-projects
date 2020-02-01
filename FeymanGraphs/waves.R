library(svgR)
library(tidyverse)
WH<-c(1600,800)

# Defined by mouse: edit with care!
ptR<-list(
  x= tribble(
      ~points,
      matrix(0,2,0)
  )
)


p<- c(500,200)
q<- c(300,500)
waves<-function(p, q, N=2, eps=.5){
  d<-c('M',p)
  pq<- p-q 
  L<-sqrt(sum((pq)^2))
  if(L>0){
    pq<-pq/L
    W<-L/(4*N)
    #W=10
    H<-  W
    tpl<-matrix(c( c(1-eps,1),c(1,1), c(eps,0), c(1,-1)),2)
    m<-matrix( c(-pq[1],-pq[2],pq[2],-pq[1]),2)
    L1<-cbind(c(W,H)*tpl)
    l1<-cbind(c(W,-H)*tpl)
    lL<-m%*%cbind(l1,L1)
    lLN<-rep(lL,N)      
    d<-c(d,'q',lLN) 
    }
  d
}

d<-waves(p, q, 2, .5)

svgR(wh=WH,
     #your custom code goes here
     path(d=d, stroke='red',stroke.width=2, fill='none')
)
