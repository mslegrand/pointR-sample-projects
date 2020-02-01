library(svgR)
library(tidyverse)
WH<-c(600,600)

# Defined by mouse: edit with care!
ptR<-list(
  x=tribble(
    ~points,
    matrix(NA,2,0)
  )
)



p<- c(400,300)
q<- c(100,200)
curls<-function(p, q, N=4, f=.4){
  d<-c('M',p)
  pq<-p-q
  L<-sqrt(sum((pq)^2))
  if(L>0){
    
    pq<-pq/L
    w<- L/(2*((N+1)/f -N) ) 
    W<-w/f
    H<-  W
    h<- -H
    tpl<-matrix(c( c(0,-1),c(1,-1), c(1,0), c(1,1)),2)
    m<-matrix( c(-pq[1],-pq[2],pq[2],-pq[1]),2)
    L1<-cbind(c(W,H)*tpl)
    l1<-cbind(c(-w,h)*tpl)
    lL<-m%*%cbind(l1,L1)
    lLN<-rep(lL,N)      
    d<-c(d,'q',m%*%L1,lLN) 
    }
  d
}

d<-curls(p, q, 6, .4)

svgR(wh=WH,
     #your custom code goes here
     path(d=d, stroke='red',stroke.width=2, fill='none')
)
