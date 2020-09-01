library(svgR)
library(tidyverse)



block %<c-% function(points, fill='none', stroke= 'black',  label=NULL, ...){
    xy<-apply(points,1,min)
    wh<-abs(apply(points,1,diff)) 
    g(
        rect(xy=xy, wh=wh, fill=fill, stroke=stroke, ...),
        text(stroke=stroke, cxy=xy+wh/2, label, ...)
        
    )
}

reactiveSource %<c-% function(points, fill='none', stroke= 'black',  label=NULL, ...){
    xy<-apply(points,1,min)
    wh<-abs(apply(points,1,diff)) 
    
    m<-c('M',xy)
    dz<-wh[2]/2
    dx<-wh[1]-dz
    
    polyPortion<-c('l', dx,0,  dz,dz, -dz,dz, -dx, 0 )
    d<-c(m, polyPortion, 'Z')
    g(
       path(d=d, fill=fill, stroke=stroke),
       text(cxy=xy+wh/2, label)
    )
    
} 


reactiveObserver %<c-% function(points, fill='none', stroke= 'black', label=NULL, ...){
    xy<-apply(points,1,min)
    wh<-abs(apply(points,1,diff)) 
   
    dz<-wh[2]/2
    dx<-wh[1]-dz
    m<-c('M',xy+c(dz,0))
    polyPortion<-c('l', dx,0,  0,2*dz, -dx, 0 )
    arcPortion<-c('a', c(dz,dz), 180, 1,1, c(0, -2*dz))
    d<-c(m, polyPortion, arcPortion)
    g(
        path(d=d, fill=fill, stroke=stroke, ... ),
        text(cxy=xy+wh/2, label)
    )
}

reactiveExpression %<c-% function(points, fill='none', stroke= 'black', label=NULL, ...){
     xy<-apply(points,1,min)
    wh<-abs(apply(points,1,diff))
   
   
    dz<-wh[2]/2
    dx<-wh[1]-2*dz
    m<-c('M',xy+c(dz,0))
    polyPortion<-c('l', dx,0,  dz,dz, -dz,dz, -dx, 0 )
    arcPortion<-c('a', c(dz,dz), 180, 1,1, c(0, -2*dz))
    d<-c(m, polyPortion, arcPortion)
    g(
        path(d=d, fill=fill, stroke=stroke, ... ),
        text(cxy=xy+wh/2, label)
    )
    
} 

arrow %<c-% function(points, r=5, stroke='black', stroke.width=2, ...){
  x<-points
  nx<-length(x)
  if(nx<4){
    return(NULL)
  }
  n<-ncol(x)
  if(n==2){
    return(path(d=c("M", x[,1], "L", x[,n]), ...))
  }

  dv<-x[,-n]-x[,-1]
  d<-apply( dv ,2, function(x)sqrt(sum(x)^2) )
  lambda<-sapply(r/d, function(x)min(x,.5))
  
  m<- n-1
  if(m==2){
    mA<-matrix(c(lambda[1],1-lambda[1]),2,1)
    mB<-matrix(c(1-lambda[2],lambda[2]),2,1)
  } else {
    mA<- rbind(diag(lambda[-m]),0) + rbind(0,diag(1-lambda[-m]))
    mB<- rbind(0,diag(lambda[-1])) + rbind(diag(1-lambda[-1]),0)
  } 
  a<-x[,-n]%*%mA
  b<-x[,-1]%*%mB   
  rL<-rep("L", n-2)
  rQ<-rep("Q", n-2)
  if(m==2){
    rr<-c(rL,a,rQ,x[,2],b)
  } else {
    rr<-rbind(rL, a, rQ, x[,-c(1,n)], b)
  }
    path(
        d=c("M", x[,1], rr, "L", x[,n]),  
        fill='none', stroke=stroke,
        marker.end= marker( viewBox=c(0, 0, 10, 10), refXY=c(9,5), stroke=stroke, fill=stroke,
              markerWidth=6, markerHeight=6, orient="auto",
              path( d=c("M", 0, 0, "L", 10, 5, "L", 0, 10, "z") )
       
        ),
    ...)
 
}

d.callout<-function(xy1, xy2,  pt=2, r=20 ){
  xy=mapply(min, xy1, xy2)
  dxy<-abs(xy1-xy2)
  r=min(r, dxy/2)
  dx<-dxy[1]
  dy<-dxy[2]
  dl<-dx-3*r
  hp<-3
  if(pt==1){
    dl1<-dl; dl2<-0
    dp<-(-2*r)
  } else if(pt==3){
    dl2<-dl; dl1<-0
    dp<-2*r
  } else {
    dl1<-dl2<-dl/2
    dp<-0
  }
  d=list(
    M=xy+c(0,r) ,
    a=c(c(r,r), 0,0,1,c(r,-r)),
    l=c(dx-2*r,0),
    a=c(c(r,r), 0,0,1,c(r,r)),
    l=c(0,dy-2*r),
    a=c(c(r,r), 0,0,1,c(-r,r)),
    l=c( c(-dl1,0), c(-r/2+dp,hp*r), c(-r/2-dp,-hp*r), c(-dl2,0)),
    a=c(c(r,r), 0,0,1,c(-r,-r)),
    z=''
  )  
}

callout%<c-%function(points, txt='',  pt=3, r=20, ...){
  if(ncol(points)<2){
    NULL
  } else {
    xy1=points[,1]
    xy2=points[,2]
    g(
      path(
          d=d.callout(xy1,xy2, pt, r),
            stroke='#0000FF',
            stroke.width=2,
            fill='none'
      ),
      text(cxy= (xy1+xy2)*c(.5,.5), txt, ...)
    )
  }
}



