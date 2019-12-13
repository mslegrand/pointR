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




