library(svgR)
library(tidyverse)
WH<-c(600,400)

WH<-c(600,620)

#Defined by mouse: edit with care!
ptR<-list(
  x=matrix(
    c(c( 100,100 ),c( 100,400 ),c( 300,400 ),c( 233.5,314 )),2),
  y=matrix(
    c(c( 202.5,211 ),c( 207.5,89 ),c( 319.5,90 ),c( 327.5,207 ),c( 403.5,196 ),c( 443.5,93 ),c( 498.5,299 ),c( 465.5,364 )), 2),
  z=matrix(
    c(c( 333.5,450 ),c( 399.5,353 )),2)
)


roundedPolyLine%<c-%function(x, r=5, ...){
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
  path(d=c("M", x[,1], rr, "L", x[,n]),  ...)
  }
  
  arrowHeadId<-autoId()
  arrowHeadUrl<-paste0("url(#",arrowHeadId,")")
  
  svgR(wh=WH, 
       #your custom code goes here
       defs(
         marker(id=arrowHeadId, viewBox=c(0, 0, 10, 10), refXY=c(9,5), 
                markerWidth=6, markerHeight=6, orient="auto",
                path( d=c("M", 0, 0, "L", 10, 5, "L", 0, 10, "z") )
         )
       ),
       roundedPolyLine(ptR$x, 40, 
                       stroke.width=3, stroke='black',  fill="none",
                       marker.end=arrowHeadUrl
       ), 
       roundedPolyLine(ptR$y, 30, 
                       stroke.width=3, stroke='black', fill="none",
                       marker.end=arrowHeadUrl
       ), 
        roundedPolyLine(ptR$z, 30, 
                       stroke.width=3, stroke='black', fill="none",
                       marker.end=arrowHeadUrl
       ) 
       
       
       
  )
 
