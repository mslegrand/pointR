arrow %<c-% function(points, r=10, stroke='black', stroke.width=2, ...){
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
        fill='none',
        stroke=stroke,
        stroke.width=stroke.width,
        marker.end= marker( viewBox=c(0, 0, 10, 10), refXY=c(9,5), 
              markerWidth=6, markerHeight=6, orient="auto",
              path( d=c("M", 0, 0, "L", 10, 5, "L", 0, 10, "z") )
       
        ),
    ...)
 
  }
  

