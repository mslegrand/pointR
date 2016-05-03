WH<-c(600,620)

#Defined by mouse: edit with care!
ptR<-list(
  x=matrix(
    c(c( 100,100 ),c( 100,400 ),c( 300,400 ),c( 302.5,250 )),
  2,),
  z=matrix(
    c(c( 153.5,151 ),c( 154.5,202 ),c( 251.5,199 ),c( 253.5,150 ),c( 21.5,21 ),c( 300.5,100 ),c( 405.5,100 ),c( 398.5,197 ),c( 499.5,200 ),c( 352.5,304 ),c( 17.5,30 ),c( 415.5,425 ),c( 482.5,342 ),c( 377.5,357 ),c( 329.5,360 ),c( 334.5,257 ),c( 11.5,10 ),c( 121.5,475 ),c( 545.5,477 ),c( 552.5,48 ),c( 279.5,46 ),c( 275.5,345 ),c( 138.5,348 ),c( 138.5,67 ),c( 65.5,69 ),c( 65.5,499 ),c( 548.5,505 ),c( 26.5,12 ),c( 183.5,50 ),c( 234.5,53 )),
  2,)
)



roundedPolyLine%<c-%function(x, r=5, ...){
  nx<-length(x)
  if(nx<4){
    return(NULL)
  }
  n<-ncol(x)
  if(2==ncol(x)){
    return( path(d=c("M", x[,1], "L", x[,n]),  ...))
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

roundedPolyLineArrays%<c-%function(p, ...){
  if(length(p)<4){
    return(NULL)
  }
  n<-ncol(p)
  brks<-apply(p,2, function(x){ max(x)<50})
  brkss<-(1+cumsum(brks))*(!brks)
  if(max(brkss)==0){
    return(NULL)
  }
  
  lapply(1:max(brkss),function(i){
    p_i<-p[,brkss==i]
    roundedPolyLine(p_i, ...)
  }) 
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
     roundedPolyLineArrays(ptR$z, r=20, 
                           stroke.width=3, stroke='black', 
                           fill="none",
                           marker.end=arrowHeadUrl
     ) 
     
     
     
     
)

