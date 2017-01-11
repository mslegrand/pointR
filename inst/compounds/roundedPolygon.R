#' Compound to create a polygon with rounded edges. 
#' @param points a 2 by n matrix with columns being coordinates
#' @param radius of arc at each corner 
#' @return a path element representing the rounded polygon
#'
#' @note This constrains the distance from a given corner point
#'  to a  point intersecting the corresponding arc and an adjacent 
#'  side to be the minimum of r and 1/2 the length of that side  
roundedPolygon %<c-% function(points, r=5, ...){
  if(length(points)<6){ # at least 3 points
    return(NULL)
  }
  
  n<-ncol(points) 
  dv<-points-points[,c(2:n,1)]
  d<-apply( dv ,2, function(x)sqrt(sum(x)^2) )
  lambda<-sapply(r/d, function(x)min(x,.5)) # lambda[n] applies to the points[,n],points[,n+1] interval
  
  mB<-diag(1-lambda) +  diag(lambda)[c(n, 1:(n-1)),]
  mA<-diag(lambda) +  diag(1-lambda)[c(n, 1:(n-1)),]
  mA<-mA[,c(n, 1:(n-1))]
  
  a<-points%*%mA
  b<-points%*%mB   
  rA<-c("M",rep("L", n-1))
  rr<-rbind(rA, a, "Q", points, b)
  d=c(rr,"Z")
  path(d=d,  ...)
}
