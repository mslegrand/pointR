pts2arrows<-function(x, stroke='black', 
    stroke.width=3, ...){
  arrow %<c-% function(m, stroke='black', stroke.width=3, ...){
    line(xy1=m[,1], xy2=m[,2], 
          stroke=stroke, stroke.width=stroke.width, ...,
         marker.end=marker(viewBox=c(0, 0, 10, 10), refXY=c(9,5), 
                           markerWidth=6, markerHeight=6, orient="auto",
                           path( d=c("M", 0, 0, "L", 10, 5, "L", 0, 10, "z") )
         ) 
    )
  }
  if(length(x)<4){
    return(NULL)
  }
  x<-x[,1:(2*ncol(x)%/%2)]
  mx<-matrix(x,4,)
  lapply(1:ncol(mx), function(i){
    m<- matrix(mx[,i],2)
    arrow(m, stroke=stroke, stroke.width=stroke.width, ...)  
  })
}
