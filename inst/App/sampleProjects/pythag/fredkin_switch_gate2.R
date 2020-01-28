library(svgR)
library(tidyverse)
WH<-c(600,600)

# Defined by mouse: edit with care!
ptR<-list(
  blockers= tribble(
      ~points,
      matrix( c(c(2,2),c(2,3),c(3,3),c(3,2)), 2),
      matrix( c(c(5,2),c(6,3),c(8,3),c(8,2)), 2),
      matrix( c(c(2,7),c(2,8),c(3,8)), 2),
      matrix( c(c(5,7),c(8,7),c(8,8),c(5,8)), 2),
      matrix(0,2,0)
  ),
  lineBlockers= tribble(
      ~points,
      matrix( c(c(2,5),c(2,8)), 2),
      matrix( c(c(6,5),c(8,5)), 2)
  ),
  pathDescriptors= tribble(
      ~points,
      matrix( c(c(1,4),c(9,4)), 2),
      matrix( c(c(4,1),c(4,9)), 2),
      matrix( c(c(1,4),c(3,4),c(3,7),c(4,7),c(4,9)), 2),
      matrix( c(c(4,1),c(4,3),c(5,3),c(5,6),c(9,6)), 2)
  )
)

lineg %<c-% function(points){
    if(ncol(points)==2){
       line(xy1=points[,1],xy2=points[,2], stroke='#AA0000', stroke.width=.2) 
    } else{
        NULL
    }
}

rb=sqrt(2)/2
asPath<-function(points){
    dd<-t(apply(points,1,diff)) #diff of pts
    dd<-c( 'm', c(0,0), 'l', dd )
}

firstPt<-function(points){
    points[,1]
}

keyTimes<-seq(from=0, to=1, length.out=5)
keyPoints3<-cumsum(c(0,2,3,1,2))/8
keyPoints4<-cumsum(c(0,2,1,3,4))/10

begin=1
dur=6

beginPt<-pmap(ptR$pathDescriptors, firstPt)
d<-pmap(ptR$pathDescriptors, asPath)

d3<-asPath(ptR$pathDescriptors$points[[3]] )
d4<-asPath(ptR$pathDescriptors$points[[4]] )

svgR(wh=WH, 
     text(cxy=c(400,25),"Fredkin gate", font.size=25, stroke='black', fill='none'),
    svg(
    viewBox=c(0,0,10,10),
     #your custom code goes here
     pmap(ptR$blockers, polygon, fill='#AA0000'),
     pmap(ptR$lineBlockers, lineg),
     circle( 
        cxy=beginPt[3],
        #ptR$pathDescriptors$points[[3]][,1], 
        r=rb, fill='#AA00FF',
        animateMotion(
            path=d[[3]], begin=begin, 
            dur=dur,
            fill="freeze",
            keyPoints=keyPoints3,
            keyTimes=keyTimes,
            calcMode="linear"
        )
     ),
     circle( 
        cxy=beginPt[4], #ptR$pathDescriptors$points[[4]][,1], 
        r=rb, fill='#AA00FF',
        animateMotion(
            path=d[[4]], begin=begin, 
            dur=dur,
            fill="freeze",
            keyPoints=keyPoints4,
            keyTimes=keyTimes,
            calcMode="linear"
        )
     )
     )
)
