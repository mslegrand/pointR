library(svgR)
library(tidyverse)
WH<-c(600,600)

# Defined by mouse: edit with care!
ptR<-list(
  re= tribble(
      ~points,
      matrix( c(c(98,100),c(198,150)), 2),
      matrix( c(c(101,199),c(201,249)), 2),
      matrix( c(c(99,300),c(199,350)), 2)
  ),
  ci= tribble(
      ~cxy,
      matrix( c(c(301,98)), 2),
      matrix( c(c(301,199)), 2),
      matrix( c(c(300,300)), 2)
  )
)


myR%<c-%function(points){
  xy<-apply(points, 1, min)
  wh<-abs(apply(points, 1, diff))
  rect(xy=xy, wh=wh, fill='orange')
}
svgR(wh=WH,
     #your custom code goes here
    pmap(ptR$re, myR),
    pmap(ptR$ci, circle, r=50, fill='lightblue')
)
