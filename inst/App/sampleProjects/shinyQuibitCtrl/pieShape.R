library(svgR)
library(tidyverse)
WH<-c(600,600)

# Defined by mouse: edit with care!
ptR<-list(
  x= tribble(
      ~points,
      matrix( c(c(69,71)), 2),
      matrix(0,2,0)
  )
)


pieShape<-function(cxy, R=min(cxy), theta1=0, theta2=2*pi){
  P0=c(cos(theta1),-sin(theta1))*R+cxy
  P1=c(cos(theta2),-sin(theta2))*R+cxy
  largeArc=ifelse( (theta2-theta1)>pi, 1,0)
  sf=0
  d=list(
      M=cxy,
      L=P0,
      A=c( c(R,R), 0, largeArc, sf, P1),
      Z=0
  )
}

svgR(wh=WH,
     #your custom code goes here
    path(
        d=pieShape(cxy=WH/2,R=100, 0, 3*pi/2),
        stroke='#0000FF',
        stroke.width=2,
        fill='lightblue'
    )
)
