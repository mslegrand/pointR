library(svgR)
library(tidyverse)
WH<-c(600,400)

# Defined by mouse: edit with care!
ptR<-list(
  x= tribble(
      ~points,
      matrix(0,2,0)
  )
)


pieShape<-function(cxy, R=min(cxy), theta1=0, theta2=2*pi){
  P0=c(cos(theta1),-sin(theta1))*R+cxy
  P1=c(cos(theta2),-sin(theta2))*R+cxy
  largeArc=ifelse( (theta2-theta1)>pi, 1,0)
  sf=0
  d=list(
      M=P0,
      A=c( c(R,R), 0, largeArc, sf, P1)
  )
}

R=100

svgR(wh=WH,
     #your custom code goes here
    circle(cxy=WH/2, R=R, fill='lightblue') ,
    path(
        d=pieShape(cxy=WH/2,R=R, 0, 3*pi/4),
        stroke='#0000FF',
        stroke.width=20,
        fill='none'
    )
)
