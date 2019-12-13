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

pieShape2<-function(cxy, R=min(cxy), thetas=c(0,2*pi), dR=30){
    thetas=sort(thetas%%(2*pi))
    RI=R-dR
    P=t(matrix(c( cos(thetas), -sin(thetas)),2))
    PP=P*RI + cxy
    P= P*R +cxy
    largeArc=ifelse(diff(thetas)>pi,1,0)
    sf=0
    d=list(
        M=P[,1],
        A=c( c(R,R), 0, largeArc, sf, P[,2]),
        L=PP[,2],
        A=c( c(RI,RI), 0, largeArc, 1-sf, PP[,1]),
        Z=0
    )
}

svgR(wh=WH,
     #your custom code goes here
    path(
        d=pieShape2(cxy=WH/2,R=100, thetas=c(0, 5*pi/4)),
        stroke='#0000FF',
        stroke.width=2,
        fill='lightblue'
    )
)
