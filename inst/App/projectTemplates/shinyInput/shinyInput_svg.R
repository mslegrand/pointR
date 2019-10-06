library(svgR)
library(tidyverse)

#-------- params ----------------------
#` default params
WH<-c(400,800)
CMDS<-paste0("alert('|", c(0,1), "> selected')")
Z<-c( 0+1i, 1-0i)
L<-sqrt(sum(Mod(Z)^2))
Z<-Z/L
ID<-'myQubitbit'
 
#----------function override of params---------- 
if(exists("params") ){
    for(n in names(params)){
        assign(n, params[[n]])
    }
}

#-----any R helper code goes here--------------------------


pieShape<-function(cxy, R=1, thetas=c(0, 2*pi)){
  thetas<-sort(thetas%%(2*pi))
  P<-t(matrix( c(cos(thetas), -sin(thetas)),2))
  P<-P*R+cxy
  largeArc=0
  sf=ifelse( diff(thetas)>=pi, 1,0)
  d=list(
      M=cxy,
      L=P[,1],
      A=c( c(R,R), 0, largeArc, sf, P[,2]),
      Z=0
  )
}

stroke.width=10/WH[1]


cmplx %<c-% function(index, XY, WH, z, stroke.width, fill='yellow', CMD){
    R=1
    r=Mod(z)
    theta=Arg(z)
    svg( xy=XY, wh=WH, id=paste0(ID, index),
        viewBox=c(-1,-1, 2,2),
        circle(cxy=c(0,0), r=R, stroke='none', stroke.width=stroke.width, fill='black'),
        circle(cxy=c(0,0), fill=fill, r=r, stroke='none'),
        path(
            d= pieShape(cxy=c(0,0) ,R=1, thetas=c(0, theta) ),
            stroke='blue',
            stroke.width=stroke.width,
            fill='white',
            opacity=.5
        ),
        onclick=CMD
    )
}


svgR(wh=WH,
     #your custom code goes here
     cmplx(index=0, XY=c(0,0),     WH=WH*c(1,.5), z=Z[1], stroke.width=stroke.width, fill='lightblue',     CMDS[1]),
     cmplx(index=1, XY=WH*c(0,.5), WH=WH*c(1,.5), z=Z[2], stroke.width=stroke.width, fill='lightgreen',    CMDS[2])
)





