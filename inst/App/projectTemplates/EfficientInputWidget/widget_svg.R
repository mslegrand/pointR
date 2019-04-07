library(svgR)
library(tidyverse)


WH<-c(400,200)
theta0<-45
CXY<-c(.5,.9)*WH
R<-.4*WH[1]
CMD<-""

if(exists("argv")){
  list2env(argv, envir = environment())
} 


theta2pos<-function(theta, cxy=CXY, r=R){
  theta<-pi*theta/180
  cxy+r*c(cos(theta),-sin(theta)) 
}

pos2theta<-function(pos, cxy=CXY){
  pos<-pos-cxy
  if(abs(pos[1])>.0001){
    theta<-pi/2
  } else {
       theta<-atan(-pos[2]/pos[1])
  }
  180*(theta)/pi
}

descriptor<-function(theta, cxy=CXY, r=R){
   d=list(
             M=CXY,
             L=theta2pos(0),
             A=c( c(R,R), 180,0,0,theta2pos(theta)),
             Z=1
    ) 
}

descriptor2<-function(theta, cxy=CXY, r=R){
   d=list(
             M=theta2pos(0),
             A=c( c(R,R), 180,0,0,theta2pos(theta))
    ) 
}


tmp<-as.character(svgR(wh=WH, 
     #your custom code goes here
     
     
     path(
         d=descriptor(180),
         stroke='#0000FF',
         stroke.width=2,
         fill='none'
     ),
     path(
         d=descriptor(180),
         stroke='#AA00FF',
         stroke.width=20,
         fill='none',
         onClick=CMD
     ),
     line( xy1=CXY, xy2=theta2pos(theta0), stroke='red', stroke.width=5),
     circle( cxy=theta2pos(theta0), r=10, fill='lightblue'),
     circle( cxy=CXY, r=10, fill='lightblue')
))

cat(tmp)

tmp


