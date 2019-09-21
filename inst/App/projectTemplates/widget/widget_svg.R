library(svgR)
library(tidyverse)


WH<-c(400,200)
theta0<-45
CXY<-c(.5,.9)*WH
R<-.4*WH[1]
CMD<-""
R0<-10

#----------function override of params---------- 
if(exists("params") ){
    for(n in names(params)){
        assign(n, params[[n]])
    }
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

descriptor2<-function( dR=10){
   d=list(
             M=CXY-c(R+dR,0),
             A=c( c(R,R)+dR, 0,1,1,CXY+c(R+dR,0)),
             L=CXY+c(R-dR,0),
             A=c(c(R,R)-dR, 0,1,0,CXY+c(-R+dR,0)),
             Z=0
    ) 
}


svgR(wh=WH, 
     #your custom code goes here
     g(
       rect(xy=c(0,CXY[2]),wh=WH , fill='#AA88FF',
          transform=list(rotate=c(0,CXY))
       ), 
       clip.path= clipPath( path(d=descriptor(180)))         
     ),
     g(
       path( d=descriptor(180),stroke='black',
         stroke.width=2*R0,
         fill=radialGradient(fxy=c(.5,.9),colors=c('lightblue', 'blue', 'black')),
         onClick=CMD
       ),
       path( d=descriptor2(), stroke='black', 
         fill=linearGradient(colors=c('brown',  'white','yellow')),
         onClick=CMD
        ),
       lapply(seq.int(from=0,to=180,by=10),function(i){
         xy1<-CXY+c(0,-R+R0)
         xy2<-CXY+c(0,-R-R0)
         xy3<-CXY+c(0,-R-2*R0)
         g(
           text(cxy=xy3,i, font.size=8),
           line(xy1=xy1,xy2=xy2, stroke='black'),
           transform=rotate(i-90,CXY),
           onClick=CMD
         )
       })
     ),
     g(
       line( xy1=CXY, xy2=CXY+c(0,-R+R0+2), stroke='white', stroke.width=2,
         marker.end=
            marker(
               viewBox=c(0, 0, 10, 10), 
               refXY=c(10,5), 
               fill='white',
                markerWidth=8, markerHeight=8, orient="auto",
               path( d=c("M", 0, 0, "L", 10, 5, "L", 0, 10, "z") )
            )
       ),
       transform=rotate(theta0-90,CXY)
     ),
     circle( cxy=CXY, r=R0,  stroke='blue', 
        fill=radialGradient(colors=c('white','brown'))
     )
)



