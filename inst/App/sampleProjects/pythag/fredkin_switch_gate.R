library(svgR)
library(tidyverse)
WH<-c(500,500)

# Defined by mouse: edit with care!
ptR<-list(
  xy= tribble(
      ~points,
      matrix( c(c(100,100),c(100,300)), 2)
  ),
  ul= tribble(
      ~points,
      matrix( c(c(100,100)), 2)
  )
)

ds<-50
ss<-seq(from=100, length.out=7, by=ds)
br<-ds*sqrt(2)/2
d1<-

svgR(wh=WH,
     lapply(ss, function(s){
        line(
            xy1=c(ss[1], s),
            xy2=c(ss[7], s),
            stroke.dasharray=8,
            stroke='green',
            stroke.width=2
        )
     }),
     lapply(ss, function(s){
        line(
            xy1=c(s,ss[1]),
            xy2=c(s,ss[7]),
            stroke.dasharray=8,
            stroke='green',
            stroke.width=2
        )
     }),
     rect(xy=ss[1]*c(1,1), wh=c(ds,ds), fill='#AA0000'),
     polygon(
        points=c(
         c(ss[1], ss[6]),
         c(ss[1], ss[7]),
         c(ss[2], ss[7])
        ),
        fill='#AA0000'
    ),
    rect(xy=c(ss[4], ss[6]), wh=c(3*ds,ds), fill='#AA0000'),
    polygon(
        points=c(
         c(ss[4], ss[1]),
         c(ss[5], ss[2]),
         c(ss[7], ss[2]),
         c(ss[7], ss[1])
        ),
        fill='#AA0000'
    ),
    line(
        xy1=c(ss[4], ss[4]),
        xy2=c(ss[7], ss[4]),
        stroke='#AA0000',
        stroke.width=10
    ),
    line(
        xy1=c(ss[1], ss[4]),
        xy2=c(ss[1], ss[7]),
        stroke='#AA0000',
        stroke.width=10
    )
)
