library(svgR)
library(tidyverse)
WH<-c(600,400)

# Defined by mouse: edit with care!
ptR<-list(
  x=tribble(
    ~points,
    matrix(NA,2,0)
  )
)

svgR(wh=WH,
     #your custom code goes here
     polygon(
        points=WH*matrix(c(.45,.5,.25,.75,.55,.5),2),
        stroke='none',
        stroke.width='black',
        fill='#00FFFF'
    ),
    rect(
        cxy=WH/2,
        wh=c(.47,.3)*WH,
        rxy=c(20,20),
        stroke='none',
        fill='#00FFFF'
    )
    
)
