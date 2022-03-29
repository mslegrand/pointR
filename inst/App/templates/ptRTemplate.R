library(svgR)
library(tidyverse)
WH<-c(500,350)

# Defined by mouse: edit with care!
ptR<-list(
  x=tribble(
    ~points,
    matrix(0,2,0)
  )
)

svgR(wh=WH,
     #your custom code goes here
     NULL
)
