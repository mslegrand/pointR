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
     NULL
)
