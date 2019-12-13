library(svgR)
library(tidyverse)
WH<-c(600,400)

# Defined by mouse: edit with care!
ptR<-list(
  x= tribble(
      ~points,
      matrix( c(c(72,54),c(264,54),c(273,174),c(372,180),c(366,49)), 2)
  )
)

 #repplace x by points 
# source('arrow.cmp.R')

# cat(dir())
  
#   WH<-c(500,300)
#   svgR(
#     wh=WH,
#   arrow(matrix(c(100,100,100,200,300,200),2)),
#     pmap(ptR$x, arrow)
#   )
  
getwd()
