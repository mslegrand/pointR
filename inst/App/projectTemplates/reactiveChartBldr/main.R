library(svgR)
library(tidyverse)
WH<-c(800,800) 

source('develObjects.R') 


#Defined by mouse: edit with care!
ptR<-list(
  source=tibble(
      points = list(matrix(0,2,0))
  ),
  observer=tibble(
      points = list(matrix(0,2,0))
  ),
  expression=tibble(
      points = list(matrix(0,2,0))
  ),
  arrows=tibble(
      points = list(matrix(0,2,0))
  ),
  block=tibble(
      points = list(matrix(0,2,0))
  )
)

svgR(wh=WH, 
  NULL # your custom code goes here 
)





