library(svgR)
library(tidyverse)
WH<-c(500,350)

svgR(wh=WH,
     #your custom code goes here
     lapply(1:150, function(i){ 
       line(
         xy1=WH/2, 
         xy2=sample(200:300,2), 
         stroke='red'
       )
     })
)
