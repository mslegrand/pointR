library(svgR)
library(tidyverse)
WH<-c(800,800) 

source('develObjects.R') 


#Defined by mouse: edit with care!
ptR<-list(
  source=tibble(
      label = c('source','source','source','source','source','source','source','source','source','source','source','source','source two','source','source','source','source','source'),
      points = list(matrix( c(c(97,53),c(283,123)), 2), matrix( c(c(450,550),c(300,600)), 2), matrix( c(c(750,150),c(650,250)), 2), matrix( c(c(200,550),c(150,500)), 2), matrix( c(c(250,400),c(150,450)), 2), matrix( c(c(650,600),c(650,450)), 2), matrix( c(c(300,450),c(400,500)), 2), matrix( c(c(350,700),c(450,650)), 2), matrix( c(c(650,400),c(700,450)), 2), matrix( c(c(150,150)), 2), matrix( c(c(650,100),c(550,200)), 2), matrix( c(c(700,350),c(650,300)), 2), matrix( c(c(100,200),c(250,300)), 2), matrix( c(c(500,500),c(600,400)), 2), matrix( c(c(150,700),c(250,600)), 2), matrix( c(c(150,150),c(250,200)), 2), matrix( c(c(350,50),c(450,150)), 2), matrix( c(c(50,150),c(150,250)), 2))
  ),
  observer=tibble(
      points = list(matrix( c(c(351,260),c(430,175)), 2), matrix( c(c(450,350),c(550,200)), 2))
  ),
  expression=tibble(
      points = list(matrix( c(c(157,281),c(296,346)), 2), matrix( c(c(379,309),c(428,357)), 2))
  ),
  arrows=tibble(
      points = list(matrix( c(c(282,87),c(318,87),c(349,211),c(433,210)), 2), matrix( c(c(250,250),c(350,250),c(350,300),c(450,300)), 2), matrix( c(c(261,113)), 2))
  ),
  block=tibble(
      points = list(matrix( c(c(72,296),c(555,622)), 2))
  )
)

svgR(wh=WH ,
    #your custom code goes here 
    pmap(ptR$source, reactiveSource, fill='yellow'),
    pmap(ptR$observer, reactiveObserver),
    pmap(ptR$expression, reactiveExpression),
    pmap(ptR$arrows, arrow),
    pmap(ptR$block, block)
    
)





