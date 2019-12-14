library(svgR)
library(tidyverse)
WH<-c(800,600)

# 1. ensure that the attribute preprocessor for circle$id
#    is set to 'fromtolinking' 
# 2. When a new circle is created via a mouse click, 
#     an id will be generated for that circle
# 3. To create an arrow connecting a two circles: 
#     i. Select the id of first circle while pressing the 'a' key
#     ii. Select the id of second circle while pressing the 'b' key
#  note: if step ii is not done, the arrow will point to the last to circle

# Defined by mouse: edit with care!
ptR<-list(
  circle= tribble(
      ~id,         ~fill,       ~cxy,
      'yemedaxb',  '#E06262',   matrix( c(c(367.5,330)), 2),
      'jfnwfdhl',  '#EAFF00',   matrix( c(c(315.5,183)), 2),
      'ncipjeju',  '#FF00EA',   matrix( c(c(191,200)), 2),
      'tqtyjzpm',  '#0CC9C9',   matrix( c(c(485,151)), 2),
      'udrtlyzw',  '#61E619',   matrix( c(c(382.5,62)), 2),
      'chmnkvnq',  '#099161',   matrix( c(c(508,293)), 2),
      'rdkqglru',  '#FF0000',   matrix( c(c(67,89)), 2),
      'pkztfnpg',  '#0011FF',   matrix( c(c(137,380)), 2)
  ),
  links= tribble(
      ~fromId,     ~toId,       ~points,
      'tqtyjzpm',  'udrtlyzw',  matrix(0,2,0),
      'tqtyjzpm',  'yemedaxb',  matrix(0,2,0),
      'chmnkvnq',  'tqtyjzpm',  matrix(0,2,0),
      'cfqzzhcj',  'rdkqglru',  matrix(0,2,0),
      'rdkqglru',  'cfqzzhcj',  matrix(0,2,0),
      'japgmptb',  'ncipjeju',  matrix(0,2,0),
      'japgmptb',  'jfnwfdhl',  matrix(0,2,0),
      'ncipjeju',  'rdkqglru',  matrix(0,2,0),
      'pkztfnpg',  'jfnwfdhl',  matrix(0,2,0)
  )
)

arrs<-ptR$links %>% 
  inner_join( ptR$circle, by=c('fromId'='id') )%>%
  inner_join( ptR$circle, by=c('toId'='id') ) %>%
  select(c(5,7)) %>% 
  rename(p1=cxy.x, p2=cxy.y) 
 

R=50
 

arrow%<c-%function(p1, p2, R=50){
  v=p2-p1
  L=sqrt(sum(v^2))
  xy1=p1 + v*(R/L)
  xy2=p2 - v*((10+R)/L)
  line(xy1=xy1, xy2=xy2, 
    stroke='black', stroke.width=2, 
    marker.end=marker(viewBox=c(0, 0, 10, 10), refXY=c(1,5), 
      markerWidth=6, markerHeight=6, orient="auto",
      path( d=c("M", 0, 0, "L", 10, 5, "L", 0, 10, "z") )
    ) 
  )
}


svgR(wh=WH,
    pmap(ptR$circle, circle, r=R, opacity=.9),
    pmap(arrs, arrow )
)

