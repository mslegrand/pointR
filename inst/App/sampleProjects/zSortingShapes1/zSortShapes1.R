library(svgR)
library(tidyverse)
WH<-c(600,400)

# We place all items in the same tibble, using a helper function
# to determine which shape to render.
# Thus z-sorting can again be accomplished by simply
# drag and drop the rows


ptR<-list(
  x= tribble(
      ~shape,     ~fill,      ~points,
      'ellipse',  'red',      matrix( c(c(173,122),c(275,172)), 2),
      'rect',     '#001EFF',  matrix( c(c(113,63),c(278,141)), 2),
      'ellipse',  '#FFEE00',  matrix( c(c(208,86),c(298,148)), 2),
      'rect',     '#00FF33',  matrix( c(c(222,145),c(342,222)), 2)
  )
)


ffn %<c-% function(shape, points, fill){
    sxy<-apply(points, 1, min)
    swh<-apply(points, 1, max)-sxy
    if(shape=='rect'){
        rect(xy=sxy, wh=swh, fill=fill)
    } else if( shape=='ellipse'){
        ellipse(cxy=sxy+swh/2, rxy=swh/2, fill=fill)
    }
}

svgR(wh=WH,
     #your custom code goes here
     pmap(ptR$x, ffn)
)
