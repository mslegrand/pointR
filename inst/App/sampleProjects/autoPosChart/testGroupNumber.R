library(svgR)
library(tidyverse)
WH<-c(600,600)

# Defined by mouse: edit with care!
ptR<-list(
  x= tribble(
      ~fill,      ~points,
      '#F2FF00',  matrix( c(c(133.5,160),c(172.5,116),c(197.5,175)), 2),
      '#6FFF00',  matrix( c(c(191,190),c(208,136),c(239,182),c(175,109)), 2),
      '#6FFF00',  matrix( c(c(348,176),c(425,94),c(474,151)), 2),
      '#6FFF00',  matrix( c(c(144.5,315),c(168.5,274),c(193.5,311)), 2),
      '#48801E',  matrix( c(c(318.5,174),c(431.5,153),c(387.5,217)), 2),
      '#6FFF00',  matrix( c(c(374,289),c(405,252),c(436,285)), 2),
      '#48801E',  matrix( c(c(116,554),c(133,500),c(164,546),c(100,473)), 2),
      '#6FFF00',  matrix( c(c(266,517),c(295,475),c(324,518)), 2),
      '#6FFF00',  matrix( c(c(424,516),c(441,462),c(472,508),c(408,435)), 2)
  ),
  y= tribble(
      ~points,
      matrix( c(c(172,77),c(236,56),c(273,115)), 2),
      matrix( c(c(433,130),c(477,45),c(521,117),c(509,298)), 2),
      matrix( c(c(288,333),c(257,107),c(329,276),c(107,155)), 2)
  ),
  num= tribble(
      ~txt,       ~fill,      ~cxy,
      '1',        '#91912A',  matrix( c(c(107.5,96)), 2),
      '2',        '#00FF00',  matrix( c(c(290,97)), 2),
      '3',        '#FF00FF',  matrix( c(c(509,96)), 2),
      '4',        '#00FF00',  matrix( c(c(89,244)), 2),
      '5',        '#91912A',  matrix( c(c(282.5,250),c(-462.5,449)), 2),
      '6',        'green',    matrix( c(c(504,247)), 2),
      '7',        '#FF00FF',  matrix( c(c(92,395)), 2),
      '8',        '#FFE100',  matrix( c(c(279,397)), 2),
      '9',        '#91912A',  matrix( c(c(461,398)), 2)
  )
)

showNum%<c-%function(txt,cxy,fill){
    text(txt, cxy=cxy, font.size=100, fill=fill)
}

svgR(wh=WH, #viewBox=c(0,0,60,60),
     #your custom code goes here
     #pmap(ptR$x,polygon)#,
     #pmap(ptR$y,polygon)
     pmap(ptR$num, showNum)
     #text(txt='2', cxy=WH/2, font.size=100, fill='red')
)
