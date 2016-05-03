#svgR elements: font-face glyph missing-glyph hkern vkern font font-face-name font-face-format font-face-uri animate animateColor animateMotion animateTransform set mpath feFuncA feFuncB feFuncG feFuncR feDistantLight feTurbulence feConvolveMatrix feDiffuseLighting feOffset filter feBlend feColorMatrix feComponentTransfer feComposite feDisplacementMap feFlood feGaussianBlur feImage feMerge feMorphology feSpecularLighting feTile feSpotLight fePointLight svg a altGlyph circle clipPath cursor defs ellipse foreignObject g image line linearGradient marker mask path pattern polygon polyline radialGradient rect script switch symbol text textPath tref tspan use view altGlyphDef altGlyphItem color-profile desc feMergeNode font-face-src glyphRef metadata stop style title font.face missing.glyph font.face.name font.face.format font.face.uri color.profile font.face.src translate rotate rotatR scale u.em u.ex u.px u.pt u.pc u.cm u.mm u.in u.prct u.rad
WH<-c(600,620)

#Defined by mouse: edit with care!
ptR<-list(
  arrows=matrix(
    c(c( 150,250 ),c( 350,250 ),c( 191,320 ),c( 351,372 ),c( 383,87 ),c( 424,92 ),c( 205,137 ),c( 140,93 ),c( 92,425 ),c( 99,350 ),c( 140,188 ),c( 217,183 ),c( 471,353 ),c( 431,179 ),c( 262,82 ),c( 322,148 )),
  2,),
  boxes=matrix(
    c(c( 357,347 ),c( 461,405 ),c( 130,297 ),c( 188,342 ),c( 183,481 ),c( 349,443 ),c( 501,81 ),c( 433,133 ),c( 184,51 ),c( 238,106 )),
  2,)
)


arrows%<c-%function(x=c(),   ...){
  n<-length(x)-length(x)%%4
  if(n==0){
    return(NULL)
  }
  x<-x[1:n]
  m<-matrix(x,4)
  apply(m, 2, function(mm){
    mm<-matrix(mm, 2,)
    line(xy1=mm[,1], xy2=mm[,2], 
         stroke='black',  ...,
         marker.end=marker(viewBox=c(0, 0, 10, 10), refXY=c(9,5), 
                           markerWidth=6, markerHeight=6, orient="auto",
                           path( d=c("M", 0, 0, "L", 10, 5, "L", 0, 10, "z") )
         )
         
    )  
  })
} 

boxes%<c-%function(x=c(), txt=NULL, ...){
  
  n<-length(x)-length(x)%%4
  if(n==0){
    return(NULL)
  }
  x<-x[1:n]
  m<-matrix(x,4)
  n<-n%/%4
  if(length(txt)<m){
    
    txt<-c(txt, length(txt):m)
  }
  lapply(1:n, function(i){
    mm<-matrix(m[,i], 2,)
    wh= abs(mm[,2]-mm[,1])
    xy<-c(min(mm[1,]),min(mm[2,]))
    g(
      rect(xy=xy, wh= wh, ...),
      if(nchar(txt)>0){
        text(cxy=xy+.5*wh, txt[i], ...)
      }  else {
        NULL
      }
    )
  })
} 

txt<-c("box")
svgR(wh=WH, 
     #your custom code goes here
     arrows(x=ptR$arrows, stroke.width=2, stroke.dash_array=4),
     boxes(ptR$boxes, txt=txt, stroke='black', fill='red')
     
)





