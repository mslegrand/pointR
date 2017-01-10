#svgR elements: font-face glyph missing-glyph hkern vkern font font-face-name font-face-format font-face-uri animate animateColor animateMotion animateTransform set mpath feFuncA feFuncB feFuncG feFuncR feDistantLight feTurbulence feConvolveMatrix feDiffuseLighting feOffset filter feBlend feColorMatrix feComponentTransfer feComposite feDisplacementMap feFlood feGaussianBlur feImage feMerge feMorphology feSpecularLighting feTile feSpotLight fePointLight svg a altGlyph circle clipPath cursor defs ellipse foreignObject g image line linearGradient marker mask path pattern polygon polyline radialGradient rect script switch symbol text textPath tref tspan use view altGlyphDef altGlyphItem color-profile desc feMergeNode font-face-src glyphRef metadata stop style title font.face missing.glyph font.face.name font.face.format font.face.uri color.profile font.face.src translate rotate rotatR scale u.em u.ex u.px u.pt u.pc u.cm u.mm u.in u.prct u.rad
WH<-c(600,620)

#Defined by mouse: edit with care!
ptR<-list(
  x=matrix(
    c(c( 139,249 ),c( 207,113 ),c( 199,346 )), 2
  )
)

#' render braces
#'
#' v1 is the vertiex of the brace
#' p2 endpoint of brace leg (abs coord)
#' p3 endpoint of brace leg (abs coord)
oneBrace %<c-% function(v, stroke='black', stroke.width=2 ){
  v1<-v[,1];v2<-v[,2];v3<-v[,3]
  xa<-v1-v2
  xb<-v3-v2
  n1<-(sum(xa*xb)/sum(xb*xb))*xb +v2 #m=midpt
  n2<-v2+v1-n1
  n3<-v3+v1-n1
  #now the braces
  list(
    path(d=c("M",v1, "C", n1, n2,v2), 
        stroke=stroke , fill='none', 
        stroke.width=stroke.width,
     marker.end=marker(viewBox=c(0, 0, 10, 10), refXY=c(9,5), 
                           markerWidth=6, markerHeight=6, orient="auto",
                           path( d=c("M", 0, 0, "L", 10, 5, "L", 0, 10, "z") )
         )),
    path(d=c("M",v1, "C", n1, n3,v3), 
        stroke=stroke , fill='none',
        stroke.width=stroke.width,
        marker.end=marker(viewBox=c(0, 0, 10, 10), refXY=c(9,5), 
                           markerWidth=6, markerHeight=6, orient="auto",
                           path( d=c("M", 0, 0, "L", 10, 5, "L", 0, 10, "z") )
         ))
  )
}

svgR(wh=WH, 
  #your custom code goes here
  
 oneBrace(ptR$x)
  
  
 
)

