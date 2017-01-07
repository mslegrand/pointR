#svgR elements: font-face glyph missing-glyph hkern vkern font font-face-name font-face-format font-face-uri animate animateColor animateMotion animateTransform set mpath feFuncA feFuncB feFuncG feFuncR feDistantLight feTurbulence feConvolveMatrix feDiffuseLighting feOffset filter feBlend feColorMatrix feComponentTransfer feComposite feDisplacementMap feFlood feGaussianBlur feImage feMerge feMorphology feSpecularLighting feTile feSpotLight fePointLight svg a altGlyph circle clipPath cursor defs ellipse foreignObject g image line linearGradient marker mask path pattern polygon polyline radialGradient rect script switch symbol text textPath tref tspan use view altGlyphDef altGlyphItem color-profile desc feMergeNode font-face-src glyphRef metadata stop style title font.face missing.glyph font.face.name font.face.format font.face.uri color.profile font.face.src translate rotate rotatR scale u.em u.ex u.px u.pt u.pc u.cm u.mm u.in u.prct u.rad
WH<-c(600,620)

roundedPolygon %<c-% function(points, r=5, ...){
  if(length(points)<6){ # at least 3 points
    return(NULL)
  }
  
  n<-ncol(points) 
  dv<-points-points[,c(2:n,1)]
  d<-apply( dv ,2, function(x)sqrt(sum(x)^2) )
  lambda<-sapply(r/d, function(x)min(x,.5)) # lambda[n] applies to the points[,n],points[,n+1] interval
  
  mB<-diag(1-lambda) +  diag(lambda)[c(n, 1:(n-1)),]
  mA<-diag(lambda) +  diag(1-lambda)[c(n, 1:(n-1)),]
  mA<-mA[,c(n, 1:(n-1))]

  a<-points%*%mA
  b<-points%*%mB   
  rA<-c("M",rep("L", n-1))
  rr<-rbind(rA, a, "Q", points, b)
  d=c(rr,"Z")
  path(d=d,  ...)
}


#Defined by mouse: edit with care!
ptR<-list(
  x=matrix(
    c(c( 93,179 ),c( 294,123 ),c( 267,349 ),c( 147,335 )),
  2,)
)

svgR(wh=WH, 
  #your custom code goes here
  
  roundedPolygon(points=ptR$x, r=50, fill="blue",opacity=.5)
  
  
 
)

