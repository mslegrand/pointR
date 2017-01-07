#svgR elements: font-face glyph missing-glyph hkern vkern font font-face-name font-face-format font-face-uri animate animateColor animateMotion animateTransform set mpath feFuncA feFuncB feFuncG feFuncR feDistantLight feTurbulence feConvolveMatrix feDiffuseLighting feOffset filter feBlend feColorMatrix feComponentTransfer feComposite feDisplacementMap feFlood feGaussianBlur feImage feMerge feMorphology feSpecularLighting feTile feSpotLight fePointLight svg a altGlyph circle clipPath cursor defs ellipse foreignObject g image line linearGradient marker mask path pattern polygon polyline radialGradient rect script switch symbol text textPath tref tspan use view altGlyphDef altGlyphItem color-profile desc feMergeNode font-face-src glyphRef metadata stop style title font.face missing.glyph font.face.name font.face.format font.face.uri color.profile font.face.src translate rotate rotatR scale u.em u.ex u.px u.pt u.pc u.cm u.mm u.in u.prct u.rad
WH<-c(600,620)

#Defined by mouse: edit with care!
ptR<-list(
  x=matrix(
    c(c( 149.5,151 ),c( 301.5,303 ),c( 200.5,403 ),c( 369.5,443 ),c( 388.5,193 ),c( 472.5,286 ),c( 428.5,357 ),c( 528.5,473 )),
  2,)
)

x<-ptR$x
length(x)
m<-matrix(x,4)

svgR(wh=WH, 
  #your custom code goes here
  
  apply(m, 2, function(x){
    rect(xy=x[1:2], wh=abs(x[3:4]-x[1:2]))
  })
  
  
 
)

