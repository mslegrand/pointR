#svgR elements: font-face glyph missing-glyph hkern vkern font font-face-name font-face-format font-face-uri animate animateColor animateMotion animateTransform set mpath feFuncA feFuncB feFuncG feFuncR feDistantLight feTurbulence feConvolveMatrix feDiffuseLighting feOffset filter feBlend feColorMatrix feComponentTransfer feComposite feDisplacementMap feFlood feGaussianBlur feImage feMerge feMorphology feSpecularLighting feTile feSpotLight fePointLight svg a altGlyph circle clipPath cursor defs ellipse foreignObject g image line linearGradient marker mask path pattern polygon polyline radialGradient rect script switch symbol text textPath tref tspan use view altGlyphDef altGlyphItem color-profile desc feMergeNode font-face-src glyphRef metadata stop style title font.face missing.glyph font.face.name font.face.format font.face.uri color.profile font.face.src getNode script translate rotate rotatR scale
WH<-c(600,620)

#Defined by mouse: edit with care!
ptR<-list(
  x=matrix(
    c(c( 131.5,180 ),c( 150.5,341 ),c( 245.5,180 ),c( 295.5,309 ),c( 246.5,413 )),
  2,)
)

cxy<-ptR$x

concentric<-function(cxy){
  substitute(lapply(1:4, function(i){
    circle(cxy=cxy, r=10*i, fill='none', stroke='green')
  }))
}

svgR(wh=WH, 
  #your custom code goes here
    apply(ptR$x, 2, function(cxy2){
        eval(concentric(cxy2))
    })
  
 
)

