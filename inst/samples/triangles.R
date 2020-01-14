#svgR elements: font-face glyph missing-glyph hkern vkern font font-face-name font-face-format font-face-uri animate animateColor animateMotion animateTransform set mpath feFuncA feFuncB feFuncG feFuncR feDistantLight feTurbulence feConvolveMatrix feDiffuseLighting feOffset filter feBlend feColorMatrix feComponentTransfer feComposite feDisplacementMap feFlood feGaussianBlur feImage feMerge feMorphology feSpecularLighting feTile feSpotLight fePointLight svg a altGlyph circle clipPath cursor defs ellipse foreignObject g image line linearGradient marker mask path pattern polygon polyline radialGradient rect script switch symbol text textPath tref tspan use view altGlyphDef altGlyphItem color-profile desc feMergeNode font-face-src glyphRef metadata stop style title font.face missing.glyph font.face.name font.face.format font.face.uri color.profile font.face.src getNode script translate rotate rotatR scale
WH<-c(600,620)

#Defined by mouse: edit with care!
ptR<-list(
  x=matrix(
    c(c( 79,195 ),c( 218,87 ),c( 229,350 )), 2
  )
)

svgR(wh=WH, 
  #your custom code goes here
  
  polygon(
    points=ptR$x,
    fill='red',
    opacity=.5,
    transform=matrix(c(0.4211, -0.907, 0.907, 0.4211, 85.26, 277.6),2),
    class='adjustable'
  )
  
  
 
)
