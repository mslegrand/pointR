#svgR elements: font-face glyph missing-glyph hkern vkern font font-face-name font-face-format font-face-uri animate animateColor animateMotion animateTransform set mpath feFuncA feFuncB feFuncG feFuncR feDistantLight feTurbulence feConvolveMatrix feDiffuseLighting feOffset filter feBlend feColorMatrix feComponentTransfer feComposite feDisplacementMap feFlood feGaussianBlur feImage feMerge feMorphology feSpecularLighting feTile feSpotLight fePointLight svg a altGlyph circle clipPath cursor defs ellipse foreignObject g image line linearGradient marker mask path pattern polygon polyline radialGradient rect script switch symbol text textPath tref tspan use view altGlyphDef altGlyphItem color-profile desc feMergeNode font-face-src glyphRef metadata stop style title font.face missing.glyph font.face.name font.face.format font.face.uri color.profile font.face.src getNode script

#Illustrates draggable

WH<-c(600,620)

#points defined by mouse clicks, edit with care!
ptR<-list(
  x=matrix(
    c(c( 137,339 ),c( 111,153 ),c( 383,133 ),c( 357,329 ),c( 281,461 )), 2
  )
)


svgR(wh=WH, 
     
     polygon(points=ptR$x, fill="blue",opacity=.5),
     rect( class="draggable", opacity=.5,
           xy=c(0,30), wh=c(80,80), fill="blue", 
           transform=matrix(c(1.284, 0.8677, -1.756, 0.6346, 244.5, 6.869),2)
     ),
     circle( class="draggable", 
             cxy=c(100,230), r=50, fill="red", opacity=.5,
             transform=matrix(c(0.5713, -1.476, 1.365, 0.6176, 93.9, 144.6),2)
     )
)
