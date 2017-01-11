#svgR elements: font-face glyph missing-glyph hkern vkern font font-face-name font-face-format font-face-uri animate animateColor animateMotion animateTransform set mpath feFuncA feFuncB feFuncG feFuncR feDistantLight feTurbulence feConvolveMatrix feDiffuseLighting feOffset filter feBlend feColorMatrix feComponentTransfer feComposite feDisplacementMap feFlood feGaussianBlur feImage feMerge feMorphology feSpecularLighting feTile feSpotLight fePointLight svg a altGlyph circle clipPath cursor defs ellipse foreignObject g image line linearGradient marker mask path pattern polygon polyline radialGradient rect script switch symbol text textPath tref tspan use view altGlyphDef altGlyphItem color-profile desc feMergeNode font-face-src glyphRef metadata stop style title font.face missing.glyph font.face.name font.face.format font.face.uri color.profile font.face.src translate rotate rotatR scale u.em u.ex u.px u.pt u.pc u.cm u.mm u.in u.prct u.rad
WH<-c(600,620)

#Defined by mouse: edit with care!
ptR<-list(
  p=matrix(
    c(c( 170,97 ),c( 309,96 ),c( 317,259 )),
  2,)
)


m<-ptR$p[,1]
q<-ptR$p[,2]
p<-ptR$p[,3]

svgR(wh=WH, 
  #your custom code goes here
  
  lapply(c(0,50,100,150),function(y){
    path( d=c("M", m+c(0,y), "Q", q+c(0,y), p+c(0,y)), stroke="red", stroke.width=1,
    fill='none')
    }
  )
)

