#svgR elements: font-face glyph missing-glyph hkern vkern font font-face-name font-face-format font-face-uri animate animateColor animateMotion animateTransform set mpath feFuncA feFuncB feFuncG feFuncR feDistantLight feTurbulence feConvolveMatrix feDiffuseLighting feOffset filter feBlend feColorMatrix feComponentTransfer feComposite feDisplacementMap feFlood feGaussianBlur feImage feMerge feMorphology feSpecularLighting feTile feSpotLight fePointLight svg a altGlyph circle clipPath cursor defs ellipse foreignObject g image line linearGradient marker mask path pattern polygon polyline radialGradient rect script switch symbol text textPath tref tspan use view altGlyphDef altGlyphItem color-profile desc feMergeNode font-face-src glyphRef metadata stop style title font.face missing.glyph font.face.name font.face.format font.face.uri color.profile font.face.src getNode script translate rotate rotatR scale
WH<-c(600,620)

#Defined by mouse: edit with care!
Pts<-list(
  pos=matrix(
    c(c( 14.5,90 ),c( 15.5,219 ),c( 14.5,356 )),
  2,)
)

txt<-c(
    "http://mslegrand.github.io/svgR/",
    "http://trestletech.com/",
    "http://shiny.rstudio.com/"
)

s<-0
svgR(wh=WH, 
  #your custom code goes here
  
  lapply(1:3, function(i){
    color<-ifelse(i==s, 'red', 'blue') 
    text(txt[i], xy=Pts$pos[,i], font.size=40, stroke=color)
  })
  
  
 
)

