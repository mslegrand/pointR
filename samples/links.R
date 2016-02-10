#svgR elements: font-face glyph missing-glyph hkern vkern font font-face-name font-face-format font-face-uri animate animateColor animateMotion animateTransform set mpath feFuncA feFuncB feFuncG feFuncR feDistantLight feTurbulence feConvolveMatrix feDiffuseLighting feOffset filter feBlend feColorMatrix feComponentTransfer feComposite feDisplacementMap feFlood feGaussianBlur feImage feMerge feMorphology feSpecularLighting feTile feSpotLight fePointLight svg a altGlyph circle clipPath cursor defs ellipse foreignObject g image line linearGradient marker mask path pattern polygon polyline radialGradient rect script switch symbol text textPath tref tspan use view altGlyphDef altGlyphItem color-profile desc feMergeNode font-face-src glyphRef metadata stop style title font.face missing.glyph font.face.name font.face.format font.face.uri color.profile font.face.src getNode script translate rotate rotatR scale
WH<-c(600,620)

#Defined by mouse: edit with care!
Pts<-list(
  pos=matrix(
    c(c( 14.5,90 ),c( 57,135 ),c( 19,202 ),c( 69,242 ),c( 25,299 ),c( 60,352 ),c( 186,471 )),
  2,)
)

txt<-c(
    "Shiny Server as Framework",
    "http://shiny.rstudio.com/",
    "Ace Editor for coding",
    "http://trestletech.com/",
    "svgR package for svg graphics",
    "http://mslegrand.github.io/svgR/",
    "(c) m.s.legrand 2016"
)

fs<-c(40,40,40,40,40,35,20)
sc<-c("red","red","green","green","blue","blue","grey")
s<-0
svgR(wh=WH, 
  #your custom code goes here
  
  lapply(1:7, function(i){
    color<-ifelse(i==s, 'red', 'blue') 
    text(txt[i], xy=Pts$pos[,i], font.size=fs[i], stroke=sc[i],
    fill=sc[i])
  })
  
  
 
)

