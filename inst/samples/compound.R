

#svgR elements: font-face glyph missing-glyph hkern vkern font font-face-name font-face-format font-face-uri animate animateColor animateMotion animateTransform set mpath feFuncA feFuncB feFuncG feFuncR feDistantLight feTurbulence feConvolveMatrix feDiffuseLighting feOffset filter feBlend feColorMatrix feComponentTransfer feComposite feDisplacementMap feFlood feGaussianBlur feImage feMerge feMorphology feSpecularLighting feTile feSpotLight fePointLight svg a altGlyph circle clipPath cursor defs ellipse foreignObject g image line linearGradient marker mask path pattern polygon polyline radialGradient rect script switch symbol text textPath tref tspan use view altGlyphDef altGlyphItem color-profile desc feMergeNode font-face-src glyphRef metadata stop style title font.face missing.glyph font.face.name font.face.format font.face.uri color.profile font.face.src translate rotate rotatR scale u.em u.ex u.px u.pt u.pc u.cm u.mm u.in u.prct u.rad
WH<-c(600,620)

#Defined by mouse: edit with care!
ptR<-list(
  x=matrix(
    c(c( 256.5,178 ),c( 195.5,242 )),
    2,),
  y=matrix(
    c(c( 382.5,204 ),c( 420.5,293 )),
    2,),
  z=matrix(
    c(c( 140.5,337 ),c( 229.5,452 )),
    2,)
)


#x<-ptR$x



myShape%<c-%function(x, txt="", stroke="white", ... ){
  if(is.null(x) || length(x)<4){
    return(NULL)
  }
  x<-x[,1:2]
  g(
    rect(
      xy=apply(x,1,min),
      wh=abs(apply(x,1, diff)),
      ...
    ),
    text(
      cxy=apply(x,1,mean),
      txt, 
      stroke=stroke, 
      ...
    )
  )
}

svgR(wh=WH, 
     #your custom code goes here
     myShape(ptR$x, txt="hello"),
     myShape(ptR$y, txt="there"),
     myShape(ptR$z, txt="ouch", txt.stroke='white',
             fill='blue', font.size=20, rxy=c(10,10) ) 
)
