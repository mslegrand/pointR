#svgR elements: font-face glyph missing-glyph hkern vkern font font-face-name font-face-format font-face-uri animate animateColor animateMotion animateTransform set mpath feFuncA feFuncB feFuncG feFuncR feDistantLight feTurbulence feConvolveMatrix feDiffuseLighting feOffset filter feBlend feColorMatrix feComponentTransfer feComposite feDisplacementMap feFlood feGaussianBlur feImage feMerge feMorphology feSpecularLighting feTile feSpotLight fePointLight svg a altGlyph circle clipPath cursor defs ellipse foreignObject g image line linearGradient marker mask path pattern polygon polyline radialGradient rect script switch symbol text textPath tref tspan use view altGlyphDef altGlyphItem color-profile desc feMergeNode font-face-src glyphRef metadata stop style title font.face missing.glyph font.face.name font.face.format font.face.uri color.profile font.face.src getNode script translate rotate rotatR scale u.em u.ex u.px u.pt u.pc u.cm u.mm u.in u.prct u.rad
WH<-c(600,620)

#Defined by mouse: edit with care!
ptR<-list(
  x=matrix(
    c(c( 424,147 ),c( 532,220 )),
  2,)
)

ce<-ptR$x[,1]
cwh<-2*(ptR$x[,2]-ce)
ident<-diag(3)[-3,]

svgR(wh=WH, 
     #your custom code goes here
     circle(cxy=WH/2, r=100, 
            fill=linearGradient( y12=c(0,1), x12=c(0,0),
                                 colors=c('red','black')
            ),
            class='draggable',
            transform=matrix(c(0.683, -0.7303, 0.7303, 0.683, -270.3, 204.3),2,)
     ),
     g(
       class='draggable',
       transform=NULL,
       ellipse(cxy=ce, rxy=cwh/2, 
        fill=radialGradient(colors=c('green','blue','black'))
       ),
       rect(cxy=ce, wh=cwh, fill="none", stroke="blue")
       
     )
     
)
