#svgR elements: font-face glyph missing-glyph hkern vkern font font-face-name font-face-format font-face-uri animate animateColor animateMotion animateTransform set mpath feFuncA feFuncB feFuncG feFuncR feDistantLight feTurbulence feConvolveMatrix feDiffuseLighting feOffset filter feBlend feColorMatrix feComponentTransfer feComposite feDisplacementMap feFlood feGaussianBlur feImage feMerge feMorphology feSpecularLighting feTile feSpotLight fePointLight svg a altGlyph circle clipPath cursor defs ellipse foreignObject g image line linearGradient marker mask path pattern polygon polyline radialGradient rect script switch symbol text textPath tref tspan use view altGlyphDef altGlyphItem color-profile desc feMergeNode font-face-src glyphRef metadata stop style title font.face missing.glyph font.face.name font.face.format font.face.uri color.profile font.face.src translate rotate rotatR scale u.em u.ex u.px u.pt u.pc u.cm u.mm u.in u.prct u.rad
WH<-c(600,620)

seqPts<-function(N, xy1, xy2){
  lambda<-matrix(seq(0,1,length.out=N),1)
  matrix(xy1,2)%*%(1-lambda) +
    matrix(xy2,2)%*%lambda
}

textAlong%<c-%function(textVector, m, ...){
  if(length(m)<4){
    return(NULL)
  }
  m<-matrix(m,2)
  xy1<-m[,1]
  xy2<-m[,2]
  if(ncol(m)>2){
    tmp<-m[,3]-m[,2]
    x<-tmp[1];y<-tmp[2]
    theta<-(180/pi)*atan2(y,x)
  } else {
    theta<-0
  }
  #stopifnot(inherits(textVector, "character"))
  N<-length(textVector)
  if(N>2){
    xys<-seqPts(N,xy1,xy2)
    lapply(1:N, function(i){
      xy<-xys[,i]
      g(
        text(textVector[i], 
             xy=xy, 
             ...),
        transform=list(rotate=c(theta,xy))
      )
    })
  } else {
    NULL
  }
}
