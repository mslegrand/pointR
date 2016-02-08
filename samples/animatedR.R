#svgR elements: font-face glyph missing-glyph hkern vkern font font-face-name font-face-format font-face-uri animate animateColor animateMotion animateTransform set mpath feFuncA feFuncB feFuncG feFuncR feDistantLight feTurbulence feConvolveMatrix feDiffuseLighting feOffset filter feBlend feColorMatrix feComponentTransfer feComposite feDisplacementMap feFlood feGaussianBlur feImage feMerge feMorphology feSpecularLighting feTile feSpotLight fePointLight svg a altGlyph circle clipPath cursor defs ellipse foreignObject g image line linearGradient marker mask path pattern polygon polyline radialGradient rect script switch symbol text textPath tref tspan use view altGlyphDef altGlyphItem color-profile desc feMergeNode font-face-src glyphRef metadata stop style title font.face missing.glyph font.face.name font.face.format font.face.uri color.profile font.face.src getNode script translate rotate rotatR scale
WH<-c(600,620)

#Defined by mouse: edit with care!
Pts<-list(
  p=matrix(
    c(c( 151,500 ),c( 150,90 ),c( 351,88 ),c( 489,201 ),c( 389,324 ),c( 504,500 ),c( 442,503 ),c( 332,329 ),c( 204,331 ),c( 204,500 )),
    2,),
  q=matrix(
    c(c( 208,286 ),c( 209,140 ),c( 353,134 ),c( 435,206 ),c( 353,284 )),
    2,)
)

mkRPath<-function(p){
  n<-ncol(p)
  I<-c(2:n,1)
  m<-.5*(p+p[,I]) #midpoints
  #modify midpoints to get rounded R
  m[,3]<-p[,3]*(c(0,1))+p[,4]*c(c(1,0))
  m[,4]<-p[,5]*(c(0,1))+p[,4]*c(c(1,0))
  d<-rbind("L", p, "Q", m, p[,I])
  d<-c("M",p[,1], d )
}

mkCircularPath<-function(n, r, cxy=WH/2){
  I<-c(2:n,1)
  theta<-seq(pi,2*pi+pi,length.out=n)
  P<-r * rbind(cos(theta),sin(theta)) 
  P<-P +cxy
  eta<-pi/n
  R<-r/cos(eta)
  THETA<-theta+eta
  M<-R*rbind(cos(THETA),sin(THETA))
  M<-M + cxy
  d<-rbind("L",P,"Q",M,P[,I])
  d<-c("M",P[,1], d )
  d
}

R.outerD<-mkRPath(ptDefs$p)
R.innerD<-mkRPath(ptDefs$q)

circle.outerD<-mkCircularPath( ncol(ptDefs$p), r=200)
circle.innerD<-mkCircularPath( ncol(ptDefs$q), r=10)

dur=5

svgR( wh=WH,
      #your custom code goes here
      #text("R", xy=c(100,500), font.size=600, 
      #font.face='bold', fill='lightblue'),
      path(d=circle.outerD, stroke='black', fill='black',
           animate(
             attributeName='d', 
             values=list(circle.outerD, R.outerD ), 
             begin=0, 
             dur=dur, fill='remove'
           ),
           animate(attributeName='fill', values=c('grey','blue'),
                   begin=0, 
                   dur=dur, fill='remove'
           )
      ),
      path(d=circle.innerD, stroke='black', fill='white',
           animate(
             attributeName='d', dur=dur,         
             values=list(circle.innerD, R.innerD ),          
             begin=0, fill='remove'    
           )
      )
)
