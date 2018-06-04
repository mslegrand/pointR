toolbarSnippets=list(
  
circle=c( "Circle","circle.svg",
"	circle(
		cxy=${1:WH/2},
		r=${2:WH[2]/3},
		stroke='black',
		fill=${3:'none'}
	)"),
ellipse=c( "Ellipse","ellipse.svg",
"	ellipse(
		cxy=${1:WH/2},
		rxy=${2:c(.47,.3)*WH},
    stroke='black',
		fill=${3:'none'}
	)"),
rectangleXY=c( "Rectangle","rectangleXY.svg",
"	rect(
		xy=${1:WH/2},
		wh=${2:c(.47,.3)*WH},
    stroke='black',
		fill=${3:'none'}
	)
${0}"),
rectangleCXY=c( "Centered rectangle","rectangleCXY.svg",
"	rect(
		cxy=${1:WH/2},
		wh=${2:c(.47,.3)*WH},
    stroke='black',
		fill=${3:'none'}
	)"),
line=c( "Line segment","line.svg",
"	line(
		xy1=${1:c(0,0)},
		xy2=${2:WH},
		stroke=${3:'black'}
	)"),
polyline=c( "Polyline (connected line segments)","polyline.svg",
"	polyline(
		points=${1:WH*matrix(c(.25,.25,.5,.5,.75,.25),2)},
		stroke=${2:'black'},
    stroke.width=${2:1},
		fill=${3:'none'}
	)"),
polygon=c( "Polygon","polygon.svg",
"	polygon(
		points=${1:WH*matrix(c(.25,.75,.5,.5,.75,.75),2)},
		stroke=${2:'black'},
    stroke.width=${2:1},
		fill=${3:'none'}
	)"),
text=c( "Text","text.svg",
"	text(
    ${1:'hello world'},
		xy=${2:WH/2},
		stroke=${3:'black'},
    font.size=${4:36},
		fill=${5:'none'}
	)"),
textc=c( "Centered text","textCentered.svg",
"	text(
    ${1:'hello world'},
		cxy=${2:WH/2},
		stroke=${3:'black'},
    font.size=${4:36},
		fill=${5:'none'}
	)"),
# pathLines=c("PathLines","",
# "	text(
#     ${1:'hello world'},
# 		cxy=${2:WH/2},
# 		stroke=${3:'black'},
#     font.size=${4:36},
# 		fill=${5:'none'}
# 	)"),
arc=c("Arc","arc.svg",
                  "  path(
    d=list(
      ${1:M=c(.5,.2)*WH,} 
      A=${2:c( .3*WH, 180,1,0,c(.8,.2)*WH)}
    ),
    stroke=${3:'#00FFFF'},
    stroke.width=${4:1},
    fill=${5:'none'}
  )"), 
quadraticBezier=c("Quadratic Bezier","quadraticBezier.svg",
              "  path(
    d=list(
      ${1:M=c(.2,.2)*WH,} 
      Q=${2:c( c(.5,1.5),c(.8,.2))*WH}
    ),
    stroke=${3:'#00FFFF'},
    stroke.width=${4:1},
    fill=${5:'none'}
  )"), 
extendedQuadraticBezier=c("Extended Quadratic Bezier","extendedQuadraticBezier.svg",
                  "  path(
    d=list(
      ${1:M=c(.3,.1)*WH,} 
      Q=${2:c( c(1,.7),c(.5,.7))*WH},
      T=${3:c(.7,.1)*WH}
    ),
    stroke=${4:'#00FFFF'},
    stroke.width=${5:1},
    fill=${6:'none'}
  )"), 
cubicBezier=c("Cubic Bezier","cubicBezier.svg",
"  path(
    d=list(
      ${1:M=c(.2,.9)*WH,} 
      C=${2:c( c(.3,-1),c(.7,2), c(.8,.2))*WH}
    ),
    stroke=${3:'#00FFFF'},
    stroke.width=${4:1},
    fill=${5:'none'}
  )"), 
ExtendedCubicBezier=c("Extended Cubic Bezier","extendedCubicBezier.svg",
              "  path(
    d=list(
      M=c(.2,.6)*WH, 
      C=c( c(.4,.0),c(.4,.8), c(.5,.8))*WH,
      S=c( c(.6,.0),c(.8,.6))*WH    
    ),
    stroke=${3:'#00FFFF'},
    stroke.width=${4:1},
    fill=${5:'none'}
  )") #, 
# arrow=c("linearGradientQuick","",
#                   "	text(
#     ${1:'hello world'},
# 		cxy=${2:WH/2},
# 		stroke=${3:'black'},
#     font.size=${4:36},
# 		fill=${5:'none'}
# 	)")#,
# linearGradientQ=c("linearGradientEZ","",
#                   "	text(
#     ${1:'hello world'},
# 		cxy=${2:WH/2},
# 		stroke=${3:'black'},
#     font.size=${4:36},
# 		fill=${5:'none'}
# 	)"),
# linearGradientFull=c("linearGradient","",
#                      "	text(
#     ${1:'hello world'},
# 		cxy=${2:WH/2},
# 		stroke=${3:'black'},
#     font.size=${4:36},
# 		fill=${5:'none'}
# 	)"),
# setBE=c( "set with begin","",
# "	  set(
# 	    attributeName=${1:name},
# 	    from=${2:from_value},
# 	    to=${3:to_value},
# 	    begin=${4:begin},
# 	    end=${5:end}
# 	  )
# 	)"),
# setD=c( "animate Set with duration","",
# "	  set(
# 	    attributeName=${1:name},
# 	    from=${2:from_value},
# 	    to=${3:to_value},
# 	    dur=${4:seconds}
# 	  )
# 	)")
)


# animatetransform=c( "animatetransform","animatetransform.svg",
# 	)"),
# snippet animate
# snippet animatetransform
# snippet gradient
# snippet transform
# snippet filter
# snippet
