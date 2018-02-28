toolbarSnippets=list(
circle=c( "circle","circle.svg",
"	circle(
		cxy=${1:center},
		r=${2:radius},
		fill=${3:fill}
	)"),
	ellipse=c( "ellipse","ellipse.svg",
"	ellipse(
		cxy=${1:center},
		rxy=${2:radii},
		fill=${3:fill}
	)"),
	rectangleXY=c( "rectangleXY","rectangleXY.svg",
"	rect(
		xy=${1:upper_left},
		wh=${2:width_height},
		fill=${3:fill}
	)"),
	rectangleCXY=c( "rectangleCXY","rectangleCXY.svg",
"	rect(
		cxy=${1:center},
		wh=${2:width_height},
		fill=${3:fill}
	)"),
	line=c( "line","line.svg",
"	line(
		xy1=${1:firstPoint},
		xy2=${2:secondPoint},
		stroke=${3:fill}
	)"),
	polyline=c( "polyline","polyline.svg",
"	polyline(
		points=${1:matrix},
		stroke=${2:color},
		fill=${3:fill}
	)"),
	polygon=c( "polygon","polygon.svg",
"	polygon(
		points=${1:matrix},
		stroke=${2:none},
		fill=${3:none}
	)"),
	set=c( "animate set","animateSet.svg",
"	  set(
	    attributeName=${1:name},
	    from=${2:from_alue},
	    to=${3:to_value},
	    begin=${4:begin},
	    end=${5:end}
	  )
	)")
)


# animatetransform=c( "animatetransform","animatetransform.svg",
# 	)"),
# snippet animate
# snippet animatetransform
# snippet gradient
# snippet transform
# snippet filter
# snippet
