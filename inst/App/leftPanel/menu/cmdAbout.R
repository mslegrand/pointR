

cmdAbout<-function(){
  showModal( modalAbout() )
}

modalAbout <- function(..., size = "m" ) {
  modalDialog(
    div( pointRLogoSVG()),
    div( p(version, align="right")),
    div( width="100%",
      p(paste(
       "The purpose of this application is to provide an open-flexible tool for creating SVG using the svgR package.",
       "The svgR package is an R package to for creating SVG graphics.",
       "SVG is short for scalar vector graphics.",
       "Applications include Shiny apps, Rdocs, or even generating a plain SVG file.")),
      p("This software is licensed under the GPL-3 License"),
      p("Copyright (c) 2020 M. S. Legrand. All rights reserved")
    ),
    title="About",
    footer=tagList( modalButton("Dismiss")),
    easyClose = TRUE,
    ...
  ) 
}



