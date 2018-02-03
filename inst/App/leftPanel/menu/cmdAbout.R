

cmdAbout<-function(){
  showModal( modalAbout() )
}

modalAbout <- function(..., size = "m" ) {
  modalDialog(
    div( width="100%",
      p("SVG is short for scalar vector graphics."),
      p("The purpose of this application is to provide an open-flexible tool for creating SVG using the svgR package."),
      p("The svgR package is an R package to for creating SVG graphics."),
      p("Applications include Shiny apps, Rdocs, or even generating a plain SVG file."),
      p("This software is licensed under the MIT License"),
      p("Copyright (c) 2016 M. S. Legrand. All rights reserved")
    ),
    title="About",
    footer=tagList( modalButton("Dismiss")),
    easyClose = TRUE,
    ...
  ) 
}



