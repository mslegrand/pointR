

cmdAbout<-function(){
  showModal( modalAbout() )
}

modalAbout <- function(..., size = "m" ) {
  modalDialog(
    div( width="100%",
      textOutput(outputId = "aboutOut")
    ),
    title="About",
    footer=tagList( modalButton("Dismiss")),
    easyClose = TRUE,
    ...
  ) 
}

output$aboutOut<-renderText(
  paste0(
    c(
      "SVG is short for scalar vector graphics.",
      "The purpose of this application is to provide an open-flexible tool for creating SVG using svgR.",
      "This software is licensed under the MIT License",
      "Copyright (c) 2016 M. S. Legrand. All rights reserved"
    ),
    collapse="\n"
  )
)

