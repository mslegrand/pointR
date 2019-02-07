modulePlotRmdUI <- function(id, input, output) { 
  ns <- NS(id)
  htmlOutput(ns( "rmd_Html" ))
}

modulePlotRmd<-function(input, output, session, 
  getPanelName,
  getCode
){
  
  output$rmd_Html <- renderUI({ 
    if(getPanelName() %in% rmdPanelTag){
      src<-getCode()
      # src<-request$code
      if(grepl("output: dnd_snippet",src)){
        # cat("founds\n")
        # cat('>--> dripplets2Rmd\n')
        src<-dripplets2Rmd(src)
        # cat(src)
        # cat('<--< dripplets2Rmd\n')
      }
      #  knit2html(text = src, fragment.only = FALSE, quiet = TRUE)
      div( style='background-color: #FFFFFF;',
        HTML(knit2html(text =src , fragment.only = TRUE, quiet = TRUE))
      )
     } else {
      HTML('')
    }
  }) #end of renderUI
  list()
}

rmdModuleList<-callModule(
  module=modulePlotRmd, 
  id="rmdMod", 
  getPanelName=getRightMidPanel,
  getCode=getCode4Rendering
)
