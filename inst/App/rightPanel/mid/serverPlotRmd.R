modulePlotRmdUI <- function(id, input, output) { 
  ns <- NS(id)
  htmlOutput(ns( "rmd_Html" ))
}

modulePlotRmd<-function(input, output, session, 
  getPanelName,
  getCode
){
  
  output$rmd_Html <- renderUI({ 
    #HTML('')
    if(getPanelName() %in% rmdPanelTag){
      src<-getCode()
      if(grepl("output: dnd_snippet",src)){
        cat("founds\n")
        src<-dripplets2Rmd(src)
        cat(src)
      }
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
  getCode=getCode 
)
