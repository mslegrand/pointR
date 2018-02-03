# http://shiny.rstudio-staging.com/articles/dynamic-ui.html

#-----log panel---------------------------




moduleLogUI <- function(id, input, output) { 
  ns <- NS(id)
  absolutePanel( 
    draggable=FALSE,
    "class"="cLogText",
    verbatimTextOutput(ns("out_log"))
  )
}

moduleLog<-function(
  input, output, session, 
  id,
  barName,
  logMssgs
){
  output$out_log<-renderText({ 
    if(barName()=="logPanel"){
      logMssgs()
    }
  })
}

errLogModuleList<-callModule( #auto  input, output, session 
  module=moduleLog, 
  id="errLogMod", 
  barName=getRightMidPanel2,
  logMssgs=getErrorMssg
)

