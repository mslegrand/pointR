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
  panelName,
  logMssgs
){
  output$out_log<-renderText({ 
    if(panelName() %in% c( errorPanelTag, RPanelTag)){
      logMssgs()
    }
  })
}

errLogModuleList<-callModule( #auto  input, output, session 
  module=moduleLog, 
  id="errLogMod", 
  panelName=getRightMidPanel,
  logMssgs=getErrorMssg
)

captLogModuleList<-callModule( #auto  input, output, session 
  module=moduleLog, 
  id="capturedLogMod", 
  panelName=getRightMidPanel,
  logMssgs=getCapturedMssg
)



