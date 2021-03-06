
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
    if(panelName() %in% c( errorPanelTag, RPanelTag, appPanelTag)){
      txt<-logMssgs()
      if(length(txt)==0){
        txt="no text"
      }
      txt
    } else {
      txt=cat(format(panelName()))
    }
    txt
  })
}
