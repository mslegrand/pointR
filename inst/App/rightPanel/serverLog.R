# http://shiny.rstudio-staging.com/articles/dynamic-ui.html

#-----log panel---------------------------

# moduleLogUI<-function(id, input, output) { 
#   ns <- NS(id)
#   absolutePanel( top=50, left=0, width="100%", draggable=TRUE,
#                  tabsetPanel( ns("transformOption"), 
#                               tabPanel("Translate"), 
#                               tabPanel("Rotate"), 
#                               tabPanel("Scale"),
#                               type="pills"
#                  ) 
#   )
# } 


moduleLogUI <- function(id, input, output) { 
  ns <- NS(id)
  absolutePanel( 
    draggable=FALSE,
    "class"="cLogText",
    verbatimTextOutput(ns("out_log"))
    #textOutput(ns("out_log"))
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
  barName=rightPanel,
  logMssgs=getErrorMssg
)

