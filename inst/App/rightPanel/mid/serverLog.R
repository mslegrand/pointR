# http://shiny.rstudio-staging.com/articles/dynamic-ui.html

#-----log panel---------------------------

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

if(usingElectron){
  aptRunnerLogModuleList<-callModule( #auto  input, output, session 
  module=moduleLog, 
  id="aptRunnerLogMod", 
  panelName=getRightMidPanel,
  logMssgs=getAptRunnerMssg
  )
}



