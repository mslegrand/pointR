
#---commit  button----- 
observeEvent(input$commitButton, {
    triggerRefresh(sender='cmd.commit', rollBack=FALSE)
})

# commitMssg is triggered by ace upon ctl+shift+enter
observeEvent(input$commitMssg, {
  triggerRefresh(sender='cmd.commit', rollBack=FALSE)
})


#---commit rmdView button----- 
observeEvent(input$writeNOpen ,{
  setTabRequest(sender='buttonCmd.rmdViewer', tabs=input$pages)
}, label= "writeNOpen")

appRunner<-reactiveValues(
  tabId="",
  log=""
)
  
if(usingElectron){

  observeEvent(input$writeNRunApp ,{
    pageId<-input$pages
    appRunner$tabId<-pageId
    appRunner$log<-""
    selection<-getAllNamedUnsavedFiles()$tabId
    if(length(selection)>0){
      setTabRequest(sender='fileCmd.runApp', tabs=selection)
    } else {
      app2RunPath<-getFileDescriptor(appRunner$tabId)$filePath
      sendPtRManagerMessage(sender='cmd.electron', app2RunPath=app2RunPath, tabId= appRunner$tabId)
    }
    
  }, label= "writeNRunApp")
 
  observeEvent(input$stopShinyApp ,{
    sendPtRManagerMessage(sender='cmd.electron', app2stop=appRunner$tabId)
  }, label= "stopShinyApp")
  
  observeEvent(input$appStatus,{
    if(identical(input$appStatus$mssg,'loaded')){
      #appRunner$log<-""
      appRunner$tabId<-input$appStatus$tabId
      disable("writeNRunApp")
      enable("stopShinyApp")
    } else {
      #appRunner$log<-""
      enable("writeNRunApp")
      disable("stopShinyApp")
    }
  }, label="appStatus")
  
  observeEvent(input$appLog,{
    appRunner$log<-c(appRunner$log, input$appLog$mssg)
  }, label="appLog")
  
}



