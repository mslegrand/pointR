
#---commit  button----- 
observeEvent(input$commitButton, {
    # cat('>---> input$commitButton\n')
    triggerRefresh(sender='cmd.commit', rollBack=FALSE)
    # cat('<---< input$commitButton\n')
})

# commitMssg is triggered by ace upon ctl+shift+enter
observeEvent(input$commitMssg, {
  # cat('>---> input$input$commitMssg\n')
  triggerRefresh(sender='cmd.commit', rollBack=FALSE)
  # cat('<---< input$input$commitMssg\n')
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
    # cat('>-----> input$writeNRunApp')
    pageId<-input$pages
    appRunner$tabId<-pageId
    appRunner$log<-""
    app2RunPath<-getFileDescriptor(pageId)$filePath
    #app2RunPath<-"/home/sup/svgRHabitat/ptR-Master/widget" # TODO!!! edit/refactor this
    sendPtRManagerMessage(sender='cmd.electron', app2RunPath=app2RunPath, tabId=pageId)
  }, label= "writeNRunApp")
 
  observeEvent(input$stopShinyApp ,{
    # cat('>-----> input$writeNRunApp')
    # pageId<-input$pages
    # appRunner$tabId<-pageId
    # appRunner$log<-""
    #app2RunPath<-getFileDescriptor(pageId)$filePath
    #app2RunPath<-"/home/sup/svgRHabitat/ptR-Master/widget" # TODO!!! edit/refactor this
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
    # cat('>---> input$appLog\n')
    # cat("class(input$appLog$mssg )=" , class(input$appLog$mssg ), "\n")
    appRunner$log<-c(appRunner$log, input$appLog$mssg)
    # cat("input$appLog$mssg=" , input$appLog$mssg , "\n")
    # cat(appRunner$log)
    # cat('<---< input$appLog\n')
  }, label="appLog")
  
}



