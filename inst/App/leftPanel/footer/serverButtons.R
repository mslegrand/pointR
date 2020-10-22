
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
  
  
  replaceContents<-function(aceId, contents){
    updateAceExt(id=aceId, sender='mause.add', setValue=contents, setDocFileSaved=TRUE)
  }
  externalReplacement<-reactiveValues(
    aceId='', 
    newContents=''
  )
  
  #!!! move this elsewhere
  observeEvent(input$fileChanged,{
    # cat('Observed Event: fileChanged')
    changedFile<-input$fileChanged$mssg
    
    
    fd<-fileDescDB()
    fd$filePath<-normalizePath(fd$filePath)
    tb<-filter(fd, filePath==changedFile)
    # cat("fileChanged 2\n")
    if(nrow(tb)>0){
      # cat("fileChanged 2\n")
      pid<-tb$tabId
      aceId<-tabID2aceID(pid)
      # cat("aceId=",aceId)
      oldeContents<-input[[aceId]]
      # print(oldeContents)
      newContents<-paste(readLines(changedFile), collapse = "\n")
      if(newContents==oldeContents){
        # print("no update")
      } else {
        # print("update")
        if(tb$isSaved){
          # print("auto-update")
          replaceContents(aceId, newContents)
        } else {
          # ask
          externalReplacement$newContents=newContents
          externalReplacement$aceId=aceId
          # print("ask to update")
          fileName<-basename(changedFile)
          showModal(updateChangedFileModal(fileName))
        }
      }
    }
    
    
    
    
    
    
  
  })
  #--------change modal------------------------------
  
  updateChangedFileModal <- function(fileName='File') {
    txt1<-paste(fileName,'has been modified on disk.' )
    txt2<-paste('Reload from disk?')
    modalDialog(
      span(txt1), 
      br(),
      span(txt2),
      footer = tagList(
        modalButton("Keep as is"),
        actionButton("updateChangedFileButton", "Reload")
      )
    ) 
  }
  
  
  
  observeEvent(input$updateChangedFileButton, {
    replaceContents(externalReplacement$aceId, externalReplacement$newContents)
    removeModal()
  })
  
}



