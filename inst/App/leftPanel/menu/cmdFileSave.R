cmdFileSave<-function(){
  # if(getFileNameStatus()==TRUE){
  #   
  #   fullFilePath<-getCurrentFilePath()
  #     # editOption$.saved<-TRUE
  #   txt<-getCode() 
  #   writeLines(txt, fullFilePath)
  #   session$sendCustomMessage(
  #     type = "shinyAceExt", 
  #     list(id= getAceEditorId(), setClean=TRUE, sender='save', setOk=TRUE)
  #   )
  # }
  #request$closeTab=NULL
  #tabId<-input$pages
  #saveFile(tabId)
  #request$sender<-"fileCmd.save"
  #request$tabsToSave<-input$pages
  setTabRequest(sender="fileCmd.save", tabs=input$pages)
}

cmdFileSaveAll<-function(){
  sendFileTabsMessage(sender= 'fileCmd.save', getAllTabIds=runif(1))
}


  