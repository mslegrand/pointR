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
  cat('cmdFileSave -----')
  request$closeTab=NULL
  tabId<-input$pages
  saveFile(tabId)
}

saveFile<-function(tabId){
  aceId<-tabName2AceId(tabId)
  cat('saveFile....')
  updateAceExt( id=aceId, sender='fileCmd.save', getDoc=TRUE)
  # session$sendCustomMessage(
  #   type = "shinyAceExt", 
  #   list(id= aceId, sender='saveFile', getDoc=TRUE)
  # )
}


