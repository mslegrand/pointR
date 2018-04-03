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

# saveFile<-function(tabId){
#   aceId<-tabID2aceID(tabId)
#   cat('saveFile...., with aceId=', aceId, "\n")
#   updateAceExt( id=aceId, sender=request$sender, getDoc=TRUE)
#   # session$sendCustomMessage(
#   #   type = "shinyAceExt", 
#   #   list(id= aceId, sender='saveFile', getDoc=TRUE)
#   # )
# }


cmdFileSaveAll<-function(){
  cat('starting in cmdFileSaveAll')
  session$sendCustomMessage(
    type = "scrollManager", 
    list( sender= 'fileCmd.save', getAllTabIds=TRUE ) 
  )
  cat('\n------exit cmdFileSaveAll------------\n')
}


  