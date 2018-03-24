# 0 either 
#    a. triggerRefresh rollback=false, sender is cmd.save
# or
#    b. commit
# 1 send message to ptRManager open save dialog
# 2 get the return, and set directory/name
# 3 write the file
cmdFileSaveAs<-function(){
  # if(getFileNameStatus()==TRUE){
  #   default<-getCurrentFile()
  # } else {
  #   default<-"Unnamed"
  # }
  cat('cmdFileSaveAs\n')
  request$closeTab=NULL;
  tabId<-input$pages
  
  sendManagerMessage( id=tabId,   sender='cmd.saveFileAs', saveFile=TRUE)
  # session$sendCustomMessage( #triggers click of buttonFileSaveHidden
  #   type = "ptRManager", 
  #   list(id= getAceEditorId(), saveFile=TRUE, sender='cmd.saveFileNow' )
  # )
}

observeEvent(input$buttonFileSaveHidden,{
  fp.dt<-parseSavePath(c(wd='~'), input$buttonFileSaveHidden)
  if(length(fp.dt)>0 && nrow(fp.dt)>0){
    datapath<-as.character(fp.dt$datapath[1])
    #updateAceExt( sender='cmd.File',  setDocFilePath=dataPath, rollBack=FALSE, getValue=TRUE )
    aceId<-getAceEditorId()
    cat("buttFileSaveHidden\n")
    #updateAceExt( id=aceId, request$sender,  setDocFileSaved=TRUE)
    updateAceExt( id=aceId, setDocFilePath=datapath,  sender='fileCmd.save', getDoc=TRUE)
    #may need to change the tab title
    # triggerRefresh(sender='cmd.saveFileNow', rollBack=FALSE, auxValue=datapath) #triggers refresh
    # then save source
  } else { #cancel saving
    request$closeTab=NULL;
  }
})


saveAsFile<-function(aceId){
  session$sendCustomMessage(
    type = "shinyAceExt", 
    list(id= aceId, sender='saveAsFile', getDoc=TRUE)
  )
}

