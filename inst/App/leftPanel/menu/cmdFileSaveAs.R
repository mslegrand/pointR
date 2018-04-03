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
  # cat('cmdFileSaveAs\n')
  # request$closeTab=NULL;
  # tabId<-input$pages
  # sendPtRManagerMessage( id=tabId,  sender='cmd.saveFileAs', saveFile=TRUE, closing=!is.null(request$closeTab), type='R')
  
  setTabRequest(sender="fileCmd.saveAs", tabs=input$pages)
  #sendPtRManagerMessage( id=tabId,   sender='cmd.saveFileAs', saveFile=TRUE)
  # session$sendCustomMessage( #triggers click of buttonFileSaveHidden
  #   type = "ptRManager", 
  #   list(id= getAceEditorId(), saveFile=TRUE, sender='cmd.saveFileNow' )
  # )
}

observeEvent(input$buttonFileSaveHidden,{
  cat('=======shinyFiles SAVE RETURN EVENT=============\n')
  rtList<-input$buttonFileSaveHidden
  cat('class(rtList)=',class(rtList),"\n")
  print(format(rtList))
  if('cancel' %in% names(rtList)){
    #if(request$sender=='fileCmd.close'){
      #if(!is.null(request$closeTab)){
    if(rtList[[1]]=='close'){ 
      cat('cancel and close')
      #removeTab(inputId = "pages", request$closeTab)
      
      tabId<-popTab()
      cat('tabId=',format(tabId),"\n")
      removeTab(inputId = "pages", tabId)
      # TODO!!!  add oldPath to recentFiles
      cat('tab should be gone by now')
      #removeTab(inputId = "pages", request$closeTab)
      #request$closeTab=NULL;
    } else {
      cat('canceling a request')
      setTabRequest(sender=NULL, tabs=NULL)
    }
  } else { 
    fp.dt<-parseSavePath(c(wd='~'), input$buttonFileSaveHidden)
    if(length(fp.dt)>0 && nrow(fp.dt)>0){
      #cat('=======shinyFiles SAVE=============\n')
      datapath<-as.character(fp.dt$datapath[1])
      # 
      
      # TODO!!!  add oldPath to recentFiles (this assumes that we can do a saveAs)
      #  The safest time is after the file has been saved under the new path, but that
      #  means the old filePath needs to be kept just a little longer ...
      #
      # We just got a new path name  So ...
      # 
      # 1. if sender is fileCmd.saveAs, change to sender = 'fileCmd.save' since we don't need to get a new path anymore
      # 2. send to aceExt.js new path  (sender will be either fileCmd.save or fileCmd.close)
      # 3. aceExt.js returns to serverAce.R with 
      #      sender either fileCmd.save or fileCmd.close
      #      the new docFilePath 
      #      saved=FALSE
      #      code
      # 4. serverAce.R 
      #      saves to docFilePath
      #      sets doc as SAVED
      #      If sender==close, removeTab
      #      ow. update tab title
      sender=request$sender
      if(request$sender=='fileCmd.saveAs'){
        sender='fileCmd.save'
      }
      tabId<-peekTab()
      aceId<-tabID2aceID(tabId)
  
      # Now the sender can be close, save or saveAs, but we leave this to ace, then we need a flag to say that we changed the name!
      updateAceExt( id=aceId, setDocFilePath=datapath,  sender=sender, getDoc=TRUE)
      
    }
  }
})


# saveAsFile<-function(aceId){
#   session$sendCustomMessage(
#     type = "shinyAceExt", 
#     list(id= aceId, sender='saveAsFile', getDoc=TRUE)
#   )
# }

