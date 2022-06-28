cmdFileClose<-function(){
  # if current tab has children
  # first add children, then current tab
  
  fd<-fileDescDB()
  aid<-getAceEditorId()
  aids<-filter(fileDescDB(), parId==aid)$tabId
  if(length(aids)>0){
    tabs<-aceID2TabID(aids)
    closeTabsNow(tabs)
  }
  setTabRequest(cmd="fileCmd.close", tabs=input$pages)
}

cmdFileCloseAll<-function(){
  # 1. get all tab Ids
  tabIds<-fileDescDB()$tabId
  # 2. sender= 'fileCmd.close'
  cmd= 'fileCmd.close'
  # 3. push onto request
  setTabRequest(cmd=cmd, tabs=tabIds)

  #sendFileTabsMessage(sender= 'fileCmd.close', getAllTabIds=runif(1))
}

observeEvent( input$closeTab, {
    if( !is.null(input$closeTab$id) ){
      log.fin(input$closeTab)
      id<-input$closeTab$id
      if(input$closeTab$type=='tabId'){
        tabId<-id
        aceId<-tabID2aceID(tabId)
      } else {
        aceId<-id
        tabId<-aceID2TabID(aceId)
      }
      aids<-filter(fileDescDB(), parId==aceId & filePath=="?")$tabId # exclude from deletion any tabs with paths assigned
      if(length(aids)>0){
        tabs<-aceID2TabID(aids)
        closeTabsNow(tabs)
      }
      #removeFileDesc(tabId)
      setTabRequest(cmd="fileCmd.close", tabs=tabId)
      log.fin(input$closeTab)
    }
} , label= "input$closeTab")



