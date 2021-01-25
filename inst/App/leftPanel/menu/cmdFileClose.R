cmdFileClose<-function(){
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
      } else {
        tabId<-aceID2TabID(id)
      }
      #removeFileDesc(tabId)
      setTabRequest(cmd="fileCmd.close", tabs=tabId)
      log.fin(input$closeTab)
    }
} , label= "input$closeTab")



