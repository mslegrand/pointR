cmdFileClose<-function(){
  setTabRequest(sender="fileCmd.close", tabs=input$pages)
}

cmdFileCloseAll<-function(){
  sendFileTabsMessage(sender= 'fileCmd.close', getAllTabIds=runif(1))
}

observeEvent( input$closeTab, {
    if( !is.null(input$closeTab$id) ){
      cat('>---> input$closeTab\n')
      id<-input$closeTab$id
      if(input$closeTab$type=='tabId'){
        tabId<-id
      } else {
        tabId<-aceID2TabID(id)
      }
      #removeFileDesc(tabId)
      setTabRequest(sender="fileCmd.close", tabs=tabId)
      cat('<---< input$closeTab\n')
    }
} , label= "input$closeTab")



