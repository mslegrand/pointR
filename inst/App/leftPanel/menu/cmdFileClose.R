cmdFileClose<-function(){
  setTabRequest(sender="fileCmd.close", tabs=input$pages)
}

cmdFileCloseAll<-function(){
  cat('\n------------inside cmdFileCloseAll\n\n')
  session$sendCustomMessage(
    type = "scrollManager", 
    list( sender= 'fileCmd.close', getAllTabIds=TRUE ) 
  )
  cat('\n----------------------------\n')
}

observeEvent( input$closeTab, {
    if( !is.null(input$closeTab$id) ){
      id<-input$closeTab$id
      if(input$closeTab$type=='tabId'){
        tabId<-id
      } else {
        tabId<-aceID2TabID(id)
      }
      setTabRequest(sender="fileCmd.close", tabs=tabId)
    }
})



