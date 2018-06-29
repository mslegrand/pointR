cmdFileSave<-function(){
  setTabRequest(sender="fileCmd.save", tabs=input$pages)
}

cmdFileSaveAll<-function(){
  sendFileTabsMessage(sender= 'fileCmd.save', getAllTabIds=runif(1))
}


  