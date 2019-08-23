cmdFileSave<-function(){
  setTabRequest(sender="fileCmd.save", tabs=input$pages)
}

cmdFileSaveAll<-function(){
  sendFileTabsMessage(sender= 'fileCmd.saveNow', getAllTabIds=runif(1))
}


  